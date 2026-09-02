/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2025 Ayrton Muñoz
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

use crate::ErrCode;
use crate::bindings::{task, taskqueue, u_int};
use crate::ffi::{ArrayCString, Lease, Loan};
use crate::intr::Priority;
use crate::malloc::MallocFlags;
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::c_void;
use core::marker::PhantomPinned;
use core::pin::Pin;
use core::ptr::null_mut;
use core::sync::atomic::{AtomicPtr, Ordering};

/// A pointer to a taskqueue struct.
///
/// taskqueue_* functions rely on **the pointer's** address (read &sc->sc_taskq, not the address
/// that sc->sc_taskq points to) being stable so they take a Pin<&Taskqueue> argument. To get a
/// Pin<&Taskqueue> from a Taskqueue softc field use `proj!(&sc.sc_taskq)`.
pub struct Taskqueue {
    inner: AtomicPtr<taskqueue>,
    _pin: PhantomPinned,
}

impl Taskqueue {
    pub fn new() -> Self {
        Self {
            inner: AtomicPtr::new(null_mut()),
            _pin: PhantomPinned,
        }
    }

    fn ptr(&self) -> *mut taskqueue {
        self.inner.load(Ordering::Relaxed)
    }
}

impl Drop for Taskqueue {
    fn drop(&mut self) {
        let ptr = self.ptr();
        if !ptr.is_null() {
            // taskqueue_free may block which I'd like to avoid in Drop impls. Requiring the user
            // call it manually and panicking if they did not is the only reasonable thing to do.
            panic!("Taskqueue dropped without being freed");
        }
    }
}

pub type TaskFn<T> = extern "C" fn(Loan<'_, T>, u32);

pub struct Task {
    inner: UnsafeCell<task>,
    count_ptr: AtomicPtr<u_int>,
    _pin: PhantomPinned,
}

unsafe impl Sync for Task {}
unsafe impl Send for Task {}

impl Task {
    const UNINIT: *mut u_int = null_mut();
    const BUSY: *mut u_int = 1 as *mut u_int;

    pub fn new() -> Self {
        Self {
            inner: UnsafeCell::new(task::default()),
            count_ptr: AtomicPtr::new(null_mut()),
            _pin: PhantomPinned,
        }
    }

    pub fn init<T: 'static + Sync>(&self, func: TaskFn<T>, lease: Lease<T>) -> Result<(), Lease<T>> {
        if self
            .count_ptr
            .compare_exchange(
                Self::UNINIT,
                Self::BUSY,
                Ordering::Relaxed,
                Ordering::Relaxed,
            )
            .is_err()
        {
            return Err(ErrCode::with_payload(EDOOFUS, lease));
        }
        let (ctx, count_ptr) = Lease::into_raw(lease);

        unsafe {
            let c_task = self.inner.get();
            (*c_task).ta_context = ctx.cast::<c_void>();
            (*c_task).ta_func = Some(core::mem::transmute(func));
        };

        self.count_ptr.store(count_ptr, Ordering::Release);
        Ok(())
    }
}

impl Drop for Task {
    fn drop(&mut self) {
        unsafe {
            let c_task = self.inner.get();
            // TODO: According to _task.h this must be accessed while holding the queue lock.
            // That will probably some slight changes on the C side
            if (*c_task).ta_pending != 0 {
                panic!("tried to drop enqueued task")
            }
            (*c_task).ta_context = null_mut();
        }
        let count_ptr = self.count_ptr.load(Ordering::Relaxed);
        if !count_ptr.is_null() {
            let last = unsafe { bindings::refcount_release(count_ptr) };
            assert!(!last);
        }
    }
}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;

    pub fn taskqueue_create(
        name: ArrayCString,
        flags: MallocFlags,
        queue: Pin<&Taskqueue>,
    ) -> Result<()> {
        // This gets cast to a *mut c_void so annotate the src pointer type out of an abundance of
        // caution in case the context were to change.
        let ctx: *mut *mut taskqueue = queue.inner.as_ptr();

        let enqueue = Some(bindings::taskqueue_thread_enqueue as _);
        let res = unsafe {
            bindings::taskqueue_create(
                name.as_c_str().as_ptr(),
                flags.0,
                enqueue,
                ctx.cast::<c_void>(),
            )
        };
        if res.is_null() {
            return Err(ENULLPTR);
        };
        queue.inner.store(res, Ordering::Relaxed);
        Ok(())
    }

    pub fn taskqueue_create_fast(
        name: ArrayCString,
        flags: MallocFlags,
        queue: Pin<&Taskqueue>,
    ) -> Result<()> {
        // This gets cast to a *mut c_void so annotate the src pointer type out of an abundance of
        // caution in case the context were to change.
        let ctx: *mut *mut taskqueue = queue.inner.as_ptr();

        let enqueue = Some(bindings::taskqueue_thread_enqueue as _);
        let res = unsafe {
            bindings::taskqueue_create_fast(
                name.as_c_str().as_ptr(),
                flags.0,
                enqueue,
                ctx.cast::<c_void>(),
            )
        };
        if res.is_null() {
            return Err(ENULLPTR);
        };
        queue.inner.store(res, Ordering::Relaxed);
        Ok(())
    }

    pub fn taskqueue_start_threads(
        queue: &Taskqueue,
        count: usize,
        prio: Priority,
        name: ArrayCString,
    ) -> Result<()> {
        if queue.ptr().is_null() {
            return Err(EDOOFUS);
        }
        let queuep = queue.inner.as_ptr();
        let res = unsafe {
            bindings::taskqueue_start_threads(
                queuep,
                count.try_into().unwrap(),
                prio.0,
                name.as_c_str().as_ptr(),
            )
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn taskqueue_enqueue(queue: &Taskqueue, ta: Pin<&Task>) -> Result<()> {
        let count_ptr = ta.count_ptr.load(Ordering::Acquire);
        if count_ptr.is_null() || count_ptr == Task::BUSY {
            return Err(EDOOFUS);
        }
        let queuep = queue.ptr();
        let c_task = ta.inner.get();
        let res = unsafe { bindings::taskqueue_enqueue(queuep, c_task) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn taskqueue_drain(queue: &Taskqueue, ta: Pin<&Task>) {
        let queuep = queue.ptr();
        let c_task = ta.inner.get();
        unsafe { bindings::taskqueue_drain(queuep, c_task) };
    }

    pub fn taskqueue_free(queue: &Taskqueue) -> Result<()> {
        let ptr = queue.ptr();
        if ptr.is_null() {
            return Err(EDOOFUS);
        }
        unsafe { bindings::taskqueue_free(ptr) };
        queue.inner.store(null_mut(), Ordering::Relaxed);
        Ok(())
    }
}
