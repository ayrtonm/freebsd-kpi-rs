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
use crate::bindings::{task, taskqueue};
use crate::ffi::{ArrayCString, Lease, Loan, LeaseSlot};
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
        // Only call taskqueue_free if a taskqueue was actually created
        if !ptr.is_null() {
            unsafe { bindings::taskqueue_free(ptr) };
        }
    }
}

pub type TaskFn<T> = extern "C" fn(Loan<'_, T>, u32);

pub struct Task<T: 'static> {
    inner: UnsafeCell<task>,
    arg: LeaseSlot<T>,
    _pin: PhantomPinned,
}

unsafe impl<T: Sync + Send> Sync for Task<T> {}
unsafe impl<T: Sync + Send> Send for Task<T> {}

impl<T> Task<T> {
    pub fn new() -> Self {
        Self {
            inner: UnsafeCell::new(task::default()),
            arg: LeaseSlot::uninit(),
            _pin: PhantomPinned,
        }
    }

    pub fn init(&self, func: TaskFn<T>, lease: Lease<T>) {
        let ctx = lease.0.as_ptr().cast::<c_void>();
        self.arg.init(lease);
        unsafe {
            let c_task = self.inner.get();
            (*c_task).ta_context = ctx;
            (*c_task).ta_func = Some(core::mem::transmute(func));
        }
    }
}

impl<T> Drop for Task<T> {
    fn drop(&mut self) {
        // TODO: release the LeaseSlot and NULL out ta_context
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

    pub fn taskqueue_enqueue<T>(queue: &Taskqueue, ta: Pin<&Task<T>>) -> Result<()> {
        let queuep = queue.ptr();
        let c_task = ta.inner.get();
        let callback = unsafe { (*c_task).ta_func };
        if callback.is_none() {
            return Err(EDOOFUS);
        }
        let res = unsafe { bindings::taskqueue_enqueue(queuep, c_task) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn taskqueue_drain<T>(queue: &Taskqueue, ta: Pin<&Task<T>>) {
        let queuep = queue.ptr();
        let c_task = ta.inner.get();
        unsafe { bindings::taskqueue_drain(queuep, c_task) };
    }
}
