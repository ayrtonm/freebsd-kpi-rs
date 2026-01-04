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
use crate::bindings::{task, task_fn_t, taskqueue};
use crate::ffi::{ArrayCString, Ext, MutExtRef, SyncPtr};
use crate::intr::Priority;
use crate::malloc::MallocFlags;
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::c_void;
use core::mem::transmute;
use core::ptr::null_mut;

#[derive(Debug)]
pub struct Taskqueue {
    inner: SyncPtr<taskqueue>,
}

impl Taskqueue {
    pub fn new() -> Self {
        Self {
            inner: SyncPtr::new(null_mut()),
        }
    }
}

pub type TaskFn<T> = extern "C" fn(Ext<T>, u32);

#[derive(Debug)]
pub struct Task {
    inner: UnsafeCell<task>,
}

impl Task {
    pub fn new() -> Self {
        Self {
            inner: UnsafeCell::new(task::default()),
        }
    }

    pub fn init<T>(&self, func: TaskFn<T>, arg: Ext<T>) {
        let c_task = self.inner.get();
        unsafe {
            (*c_task).ta_func = transmute::<Option<TaskFn<T>>, task_fn_t>(Some(func));
            (*c_task).ta_context = Ext::into_raw(arg).cast::<c_void>();
        }
    }
}

unsafe impl Sync for Task {}
unsafe impl Send for Task {}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;

    // Max queue name is 32 chars which is over the ArrayCString limit
    pub fn taskqueue_create(
        name: ArrayCString,
        flags: MallocFlags,
        mut queue: MutExtRef<Taskqueue>,
    ) -> Result<()> {
        let ctx: *mut *mut bindings::taskqueue = &raw mut queue.inner.0;

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
        queue.inner = SyncPtr::new(res);
        Ok(())
    }

    // Max queue name is 32 chars which is over the ArrayCString limit
    pub fn taskqueue_create_fast(
        name: ArrayCString,
        flags: MallocFlags,
        mut queue: MutExtRef<Taskqueue>,
    ) -> Result<()> {
        let ctx: *mut *mut bindings::taskqueue = &raw mut queue.inner.0;

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
        queue.inner = SyncPtr::new(res);
        Ok(())
    }

    pub fn taskqueue_start_threads(
        mut queue: MutExtRef<Taskqueue>,
        count: usize,
        prio: Priority,
        name: ArrayCString,
    ) -> Result<()> {
        let queuep = &raw mut queue.inner.0;
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

    pub fn taskqueue_enqueue(queue: &Taskqueue, ta: Ext<Task>) -> Result<()> {
        let queuep = queue.inner.as_ptr();
        let c_task = ta.inner.get();
        let res = unsafe { bindings::taskqueue_enqueue(queuep, c_task) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }
}
