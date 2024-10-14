/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2024 Ayrton Muñoz
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

use core::mem::transmute;
use core::ffi::{c_void, c_int};
use alloc::boxed::Box;
use crate::{PointsTo, RefMut, Ptr, ErrCode, Result, FFICell, bindings};
use crate::bindings::{task_fn_t, task, taskqueue};
use crate::allocator::KernelAllocator;

pub struct Task {
    inner: FFICell<task>,
    is_initialized: bool,
}

// TODO: assert this is ABI-compatible with task_fn_t
pub type RawTaskFn = extern "C" fn(context: *mut c_void, pending: c_int);
// TODO: remove unsafe
pub type TaskFn<P> = unsafe extern "C" fn(context: P, pending: c_int);

impl Task {
    pub fn new_in_heap(flags: KernelAllocator) -> RefMut<Self> {
        RefMut::new_in_heap(Task::new(), flags)
    }

    pub fn new() -> Self {
        Self {
            inner: FFICell::zeroed(),
            is_initialized: false,
        }
    }
}

use crate::bindings::rtkit_task;
impl PointsTo<rtkit_task> for *mut rtkit_task {
    fn as_ptr(&self) -> *mut rtkit_task {
        *self
    }
}

impl RefMut<Task> {
    pub fn init<T, P: PointsTo<T>>(&mut self, callback: TaskFn<P>, ctx: P) {
        let inner_ptr = self.inner.get_out_ptr();
        let callback = unsafe { transmute(callback) };
        let ctx = ctx.as_ptr().cast();
        unsafe {
            get_field!(inner_ptr, ta_func).write(Some(callback));
            get_field!(inner_ptr, ta_context).write(ctx);
        }
        self.is_initialized = true;
    }

    pub fn enqueue(&mut self, tq: *mut taskqueue) -> Result<()> {
        if !self.is_initialized {
            return Err(ErrCode::EPERM);
        }
        let task_ptr = self.inner.get_out_ptr().as_ptr();
        let res = unsafe {
            bindings::taskqueue_enqueue(tq, task_ptr)
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }
}
