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

use crate::err_codes::*;
use crate::allocator::KernelAllocator;
use crate::bindings::{task, task_fn_t, taskqueue};
use crate::{bindings, OutPtr, ErrCode, FFICell, PointsTo, Ptr, RefMut, Result};
use alloc::boxed::Box;
use core::ffi::{c_int, c_void};
use core::mem::transmute;

pub type TaskQueue = *mut taskqueue;

pub struct Task {
    inner: FFICell<task>,
}

// TODO: assert this is ABI-compatible with task_fn_t
pub type RawTaskFn = extern "C" fn(context: *mut c_void, pending: c_int);
// TODO: remove unsafe
pub type TaskFn<P> = unsafe extern "C" fn(context: P, pending: c_int);

impl Task {
    pub fn new() -> Self {
        Self {
            inner: FFICell::zeroed(),
        }
    }

    pub fn init<T, P: PointsTo<T>>(task: &mut Box<Task>, callback: TaskFn<P>, ctx: P) {
        let callback = unsafe { transmute(callback) };
        let ctx = ctx.as_ptr().cast();
        let inner_ptr = task.inner.get_out_ptr();
        unsafe {
            get_field!(inner_ptr, ta_func).write(Some(callback));
            get_field!(inner_ptr, ta_context).write(ctx);
        }
    }

    pub fn enqueue(self: &mut Box<Task>, tq: TaskQueue) -> Result<()> {
        let inner_ptr = self.inner.get_out_ptr();
        let old_func = get_field!(inner_ptr, ta_func).as_ptr();
        if old_func.is_null() {
            return Err(EDOOFUS);
        }
        let task_ptr = self.inner.get_out_ptr().as_ptr();
        let res = unsafe { bindings::taskqueue_enqueue(tq, task_ptr) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    //pub fn init_and_enqueue<T, P: PointsTo<T>>(
    //    &mut self,
    //    callback: TaskFn<P>,
    //    ctx: P,
    //    tq: *mut taskqueue,
    //) -> Result<()> {
    //    self.init(callback, ctx);
    //    self.enqueue(tq)
    //}
}
