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

use crate::bindings::{task, taskqueue};
use crate::kpi_prelude::*;
use core::ffi::{c_int, c_void};
use core::mem::transmute;

pub type TaskQueue = *mut taskqueue;
pub type Task = task;

pub fn thread() -> TaskQueue {
    unsafe { bindings::taskqueue_thread }
}

// TODO: assert this is ABI-compatible with task_fn_t
pub type TaskFn<F> = extern "C" fn(context: Box<SubClass<Task, F>>, pending: c_int);

impl<F> SubClass<Task, F> {
    pub fn enqueue_with_self(self: Box<Self>, callback: TaskFn<F>, queue: TaskQueue) -> Result<()> {
        let callback = unsafe { transmute(callback) };
        let task = SubClass::get_base_ptr(self.as_ref());
        let ctx = Box::into_raw(self).cast::<c_void>();
        // SAFETY: The pointer to ta_func is derived from Box<Self> so nothing else on the rust side
        // can access it.
        // TODO: I don't think this needs to be synchronized with C side accesses but that's TBD
        unsafe {
            get_field!(task, ta_func).write(Some(callback));
        }
        // SAFETY: Same justification as ta_func. Additionally the function signature's context
        // argument matches the Box that the `ctx` pointer was derived from. The implicit cast back
        // from *mut c_void `ctx` to `Box<SubClass<Task, F>>` is legal because SubClass: Sized so
        // Box<SubClass<_, _>> is ABI-compatible with C pointers.
        unsafe {
            get_field!(task, ta_context).write(ctx);
        }
        // SAFETY: The necessary fields in task were initialized and the context will be alive when
        // ta_func is called because it's on the heap.
        let res = unsafe { bindings::taskqueue_enqueue(queue, task.as_ptr()) };
        if res != 0 {
            // TODO: don't leak the box here
            return Err(ErrCode::from(res));
        }
        Ok(())
    }
}
