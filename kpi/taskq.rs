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
use crate::cell::SubClass;
use crate::prelude::*;
use crate::ErrCode;
use core::ffi::{c_int, c_void};
use core::mem::{forget, transmute};

pub type Task<T> = SubClass<task, T>;

pub struct Taskqueue(*mut taskqueue);

// TODO: type check with task_fn_t modulo the unsafe/Option
pub type TaskFn<T> = extern "C" fn(context: Box<Task<T>>, pending: c_int);

impl<T: 'static + Send> Task<T> {
    pub fn init(&mut self, callback: TaskFn<T>) {
        let callback = unsafe { transmute(callback) };
        let task_ptr = SubClass::get_base_ptr(&self);
        unsafe {
            (*task_ptr).ta_func = Some(callback);
        }
    }
}
pub mod wrappers {
    use super::*;

    pub fn taskqueue_thread() -> Taskqueue {
        // Needs unsafe since bindgen generates a static mut but the pointer shouldn't change after
        // initialization
        let ptr = unsafe { bindings::taskqueue_thread };
        Taskqueue(ptr)
    }

    pub fn taskqueue_enqueue<T: 'static + Send>(queue: Taskqueue, task: Box<Task<T>>) -> Result<()> {
        let task_ptr = SubClass::get_base_ptr(&task);
        unsafe {
            (*task_ptr).ta_context = task_ptr as *mut c_void;
        }
        let res = unsafe { bindings::taskqueue_enqueue(queue.0, task_ptr) };
        if res != 0 {
            // If we could not enqueue the task return ownership of self to the callee
            return Err(ErrCode::from(res));
        }
        // If we enqueued the task and context skip running self's destructor at the end of this function
        // The destructor will instead run at the end of the enqueued task's callback
        forget(task);
        Ok(())
    }
}
