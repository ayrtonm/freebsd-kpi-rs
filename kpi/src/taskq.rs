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

use crate::ErrCode;
use crate::bindings::{task, task_fn_t, taskqueue};
use crate::boxed::Box;
use crate::ffi::ExtRef;
use crate::prelude::*;
use crate::sync::arc::Arc;
use core::cell::UnsafeCell;
use core::ffi::c_void;
use core::mem::{forget, transmute};

pub type TaskFn<T> = extern "C" fn(ExtRef<T>, u32);

#[derive(Debug)]
pub struct Task {
    inner: UnsafeCell<task>,
}

impl Task {
    pub fn new() -> Self {
        let c_task = task::default();
        Self {
            inner: UnsafeCell::new(c_task),
        }
    }

    pub fn init<T>(&mut self, func: TaskFn<T>, arg: Arc<T>) {
        let func = unsafe { transmute::<Option<TaskFn<T>>, task_fn_t>(Some(func)) };
        let arg_ptr = Arc::into_raw(arg);
        let c_task = self.inner.get_mut();
        c_task.ta_func = func;
        c_task.ta_context = arg_ptr.cast::<c_void>();
    }
}

pub struct Taskqueue(*mut taskqueue);

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;

    pub fn taskqueue_thread() -> Taskqueue {
        // Needs unsafe since bindgen generates a static mut but the pointer shouldn't change after
        // initialization
        let ptr = unsafe { bindings::taskqueue_thread };
        Taskqueue(ptr)
    }

    pub fn taskqueue_enqueue(queue: Taskqueue, task: Box<Task>) -> Result<()> {
        let c_task = task.inner.get();
        forget(task);
        let res = unsafe { bindings::taskqueue_enqueue(queue.0, c_task) };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }
}
