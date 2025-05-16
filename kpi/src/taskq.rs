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
use crate::boxed::Box;
use crate::cell::OwnedVar;
use crate::cell::SubClass;
use crate::malloc::MallocType;
use crate::prelude::*;
use crate::ErrCode;
use core::ffi::{c_int, c_void};
use core::mem::{forget, transmute};
use core::ops::DerefMut;

pub type Task<T: 'static = ()> = SubClass<task, T>;

pub struct Taskqueue(*mut taskqueue);

pub mod wrappers {
    use super::*;

    pub fn taskqueue_thread() -> Taskqueue {
        // Needs unsafe since bindgen generates a static mut but the pointer shouldn't change after
        // initialization
        let ptr = unsafe { bindings::taskqueue_thread };
        Taskqueue(ptr)
    }

    // This takes ownership of task, passing it to the taskqueue which passes it on to the callback
    pub fn taskqueue_enqueue<
        T: 'static,
        O,
        P: OwnedVar<Task<T>, O> + DerefMut<Target = Task<T>>,
    >(
        queue: Taskqueue,
        mut taskp: P,
        callback: extern "C" fn(P, c_int),
    ) -> core::result::Result<(), (ErrCode, P)> {
        let task_ptr = taskp.get_var_ptr();
        if task_ptr != taskp.get_owner().cast() {
            // Only boxed tasks are currently supported
            return Err((EDOOFUS, taskp));
        }
        taskp.deref_mut().get_base_mut().ta_context = task_ptr.cast();
        let callback = unsafe { transmute(callback) };
        taskp.get_base_mut().ta_func = Some(callback);
        let res = unsafe { bindings::taskqueue_enqueue(queue.0, SubClass::base_ptr(task_ptr)) };
        if res != 0 {
            return Err((ErrCode::from(res), taskp));
        }
        // Drop the box without running its destructor. When we materialize a new Box out of
        // ta_context in the callback that destructor will be run instead.
        forget(taskp);
        Ok(())
    }
}
