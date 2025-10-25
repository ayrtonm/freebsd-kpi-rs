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
use crate::bindings::{task, taskqueue};
use crate::boxed::{Box, BoxedThing};
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::c_void;
use core::ptr::NonNull;

pub type TaskFn<T> = fn(&T, u32) -> Result<()>;

pub trait Enqueueable {
    #[doc(hidden)]
    fn enqueue(self) -> *mut task;
}

impl<T> Enqueueable for Box<Task<T>> {
    fn enqueue(self) -> *mut task {
        let this_task = Box::leak(self);
        this_task.thing.c_task.get_mut().ta_context =
            (this_task as *mut BoxedThing<Task<T>>).cast::<c_void>();
        this_task.thing.c_task.get()
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct Task<T = ()> {
    c_task: UnsafeCell<task>,
    func: TaskFn<T>,
    ctx: T,
}

impl<T> Task<T> {
    pub fn new(func: TaskFn<T>, ctx: T) -> Self {
        let mut c_task = task::default();
        c_task.ta_func = Some(Self::invoke_rust_func::<T>);
        Self {
            c_task: UnsafeCell::new(c_task),
            func,
            ctx,
        }
    }

    extern "C" fn invoke_rust_func<U>(arg: *mut c_void, pending: i32) {
        let task_ptr = arg.cast::<BoxedThing<Task<U>>>();
        let this_task = unsafe { Box::from_raw(NonNull::new_unchecked(task_ptr)) };
        (this_task.func)(&this_task.ctx, pending as u32).unwrap();
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

    pub fn taskqueue_enqueue<T: Enqueueable>(queue: Taskqueue, task: T) -> Result<()> {
        let task_ptr = task.enqueue();
        let res = unsafe { bindings::taskqueue_enqueue(queue.0, task_ptr) };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }
}
