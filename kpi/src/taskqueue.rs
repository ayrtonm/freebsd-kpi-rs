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

use crate::bindings::{taskqueue, taskqueue_enqueue_fn};
use crate::ffi::SyncPtr;
use crate::malloc::MallocFlags;
use crate::prelude::*;
use core::ffi::{CStr, c_void};
use core::ptr::null_mut;

pub struct Taskqueue {
    inner: SyncPtr<taskqueue>,
}

impl Taskqueue {
    pub fn new() -> Self {
        Self {
            inner: SyncPtr::new(null_mut())
        }
    }
}

pub type TaskqueueEnqueueFn<T> = extern "C" fn(*mut T);

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;

    pub fn taskqueue_create<T>(
        name: &CStr,
        flags: MallocFlags,
        //enqueue: TaskqueueEnqueueFn<T>,
        //ctx: *mut T,
        //queue: &Taskqueue,
    ) -> Result<Taskqueue> {
        //let mut outp = null_mut();
        let enqueue = Some(bindings::taskqueue_thread_enqueue as _);
        let ctx = todo!("");
        let res = unsafe { bindings::taskqueue_create(name.as_ptr(), flags.0, enqueue, ctx) };
        if res.is_null() {
            return Err(ENULLPTR);
        };
        Ok(Taskqueue {
            inner: SyncPtr::new(res),
        })
    }
}
