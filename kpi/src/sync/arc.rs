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

use crate::bindings::u_int;
use crate::boxed::{Box, BoxedThing};
use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use core::ops::Deref;
use core::ptr::NonNull;

#[repr(C)]
struct InternalArc<T> {
    count: u_int,
    t: T,
}

/// A thread-safe reference-counting pointer.
///
/// Like `Arc` in the standard library this is a thin pointer to a heap-allocated object.
#[repr(C)]
#[derive(Debug)]
pub struct Arc<T>(NonNull<BoxedThing<InternalArc<T>>>);

impl<T> Arc<T> {
    pub fn try_new(t: T, ty: MallocType, flags: MallocFlags) -> Result<Self> {
        let internal_arc = InternalArc { count: 0, t };
        let boxed_arc: Box<InternalArc<T>> = Box::try_new(internal_arc, ty, flags)?;
        let arc_ref: &mut BoxedThing<InternalArc<T>> = Box::leak(boxed_arc);
        let count_ptr: *mut u_int = &raw mut arc_ref.thing.count;
        unsafe { bindings::refcount_init(count_ptr, 1) };
        Ok(Self(NonNull::from_ref(arc_ref)))
    }

    fn get_count_ptr(&self) -> *mut u_int {
        unsafe { &raw mut (*self.0.as_ptr()).thing.count }
    }

    #[cfg(test)]
    pub(crate) fn snapshot_refcount(&self) -> u_int {
        let count_ptr = self.get_count_ptr();
        unsafe { bindings::refcount_load(count_ptr) }
    }
}

impl<T> Clone for Arc<T> {
    fn clone(&self) -> Self {
        let count_ptr = self.get_count_ptr();
        unsafe { bindings::refcount_acquire(count_ptr) };
        Self(self.0)
    }
}

unsafe impl<T: Sync + Send> Send for Arc<T> {}
unsafe impl<T: Sync + Send> Sync for Arc<T> {}

impl<T> Deref for Arc<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        let boxed_thing_ref = unsafe { self.0.as_ref() };
        &boxed_thing_ref.thing.t
    }
}

impl<T> Drop for Arc<T> {
    fn drop(&mut self) {
        let count_ptr = self.get_count_ptr();
        let last = unsafe { bindings::refcount_release(count_ptr) };
        if last {
            unsafe {
                Box::from_raw(self.0);
            }
        }
    }
}
