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
use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::c_void;
use core::marker::PhantomData;
use core::ops::Deref;
use core::ptr;
use core::ptr::{NonNull, drop_in_place};

#[repr(u64)]
enum ArcMetadata {
    DropFn(Option<unsafe fn(*mut u_int)>) = 0,
    Malloc(MallocType) = 1,
}

#[repr(C)]
struct InnerArc<T> {
    // This field must be first to support subclasses properly
    thing: T,
    metadata: ArcMetadata,
    count: UnsafeCell<u_int>,
}

#[derive(Debug)]
#[repr(C)]
pub struct Arc<T>(NonNull<InnerArc<T>>, PhantomData<T>);

impl<T> Arc<T> {
    pub fn new(thing: T, ty: MallocType, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_new(thing, ty, flags).unwrap()
    }

    pub fn try_new(thing: T, ty: MallocType, flags: MallocFlags) -> Result<Self> {
        if flags.contains(M_NOWAIT) && flags.contains(M_WAITOK) {
            return Err(EDOOFUS);
        }
        let inner_size = size_of::<InnerArc<T>>();
        let inner_align = align_of::<InnerArc<T>>();
        let void_ptr = malloc_aligned(inner_size, inner_align, ty, flags);
        if void_ptr.is_null() {
            return Err(ENOMEM);
        }
        let inner_ptr = void_ptr.cast::<InnerArc<T>>();
        let metadata = ArcMetadata::Malloc(ty);
        unsafe {
            inner_ptr.write(InnerArc {
                thing,
                metadata,
                count: UnsafeCell::new(0),
            })
        };
        let inner_ref = unsafe { inner_ptr.as_ref().unwrap() };
        let count_ptr = inner_ref.count.get();
        unsafe { bindings::refcount_init(count_ptr, 1) };
        Ok(Self(NonNull::new(inner_ptr).unwrap(), PhantomData))
    }

    fn get_count_ptr(&self) -> *mut u_int {
        unsafe { self.0.as_ref().count.get() }
    }

    #[cfg(test)]
    pub(super) fn snapshot_refcount(&self) -> u_int {
        let count_ptr = self.get_count_ptr();
        unsafe { bindings::refcount_load(count_ptr) }
    }
}

impl<T> Clone for Arc<T> {
    fn clone(&self) -> Self {
        let count_ptr = self.get_count_ptr();
        unsafe { bindings::refcount_acquire(count_ptr) };
        Self(self.0, PhantomData)
    }
}

unsafe impl<T: Sync + Send> Send for Arc<T> {}
unsafe impl<T: Sync + Send> Sync for Arc<T> {}

impl<T> Deref for Arc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        let inner_ref = unsafe { self.0.as_ref() };
        &inner_ref.thing
    }
}

impl<T> Drop for Arc<T> {
    fn drop(&mut self) {
        let count_ptr = self.get_count_ptr();
        let last = unsafe { bindings::refcount_release(count_ptr) };
        if last {
            let inner_ptr = self.0.as_ptr();
            let metadata_ptr = unsafe { &raw mut (*inner_ptr).metadata };
            let metadata = unsafe { ptr::read(metadata_ptr) };
            match metadata {
                ArcMetadata::DropFn(func) => {
                    let func = func.unwrap();
                    unsafe { func(count_ptr) }
                }
                ArcMetadata::Malloc(ty) => {
                    let thing_ptr = unsafe { &raw mut (*inner_ptr).thing };
                    unsafe { drop_in_place(thing_ptr) };
                    unsafe { free(inner_ptr.cast::<c_void>(), ty) }
                }
            }
        }
    }
}
