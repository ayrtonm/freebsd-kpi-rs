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
use crate::malloc::{Malloc, MallocFlags};
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::c_void;
use core::marker::PhantomData;
use core::mem::forget;
use core::ops::Deref;
use core::ptr::{NonNull, drop_in_place};

#[repr(C)]
pub struct InnerArc<T> {
    t: T,
    count: UnsafeCell<u_int>,
}

/// A pointer to an object on the heap which owns a refcount to its pointee.
///
/// `Arc` stands for 'Atomically Reference Counted' so this is thread-safe.
#[derive(Debug)]
#[repr(C)]
pub struct Arc<T, M: Malloc = M_DEVBUF>(NonNull<InnerArc<T>>, PhantomData<*mut M>);

impl<T, M: Malloc> Arc<T, M> {
    /// Creates a new `Arc<T>`.
    ///
    /// Allocates space on the heap for the `T` as well as the `Arc`'s metadata. `flags` must
    /// contain `M_WAITOK`.
    pub fn new(t: T, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_new(t, flags).unwrap()
    }

    /// Attempts to creates a new `Arc<T>`.
    ///
    /// `flags` must contain either `M_NOWAIT` or `M_WAITOK`.
    pub fn try_new(t: T, flags: MallocFlags) -> Result<Self> {
        if flags.contains(M_NOWAIT) == flags.contains(M_WAITOK) {
            return Err(EDOOFUS);
        }
        let t_size = size_of::<InnerArc<T>>();
        let t_align = align_of::<InnerArc<T>>();
        let void_ptr = malloc_aligned(t_size, t_align, M::malloc_type(), flags);
        match NonNull::new(void_ptr) {
            Some(nonnull_void_ptr) => {
                let inner_ptr = nonnull_void_ptr.cast::<InnerArc<T>>();
                let count_ptr =
                    UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr.as_ptr()).count });
                unsafe { bindings::refcount_init(count_ptr, 1) };
                let arc_t_ptr = unsafe { &raw mut (*inner_ptr.as_ptr()).t };
                unsafe { arc_t_ptr.write(t) };
                Ok(Self(inner_ptr, PhantomData))
            }
            None => Err(ENOMEM),
        }
    }

    /// Destroys an `Arc` without releasing its refcount, returning a pointer in the process.
    pub fn into_raw(x: Self) -> *mut T {
        let inner_ptr = x.0.as_ptr();
        let res = unsafe { &raw mut (*inner_ptr).t };
        forget(x);
        res
    }

    /// Creates an `Arc<T>` from a pointer.
    pub unsafe fn from_raw(ptr: *mut InnerArc<T>) -> Self {
        Self(NonNull::new(ptr).unwrap(), PhantomData)
    }

    fn count_ptr(&self) -> *mut u_int {
        let inner_ptr = self.0.as_ptr();
        UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count })
    }

    #[cfg(test)]
    pub(crate) fn snapshot_refcount(&self) -> u_int {
        unsafe { bindings::refcount_load(self.count_ptr()) }
    }
}

impl<T, M: Malloc> Clone for Arc<T, M> {
    fn clone(&self) -> Self {
        unsafe { bindings::refcount_acquire(self.count_ptr()) };
        let inner_ptr = self.0.as_ptr();
        unsafe { Arc::from_raw(inner_ptr) }
    }
}

unsafe impl<T: Sync + Send, M: Malloc> Send for Arc<T, M> {}
unsafe impl<T: Sync + Send, M: Malloc> Sync for Arc<T, M> {}

impl<T, M: Malloc> Deref for Arc<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.0.as_ref().t }
    }
}

impl<T, M: Malloc> Drop for Arc<T, M> {
    fn drop(&mut self) {
        let inner_ptr = self.0.as_ptr();
        let count_ptr = self.count_ptr();
        let last = unsafe { bindings::refcount_release(count_ptr) };
        if last {
            let arc_t_ptr = unsafe { &raw mut (*inner_ptr).t };
            unsafe { drop_in_place(arc_t_ptr) };
            unsafe { free(inner_ptr.cast::<c_void>(), M::malloc_type()) }
        }
    }
}
