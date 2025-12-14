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
use core::mem::{forget, offset_of};
use core::ops::{Deref, DerefMut};
use core::ptr;
use core::ptr::{NonNull, drop_in_place};

pub struct ArcRefCount(*mut ArcMetadata);

impl Drop for ArcRefCount {
    fn drop(&mut self) {
        let metadata_ptr = self.0;
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*metadata_ptr).count });
        let last = unsafe { bindings::refcount_release(count_ptr) };
        if last {
            let drop_fn_ptr = unsafe { &raw mut (*metadata_ptr).drop_fn };
            let drop_fn = unsafe { ptr::read(drop_fn_ptr) };
            unsafe { drop_fn(metadata_ptr) }
        }
    }
}

pub struct ArcMetadata {
    drop_fn: unsafe fn(*mut Self),
    ty: Option<MallocType>,
    count: UnsafeCell<u_int>,
}

#[repr(C)]
pub struct InnerArc<T> {
    // This field must be first to support subclasses properly
    thing: T,
    pub metadata: ArcMetadata,
}

#[repr(C)]
#[derive(Debug)]
pub struct ArcRef<'a, T>(NonNull<InnerArc<T>>, PhantomData<&'a T>);

impl<'a, T> ArcRef<'a, T> {
    pub unsafe fn from_raw(ptr: *mut InnerArc<T>) -> Self {
        Self(NonNull::new(ptr).unwrap(), PhantomData)
    }

    pub fn into_raw(x: Self) -> *mut InnerArc<T> {
        x.0.as_ptr()
    }

    pub fn into_arc(&self) -> Arc<T> {
        Arc::new_from_raw(self.0.as_ptr())
    }
}

impl<'a, T> Deref for ArcRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        let inner_ref = unsafe { self.0.as_ref() };
        &inner_ref.thing
    }
}

impl<'a, T> Copy for ArcRef<'a, T> {}

impl<'a, T> Clone for ArcRef<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

#[derive(Debug)]
pub struct UniqueArcRef<T>(NonNull<InnerArc<T>>);

impl<T> Deref for UniqueArcRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        let inner_ref = unsafe { self.0.as_ref() };
        &inner_ref.thing
    }
}

impl<T> DerefMut for UniqueArcRef<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let inner_ref = unsafe { self.0.as_mut() };
        &mut inner_ref.thing
    }
}

impl<T> UniqueArcRef<T> {
    pub unsafe fn from_raw(ptr: *mut InnerArc<T>) -> Self {
        Self(NonNull::new(ptr).unwrap())
    }

    pub fn into_raw(x: Self) -> *mut InnerArc<T> {
        let res = x.0.as_ptr();
        forget(x);
        res
    }

    pub fn into_arc(self) -> Arc<T> {
        Arc(self.0, PhantomData)
    }
}

#[derive(Debug)]
pub struct UninitArc<T>(NonNull<InnerArc<T>>);

impl<T> UninitArc<T> {
    pub fn from_raw(inner_ptr: *mut InnerArc<T>) -> Self {
        let metadata_ptr = unsafe { &raw mut (*inner_ptr).metadata };
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*metadata_ptr).count });
        unsafe { *count_ptr = 0 };
        Self(NonNull::new(inner_ptr).unwrap())
    }

    pub fn init(self, t: T) -> UniqueArcRef<T> {
        let inner_ptr = self.0.as_ptr();
        let thing_ptr = unsafe { &raw mut (*inner_ptr).thing };
        unsafe { ptr::write(thing_ptr.cast::<T>(), t) };
        let inner_ref = unsafe { inner_ptr.as_ref().unwrap() };
        let count_ptr = inner_ref.metadata.count.get();
        unsafe { bindings::refcount_init(count_ptr, 1) };
        UniqueArcRef(self.0)
    }
}

/// A pointer to an object on the heap which owns a refcount to its pointee.
///
/// `Arc` stands for 'Atomically Reference Counted' so this is thread-safe.
#[derive(Debug)]
#[repr(C)]
pub struct Arc<T>(NonNull<InnerArc<T>>, PhantomData<T>);

impl<T> Arc<T> {
    const LAYOUT_SUPPORTS_SUBCLASSES: () = {
        if offset_of!(InnerArc<T>, thing) != 0 {
            panic!("InnerArc layout does not support subclasses")
        }
    };
    /// Creates a new `Arc<T>`.
    ///
    /// Allocates space on the heap for the `T` as well as the `Arc`'s metadata. `flags` must
    /// contain `M_WAITOK`.
    pub fn new(thing: T, ty: MallocType, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_new(thing, ty, flags).unwrap()
    }

    /// Attempts to creates a new `Arc<T>`.
    ///
    /// `flags` must contain either `M_NOWAIT` or `M_WAITOK`.
    pub fn try_new(thing: T, ty: MallocType, flags: MallocFlags) -> Result<Self> {
        // compile-time assertion about layout of InnerArc<T>
        let _ = Self::LAYOUT_SUPPORTS_SUBCLASSES;
        if flags.contains(M_NOWAIT) == flags.contains(M_WAITOK) {
            return Err(EDOOFUS);
        }
        let inner_size = size_of::<InnerArc<T>>();
        let inner_align = align_of::<InnerArc<T>>();
        let void_ptr = malloc_aligned(inner_size, inner_align, ty, flags);
        if void_ptr.is_null() {
            return Err(ENOMEM);
        }
        let inner_ptr = void_ptr.cast::<InnerArc<T>>();

        unsafe fn default_drop_fn<T>(metadata_ptr: *mut ArcMetadata) {
            let inner_ptr = unsafe {
                metadata_ptr
                    .byte_sub(offset_of!(InnerArc<T>, metadata))
                    .cast::<InnerArc<T>>()
            };
            let ty_ptr = unsafe { &raw mut (*metadata_ptr).ty };
            // ty is set to Some later on
            let ty = unsafe { ptr::read(ty_ptr).unwrap() };
            let thing_ptr = unsafe { &raw mut (*inner_ptr).thing };
            unsafe {
                drop_in_place(thing_ptr);
            }
            unsafe { free(inner_ptr.cast::<c_void>(), ty) }
        }
        unsafe {
            inner_ptr.write(InnerArc {
                thing,
                metadata: ArcMetadata {
                    drop_fn: default_drop_fn::<T>,
                    ty: Some(ty),
                    count: UnsafeCell::new(0),
                },
            })
        };
        let inner_ref = unsafe { inner_ptr.as_ref().unwrap() };
        let count_ptr = inner_ref.metadata.count.get();
        unsafe { bindings::refcount_init(count_ptr, 1) };
        Ok(Self(NonNull::new(inner_ptr).unwrap(), PhantomData))
    }

    /// Destroys an `Arc` without releasing its refcount, returning a `&'static T` in the process.
    pub fn leak(x: Self) -> &'static T {
        let res = unsafe { &x.0.as_ref().thing };
        forget(x);
        res
    }

    pub fn take_refcount(x: Self) -> (*mut T, ArcRefCount) {
        let inner_ptr = x.0.as_ptr();
        let t_ptr = unsafe { &raw mut (*inner_ptr).thing };
        let metadata_ptr = unsafe { &raw mut (*inner_ptr).metadata };
        let refcount = ArcRefCount(metadata_ptr);
        forget(x);
        (t_ptr.cast::<T>(), refcount)
    }

    /// Destroys an `Arc` without releasing its refcount, returning a pointer in the process.
    pub fn into_raw(x: Self) -> *mut InnerArc<T> {
        let res = x.0.as_ptr();
        forget(x);
        res
    }

    /// Creates an `Arc<T>` from a pointer.
    pub unsafe fn from_raw(ptr: *mut InnerArc<T>) -> Self {
        Self(NonNull::new(ptr).unwrap(), PhantomData)
    }

    /// Only used by KPI glue
    #[doc(hidden)]
    pub fn set_drop_fn(inner_ptr: *mut InnerArc<T>, drop_fn: unsafe fn(*mut ArcMetadata)) {
        let metadata_ptr = unsafe { &raw mut (*inner_ptr).metadata };
        let drop_fn_ptr = unsafe { &raw mut (*metadata_ptr).drop_fn };
        unsafe {
            drop_fn_ptr.write(drop_fn);
        }
    }

    fn new_from_raw(ptr: *mut InnerArc<T>) -> Self {
        let res = unsafe { Arc::from_raw(ptr) };
        let count_ptr = Arc::get_count_ptr(res.0);
        unsafe { bindings::refcount_acquire(count_ptr) };
        res
    }

    pub(crate) fn get_count_ptr(ptr: NonNull<InnerArc<T>>) -> *mut u_int {
        unsafe { ptr.as_ref().metadata.count.get() }
    }

    /// Get a snapshot of the refcount
    ///
    /// This is thread-safe since it uses `refcount_load`, but the count may be immediately
    /// invalidated so care must be taken in how the result is used.
    pub fn get_count(ptr: *mut InnerArc<T>) -> u_int {
        let count_ptr = unsafe { ptr.as_ref().unwrap().metadata.count.get() };
        unsafe { bindings::refcount_load(count_ptr) }
    }

    #[cfg(test)]
    pub(super) fn snapshot_refcount(&self) -> u_int {
        let count_ptr = Arc::get_count_ptr(self.0);
        unsafe { bindings::refcount_load(count_ptr) }
    }
}

impl<T> Clone for Arc<T> {
    fn clone(&self) -> Self {
        Arc::new_from_raw(self.0.as_ptr())
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
        let inner_ptr = self.0.as_ptr();
        let metadata_ptr = unsafe { &raw mut (*inner_ptr).metadata };
        // delegate the work to ArcRefCount's Drop impl
        let _refcount = ArcRefCount(metadata_ptr);
    }
}
