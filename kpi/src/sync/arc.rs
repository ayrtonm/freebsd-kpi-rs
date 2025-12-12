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
use core::mem::{MaybeUninit, forget};
use core::ops::{Deref, DerefMut};
use core::ptr;
use core::ptr::{NonNull, drop_in_place};

#[repr(u64)]
enum ArcMetadata<T> {
    DropFn(Option<unsafe fn(*mut InnerArc<T>)>) = 0,
    Malloc(MallocType) = 1,
}

#[repr(C)]
pub struct InnerArc<T> {
    // This field must be first to support subclasses properly
    thing: MaybeUninit<T>,
    metadata: ArcMetadata<T>,
    count: UnsafeCell<u_int>,
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
        unsafe { inner_ref.thing.assume_init_ref() }
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
        unsafe { inner_ref.thing.assume_init_ref() }
    }
}

impl<T> DerefMut for UniqueArcRef<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let inner_ref = unsafe { self.0.as_mut() };
        unsafe { inner_ref.thing.assume_init_mut() }
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
        Self(NonNull::new(inner_ptr).unwrap())
    }

    pub fn init(self, t: T) -> UniqueArcRef<T> {
        let inner_ptr = self.0.as_ptr();
        let thing_ptr = unsafe { &raw mut (*inner_ptr).thing };
        unsafe { ptr::write(thing_ptr.cast::<T>(), t) };
        let inner_ref = unsafe { inner_ptr.as_ref().unwrap() };
        let count_ptr = inner_ref.count.get();
        unsafe { bindings::refcount_init(count_ptr, 1) };
        UniqueArcRef(self.0)
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct Arc<T>(NonNull<InnerArc<T>>, PhantomData<T>);

impl<T> Arc<T> {
    pub fn set_drop_fn(inner_ptr: *mut InnerArc<T>, drop_fn: unsafe fn(*mut InnerArc<T>)) {
        let metadata = ArcMetadata::DropFn(Some(drop_fn));
        unsafe {
            inner_ptr.write(InnerArc {
                thing: MaybeUninit::zeroed(),
                metadata,
                count: UnsafeCell::new(0),
            })
        };
    }

    pub fn leak(x: Self) -> &'static T {
        let res = unsafe { x.0.as_ref().thing.assume_init_ref() };
        forget(x);
        res
    }

    pub unsafe fn from_raw(ptr: *mut InnerArc<T>) -> Self {
        Self(NonNull::new(ptr).unwrap(), PhantomData)
    }

    pub fn into_raw(x: Self) -> *mut InnerArc<T> {
        let res = x.0.as_ptr();
        forget(x);
        res
    }

    pub fn new_from_raw(ptr: *mut InnerArc<T>) -> Self {
        let res = unsafe { Arc::from_raw(ptr) };
        let count_ptr = Arc::get_count_ptr(res.0);
        unsafe { bindings::refcount_acquire(count_ptr) };
        res
    }

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
                thing: MaybeUninit::new(thing),
                metadata,
                count: UnsafeCell::new(0),
            })
        };
        let inner_ref = unsafe { inner_ptr.as_ref().unwrap() };
        let count_ptr = inner_ref.count.get();
        unsafe { bindings::refcount_init(count_ptr, 1) };
        Ok(Self(NonNull::new(inner_ptr).unwrap(), PhantomData))
    }

    pub(crate) fn get_count_ptr(ptr: NonNull<InnerArc<T>>) -> *mut u_int {
        unsafe { ptr.as_ref().count.get() }
    }

    pub fn get_count(ptr: *mut InnerArc<T>) -> u_int {
        let count_ptr = unsafe { ptr.as_ref().unwrap().count.get() };
        unsafe { ptr::read(count_ptr) }
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
        unsafe { inner_ref.thing.assume_init_ref() }
    }
}

impl<T> Drop for Arc<T> {
    fn drop(&mut self) {
        let count_ptr = Arc::get_count_ptr(self.0);
        let last = unsafe { bindings::refcount_release(count_ptr) };
        if last {
            let inner_ptr = self.0.as_ptr();

            let metadata_ptr = unsafe { &raw mut (*inner_ptr).metadata };
            let metadata = unsafe { ptr::read(metadata_ptr) };

            let maybeuninit_thing_ptr = unsafe { &raw mut (*inner_ptr).thing };
            let thing_ptr = maybeuninit_thing_ptr.cast::<T>();
            unsafe { drop_in_place(thing_ptr) };

            match metadata {
                ArcMetadata::DropFn(func) => {
                    let func = func.unwrap();
                    unsafe { func(inner_ptr) }
                }
                ArcMetadata::Malloc(ty) => unsafe { free(inner_ptr.cast::<c_void>(), ty) },
            }
        }
    }
}
