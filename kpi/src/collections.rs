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
use crate::bindings::sglist;
use crate::boxed::Box;
use crate::ffi::SyncPtr;
use crate::malloc::{Malloc, MallocFlags};
use crate::prelude::*;
use crate::vec::Vec;
use core::ffi::c_void;
use core::mem::{forget, size_of};
use core::ops::{DerefMut, Index};

/// An array with a size determined at runtime.
#[derive(Debug)]
pub struct RuntimeArray<T, M: Malloc = M_DEVBUF>(Vec<T, M>);

/// A 2D-array with a size determined at runtime.
#[derive(Debug)]
pub struct Runtime2DArray<T, M: Malloc = M_DEVBUF>(Vec<Vec<T, M>, M>);

impl<T, M: Malloc> RuntimeArray<T, M> {
    pub fn new(len: usize, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_new(len, flags).unwrap()
    }
    pub fn try_new(len: usize, flags: MallocFlags) -> Result<Self> {
        let array = Vec::try_with_capacity(len, flags)?;
        Ok(Self(array))
    }

    pub fn push(&mut self, value: T) {
        self.0.push(value);
    }
}

impl<T, M: Malloc> Index<usize> for RuntimeArray<T, M> {
    type Output = T;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.0[idx]
    }
}

impl<T, M: Malloc> Runtime2DArray<T, M> {
    pub fn new(len: usize, width: usize, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_new(len, width, flags).unwrap()
    }
    pub fn try_new(len: usize, width: usize, flags: MallocFlags) -> Result<Self> {
        let mut array = Vec::try_with_capacity(len, flags)?;
        for v in &mut array {
            *v = Vec::try_with_capacity(width, flags)?;
        }
        Ok(Self(array))
    }

    pub fn push(&mut self, value: T) {
        for v in &mut self.0 {
            if v.len() == v.capacity() {
                continue;
            }
            v.push(value);
            return;
        }
    }
}

impl<T, M: Malloc> Index<usize> for Runtime2DArray<T, M> {
    type Output = [T];

    fn index(&self, idx: usize) -> &Self::Output {
        &self.0[idx]
    }
}

/// Plain-ol-data which is valid for any bitpattern
///
/// # Implementation Safety
///
/// This may only be implemented for types that are valid for an arbitrary bitpattern. Typically
/// this will be a struct, union or tuple consisting of other types implementing `Pod`. Usually
/// enums cannot implement `Pod` since the arbitrary bitpattern also covers the discriminant.
pub unsafe trait Pod: Sized + Copy + 'static {}

macro_rules! impl_pod_for {
    ($($ty:ty)*) => {
        $(unsafe impl Pod for $ty {})*
        $(unsafe impl<const N: usize> Pod for [$ty; N] {})*
    };
}

impl_pod_for! {
    u8 u16 u32 u64 u128
    i8 i16 i32 i64 i128
}

/// A type owning data which may be appended to an sglist
///
/// # Implementation Safety
///
/// This may only be implemented for types that have unique access to data which is contiguous in
/// the kernel virtual address space. The call to get_vaddr_range may modify this memory before it
/// returns (e.g. the impl for Vec fills  it to capacity with T::Default), but it may not be
/// acccessed again until SgBuffer::get_buffer is called. For types that own their data directly
/// (e.g. Box, Vec) this is not a concern since SgBuffer owns the Appendable buffer. For types that
/// implement Appendable but are a proxy for the real owner, the owner must ensure that it does not
/// access or drop the memory while the proxy exists.
pub unsafe trait Appendable {
    fn get_vaddr_range(&mut self) -> (*mut c_void, usize);
}

unsafe impl<T: Pod> Appendable for Box<T> {
    fn get_vaddr_range(&mut self) -> (*mut c_void, usize) {
        let ptr = self.deref_mut() as *mut T;
        (ptr.cast::<c_void>(), size_of::<T>())
    }
}

unsafe impl<T: Default + Pod> Appendable for Vec<T> {
    fn get_vaddr_range(&mut self) -> (*mut c_void, usize) {
        let ptr = self.as_ptr().cast_mut();
        self.fill_to_capacity(T::default());
        let size = self.capacity() * size_of::<T>();
        (ptr.cast::<c_void>(), size)
    }
}

type SgListPtr = SyncPtr<sglist>;

/// A handle to a buffer in a scatter-gather list
///
/// This has both ownership of the appended buffer and owns a refcount to the list. Since the buffer
/// has aliasing pointers in the list this type does not allow access to the buffer until the list
/// is reset. Once the list has been reset the buffer can be accessed by calling
/// `list_buffer.get_buffer()`.
pub struct SgBuffer<B: Appendable> {
    // The Option is only necessary to allow taking `buffer` out of the SgBuffer.
    buffer: Option<B>,
    list: SgListPtr,
}

unsafe impl<T: Send + Appendable> Send for SgBuffer<T> {}
unsafe impl<T: Send + Appendable> Sync for SgBuffer<T> {}

impl<B: Appendable> SgBuffer<B> {
    /// Create a handle to a buffer in a scatter-gather list.
    fn new(buffer: B, sg: &SgList) -> Self {
        // Grab a refcount to the sglist
        sglist_hold(sg);
        Self {
            buffer: Some(buffer),
            list: sg.list,
        }
    }

    pub fn get_buffer(mut self) -> B {
        let len = unsafe { bindings::sglist_length(self.list.as_ptr()) };
        if len != 0 {
            panic!("Must call sglist_reset before calling SgBuffer::get_buffer")
        }
        // Should not panic because SgBuffer::new always sets this to Some
        self.buffer.take().unwrap()
        // Drop impl calls sglist_free to release the sglist refcount
    }
}

impl<B: Appendable> Drop for SgBuffer<B> {
    fn drop(&mut self) {
        let len = unsafe { bindings::sglist_length(self.list.as_ptr()) };
        if len != 0 {
            panic!("Must call sglist_reset before dropping an SgBuffer")
        }
        // Drop the refcount grabbed in the constructor
        unsafe { bindings::sglist_free(self.list.as_ptr()) };
    }
}

pub struct SgList {
    list: SgListPtr,
}

impl SgList {
    pub unsafe fn from_raw(ptr: *mut sglist) -> Self {
        Self {
            list: SgListPtr::new(ptr),
        }
    }

    pub fn into_raw(x: Self) -> *mut sglist {
        let res = x.as_ptr();
        forget(x);
        res
    }

    pub fn new(nsegs: usize, flags: MallocFlags) -> Result<Self> {
        let list = unsafe { bindings::sglist_alloc(nsegs.try_into().unwrap(), flags.0) };
        if list.is_null() {
            return Err(ENOMEM);
        }
        let list = SgListPtr::new(list);
        Ok(Self { list })
    }

    pub fn as_ptr(&self) -> *mut sglist {
        self.list.as_ptr()
    }
}

impl Drop for SgList {
    fn drop(&mut self) {
        // If any buffers were appended they also hold refcounts to the sglist just in case
        unsafe { bindings::sglist_free(self.as_ptr()) }
    }
}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;

    /// Creates a new scatter-gather list with `nsegs` segments.
    pub fn sglist_alloc(nsegs: usize, flags: MallocFlags) -> Result<SgList> {
        SgList::new(nsegs, flags)
    }

    /// Append the virtual addresses spanned by `buffer` to the scatter-gather list.
    ///
    /// Returns an `SgBuffer` to access the buffer after the list has been reset. Dropping an
    /// `SgBuffer` before the SgList is reset panics (because we can't free the appended buffer
    /// while it's in the list and potentially being accessed) so the result of this function
    /// must always be assigned to a `let` binding to avoid panicking.
    pub fn sglist_append<B: Appendable>(
        sg: &mut SgList,
        buffer_opt: &mut Option<B>,
    ) -> Result<SgBuffer<B>> {
        let mut buffer = match buffer_opt.take() {
            Some(buffer) => buffer,
            None => {
                return Err(EDOOFUS);
            }
        };
        let (ptr, size) = buffer.get_vaddr_range();
        let res = unsafe { bindings::sglist_append(sg.list.as_ptr(), ptr, size) };
        if res != 0 {
            buffer_opt.replace(buffer);
            return Err(ErrCode::from(res));
        }
        Ok(SgBuffer::new(buffer, sg))
    }

    pub fn sglist_reset(sg: &mut SgList) {
        unsafe { bindings::sglist_reset(sg.list.as_ptr()) }
    }

    pub fn sglist_length(sg: &SgList) -> usize {
        unsafe { bindings::sglist_length(sg.list.as_ptr()) }
    }

    pub fn sglist_hold(sg: &SgList) {
        unsafe { bindings::sglist_hold(sg.list.as_ptr()) };
    }
}
