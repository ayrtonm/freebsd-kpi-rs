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
use crate::bindings::{sglist, sglist_seg};
use crate::boxed::Box;
use crate::malloc::MallocFlags;
use crate::prelude::*;
use crate::vec::Vec;
use core::array;
use core::ffi::c_void;
use core::marker::PhantomData;
use core::mem::{forget, size_of};
use core::num::{Saturating, Wrapping};
use core::ops::DerefMut;
use core::slice;
use core::sync::atomic::{AtomicPtr, Ordering};

// TODO: drop the Copy bound to allow impl Pod for atomics
/// Plain-ol-data which is valid for any bitpattern and has no implicit padding
///
/// # Implementation Safety
///
/// This may only be implemented for types that are valid for an arbitrary bitpattern. Typically
/// this will be a struct consisting of other types implementing `Pod`. Unions may also implement it
/// if all variants impl `Pod` and have the same size. Usually enums cannot implement `Pod` since
/// the arbitrary bitpattern also covers the discriminant. Tuples also cannot implement it.
///
/// If you're defining a struct yourself wrap the type definition in `define_pod_struct!` to get
/// this implemented safely for free. If the type has implicit padding you'll get a compiler error.
pub unsafe trait Pod: Sized + Copy + 'static {}

macro_rules! impl_pod_for {
    ($($ty:ty)*) => {
        $(unsafe impl Pod for $ty {})*
        $(unsafe impl Pod for Wrapping<$ty> {})*
        $(unsafe impl Pod for Saturating<$ty> {})*
        $(unsafe impl<const N: usize> Pod for [$ty; N] {})*
    };
}

impl_pod_for! {
    u8 u16 u32 u64 u128 usize
    i8 i16 i32 i64 i128 isize
}

unsafe impl Pod for () {}
unsafe impl<T: 'static> Pod for PhantomData<T> {}

// TODO: Move all Pod stuff somewhere else (maybe the ffi module)
pub fn assert_is_pod<T: Pod>() {}

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

/// A handle to a buffer in a scatter-gather list
///
/// This has both ownership of the appended buffer and owns a refcount to the list. Since the buffer
/// has aliasing pointers in the list this type does not allow access to the buffer until the list
/// is reset. Once the list has been reset the buffer can be accessed by calling
/// `list_buffer.get_buffer()`.
pub struct SgBuffer<B: Appendable> {
    // The Option is only necessary to allow taking `buffer` out of the SgBuffer.
    buffer: Option<B>,
    list: AtomicPtr<sglist>,
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
            list: AtomicPtr::new(sg.list.load(Ordering::Relaxed)),
        }
    }

    pub fn get_buffer(mut self) -> B {
        let len = unsafe { bindings::sglist_length(self.list.load(Ordering::Relaxed)) };
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
        let len = unsafe { bindings::sglist_length(self.list.load(Ordering::Relaxed)) };
        if len != 0 {
            panic!("Must call sglist_reset before dropping an SgBuffer")
        }
        // Drop the refcount grabbed in the constructor
        unsafe { bindings::sglist_free(self.list.load(Ordering::Relaxed)) };
    }
}

pub struct SgList {
    list: AtomicPtr<sglist>,
}

impl SgList {
    pub unsafe fn from_raw(ptr: *mut sglist) -> Self {
        Self {
            list: AtomicPtr::new(ptr),
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
        let list = AtomicPtr::new(list);
        Ok(Self { list })
    }

    pub fn as_ptr(&self) -> *mut sglist {
        self.list.load(Ordering::Relaxed)
    }
}

impl Drop for SgList {
    fn drop(&mut self) {
        // If any buffers were appended they also hold refcounts to the sglist just in case
        unsafe { bindings::sglist_free(self.as_ptr()) }
    }
}

pub struct LocalSgList<'a, const N: usize> {
    sg: sglist,
    segs: [sglist_seg; N],
    read_end: Option<u16>,
    _marker: PhantomData<&'a mut ()>,
}

impl<'a, const N: usize> LocalSgList<'a, N> {
    pub fn new() -> Self {
        const { assert!(N <= u16::MAX as usize); }
        let mut sg = sglist::default();
        sg.sg_maxseg = N.try_into().unwrap();
        let segs = array::from_fn(|_| sglist_seg::default());
        LocalSgList {
            sg,
            segs,
            read_end: None,
            _marker: PhantomData,
        }
    }

    pub fn as_ptr(&mut self) -> *mut sglist {
        self.sg.sg_segs = self.segs.as_mut_ptr();
        (&raw const self.sg).cast_mut()
    }

    pub fn nseg_readable(&self) -> u16 {
        match self.read_end {
            Some(r) => r,
            None => self.sg.sg_nseg,
        }
    }

    pub fn nseg_writeable(&self) -> u16 {
        match self.read_end {
            Some(r) => self.sg.sg_nseg - r,
            None => 0,
        }
    }

    pub fn append_readable<T: Pod>(&mut self, buf: &'a T) -> Result<()> {
        self.append_readable_slice(slice::from_ref(buf))
    }

    pub fn append_readable_slice<T: Pod>(&mut self, buf: &'a [T]) -> Result<()> {
        // FIXME: This is only a virtqueue requirement
        if self.read_end.is_some() {
            return Err(EINVAL);
        }
        let len = size_of_val(buf);
        if len == 0 {
            return Ok(());
        }
        self.sg.sg_segs = self.segs.as_mut_ptr();
        let sg_ptr = &raw mut self.sg;
        let buf_ptr = buf.as_ptr().cast_mut();
        let res = unsafe { bindings::sglist_append(sg_ptr, buf_ptr.cast::<c_void>(), len) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn append_writeable<T: Pod>(&mut self, buf: &'a mut T) -> Result<()> {
        self.append_writeable_slice(slice::from_mut(buf))
    }

    pub fn append_writeable_slice<T: Pod>(&mut self, buf: &'a mut [T]) -> Result<()> {
        let len = size_of_val(buf);
        if len == 0 {
            return Ok(());
        }
        let nseg_readable = self.sg.sg_nseg;
        let first = self.read_end.is_none();
        self.sg.sg_segs = self.segs.as_mut_ptr();

        let mut tail = sglist::default();
        let sg_ptr = if first {
            let start = usize::from(nseg_readable);
            if start >= self.segs.len() {
                return Err(EFBIG);
            }
            tail.sg_segs = self.segs[start..].as_mut_ptr();
            tail.sg_maxseg = (self.segs.len() - start) as u16;
            &raw mut tail
        } else {
            &raw mut self.sg
        };
        let buf_ptr = buf.as_mut_ptr();
        let res = unsafe { bindings::sglist_append(sg_ptr, buf_ptr.cast::<c_void>(), len) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        if first {
            self.sg.sg_nseg += tail.sg_nseg;
            self.read_end = Some(nseg_readable);
        }
        Ok(())
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
        let res = unsafe { bindings::sglist_append(sg.list.load(Ordering::Relaxed), ptr, size) };
        if res != 0 {
            buffer_opt.replace(buffer);
            return Err(ErrCode::from(res));
        }
        Ok(SgBuffer::new(buffer, sg))
    }

    pub fn sglist_reset(sg: &mut SgList) {
        unsafe { bindings::sglist_reset(sg.list.load(Ordering::Relaxed)) }
    }

    pub fn sglist_length(sg: &SgList) -> usize {
        unsafe { bindings::sglist_length(sg.list.load(Ordering::Relaxed)) }
    }

    pub fn sglist_hold(sg: &SgList) {
        unsafe { bindings::sglist_hold(sg.list.load(Ordering::Relaxed)) };
    }
}
