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

use crate::bindings::sglist;
use crate::boxed::Box;
use crate::ffi::SyncPtr;
use crate::malloc::MallocFlags;
use crate::prelude::*;
use crate::vec::Vec;
use core::ffi::c_void;
use core::mem::{forget, size_of};
use core::ops::DerefMut;
use core::slice;

pub unsafe trait Pod: Sized + Copy {}
macro_rules! impl_trait_for {
    ($trait:ident $($ty:ty)*) => {
        $(unsafe impl $trait for $ty {})*
    };
}
impl_trait_for! {
    Pod
    u8 u16 u32 u64 u128
    i8 i16 i32 i64 i128
}

pub trait Appendable: Sized {
    type Appended: Sized;

    fn append(self) -> (Self::Appended, *mut c_void, usize);

    fn reconstruct(x: Self::Appended) -> Self;
}

impl<T: Pod> Appendable for Box<T> {
    type Appended = Box<T>;

    fn append(mut self) -> (Self, *mut c_void, usize) {
        let ptr = self.deref_mut() as *mut T;
        (self, ptr.cast::<c_void>(), size_of::<T>())
    }

    fn reconstruct(x: Self) -> Self {
        x
    }
}

impl<T: Default + Pod> Appendable for Vec<T> {
    type Appended = Vec<T>;

    fn append(mut self) -> (Self, *mut c_void, usize) {
        let ptr = self.as_ptr().cast_mut();
        self.fill_to_capacity(T::default());
        let size = self.capacity() * size_of::<T>();
        (self, ptr.cast::<c_void>(), size)
    }

    fn reconstruct(x: Self) -> Self {
        x
    }
}

impl<'a, T: Pod> Appendable for &'a mut T {
    type Appended = *mut T;

    fn append(self) -> (Self::Appended, *mut c_void, usize) {
        let ptr = self as *mut T;
        (ptr, ptr.cast::<c_void>(), size_of::<T>())
    }

    fn reconstruct(ptr: Self::Appended) -> Self {
        unsafe { ptr.as_mut().unwrap() }
    }
}

impl<'a, T: Pod> Appendable for &'a mut [T] {
    type Appended = (*mut T, usize);

    fn append(self) -> (Self::Appended, *mut c_void, usize) {
        let ptr = self.as_mut_ptr();
        let deconstructed = (ptr, self.len());

        let size = self.len() * size_of::<T>();
        (deconstructed, ptr.cast::<c_void>(), size)
    }

    fn reconstruct((ptr, len): Self::Appended) -> Self {
        unsafe { slice::from_raw_parts_mut(ptr, len) }
    }
}

type SgListPtr = SyncPtr<sglist>;

/// A handle to a buffer in a scatter-gather list
///
/// This has both ownership of the buffer and owns a refcount to the list. Since the buffer has
/// aliasing pointers in the list this type does not allow access to the buffer until the list is
/// reset. Once the list has been reset the buffer can be accessed by calling
/// `list_buffer.get_buffer()`.
pub struct SgBuffer<B: Appendable> {
    buffer: Option<B::Appended>,
    list: SgListPtr,
}

impl<B: Appendable> SgBuffer<B> {
    /// Create a handle to a buffer in a scatter-gather list.
    pub fn new(buffer: B::Appended, sg: &SgList) -> Self {
        sglist_hold(sg);
        Self {
            buffer: Some(buffer),
            list: sg.list,
        }
    }

    pub fn get_buffer(mut self) -> B {
        assert!(unsafe { bindings::sglist_length(self.list.as_ptr()) } == 0);
        Appendable::reconstruct(self.buffer.take().unwrap())
        // Drop impl calls sglist_free to release the sglist refcount
    }

    pub fn leak_buffer(self) {
        // Release the sglist reference
        unsafe { bindings::sglist_free(self.list.as_ptr()) };
        forget(self);
    }
}

impl<B: Appendable> Drop for SgBuffer<B> {
    fn drop(&mut self) {
        unsafe { bindings::sglist_free(self.list.as_ptr()) };
        if self.buffer.is_some() {
            panic!(
                "Either call `list_buffer.get_buffer()` or `list_buffer.leak_buffer()` instead of dropping it"
            );
        }
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
    /// This returns a `SgBuffer` to access the buffer after the list has been reset.
    pub fn sglist_append<B: Appendable>(sg: &mut SgList, buffer: B) -> Result<SgBuffer<B>> {
        let (appended_buffer, ptr, size) = buffer.append();
        let res = unsafe { bindings::sglist_append(sg.list.as_ptr(), ptr, size) };
        if res != 0 {
            forget(appended_buffer);
            return Err(ErrCode::from(res));
        }
        Ok(SgBuffer::new(appended_buffer, sg))
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
