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
use core::ops::Deref;

mod sealed {
    use super::*;

    pub trait ScatterBufPrivate {
        fn get_buf(&self) -> (*mut c_void, usize);
    }
}

pub trait ScatterBuf: sealed::ScatterBufPrivate {}

impl<T> ScatterBuf for Box<T> {}
impl<T> ScatterBuf for Vec<T> {}

impl<T> sealed::ScatterBufPrivate for Box<T> {
    fn get_buf(&self) -> (*mut c_void, usize) {
        let ptr = self.deref() as *const T;
        (ptr.cast::<c_void>().cast_mut(), size_of::<T>())
    }
}

impl<T> sealed::ScatterBufPrivate for Vec<T> {
    fn get_buf(&self) -> (*mut c_void, usize) {
        let ptr = self.as_ptr().cast_mut();
        (ptr.cast::<c_void>(), self.len())
    }
}

pub type ScatterListPtr = SyncPtr<sglist>;

/// A handle to a buffer in a scatter-gather list
///
/// This has both ownership of the buffer and owns a refcount to the list. Since the buffer has
/// aliasing pointers in the list this type does not allow access to the buffer until the list is
/// reset. Once the list has been reset the buffer can be accessed by calling
/// `list_buffer.get_buffer()`.
pub struct ListBuffer<B: ScatterBuf> {
    buffer: Option<B>,
    list: ScatterListPtr,
}

impl<B: ScatterBuf> ListBuffer<B> {
    /// Create a handle to a buffer in a scatter-gather list.
    pub fn new(buffer: B, sg: &ScatterList) -> Self {
        sglist_hold(sg);
        Self {
            buffer: Some(buffer),
            list: sg.list,
        }
    }

    pub fn get_buffer(mut self) -> B {
        assert!(unsafe { bindings::sglist_length(self.list.as_ptr()) } == 0);
        self.buffer.take().unwrap()
        // Drop impl calls sglist_free to release the sglist refcount
    }

    pub fn leak_buffer(self) {
        // Release the sglist reference
        unsafe { bindings::sglist_free(self.list.as_ptr()) };
        forget(self);
    }
}

impl<B: ScatterBuf> Drop for ListBuffer<B> {
    fn drop(&mut self) {
        unsafe { bindings::sglist_free(self.list.as_ptr()) };
        if self.buffer.is_some() {
            panic!(
                "Either call `list_buffer.get_buffer()` or `list_buffer.leak_buffer()` instead of dropping it"
            );
        }
    }
}

pub struct ScatterList {
    list: ScatterListPtr,
}

impl ScatterList {
    pub fn new(nsegs: usize, flags: MallocFlags) -> Result<Self> {
        let list = unsafe { bindings::sglist_alloc(nsegs.try_into().unwrap(), flags.0) };
        if list.is_null() {
            return Err(ENOMEM);
        }
        let list = ScatterListPtr::new(list);
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
    pub fn sglist_alloc(nsegs: usize, flags: MallocFlags) -> Result<ScatterList> {
        ScatterList::new(nsegs, flags)
    }

    /// Append the virtual addresses spanned by `buffer` to the scatter-gather list.
    ///
    /// This returns a `ListBuffer` to access the buffer after the list has been reset.
    pub fn sglist_append<B: ScatterBuf>(sg: &mut ScatterList, buffer: B) -> Result<ListBuffer<B>> {
        let (ptr, size) = buffer.get_buf();
        let res = unsafe { bindings::sglist_append(sg.list.as_ptr(), ptr, size) };
        if res != 0 {
            forget(buffer);
            return Err(ErrCode::from(res));
        }
        Ok(ListBuffer::new(buffer, sg))
    }

    pub fn sglist_reset(sg: &mut ScatterList) {
        unsafe { bindings::sglist_reset(sg.list.as_ptr()) }
    }

    pub fn sglist_length(sg: &ScatterList) -> usize {
        unsafe { bindings::sglist_length(sg.list.as_ptr()) }
    }

    pub fn sglist_hold(sg: &ScatterList) {
        unsafe { bindings::sglist_hold(sg.list.as_ptr()) };
    }
}
