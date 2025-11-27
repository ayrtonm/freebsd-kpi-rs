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

//! The `Vec<T>` type for contiguous growable arrays allocated in the heap.

use crate::boxed::Box;
use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use core::alloc::Layout;
use core::cmp::max;
use core::ffi::c_void;
use core::fmt::{Debug, Formatter};
use core::mem::{align_of, replace};
use core::ops::{Deref, DerefMut};
use core::ptr::{NonNull, drop_in_place, read, write};
use core::{fmt, slice};

/// A growable array of some type T.
///
/// The memory layout of this type is equivalent to
/// ```c
/// struct vec {
///     T *ptr;
///     size_t len;
///     size_t capacity;
/// };
/// ```
/// while the pointee is laid out as follows
/// ```
/// [malloc_type *][padding][T][T][T][padding]
/// ```
/// where padding 1 is that necessary to align the first `T` and padding 2 is that necessary to
/// align the whole `Vec<T>` (note: this is only present when
/// `alignof(T) < alignof(malloc_type *)`). Also the `Vec<T>` pointer is always either non-null,
/// dangling or points to the first `T`.
#[repr(C)]
pub struct Vec<T> {
    ptr: NonNull<T>,
    // TODO: Make these private again. It's only pub(crate) for Vec<T>'s ScatterBuf impl
    pub(crate) len: usize,
    pub(crate) capacity: usize,
}

impl<T: Debug> Debug for Vec<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Use Vec<T>'s Deref impl since it returns a &[T]
        Debug::fmt(self.deref(), f)
    }
}

impl<T> Vec<T> {
    /// Constructs an empty `Vec<T>`.
    ///
    /// The vector will not allocate unless a method takes a `MallocFlags` argument.
    pub const fn new() -> Self {
        // TODO: Document that this doesn't store the MallocType
        Self {
            ptr: NonNull::dangling(),
            len: 0,
            capacity: 0,
        }
    }

    pub const fn as_ptr(&self) -> *const T {
        self.ptr.as_ptr()
    }

    pub const fn as_slice(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }

    fn array_ptr_offset() -> usize {
        let metadata_layout = Layout::new::<MallocType>();
        let item_layout = Layout::new::<T>();
        let (_concat_layout, offsetof_array) = metadata_layout.extend(item_layout).unwrap();
        offsetof_array
    }

    fn size_for_n_elements(n: usize) -> usize {
        // Get metadata layout
        let metadata_layout = Layout::new::<MallocType>();
        // Get layout of a Vec<T> with n elements
        let item_layout = Layout::array::<T>(n).unwrap();
        // Concatenate layouts and pad end
        let (concat_layout, _offsetof_array) = metadata_layout.extend(item_layout).unwrap();
        let whole_layout = concat_layout.pad_to_align();
        whole_layout.size()
    }

    fn get_metadata_ptr(&self) -> NonNull<MallocType> {
        unsafe {
            self.ptr
                .byte_sub(Self::array_ptr_offset())
                .cast::<MallocType>()
        }
    }

    pub fn with_capacity(capacity: usize, ty: MallocType, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_with_capacity(capacity, ty, flags).unwrap()
    }
    /// Constructs an empty `Vec<T>` with *exactly* the specified capacity.
    pub fn try_with_capacity(capacity: usize, ty: MallocType, flags: MallocFlags) -> Result<Self> {
        let align = max(align_of::<T>(), align_of::<MallocType>());
        let void_ptr: *mut c_void =
            malloc_aligned(Self::size_for_n_elements(capacity), align, ty, flags);
        match NonNull::new(void_ptr) {
            Some(nonnull_void_ptr) => {
                let metadata_ptr = nonnull_void_ptr.cast::<MallocType>();
                unsafe { metadata_ptr.write(ty) };
                let ptr = unsafe {
                    nonnull_void_ptr
                        .byte_add(Self::array_ptr_offset())
                        .cast::<T>()
                };
                Ok(Self {
                    ptr,
                    len: 0,
                    capacity,
                })
            }
            None => Err(ENOMEM),
        }
    }

    /// Returns the total number of elements the vector can hold without reallocating.
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Returns the number of elements in the vector.
    pub fn length(&self) -> usize {
        self.len
    }

    /// Tries to reserve capacity for exactly `additional` more elements to be inserted in the given
    /// `Vec<T>`. The collection will never speculatively reserve more space. After calling
    /// `try_reserve`, the capacity will equal `self.len() + additional` if it returns `Ok(())`.
    /// Does nothing if capacity is already sufficient.
    pub fn try_reserve(
        &mut self,
        additional: usize,
        ty: MallocType,
        flags: MallocFlags,
    ) -> Result<()> {
        let new_capacity = self.len() + additional;
        if self.capacity >= new_capacity {
            return Ok(());
        }
        if self.capacity == 0 {
            let new_vec = Vec::try_with_capacity(additional, ty, flags)?;
            let _old_vec = replace(self, new_vec);
            // _old_vec gets dropped but since it had capacity 0 that's a no-op
            return Ok(());
        }
        let metadata_ptr = self.get_metadata_ptr().as_ptr();
        let malloc_ptr = metadata_ptr.cast::<c_void>();
        let new_size = Self::size_for_n_elements(new_capacity);
        let stored_ty = unsafe { *metadata_ptr };
        if ty != stored_ty {
            return Err(EDOOFUS);
        }
        // Allocate a larger block and copy over contents. This includes the metadata, all elements
        // (including uninitialized) and padding.
        let new_ptr = unsafe { realloc(malloc_ptr, new_size, ty, flags) };
        match NonNull::new(new_ptr) {
            Some(nonnull_void_ptr) => {
                let new_ptr = unsafe {
                    nonnull_void_ptr
                        .byte_add(Self::array_ptr_offset())
                        .cast::<T>()
                };
                self.ptr = new_ptr;
                self.capacity = new_capacity;
                Ok(())
            }
            None => Err(ENOMEM),
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            return None;
        }
        self.len -= 1;
        Some(unsafe { read(self.ptr.add(self.len).as_ptr()) })
    }

    pub fn push(&mut self, value: T) {
        assert!(self.try_push(value).is_none());
    }

    pub fn try_push(&mut self, value: T) -> Option<T> {
        if self.len < self.capacity {
            unsafe {
                let next_ptr = self.ptr.add(self.len).as_ptr();
                write(next_ptr, value);
            }
            self.len += 1;
            None
        } else {
            Some(value)
        }
    }

    pub fn into_boxed_slice(self) -> Box<[T]> {
        todo!("")
        //// TODO: Ensure the excess capacity is not leaked
        //let fat_ptr = NonNull::slice_from_raw_parts(self.ptr, self.len);
        //// Ownership is transferred to Box which is now responsible for dropping all the initialized contents
        //forget(self);
        ////Box(fat_ptr)
        //todo!("")
    }
}

impl<T> Deref for Vec<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.len) }
    }
}

impl<T> DerefMut for Vec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len) }
    }
}

impl<'a, T> IntoIterator for &'a mut Vec<T> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;
    fn into_iter(self) -> <&'a mut Vec<T> as IntoIterator>::IntoIter {
        // Rely on DerefMut<[T]> to use core::slice impl
        self.iter_mut()
    }
}

impl<T> Drop for Vec<T> {
    fn drop(&mut self) {
        if self.capacity == 0 {
            return;
        }
        // Drop the initialized elements in place
        for n in 0..self.len {
            unsafe {
                let ptr = self.ptr.add(n).as_ptr();
                drop_in_place(ptr);
            }
        }
        // Don't need to drop the excess capacity or MallocType pointer in place

        // Get the MallocType to free the entire thing
        let metadata_ptr = self.get_metadata_ptr().as_ptr();
        let ty = unsafe { *metadata_ptr };
        unsafe { free(metadata_ptr.cast::<c_void>(), ty) }
    }
}

unsafe impl<T: Sync> Sync for Vec<T> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::LoudDrop;
    use std::ffi::CStr;

    #[test]
    fn small_vec() {
        let mut x: Vec<(u32, LoudDrop)> = Vec::try_with_capacity(3, M_DEVBUF, M_NOWAIT).unwrap();
        x.push((1, LoudDrop));
        x.push((2, LoudDrop));
    }

    #[test]
    fn push_pop() {
        let mut x: Vec<&'static CStr> = Vec::new();
        assert_eq!(x.len(), 0);
        assert_eq!(x.capacity(), 0);
        x.try_reserve(5, M_DEVBUF, M_WAITOK).unwrap();
        assert_eq!(x.len(), 0);
        assert_eq!(x.capacity(), 5);
        x.push(c"hello");
        assert_eq!(x.len(), 1);
        assert_eq!(x.capacity(), 5);
        x.push(c"world");
        x.push(c"this");
        x.push(c"is");
        x.push(c"reversed");
        assert_eq!(x.pop(), Some(c"reversed"));
        assert_eq!(x.len(), 4);
        assert_eq!(x.capacity(), 5);
        assert_eq!(x.pop(), Some(c"is"));
        assert_eq!(x.pop(), Some(c"this"));
    }

    #[test]
    fn simple_drop() {
        let _x = Vec::<u16>::new();
        // Test that the no-op Drop path works
    }

    #[test]
    fn oom() {
        let x = Vec::<u8>::try_with_capacity(4, M_DEVBUF, M_USE_RESERVE);
        assert_eq!(x.err(), Some(ENOMEM));
    }
}
