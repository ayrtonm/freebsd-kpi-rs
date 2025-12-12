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

use crate::boxed::InnerBox;
use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use core::alloc::Layout;
use core::ffi::c_void;
use core::fmt::{Debug, Formatter};
use core::mem::replace;
use core::ops::{Deref, DerefMut};
use core::ptr::{NonNull, drop_in_place, read, write};
use core::{fmt, slice};

/// A growable array of some type T.
pub struct Vec<T> {
    ptr: NonNull<InnerBox<T>>,
    len: usize,
    capacity: usize,
}

impl<T: Debug> Debug for Vec<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Use Vec<T>'s Deref impl since it returns a &[T]
        f.debug_struct("Vec")
            .field("contents", &self.deref())
            .field("len", &self.len)
            .field("capacity", &self.capacity)
            .finish()
    }
}

impl<T> Vec<T> {
    /// Constructs an empty `Vec<T>`.
    ///
    /// This method does not allocate.
    pub const fn new() -> Self {
        Self {
            ptr: NonNull::dangling(),
            len: 0,
            capacity: 0,
        }
    }

    // Get a pointer to the contents
    pub const fn as_ptr(&self) -> *const T {
        let inner_box_ptr = self.ptr.as_ptr();
        unsafe { &raw mut (*inner_box_ptr).thing }
    }

    pub const fn as_slice(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.as_ptr(), self.len) }
    }

    pub const fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { slice::from_raw_parts_mut(self.as_ptr().cast_mut(), self.len) }
    }

    /// Returns the total number of elements the vector can hold without reallocating.
    pub const fn capacity(&self) -> usize {
        self.capacity
    }

    /// Returns the number of elements in the vector.
    pub const fn length(&self) -> usize {
        self.len
    }

    pub fn with_capacity(capacity: usize, ty: MallocType, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_with_capacity(capacity, ty, flags).unwrap()
    }

    /// Constructs an empty `Vec<T>` with *exactly* the specified capacity.
    pub fn try_with_capacity(capacity: usize, ty: MallocType, flags: MallocFlags) -> Result<Self> {
        if flags.contains(M_NOWAIT) && flags.contains(M_WAITOK) {
            return Err(EDOOFUS);
        };

        let whole_layout = Self::inner_layout(capacity);

        let void_ptr = malloc_aligned(whole_layout.size(), whole_layout.align(), ty, flags);
        match NonNull::new(void_ptr) {
            Some(nonnull_void_ptr) => {
                // malloc_aligned's result points to the MallocType
                let ptr = nonnull_void_ptr.cast::<MallocType>();
                unsafe { ptr.write(ty) };
                Ok(Self {
                    ptr: ptr.cast::<InnerBox<T>>(),
                    len: 0,
                    capacity,
                })
            }
            None => Err(ENOMEM),
        }
    }

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
            let new_vec = Self::try_with_capacity(additional, ty, flags)?;
            let _old_vec = replace(self, new_vec);
            // _old_vec gets dropped but since it had capacity 0 that's a no-op
            return Ok(());
        }
        // We can only call get_malloc_type if capacity is non-zero
        if self.get_malloc_type() != ty {
            return Err(EDOOFUS);
        }

        let whole_layout = Self::inner_layout(new_capacity);

        let void_ptr = unsafe {
            realloc(
                self.ptr.as_ptr().cast::<c_void>(),
                whole_layout.size(),
                ty,
                flags,
            )
        };
        match NonNull::new(void_ptr) {
            Some(nonnull_void_ptr) => {
                self.ptr = nonnull_void_ptr.cast::<InnerBox<T>>();
                self.capacity = new_capacity;
                Ok(())
            }
            None => Err(ENOMEM),
        }
    }

    /// Appends `value` returning `Some(value)` if there is not enough space to do so without resizing.
    pub fn push(&mut self, value: T) -> Option<T> {
        if self.len < self.capacity {
            unsafe {
                let next_ptr = self.item_ptr(self.len);
                write(next_ptr, value);
            }
            self.len += 1;
            None
        } else {
            Some(value)
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            return None;
        }
        self.len -= 1;
        Some(unsafe { read(self.item_ptr(self.len)) })
    }

    fn inner_layout(capacity: usize) -> Layout {
        let header_layout = Layout::new::<MallocType>();
        let contents_layout = Layout::array::<T>(capacity).unwrap();
        // Get the layout of struct { MallocType, [T; n] } minus the padding at the end
        let (whole_layout, _offset_of_contents) = header_layout.extend(contents_layout).unwrap();
        whole_layout
    }

    fn item_ptr(&self, n: usize) -> *mut T {
        let ptr = self.ptr.as_ptr();
        unsafe {
            let item_ptr = &raw mut (*ptr).thing;
            item_ptr.add(n)
        }
    }

    fn get_malloc_type(&self) -> MallocType {
        let ptr = self.ptr.as_ptr();
        let metadata_ptr = unsafe { &raw mut (*ptr).ty };
        unsafe { *metadata_ptr }
    }
}

impl<T: Copy> Vec<T> {
    pub fn fill_to_capacity(&mut self, value: T) {
        for _ in self.len..self.capacity {
            self.push(value);
        }
    }
}

impl<T> Deref for Vec<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T> DerefMut for Vec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

impl<'a, T> IntoIterator for &'a Vec<T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;
    fn into_iter(self) -> <&'a Vec<T> as IntoIterator>::IntoIter {
        // Rely on DerefMut<[T]> to use core::slice impl
        self.iter()
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
                drop_in_place(self.item_ptr(n));
            }
        }
        // Don't need to drop the excess capacity or MallocType pointer in place
        unsafe { free(self.ptr.as_ptr().cast::<c_void>(), self.get_malloc_type()) }
    }
}

unsafe impl<T: Sync> Sync for Vec<T> {}
unsafe impl<T: Send> Send for Vec<T> {}

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
