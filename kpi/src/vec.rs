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

use crate::prelude::*;
use crate::boxed::Box;
use crate::malloc::{MallocFlags, MallocType};
use core::alloc::Layout;
use core::mem::forget;
use core::ptr::{NonNull, drop_in_place, write};
use core::marker::PhantomData;
use core::ops::{Deref, DerefMut};
use core::slice;

#[derive(Debug)]
pub struct Vec<T, M: MallocType = M_DEVBUF> {
    ptr: NonNull<T>,
    len: usize,
    capacity: usize,
    _ty: PhantomData<M>,
}

impl<T, M: MallocType> Vec<T, M> {
    /// Constructs an empty `Vec<T>`.
    ///
    /// The vector will not allocate unless a method takes a `MallocFlags` argument.
    pub const fn new() -> Self {
        Self {
            ptr: NonNull::dangling(),
            len: 0,
            capacity: 0,
            _ty: PhantomData,
        }
    }

    /// Constructs an empty `Vec<T>` with *exactly* the specified capacity.
    pub fn try_with_capacity(capacity: usize, flags: MallocFlags) -> Result<Self> {
        let layout = Layout::new::<T>();
        let padded_layout = layout.pad_to_align();
        let total_size = padded_layout.size() * capacity;
        let void_ptr = malloc(total_size, M::TYPE, flags);
        match NonNull::new(void_ptr) {
            Some(nonnull_void_ptr) => {
                let ptr = nonnull_void_ptr.cast::<T>();
                Ok(Self {
                    ptr,
                    len: 0,
                    capacity,
                    _ty: PhantomData,
                })
            },
            None => Err(ENOMEM),
        }
    }

    /// Returns the total number of elements the vector can hold without reallocating.
    pub fn capacity(&self) -> usize { self.capacity }

    /// Returns the number of elements in the vector.
    pub fn length(&self) -> usize { self.len }

    /// Tries to reserve capacity for exactly `additional` more elements to be inserted in the given
    /// `Vec<T>`. The collection will never speculatively reserve more space. After calling
    /// `try_reserve`, the capacity will equal `self.len() + additional` if it returns `Ok(())`.
    /// Does nothing if capacity is already sufficient.
    pub fn reserve(&mut self, additional: usize, _flags: MallocFlags) -> Result<()> {
        if self.capacity >= self.len() + additional {
            return Ok(())
        }
        todo!("call realloc")
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            return None;
        }
        self.len -= 1;
        todo!("")
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

    pub(crate) fn as_ptr(&self) -> *mut T {
        self.ptr.as_ptr()
    }

    pub fn into_boxed_slice(self) -> Box<[T], M> {
        // TODO: Ensure the excess capacity is not leaked
        let fat_ptr = NonNull::slice_from_raw_parts(self.ptr, self.len);
        // Ownership is transferred to Box which is now responsible for dropping all the initialized contents
        forget(self);
        Box(fat_ptr, PhantomData)
    }
}

impl<T, M: MallocType> Deref for Vec<T, M> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        unsafe {
            slice::from_raw_parts(self.ptr.as_ptr(), self.len)
        }
    }
}

impl<T, M: MallocType> DerefMut for Vec<T, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len)
        }
    }
}


impl<'a, T, M: MallocType> IntoIterator for &'a mut Vec<T, M> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;
    fn into_iter(self) -> <&'a mut Vec<T, M> as IntoIterator>::IntoIter {
        // Rely on DerefMut<[T]> to use core::slice impl
        self.iter_mut()
    }
}

impl<T, M: MallocType> Drop for Vec<T, M> {
    fn drop(&mut self) {
        for n in 0..self.len {
            unsafe {
                let ptr = self.ptr.add(n).as_ptr();
                drop_in_place(ptr);
            }
        }
    }
}
