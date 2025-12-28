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

//! The `Box<T>` type for heap allocation.

use crate::malloc::{Malloc, MallocFlags};
use crate::prelude::*;
use core::cmp::PartialEq;
use core::ffi::c_void;
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::marker::PhantomData;
use core::mem::{forget, size_of};
use core::ops::{Deref, DerefMut};
use core::ptr::{NonNull, drop_in_place};

/// A pointer to something on the heap.
///
/// When a `Box<T>` is dropped, the T on the heap is deallocated. The memory layout of this type is
/// equivalent to `void *ptr` in C.
#[repr(C)]
pub struct Box<T, M: Malloc = M_DEVBUF>(pub(crate) NonNull<T>, PhantomData<*mut M>);

// impl Deref to allow using a Box<T> like a T transparently
impl<T, M: Malloc> Deref for Box<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<T, M: Malloc> DerefMut for Box<T, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

impl<T: Debug, M: Malloc> Debug for Box<T, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Use Box<T>'s Deref impl since it returns a &T
        Debug::fmt(self.deref(), f)
    }
}

impl<T: PartialEq, M: Malloc> PartialEq for Box<T, M> {
    fn eq(&self, other: &Self) -> bool {
        // Use Box<T>'s Deref impl since it returns a &T
        PartialEq::eq(self.deref(), other)
    }
}

impl<T: Eq, M: Malloc> Eq for Box<T, M> {}

unsafe impl<T: Sync, M: Malloc> Sync for Box<T, M> {}
unsafe impl<T: Send, M: Malloc> Send for Box<T, M> {}

impl<T, M: Malloc> Drop for Box<T, M> {
    fn drop(&mut self) {
        let ptr = self.0.as_ptr();
        // Drop everything that the T owns
        unsafe { drop_in_place(ptr) }
        // Deallocate the memory for the T
        unsafe { free(ptr.cast::<c_void>(), M::malloc_type()) }
    }
}

impl<T, M: Malloc> Box<T, M> {
    pub fn new(t: T, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_new(t, flags).unwrap()
    }

    pub fn try_new(t: T, flags: MallocFlags) -> Result<Self> {
        if flags.contains(M_NOWAIT) && flags.contains(M_WAITOK) {
            return Err(EDOOFUS);
        };
        let t_size = size_of::<T>();
        let t_align = align_of::<T>();
        let void_ptr = malloc_aligned(t_size, t_align, M::malloc_type(), flags);
        match NonNull::new(void_ptr) {
            Some(nonnull_void_ptr) => {
                let boxed_t_ptr = nonnull_void_ptr.cast::<T>();
                unsafe { boxed_t_ptr.write(t) };
                Ok(Self(boxed_t_ptr, PhantomData))
            }
            None => Err(ENOMEM),
        }
    }
}

impl<T, M: Malloc> Box<T, M> {
    pub fn into_raw(b: Self) -> *mut T {
        let res = b.0.as_ptr();
        forget(b);
        res
    }

    pub unsafe fn from_raw(raw: *mut T) -> Self {
        Self(NonNull::new(raw).unwrap(), PhantomData)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::LoudDrop;

    #[test]
    fn boxed() {
        let value = 42;
        let x: Box<u64> = Box::try_new(value, M_DEVBUF, M_NOWAIT).unwrap();
        assert_eq!(*x, value);
        println!("{x:?}");
    }

    #[test]
    fn bad_flags() {
        let x = Box::try_new(0xdeadbeefu32, M_DEVBUF, M_NOWAIT | M_WAITOK);
        assert!(x == Err(EDOOFUS));
    }

    #[test]
    fn oom() {
        let x = Box::try_new(111u8, M_DEVBUF, M_USE_RESERVE);
        assert!(x == Err(ENOMEM));
    }

    #[test]
    fn round_trip_drop() {
        let y = Box::try_new(LoudDrop, M_DEVBUF, M_NOWAIT).unwrap();
        let y_ref = Box::leak(y);
        let y_ptr = y_ref as *const InnerBox<LoudDrop>;
        let nonnull_y = NonNull::new(y_ptr.cast_mut()).unwrap();
        let _new_y = unsafe { Box::from_raw(nonnull_y) };
    }
}
