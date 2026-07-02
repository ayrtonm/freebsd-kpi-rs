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
 *    notice, this list of conditions and the following dialaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following dialaimer in the
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

//! Utilities related to FFI with C.

use core::fmt;
use core::fmt::{Debug, Formatter};
use core::pin::Pin;
use core::mem::MaybeUninit;
use crate::sync::arc::Arc;
use crate::boxed::Box;
use crate::malloc::Malloc;
use core::ptr::null_mut;

mod cstring;
mod subclass;

pub use cstring::{ArrayCString, CString, ToArrayCString};
pub use subclass::{SubClass, SubClassOf};

/// A pointer type implementing `Sync`.
///
/// This is useful for pointer types that are expected to be shared between threads without explicit
/// synchronization.
#[repr(C)]
pub struct Ptr<T>(pub(crate) *mut T);

impl<T> Debug for Ptr<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ptr").field("addr", &self.0).finish()
    }
}

impl<T> Default for Ptr<T> {
    fn default() -> Self {
        Self(null_mut())
    }
}

// Allows explicitly cloning a `Ptr` just like a regular raw pointer
impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

// Allows implicitly copying a `Ptr` just like a raw pointer
impl<T> Copy for Ptr<T> {}

impl<T> Ptr<T> {
    /// Creates a new null `Ptr`
    pub const fn null() -> Self {
        Self(null_mut())
    }

    /// Creates a new `Ptr` from a raw pointer.
    pub const fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }

    /// Creates a new `Ptr` from a reference.
    ///
    /// `Ptr` does not guarantee that the pointee will not be freed/move so the caller should ensure
    /// that the pointee will live at the same address for as long as the return value will be
    /// needed.
    pub const fn from_ref(x: &T) -> Self {
        Self(x as *const T as *mut T)
    }

    /// Get a raw pointer for the `Ptr`
    pub fn as_ptr(self) -> *mut T {
        self.0
    }

    pub fn is_null(&self) -> bool {
        self.0.is_null()
    }

    pub unsafe fn get(&self) -> &T {
        unsafe { self.0.as_ref().unwrap() }
    }
}

// SAFETY: `Ptr` is intended for cases where `Sync` is intentionally desired on the pointer
unsafe impl<T> Sync for Ptr<T> {}
unsafe impl<T> Send for Ptr<T> {}

/// A unique pointer to an uninitialized, externally-managed object.
#[derive(Debug)]
pub struct UninitRef<'a, T>(&'a mut MaybeUninit<T>, &'a mut bool);

impl<'a, T> UninitRef<'a, T> {
    pub unsafe fn from_raw(ptr: &'a *mut T, init: &'a mut bool) -> Self {
        *init = false;
        Self(
            unsafe { ptr.cast::<MaybeUninit<T>>().as_mut().unwrap() },
            init,
        )
    }

    /// Initialize the externally-managed object to `t` and return a pinned reference to the pointee
    pub fn init(self, t: T) -> Pin<&'a T> {
        *self.1 = true;
        let res = self.0.write(t);
        unsafe { Pin::new_unchecked(res) }
    }
}

pub unsafe trait FixedIndex {}
unsafe impl<T, const N: usize> FixedIndex for [T; N] {}
unsafe impl<T> FixedIndex for [T] {}

unsafe impl<T, M: Malloc> FixedIndex for Box<[T], M> {}
unsafe impl<T, M: Malloc, const N: usize> FixedIndex for Box<[T; N], M> {}

unsafe impl<T, M: Malloc, const N: usize> FixedIndex for Arc<[T; N], M> {}

pub fn assert_pin_has_fixed_index<T: FixedIndex>(_p: Pin<&T>) {}
