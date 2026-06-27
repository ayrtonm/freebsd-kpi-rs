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
use core::mem::MaybeUninit;
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
    pub unsafe fn from_raw(ptr: *mut T, init: &'a mut bool) -> Self {
        *init = false;
        Self(
            unsafe { ptr.cast::<MaybeUninit<T>>().as_mut().unwrap() },
            init,
        )
    }

    // FIXME: It's super easy to coerce this to a shared reference, hand it off to C then reuse the
    // mutable reference while the C code can potentially invoke a callback. This should return &T
    // and and unsafe init_mut should return &mut T. I'm pretty sure there's no real way to make
    // init_mut safe.
    /// Initialize the externally-managed object to `t` and return a `UniqueRef` to the pointee.
    pub fn init(self, t: T) -> &'a mut T {
        *self.1 = true;
        self.0.write(t)
    }
}
