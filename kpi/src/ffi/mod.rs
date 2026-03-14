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

use crate::bindings;
use crate::bindings::device_t;
use crate::boxed::Box;
use core::fmt::Debug;
use core::mem::MaybeUninit;
use core::ops::{Deref, DerefMut};
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
#[derive(Debug)]
pub struct Ptr<T>(pub(crate) *mut T);

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

    /// Initialize the externally-managed object to `t` and return a `UniqueRef` to the pointee.
    pub fn init(self, t: T) -> UniqueRef<'a, T> {
        *self.1 = true;
        UniqueRef(self.0.write(t))
    }
}

/// A unique pointer to an externally-managed object.
#[repr(C)]
#[derive(Debug)]
pub struct UniqueRef<'a, T>(&'a mut T);

impl<'a, T> UniqueRef<'a, T> {
    /// Destroys a `UniqueRef` and returns an `Ref` to the same object.
    pub fn into_ref(self) -> Ref<'a, T> {
        Ref(self.0)
    }

    #[cfg(test)]
    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        UniqueRef(unsafe { ptr.as_mut().unwrap() })
    }
}

/// Allows transparently using `UniqueRef<T>` like a `&T`.
impl<'a, T> Deref for UniqueRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Allows transparently using `UniqueRef<T>` like a `&mut T`.
impl<'a, T> DerefMut for UniqueRef<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

/// A pointer to an externally-managed object.
#[repr(C)]
#[derive(Debug)]
pub struct Ref<'a, T: ?Sized>(&'a T);

/// Allows implicitly making copies of the `Ref<T>` just like `&T` allows.
impl<'a, T> Copy for Ref<'a, T> {}

/// Allows explicitly making copies of the `Ref<T>` just like `&T` allows.
impl<'a, T> Clone for Ref<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'a, T> Ref<'a, T> {
    pub fn into_raw(x: Self) -> *mut T {
        (x.0 as *const T).cast_mut()
    }

    pub fn into_ptr(x: Self) -> Ptr<T> {
        let ptr = Self::into_raw(x);
        Ptr::new(ptr)
    }

    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        Self(unsafe { ptr.as_ref().unwrap() })
    }

    pub unsafe fn from_sync_ptr(ptr: Ptr<T>) -> Self {
        Self(unsafe { ptr.0.as_ref().unwrap() })
    }

    /// Create an Ref<U> from an Ref<T> by accessing a field on T
    ///
    /// The `proj!` macro should be used instead of calling this directly.
    ///
    /// # Safety
    ///
    /// The function `f` must only access a single field (i.e. `x.field`)
    pub unsafe fn map_field<U, F: FnOnce(&T) -> &U>(x: Self, f: F) -> Ref<'a, U> {
        Ref(f(x.0))
    }
}

impl<'a, T: FixedIdx> Ref<'a, T> {
    /// Create an Ref<U> from an Ref<T> by indexing into T
    ///
    /// The `proj!` macro should be used instead of calling this directly.
    ///
    /// # Safety
    ///
    /// The function `f` must only index once into T (i.e. `x[n]`)
    pub unsafe fn map_idx<U, F: FnOnce(&T) -> &U>(x: Self, f: F) -> Ref<'a, U> {
        Ref(f(x.0))
    }
}

#[diagnostic::on_unimplemented(
    message = "Indexing into {Self} might not access the same address every time",
    label = "Cannot create an Ref<T> from this. Use a boxed slice or array instead"
)]
pub unsafe trait FixedIdx {}
unsafe impl<T, const N: usize> FixedIdx for [T; N] {}
unsafe impl<T> FixedIdx for Box<[T]> {}

/// Allows transparently using `Ref<T>` like a `&T`.
impl<'a, T: ?Sized> Deref for Ref<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}
