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
pub struct SyncPtr<T>(pub(crate) *mut T);

impl<T> Default for SyncPtr<T> {
    fn default() -> Self {
        Self(null_mut())
    }
}

// Allows explicitly cloning a `SyncPtr` just like a regular raw pointer
impl<T> Clone for SyncPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

// Allows implicitly copying a `SyncPtr` just like a raw pointer
impl<T> Copy for SyncPtr<T> {}

impl<T> SyncPtr<T> {
    /// Creates a new null `SyncPtr`
    pub const fn null() -> Self {
        Self(null_mut())
    }

    /// Creates a new `SyncPtr` from a raw pointer.
    pub const fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }

    /// Get a raw pointer for the `SyncPtr`
    pub fn as_ptr(self) -> *mut T {
        self.0
    }

    pub fn is_null(&self) -> bool {
        self.0.is_null()
    }
}

// SAFETY: `SyncPtr` is intended for cases where `Sync` is intentionally desired on the pointer
unsafe impl<T> Sync for SyncPtr<T> {}
unsafe impl<T> Send for SyncPtr<T> {}

/// A unique pointer to an uninitialized, externally-managed object.
#[derive(Debug)]
pub struct UninitExt<'a, T>(&'a mut MaybeUninit<T>, &'a mut bool);

impl<'a, T> UninitExt<'a, T> {
    pub unsafe fn from_raw(ptr: *mut T, init: &'a mut bool) -> Self {
        *init = false;
        Self(
            unsafe { ptr.cast::<MaybeUninit<T>>().as_mut().unwrap() },
            init,
        )
    }

    /// Initialize the externally-managed object to `t` and return a `UniqueExt` to the pointee.
    pub fn init(self, t: T) -> UniqueExt<'a, T> {
        *self.1 = true;
        UniqueExt(self.0.write(t))
    }
}

/// A unique pointer to an externally-managed object.
#[repr(C)]
#[derive(Debug)]
pub struct UniqueExt<'a, T>(&'a mut T);

impl<'a, T> UniqueExt<'a, T> {
    /// Destroys a `UniqueExt` and returns an `Ext` to the same object.
    pub fn into_ext(self) -> Ext<'a, T> {
        Ext(self.0)
    }

    #[cfg(test)]
    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        UniqueExt(unsafe { ptr.as_mut().unwrap() })
    }
}

/// Allows transparently using `UniqueExt<T>` like a `&T`.
impl<'a, T> Deref for UniqueExt<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Allows transparently using `UniqueExt<T>` like a `&mut T`.
impl<'a, T> DerefMut for UniqueExt<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

/// A pointer to an externally-managed object.
#[repr(C)]
#[derive(Debug)]
pub struct Ext<'a, T: ?Sized>(&'a T);

/// Allows implicitly making copies of the `Ext<T>` just like `&T` allows.
impl<'a, T> Copy for Ext<'a, T> {}

/// Allows explicitly making copies of the `Ext<T>` just like `&T` allows.
impl<'a, T> Clone for Ext<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'a, T> Ext<'a, T> {
    pub fn into_raw(x: Self) -> *mut T {
        (x.0 as *const T).cast_mut()
    }

    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        Self(unsafe { ptr.as_ref().unwrap() })
    }

    /// Create an Ext<U> from an Ext<T> by accessing a field on T
    ///
    /// The `ext!` macro should be used instead of calling this directly.
    ///
    /// # Safety
    ///
    /// The function `f` must only access a single field (i.e. `x.field`)
    pub unsafe fn map_field<U, F: FnOnce(&T) -> &U>(x: Self, f: F) -> Ext<'a, U> {
        Ext(f(x.0))
    }
}

impl<'a, T: FixedIdx> Ext<'a, T> {
    /// Create an Ext<U> from an Ext<T> by indexing into T
    ///
    /// The `ext!` macro should be used instead of calling this directly.
    ///
    /// # Safety
    ///
    /// The function `f` must only index once into T (i.e. `x[n]`)
    pub unsafe fn map_idx<U, F: FnOnce(&T) -> &U>(x: Self, f: F) -> Ext<'a, U> {
        Ext(f(x.0))
    }
}

#[diagnostic::on_unimplemented(
    message = "Indexing into {Self} might not access the same address every time",
    label = "Cannot create an Ext<T> from this. Use a boxed slice or array instead"
)]
pub unsafe trait FixedIdx {}
unsafe impl<T, const N: usize> FixedIdx for [T; N] {}
unsafe impl<T> FixedIdx for Box<[T]> {}

/// Allows transparently using `Ext<T>` like a `&T`.
impl<'a, T: ?Sized> Deref for Ext<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[derive(Debug)]
pub struct SoftcRef<T>(SyncPtr<T>);

impl<T> SoftcRef<T> {
    pub fn new(dev: device_t) -> Self {
        // TODO: typecheck
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev) };
        let sc_ptr = sc_void_ptr.cast::<T>();
        unsafe { Self::from_raw(sc_ptr) }
    }

    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        Self(SyncPtr::new(ptr))
    }

    pub unsafe fn get(&self) -> &T {
        unsafe { self.0.as_ptr().as_ref().unwrap() }
    }
}
