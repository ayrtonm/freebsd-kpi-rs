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

use crate::boxed::Box;
use crate::intr::Callout;
use crate::malloc::Malloc;
use core::fmt::Debug;
use core::marker::PhantomData;
use core::ops::{Deref, DerefMut};
use core::ptr::{NonNull, null_mut};

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
///
/// This also holds a mutable reference a `bool` flag which is set only if the `init` method was
/// called.
#[repr(C)]
#[derive(Debug)]
pub struct UninitExt<'a, T>(NonNull<T>, &'a mut bool);

impl<'a, T> UninitExt<'a, T> {
    pub unsafe fn from_raw(ptr: *mut T, init: &'a mut bool) -> Self {
        *init = false;
        Self(NonNull::new(ptr).unwrap(), init)
    }

    /// Initialize the externally-managed object to `t` and return a `MutExt` to the pointee.
    pub fn init<'b>(self, t: T) -> MutExt<'b, T> {
        *self.1 = true;
        unsafe { self.0.write(t) }
        MutExt(self.0, PhantomData)
    }
}

#[diagnostic::on_unimplemented(
    message = "Can't create MutExtRef from variable on the stack",
    label = "This must be an externally-managed object"
)]
pub trait MapMutExt<T> {
    unsafe fn map_mut<U, F: FnOnce(&mut T) -> &mut U>(&mut self, f: F) -> MutExtRef<'_, U>;
}

impl<'a, T> MapMutExt<T> for MutExt<'a, T> {
    unsafe fn map_mut<U, F: FnOnce(&mut T) -> &mut U>(&mut self, f: F) -> MutExtRef<'_, U> {
        let new_ptr = f(self.deref_mut()) as *mut U;
        MutExtRef(NonNull::new(new_ptr).unwrap(), PhantomData)
    }
}

impl<'a, T> MapMutExt<T> for MutExtRef<'a, T> {
    unsafe fn map_mut<U, F: FnOnce(&mut T) -> &mut U>(&mut self, f: F) -> MutExtRef<'_, U> {
        let new_ptr = f(self.deref_mut()) as *mut U;
        MutExtRef(NonNull::new(new_ptr).unwrap(), PhantomData)
    }
}

impl<T, M: Malloc> MapMutExt<T> for Box<T, M> {
    unsafe fn map_mut<U, F: FnOnce(&mut T) -> &mut U>(&mut self, f: F) -> MutExtRef<'_, U> {
        let new_ptr = f(self.deref_mut()) as *mut U;
        MutExtRef(NonNull::new(new_ptr).unwrap(), PhantomData)
    }
}

/// A unique pointer to an externally-managed object.
///
/// This is the mutable version of `Ext` which is only ever created after initializing an
/// `UninitExt`. Currently this is only happens after initializing a device softc. `MutExt`
/// may be used transparently as a mutable reference. Some functions that take callback args may
/// require `Ext` which can be created by calling the `into_ref` method. This creates a new
/// `Ext` and destroys the `MutExt` so mutating the pointee after calling this method must be
/// done using `OnceInit`, `Mutable` or some kind of lock (e.g. `Mutex`/`SpinLock`) or atomic.
/// Destroying the `MutExt` makes mutation more cumbersome, but is done to avoid the potential
/// for undefined behavior caused by having an aliased mutable reference (and subsequent
/// opportunities for the compiler to make invalid optimizations).
#[repr(C)]
#[derive(Debug)]
pub struct MutExt<'a, T>(NonNull<T>, PhantomData<&'a mut T>);

impl<'a, T> MutExt<'a, T> {
    /// Destroys a `MutExt` and returns an `Ext` to the same object.
    pub fn into_ref<'b>(self) -> Ext<'b, T> {
        Ext(self.0, PhantomData)
    }

    pub fn as_mut(&mut self) -> MutExtRef<'_, T> {
        MutExtRef(self.0, PhantomData)
    }

    #[cfg(test)]
    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        MutExt(NonNull::new(ptr).unwrap(), PhantomData)
    }
}

/// Allows transparently using `MutExt<T>` like a `&T`.
impl<'a, T> Deref for MutExt<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

/// Allows transparently using `MutExt<T>` like a `&mut T`.
impl<'a, T> DerefMut for MutExt<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct MutExtRef<'a, T>(NonNull<T>, PhantomData<&'a mut T>);

/// Allows transparently using `MutExtRef<T>` like a `&T`.
impl<'a, T> Deref for MutExtRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

/// Allows transparently using `MutExtRef<T>` like a `&mut T`.
impl<'a, T> DerefMut for MutExtRef<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

/// A pointer to an externally-managed object.
///
/// This may be treated as a normal shared reference to an object whose lifecycle is managed by C
/// code, but which is only ever dereferenced from rust. It's mainly intended to be used
/// transparently as a shared reference (e.g. by accessing the fields and methods of the type `T`).
///
/// The objects may be allocated and freed by C (e.g. a device's softc) or rust (e.g the
/// driver-specific data returned by `channel_init` in channel_if.m), but in either case C code may
/// pass pointers to this object to rust code while the driver is operational. These pointers are
/// represented by `Ext` and should not be created directly. Instead kobj interface functions are
/// passed `Ext`s as arguments where appropriate.
#[repr(C)]
#[derive(Debug)]
pub struct Ext<'a, T: ?Sized>(NonNull<T>, PhantomData<&'a T>);

/// Allows implicitly making copies of the `Ext<T>` just like `&T` allows.
impl<'a, T> Copy for Ext<'a, T> {}

/// Allows explicitly making copies of the `Ext<T>` just like `&T` allows.
impl<'a, T> Clone for Ext<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<'a, T> Ext<'a, T> {
    pub fn into_raw(x: Self) -> *mut T {
        x.0.as_ptr()
    }

    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        Self(NonNull::new(ptr).unwrap(), PhantomData)
    }

    /// Create an Ext<U> from an Ext<T> by accessing a field on T
    ///
    /// The `ext!` macro should be used instead of calling this directly.
    ///
    /// # Safety
    ///
    /// The function `f` must only access a single field (i.e. `x.field`)
    pub unsafe fn map_field<U, F: FnOnce(&T) -> &U>(x: Self, f: F) -> Ext<'a, U> {
        let new_ptr = f(x.deref()) as *const U;
        Ext(NonNull::new(new_ptr.cast_mut()).unwrap(), PhantomData)
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
        let new_ptr = f(x.deref()) as *const U;
        Ext(NonNull::new(new_ptr.cast_mut()).unwrap(), PhantomData)
    }
}

#[diagnostic::on_unimplemented(
    message = "Indexing into {Self} may not access the same address every time",
    label = "Cannot create an Ext<T> from this. Use a boxed slice or array instead"
)]
pub trait FixedIdx {}
impl<T, const N: usize> FixedIdx for [T; N] {}
impl<T> FixedIdx for Box<[T]> {}

/// Allows transparently using `Ext<T>` like a `&T`.
impl<'a, T: ?Sized> Deref for Ext<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

#[diagnostic::on_unimplemented(message = "
Implement the CallbackArg trait with `impl CallbackArg for {Self} {{}}`.
If the {Self} may be passed to a callout, override its default `get_callout` method with
```
impl CallbackArg for {Self} {{
    fn get_callout(&self) -> Option<*mut Callout> {{
        /* return a reference to the callout starting from a reference to {Self} */
    }}
}}
```
")]
pub trait CallbackArg {
    fn get_callout(&self) -> Option<*mut Callout> {
        None
    }
}

impl<B, F: CallbackArg> CallbackArg for SubClass<B, F> {
    fn get_callout(&self) -> Option<*mut Callout> {
        let sub_fields: &F = self.deref();
        sub_fields.get_callout()
    }
}
