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

//! Utilities related to FFI with C.

use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use crate::vec::Vec;
use core::cell::UnsafeCell;
use core::ffi::CStr;
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::ops::{Deref, DerefMut};
use core::ptr::null_mut;

/// A pointer type implementing `Sync`.
///
/// This is useful for creating pointer types that are expected to be shared between threads without
/// explicit synchronization.
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

pub enum CString<const N: usize = 16> {
    Small([u8; N]),
    Heap(Vec<u8>),
}

impl<const N: usize> CString<N> {
    pub fn new(msg: &str, ty: MallocType, flags: MallocFlags) -> Result<Self> {
        let mut buf = Vec::try_with_capacity(msg.as_bytes().len() + 1, ty, flags).unwrap();
        for &b in msg.as_bytes() {
            buf.try_push(b);
        }
        buf.try_push(0);
        Ok(Self::Heap(buf))
    }

    pub fn as_c_str(&self) -> &CStr {
        match self {
            Self::Small(_) => todo!(""),
            Self::Heap(s) => unsafe { CStr::from_ptr(s.as_ptr().cast()) },
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Small(s) => unsafe { CStr::from_ptr(s.as_ptr().cast()).count_bytes() },
            Self::Heap(s) => s.len() - 1,
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        match self {
            Self::Small(s) => &s.as_slice()[0..self.len()],
            Self::Heap(s) => s.as_slice(),
        }
    }
}

impl<const N: usize> From<[u8; N]> for CString<N> {
    fn from(array: [u8; N]) -> Self {
        Self::Small(array)
    }
}

/// A struct containing a base class `B` followed by extra fields `F`.
///
/// This type assumes the base class is shared with C code and is primarily intended to be used for
/// interfacing with existing KPIs which require a subclass of a C type. If that is not necessary
/// this type should be avoided.
///
/// `SubClass<B, F>` has a `Deref<Target = F>` impl to allow using it like an `F`. For example if
/// `F` is a struct with fields named `foo` and `bar`, a subclass allows transparently accessing
/// them using `my_subclass.foo` and `my_subclass.bar`.
///
/// Accessing the base class fields requires using the [`base!`] macro which uses C-like syntax for
/// reading, writing and getting the address of fields.
///
/// ```
/// let baz_addr: *mut BazTy = base!(&my_subclass->baz);
/// let old_baz_value = unsafe { base!(my_subclass->baz) };
/// unsafe { base!(my_subclass->baz) = new_baz_value; };
///
/// ```
///
/// For example if `B` is a struct with a field named `baz`, a subclass allows getting its address using `base!(&my_subclass->baz)` and accessing it using `unsafe { base!(my_subclass->baz) = baz_value };` or
/// `let baz_value = unsafe { base!(my_subclass->baz) };`. Since the base class is assumed to be
/// shared with C code it requires the user to manually ensure that the field is not being accessed
/// concurrently by other threads.
#[repr(C)]
#[derive(Default)]
pub struct SubClass<B, F> {
    // This needs to be first for some uses of subclass (e.g. simplebus subclass drivers)
    base_class: UnsafeCell<B>,
    // Extra fields last allows making them unsized
    sub_fields: F,
}

impl<B, F: Debug> Debug for SubClass<B, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("SubClass")
            .field("base_class", &..)
            .field("sub_fields", &self.sub_fields)
            .finish()
    }
}

// SAFETY: &SubClass provides no direct access to the &UnsafeCell<B>
unsafe impl<B, F: Sync + Send> Sync for SubClass<B, F> {}
unsafe impl<B, F: Sync + Send> Send for SubClass<B, F> {}

impl<B: Default, F> SubClass<B, F> {
    pub fn new(sub_fields: F) -> Self {
        Self::new_with_base(B::default(), sub_fields)
    }
}
impl<B, F> SubClass<B, F> {
    pub const fn new_with_base(base: B, sub_fields: F) -> Self {
        Self {
            base_class: UnsafeCell::new(base),
            sub_fields,
        }
    }

    pub fn base_ptr_from_raw(sub: *mut Self) -> *mut B {
        let res: *mut UnsafeCell<B> = unsafe { &raw mut (*sub).base_class };
        res.cast::<B>()
    }

    pub fn as_base_ptr(sub: &Self) -> *mut B {
        sub.base_class.get()
    }

    pub unsafe fn from_base_ptr<'a>(ptr: *mut B) -> &'a Self {
        unsafe { ptr.cast::<Self>().as_ref().unwrap() }
    }

    pub unsafe fn from_base_ptr_mut<'a>(ptr: *mut B) -> &'a mut Self {
        unsafe { ptr.cast::<Self>().as_mut().unwrap() }
    }
}

impl<B, F> Deref for SubClass<B, F> {
    type Target = F;

    fn deref(&self) -> &Self::Target {
        &self.sub_fields
    }
}

impl<B, F> DerefMut for SubClass<B, F> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.sub_fields
    }
}
