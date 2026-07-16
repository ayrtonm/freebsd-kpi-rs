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
use crate::bindings::{device_t, u_int};
use core::fmt::{Debug, Formatter};
use core::pin::Pin;
use core::mem::{forget, MaybeUninit};
use crate::sync::arc::{Arc};
use crate::boxed::Box;
use crate::device::Device;
use crate::malloc::Malloc;
use core::ptr;
use core::ptr::null_mut;
use core::ops::Deref;
use core::cell::UnsafeCell;
use crate::prelude::*;

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

#[repr(C)]
pub struct LoanLayout<T> {
    t: MaybeUninit<T>,
    dev: device_t,
    count: UnsafeCell<u_int>,
}

/// A unique pointer to an uninitialized, externally-managed object.
pub struct Uninit<'a, T>(&'a mut LoanLayout<T>, Option<&'a mut bool>);

impl<'a, T> Uninit<'a, T> {
    pub unsafe fn from_raw(ptr: &'a mut LoanLayout<T>, dev: device_t) -> Self {
        ptr.dev = dev;
        Self(ptr, None)
    }

    pub fn set_init_flag(&mut self, flag: &'a mut bool) {
        self.1 = Some(flag);
    }

    pub fn device(&self) -> Device {
        Device::new(self.0.dev)
    }

    /// Initialize the externally-managed object to `t` and return a pinned reference to the pointee
    pub fn init(self, t: T) -> Loan<'a, T> {
        self.0.t.write(t);
        self.1.map(|init| *init = true);
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        unsafe { bindings::refcount_init(count_ptr, 1) };
        Loan(unsafe { inner_ptr.as_ref().unwrap() })
    }
}

/// A pointer that may be opted into refcounting if requested.
#[repr(C)]
pub struct Loan<'a, T: 'static>(pub(crate) &'a LoanLayout<T>);

impl<'a, T> Loan<'a, T> {
    pub unsafe fn from_raw(ptr: &'a LoanLayout<T>) -> Self {
        Self(ptr)
    }

    pub fn into_raw(self) -> (*mut T, *mut u_int) {
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        let t_ptr = self.0.t.as_ptr().cast_mut();
        (t_ptr, count_ptr)
    }

    pub fn device(&self) -> Device {
        Device::new(self.0.dev)
    }

    pub fn lease(&self) -> Lease<T> {
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        unsafe { bindings::refcount_acquire(count_ptr) };
        Lease(unsafe { inner_ptr.as_ref().unwrap() })
    }
}

impl<'a, T: 'static + Debug> Debug for Loan<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0.t, f)
    }
}

impl<'a, T> Copy for Loan<'a, T> {}

impl<'a, T> Clone for Loan<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'a, T> Deref for Loan<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            self.0.t.assume_init_ref()
        }
    }
}

/// A pointer to a refcounted object.
#[repr(C)]
pub struct Lease<T: 'static>(pub(crate) &'static LoanLayout<T>);

impl<T> Lease<T> {
    pub fn device(&self) -> Device {
        Device::new(self.0.dev)
    }

    pub fn lease(&self) -> Self {
        Loan(self.0).lease()
    }

    pub fn into_raw(self) -> (*mut T, *mut u_int) {
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        let t_ptr = self.0.t.as_ptr().cast_mut();
        forget(self);
        (t_ptr, count_ptr)
    }
}

impl<T> Drop for Lease<T> {
    fn drop(&mut self) {
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        let last = unsafe { bindings::refcount_release(count_ptr) };
        assert!(!last);
    }
}

impl<T> Deref for Lease<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            self.0.t.assume_init_ref()
        }
    }
}

pub unsafe trait FixedIndex {}
unsafe impl<T, const N: usize> FixedIndex for [T; N] {}
unsafe impl<T> FixedIndex for [T] {}

unsafe impl<T, M: Malloc> FixedIndex for Box<[T], M> {}
unsafe impl<T, M: Malloc, const N: usize> FixedIndex for Box<[T; N], M> {}

unsafe impl<T, M: Malloc, const N: usize> FixedIndex for Arc<[T; N], M> {}

pub fn assert_pin_has_fixed_index<T: FixedIndex>(_p: Pin<&T>) {}

pub unsafe trait IsPinning {}
unsafe impl<T> IsPinning for Pin<T> {}
unsafe impl<'a, T> IsPinning for Loan<'a, T> {}
unsafe impl<T> IsPinning for Lease<T> {}
pub fn assert_is_pinning<P: IsPinning>(_p: &P) {}
