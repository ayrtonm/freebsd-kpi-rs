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

use core::fmt::Debug;
use core::ptr::null_mut;

mod cstring;
mod subclass;

pub use cstring::{CString, ToCString};
pub use subclass::{SubClass, SubClassOf};

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
