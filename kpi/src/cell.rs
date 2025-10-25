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

//! Shareable mutable containers.

use crate::ErrCode;
use crate::bindings;
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::{CStr, c_void};
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::ops::{Deref, DerefMut, Drop};
use core::ptr::{null_mut, read};
use core::sync::atomic::{AtomicBool, AtomicU8, AtomicU16, AtomicU32, AtomicU64, Ordering};

/// A value borrow-checked at runtime
pub struct Mutable<T> {
    t: UnsafeCell<T>,
    borrowed: AtomicBool,
}

impl<T: Debug> Debug for Mutable<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Just read the pointer and don't overcomplicate things. There may be mutable references to
        // the pointee so this is a best effort thing and may not be fully reliable, but it avoids
        // panics (as self.get_mut() might) while being informative.
        let t = unsafe { read(self.t.get()) };
        f.debug_struct("Mutable")
            .field("t", &t)
            .field("borrowed", &self.borrowed)
            .finish()
    }
}

unsafe impl<T> Sync for Mutable<T> {}

impl<T> Mutable<T> {
    pub const fn new(t: T) -> Self {
        Self {
            t: UnsafeCell::new(t),
            borrowed: AtomicBool::new(false),
        }
    }

    pub fn get_mut(&self) -> RefMut<'_, T> {
        if !self
            .borrowed
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            panic!("already borrowed");
        }
        RefMut {
            value: self.t.get(),
            borrowed: &self.borrowed,
        }
    }
}

pub struct RefMut<'b, T: 'b + ?Sized> {
    value: *mut T,
    borrowed: &'b AtomicBool,
}

impl<'b, T: Debug> Debug for RefMut<'b, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

impl<T: ?Sized> Deref for RefMut<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.value.as_ref().unwrap() }
    }
}

impl<T: ?Sized> DerefMut for RefMut<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.value.as_mut().unwrap() }
    }
}

impl<T: ?Sized> Drop for RefMut<'_, T> {
    fn drop(&mut self) {
        self.borrowed.store(false, Ordering::Relaxed);
    }
}

pub trait Sleepable {
    fn as_ptr(&self) -> *mut c_void;
}
impl<T> Sleepable for Mutable<T> {
    fn as_ptr(&self) -> *mut c_void {
        self.t.get().cast::<c_void>()
    }
}
impl Sleepable for AtomicU8 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}
impl Sleepable for AtomicU16 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}
impl Sleepable for AtomicU32 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}
impl Sleepable for AtomicU64 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;

    pub fn tsleep<T: Sleepable>(chan: &T, priority: i32, wmesg: &CStr, timo: i32) -> Result<()> {
        let chan_ptr = chan.as_ptr();
        let wmesg_ptr = wmesg.as_ptr();
        let res = unsafe {
            bindings::_sleep(
                chan_ptr,
                null_mut(),
                priority,
                wmesg_ptr,
                bindings::tick_sbt * timo as i64,
                0,
                bindings::C_HARDCLOCK,
            )
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn wakeup<T: Sleepable>(chan: &T) {
        let chan_ptr = chan.as_ptr();
        unsafe { bindings::wakeup(chan_ptr) }
    }
}
