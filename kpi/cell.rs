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

use core::cell::UnsafeCell;
use core::mem::{offset_of, MaybeUninit};
use core::ops::{Deref, DerefMut, Drop};
use core::sync::atomic::{AtomicBool, Ordering};

#[derive(Debug)]
pub struct Checked<T> {
    t: UnsafeCell<T>,
    borrowed: AtomicBool,
}

unsafe impl<T> Sync for Checked<T> {}

impl<T> Checked<T> {
    pub fn new(t: T) -> Self {
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

/// Rust's view of a variable of type T that has its address shared with C.
///
/// This opts out of Rust's requirements that &T must be immutable and T always have a valid value.
/// While creating a &mut FFICell<T> is perfectly fine, it may not be used to get a &mut T.
#[derive(Debug)]
#[repr(transparent)]
pub struct FFICell<T>(UnsafeCell<MaybeUninit<T>>);

unsafe impl<T> Sync for FFICell<T> {}

impl<T> FFICell<T> {
    pub const fn zeroed() -> Self {
        Self(UnsafeCell::new(MaybeUninit::zeroed()))
    }

    pub const fn as_ptr(&self) -> *mut T {
        self.0.get().cast()
    }
}

/// A struct containing a base class B and extra fields F.
///
/// The definition assumes that the base class is shared between Rust and C but places no
/// restriction on the extra fields so it may be possible to create references to them.
#[derive(Debug)]
pub struct SubClass<B, F> {
    base: FFICell<B>,
    pub sub: F,
}

impl<B, F> SubClass<B, F> {
    pub const fn new(sub: F) -> Self {
        Self {
            base: FFICell::zeroed(),
            sub,
        }
    }

    // This could take `&self` but since `SubClass` `Deref`s to its subclass `F` that would
    // introduce behavior that's hard to analyze if `F` had another method named `get_base_ptr`.
    pub fn get_base_ptr(sub: &Self) -> *mut B {
        sub.base.as_ptr()
    }

    pub unsafe fn from_base<'a>(ptr: *mut B) -> &'a mut Self {
        let super_ptr = ptr.byte_sub(offset_of!(Self, base)).cast::<Self>();
        super_ptr.as_mut().unwrap()
    }
}

impl<B, F> Deref for SubClass<B, F> {
    type Target = F;

    fn deref(&self) -> &Self::Target {
        &self.sub
    }
}

impl<B, F> DerefMut for SubClass<B, F> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.sub
    }
}
