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

use crate::kpi_prelude::*;
use core::mem::{offset_of, MaybeUninit};
use core::ops::{Deref, DerefMut, Drop};
use core::sync::atomic::{AtomicU32, Ordering};
use core::cell::UnsafeCell;
use core::marker::PhantomData;

pub unsafe trait UniqueOwner {}

#[derive(Debug)]
pub struct UniqueCell<T, O: UniqueOwner, S> {
    t: UnsafeCell<T>,
    owner: PhantomData<O>,
    state: PhantomData<S>,
}

unsafe impl<T, O: UniqueOwner, S> Sync for UniqueCell<T, O, S> {}

impl<T, O: UniqueOwner, S> UniqueCell<T, O, S> {
    pub fn new(t: T) -> Self {
        Self {
            t: UnsafeCell::new(t),
            owner: PhantomData,
            state: PhantomData,
        }
    }

    pub const fn as_ptr(&self) -> *mut T {
        self.t.get()
    }
}

impl<T, O: UniqueOwner> UniqueCell<T, O, O> {
    pub fn get(&self) -> &T {
        unsafe {
            self.t.get().as_ref().unwrap()
        }
    }
    // TODO: This by itself is unsound so impl UniqueOwner must require that only a single instance
    pub fn get_mut(&self) -> &mut T {
        unsafe {
            self.t.get().as_mut().unwrap()
        }
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
