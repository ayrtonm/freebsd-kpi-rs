/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2025 Ayrton Muñoz
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

use crate::boxed::{Box, InnerBox};
use crate::ffi::{DevRef, SubClass};
use crate::kobj::{AsCType, AsRustType};
use crate::sync::arc::{Arc, ArcRef, InnerArc, UniqueArcRef};
use core::cell::UnsafeCell;
use core::ffi::c_void;
use core::ptr::NonNull;

// Allow passing through C types into rust unchanged
impl<T> AsRustType<T> for T {
    fn as_rust_type(self) -> T {
        self
    }
}

impl<T> AsCType<T> for T {
    fn as_c_type(self) -> T {
        self
    }
}

// Allow turning pointers to shared references behind an UnsafeCell
impl<'a, T> AsRustType<&'a UnsafeCell<T>> for *mut T {
    fn as_rust_type(self) -> &'a UnsafeCell<T> {
        unsafe { self.cast::<UnsafeCell<T>>().as_ref().unwrap() }
    }
}

// Allow turning pointers to shared references
impl<'a, T> AsRustType<&'a T> for *mut T {
    fn as_rust_type(self) -> &'a T {
        unsafe { self.as_ref().unwrap() }
    }
}

// Allow casting void pointers then turning them to shared references
impl<'a, T> AsRustType<&'a T, c_void> for *mut c_void {
    fn as_rust_type(self) -> &'a T {
        unsafe { self.cast::<T>().as_ref().unwrap() }
    }
}

// Allow turning pointers to a base type to shared references to a SubClass<B, _>
impl<'a, B, F> AsRustType<&'a SubClass<B, F>> for *mut B {
    fn as_rust_type(self) -> &'a SubClass<B, F> {
        unsafe { SubClass::from_base_ptr(self) }
    }
}

// Allow the previous three operations for unique references
impl<'a, T> AsRustType<&'a mut T> for *mut T {
    fn as_rust_type(self) -> &'a mut T {
        unsafe { self.as_mut().unwrap() }
    }
}

impl<'a, T> AsRustType<&'a mut T, c_void> for *mut c_void {
    fn as_rust_type(self) -> &'a mut T {
        unsafe { self.cast::<T>().as_mut().unwrap() }
    }
}

impl<'a, B, F> AsRustType<&'a mut SubClass<B, F>> for *mut B {
    fn as_rust_type(self) -> &'a mut SubClass<B, F> {
        unsafe { SubClass::from_base_ptr_mut(self) }
    }
}

impl<T> AsRustType<Box<T>> for *mut T {
    fn as_rust_type(self) -> Box<T> {
        unsafe { Box::from_raw(NonNull::new(self.cast::<InnerBox<T>>()).unwrap()) }
    }
}

impl<T> AsRustType<Arc<T>> for *mut T {
    fn as_rust_type(self) -> Arc<T> {
        unsafe { Arc::from_raw(self.cast::<InnerArc<T>>()) }
    }
}

impl<'a, T> AsRustType<ArcRef<'a, T>> for *mut T {
    fn as_rust_type(self) -> ArcRef<'a, T> {
        unsafe { ArcRef::from_raw(self.cast::<InnerArc<T>>()) }
    }
}

impl<T> AsRustType<UniqueArcRef<T>> for *mut T {
    fn as_rust_type(self) -> UniqueArcRef<T> {
        unsafe { UniqueArcRef::from_raw(self.cast::<InnerArc<T>>()) }
    }
}

impl<T> AsRustType<Box<T>, c_void> for *mut c_void {
    fn as_rust_type(self) -> Box<T> {
        unsafe { Box::from_raw(NonNull::new(self.cast::<InnerBox<T>>()).unwrap()) }
    }
}

impl<T> AsRustType<Arc<T>, c_void> for *mut c_void {
    fn as_rust_type(self) -> Arc<T> {
        unsafe { Arc::from_raw(self.cast::<InnerArc<T>>()) }
    }
}

impl<'a, T> AsRustType<ArcRef<'a, T>, c_void> for *mut c_void {
    fn as_rust_type(self) -> ArcRef<'a, T> {
        unsafe { ArcRef::from_raw(self.cast::<InnerArc<T>>()) }
    }
}

impl<T> AsRustType<UniqueArcRef<T>, c_void> for *mut c_void {
    fn as_rust_type(self) -> UniqueArcRef<T> {
        unsafe { UniqueArcRef::from_raw(self.cast::<InnerArc<T>>()) }
    }
}

impl<'a, T> AsRustType<DevRef<'a, T>, c_void> for *mut c_void {
    fn as_rust_type(self) -> DevRef<'a, T> {
        unsafe { DevRef::from_raw(self.cast::<T>()) }
    }
}

impl<T> AsCType<*mut T> for Box<T> {
    fn as_c_type(self) -> *mut T {
        Box::into_raw(self).cast::<T>()
    }
}

impl<T> AsCType<*mut T> for Arc<T> {
    fn as_c_type(self) -> *mut T {
        Arc::into_raw(self).cast::<T>()
    }
}

impl<'a, T> AsCType<*mut T> for ArcRef<'a, T> {
    fn as_c_type(self) -> *mut T {
        ArcRef::into_raw(self).cast::<T>()
    }
}

impl<T> AsCType<*mut T> for UniqueArcRef<T> {
    fn as_c_type(self) -> *mut T {
        UniqueArcRef::into_raw(self).cast::<T>()
    }
}

impl<T> AsCType<*mut c_void, c_void> for Box<T> {
    fn as_c_type(self) -> *mut c_void {
        Box::into_raw(self).cast::<c_void>()
    }
}

impl<T> AsCType<*mut c_void, c_void> for Arc<T> {
    fn as_c_type(self) -> *mut c_void {
        Arc::into_raw(self).cast::<c_void>()
    }
}

impl<'a, T> AsCType<*mut c_void, c_void> for ArcRef<'a, T> {
    fn as_c_type(self) -> *mut c_void {
        ArcRef::into_raw(self).cast::<c_void>()
    }
}

impl<T> AsCType<*mut c_void, c_void> for UniqueArcRef<T> {
    fn as_c_type(self) -> *mut c_void {
        UniqueArcRef::into_raw(self).cast::<c_void>()
    }
}
