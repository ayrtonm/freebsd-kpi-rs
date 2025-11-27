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

use crate::ffi::SubClass;
use core::ffi::c_void;

mod nvme;

pub use crate::device::DeviceIf;
#[cfg(feature = "intrng")]
pub use crate::intr::PicIf;

pub use nvme::NvmeIf;

pub trait AsCType<T> {
    fn as_c_type(self) -> T;
}

pub trait AsRustType<T, X = ()> {
    fn as_rust_type(self) -> T;
}

// Allow passing through C types into rust unchanged
impl<T> AsRustType<T> for T {
    fn as_rust_type(self) -> T {
        self
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
