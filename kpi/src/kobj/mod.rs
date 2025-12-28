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

use crate::bindings::{kobj_class, kobj_class_t, kobj_method_t};
use crate::ffi::CallbackArg;
use core::cell::UnsafeCell;

mod casts;
mod define_class;
mod define_interface;
mod method_table;

pub mod interfaces {
    pub use crate::device::DeviceIf;
    #[cfg(feature = "intrng")]
    pub use crate::intr::PicIf;
}

pub trait KobjLayout: Sized {
    type Layout: CallbackArg;
}

pub trait KobjClass: KobjLayout {
    fn get_class() -> *mut kobj_class;
}

pub trait AsCType<T, X = ()> {
    fn as_c_type(self) -> T;
}

pub trait AsRustType<T, X = ()> {
    fn as_rust_type(self) -> T;
}

// UnsafeCell needed to ensure static ends up in .data
#[doc(hidden)]
#[repr(C)]
pub struct BaseClasses<const N: usize>(pub UnsafeCell<[kobj_class_t; N]>);
unsafe impl<const N: usize> Sync for BaseClasses<N> {}

// UnsafeCell needed to ensure static ends up in .data
#[doc(hidden)]
#[repr(C)]
pub struct MethodTable<const N: usize>(pub UnsafeCell<[kobj_method_t; N]>);
unsafe impl<const N: usize> Sync for MethodTable<N> {}
