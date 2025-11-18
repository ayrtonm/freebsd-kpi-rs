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
use core::any::TypeId;
use core::ffi::c_void;

mod nvme;

pub use crate::device::DeviceIf;
#[cfg(feature = "intrng")]
pub use crate::intr::PicIf;

pub use nvme::NvmeIf;

pub trait AsCType<T> {
    fn as_c_type(self) -> T;
}

pub trait AsRustType<T> {
    fn as_rust_type(self) -> T;
}

impl<T> AsRustType<T> for T {
    fn as_rust_type(self) -> T {
        self
    }
}

impl<'a, IN: 'static, OUT: 'static> AsRustType<&'a OUT> for *mut IN {
    fn as_rust_type(self) -> &'a OUT {
        // TODO: Make this check const
        let same_type = TypeId::of::<IN>() == TypeId::of::<OUT>();
        //let is_subclass = TypeId::of::<OUT>() == TypeId::of::<SubClass<IN, F>>();
        let from_void_ptr = TypeId::of::<IN>() == TypeId::of::<c_void>();
        if same_type || from_void_ptr {
            unsafe { self.cast::<OUT>().as_ref().unwrap() }
        //} else if is_subclass {
        //    unsafe { SubClass::from_base_ptr(self) }
        } else {
            panic!("uh oh")
        }
    }
}

impl<'a, IN: 'static, OUT: 'static> AsRustType<&'a mut OUT> for *mut IN {
    fn as_rust_type(self) -> &'a mut OUT {
        // TODO: Make this check const
        let same_type = TypeId::of::<IN>() == TypeId::of::<OUT>();
        //let is_subclass = TypeId::of::<OUT>() == TypeId::of::<SubClass<IN, F>>();
        let from_void_ptr = TypeId::of::<IN>() == TypeId::of::<c_void>();
        if same_type || from_void_ptr {
            unsafe { self.cast::<OUT>().as_mut().unwrap() }
        //} else if is_subclass {
        //    unsafe { SubClass::from_base_ptr_mut(self) }
        } else {
            panic!("uh oh")
        }
    }
}
