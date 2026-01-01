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

use crate::bindings;
use crate::bindings::malloc_type;
use core::ffi::{c_int, c_void};
use core::ops::BitOr;
use core::ptr::null_mut;

#[repr(C)]
#[derive(Copy, Clone, Debug, Default)]
pub struct MallocFlags(pub(crate) c_int);

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct MallocType(*mut malloc_type);

impl BitOr<MallocFlags> for MallocFlags {
    type Output = MallocFlags;

    fn bitor(self, rhs: MallocFlags) -> MallocFlags {
        Self(self.0 | rhs.0)
    }
}

impl MallocFlags {
    pub fn contains(&self, flag: MallocFlags) -> bool {
        self.0 & flag.0 != 0
    }
}

pub trait Malloc {
    fn malloc_type() -> MallocType;
}

macro_rules! gen_malloc_type {
    ($($name:ident)*) => {
    $(
    #[derive(Debug)]
    pub struct $name(());

    impl Malloc for $name {
        fn malloc_type() -> MallocType {
            MallocType(unsafe { &raw mut bindings::$name[0] })
        }
    }
    )*

    };
}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
#[allow(non_camel_case_types)]
pub mod wrappers {
    use super::*;

    gen_newtype! {
        MallocFlags as i32,
        M_ZERO,
        M_NODUMP,
        M_NOWAIT,
        M_WAITOK,
        M_USE_RESERVE,
    }

    gen_malloc_type! {
        M_DEVBUF
        M_CACHE
        M_PARGS
        M_SESSION
        M_SUBPROC
        M_TEMP
        M_IOV
    }

    pub fn malloc_aligned(
        size: usize,
        align: usize,
        ty: MallocType,
        flags: MallocFlags,
    ) -> *mut c_void {
        if cfg!(test) {
            // Used to simulate OOM in userspace unit tests
            if flags.contains(M_USE_RESERVE) {
                return null_mut();
            }
        };
        unsafe { bindings::malloc_aligned(size, align, ty.0, flags.0) }
    }

    pub fn malloc(size: usize, ty: MallocType, flags: MallocFlags) -> *mut c_void {
        if cfg!(test) {
            // Used to simulate OOM in userspace unit tests
            if flags.contains(M_USE_RESERVE) {
                return null_mut();
            }
        };
        unsafe { bindings::malloc(size, ty.0, flags.0) }
    }

    pub unsafe fn free(ptr: *mut c_void, ty: MallocType) {
        unsafe { bindings::free(ptr, ty.0) }
    }

    pub unsafe fn realloc(
        ptr: *mut c_void,
        size: usize,
        ty: MallocType,
        flags: MallocFlags,
    ) -> *mut c_void {
        if cfg!(test) {
            // Used to simulate OOM in userspace unit tests
            if flags.contains(M_USE_RESERVE) {
                return null_mut();
            }
        };
        unsafe { bindings::realloc(ptr, size, ty.0, flags.0) }
    }
}
