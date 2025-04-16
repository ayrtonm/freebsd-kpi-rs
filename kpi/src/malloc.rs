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
use crate::bindings::{malloc_type};
use core::ops::BitOr;
use core::ffi::{c_int, c_void};

#[derive(Copy, Clone, Debug)]
pub struct MallocFlags(c_int);

//#[derive(Copy, Clone, Debug)]
//pub struct MallocType(*mut malloc_type);

//unsafe impl Sync for MallocType {}

#[allow(non_camel_case_types)]
pub mod wrappers {
    use super::*;

    pub static M_ZERO: MallocFlags = MallocFlags(bindings::M_ZERO);
    pub static M_NODUMP: MallocFlags = MallocFlags(bindings::M_NODUMP);
    pub static M_NOWAIT: MallocFlags = MallocFlags(bindings::M_NOWAIT);
    pub static M_WAITOK: MallocFlags = MallocFlags(bindings::M_WAITOK);
    pub static M_USE_RESERVE: MallocFlags = MallocFlags(bindings::M_USE_RESERVE);

    //pub static M_DEVBUF: MallocType = MallocType(&raw mut bindings::M_DEVBUF as *mut malloc_type);
    #[derive(Debug)]
    pub struct M_DEVBUF(());
    impl MallocType for M_DEVBUF {
        const TYPE: *mut malloc_type = &raw mut bindings::M_DEVBUF as *mut malloc_type;
    }

    pub fn malloc(size: usize, ty: *mut malloc_type, flags: MallocFlags) -> *mut c_void {
        unsafe {
            bindings::malloc(size, ty, flags.0)
        }
    }
}

pub trait MallocType {
    const TYPE: *mut malloc_type;
}

impl BitOr<MallocFlags> for MallocFlags {
    type Output = MallocFlags;

    fn bitor(self, rhs: MallocFlags) -> MallocFlags {
        Self(self.0 | rhs.0)
    }
}
