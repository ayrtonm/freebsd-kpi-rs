/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2026 Ayrton Muñoz
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

use crate::prelude::*;
use core::ffi::c_void;

pub trait Module {
    fn on_load(data: *mut c_void) -> Result<()> {
        Ok(())
    }

    fn on_unload(data: *mut c_void) -> Result<()> {
        Ok(())
    }
}

#[macro_export]
macro_rules! define_module {
    ($module_ty:ident, $modevent_name:ident) => {
        #[unsafe(no_mangle)]
        extern "C" fn $modevent_name(
            m: $crate::bindings::module_t,
            ty: core::ffi::c_int,
            data: *mut core::ffi::c_void
        ) -> core::ffi::c_int {
            use $crate::kobj::AsCType;
            use $crate::module::Module;
            let res = match ty as core::ffi::c_uint {
                $crate::bindings::MOD_LOAD => <$module_ty as Module>::on_load(data),
                $crate::bindings::MOD_UNLOAD => <$module_ty as Module>::on_unload(data),
                _ => Err(ENOTSUP),
            };
            match res {
                Ok(()) => 0,
                Err(e) => e.as_c_type(),
            }
        }
    };
}
