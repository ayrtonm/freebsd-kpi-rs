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

use crate::bindings::{kobj, pcm_channel, pcmchan_caps, pcmchan_matrix, snd_dbuf};
use crate::prelude::*;
use core::ffi::c_void;

#[doc(hidden)]
#[macro_export]
macro_rules! channel_init {
    ($driver_ty:ident $impl_fn_name:ident) => {
        pub unsafe extern "C" fn $impl_fn_name(
            _obj: *mut kobj,
            devinfo: *mut void,
            b: *mut snd_dbuf,
            c: *mut pcm_channel,
            dir: int,
        ) -> *mut void {
            const _TYPES_MATCH: $crate::bindings::channel_init_t = Some($impl_fn_name);
            let c_devinfo = devinfo;
            let devinfo = devinfo.as_rust_type();
            let b = b.as_rust_type();
            let c = c.as_rust_type();
            let dir = dir.as_rust_type();

            $driver_ty::channel_init(devinfo, b, c, dir);

            return c_devinfo;
        }
    };
}

impl<'a, T> AsRustType<&'a T> for *mut c_void {
    fn as_rust_type(self) -> &'a T {
        unsafe { self.cast::<T>().as_ref().unwrap() }
    }
}

pub trait ChannelIf {
    type DevInfo;
    fn channel_init(devinfo: &Self::DevInfo, b: *mut snd_dbuf, c: *mut pcm_channel, dir: i32);
    fn channel_setspeed(devinfo: &Self::DevInfo, speed: u32) -> Result<()> {
        todo!("")
    }
    fn channel_setformat(devinfo: &Self::DevInfo, format: u32) -> Result<()> {
        todo!("")
    }
    fn channel_getcaps(devinfo: &Self::DevInfo) -> &pcmchan_caps {
        todo!("")
    }
    fn channel_getmatrix(devinfo: &Self::DevInfo, format: u32) -> &pcmchan_matrix {
        todo!("")
    }
}
