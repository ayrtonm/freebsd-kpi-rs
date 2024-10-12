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

use crate::bindings::_device;
use crate::{bindings, AsCType, AsRustType, ErrCode, PointsTo, Ptr, Result};
use core::ffi::{c_int, CStr};
use core::ptr::NonNull;

enum_c_macros! {
    #[repr(i32)]
    #[derive(Copy, Clone, Debug)]
    #[allow(nonstandard_style)]
    pub enum ProbeRes {
        BUS_PROBE_SPECIFIC,
        BUS_PROBE_VENDOR,
        BUS_PROBE_DEFAULT,
        BUS_PROBE_LOW_PRIORITY,
        BUS_PROBE_GENERIC,
        BUS_PROBE_HOOVER,
        BUS_PROBE_NOWILDCARD,
    }
}

impl AsCType for ProbeRes {
    fn as_c_int(self) -> c_int {
        self as c_int
    }
}

impl AsRustType<Device> for *mut _device {
    fn as_rust_type(self) -> Device {
        Device::new(self)
    }
}

pub trait DeviceIf {
    fn device_probe(&self, dev: Device) -> Result<ProbeRes>;
    fn device_attach(&self, dev: Device) -> Result<()>;
    fn device_detach(&self, dev: Device) -> Result<()>;
}

pub type Device = Ptr<_device>;

impl Device {
    pub fn get_parent(&self) -> Result<Device> {
        let dev_ptr = self.0;
        let res = unsafe { bindings::device_get_parent(dev_ptr) };
        if res.is_null() {
            Err(ErrCode::ENULLPTR)
        } else {
            Ok(Device::new(res))
        }
    }

    pub fn set_desc(&self, desc: &'static CStr) {
        let dev_ptr = self.0;
        let desc_ptr = desc.as_ptr();
        unsafe { bindings::device_set_desc(dev_ptr, desc_ptr) }
    }

    pub fn get_nameunit(&self) -> &CStr {
        let dev_ptr = self.0;
        let name = unsafe { bindings::device_get_nameunit(dev_ptr) };
        unsafe { CStr::from_ptr(name) }
    }

    pub fn get_softc<SC>(&mut self) -> *mut SC {
        let dev_ptr = self.0;
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        sc_void_ptr.cast::<SC>()
    }
}
