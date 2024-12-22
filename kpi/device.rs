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

use crate::bindings::{_device, kobj_method_t, kobjop_desc};
use crate::prelude::*;
use core::ffi::{c_int, CStr};
use core::ptr::null_mut;

enum_c_macros! {
    #[repr(i32)]
    #[derive(Copy, Clone, Debug)]
    #[allow(nonstandard_style)]
    pub enum BusProbe {
        BUS_PROBE_SPECIFIC,
        BUS_PROBE_VENDOR,
        BUS_PROBE_DEFAULT,
        BUS_PROBE_LOW_PRIORITY,
        BUS_PROBE_GENERIC,
        BUS_PROBE_HOOVER,
        BUS_PROBE_NOWILDCARD,
    }
}

#[derive(Debug)]
pub struct SoftcInit(());

impl AsCType<c_int> for BusProbe {
    fn as_c_type(self) -> c_int {
        self as c_int
    }
}

impl AsCType<c_int> for SoftcInit {
    fn as_c_type(self) -> c_int {
        0
    }
}

impl AsRustType<Device> for *mut _device {
    fn as_rust_type(self) -> Device {
        Device::new(self)
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct DeviceMethod(kobj_method_t);

impl DeviceMethod {
    pub const fn new(desc: *mut kobjop_desc, func: *const ()) -> Self {
        Self(kobj_method_t {
            desc,
            func: Some(unsafe { core::mem::transmute(func) }),
        })
    }
    pub const fn null() -> Self {
        Self(kobj_method_t {
            desc: null_mut(),
            func: None,
        })
    }
}

unsafe impl Sync for DeviceMethod {}

pub trait HasSoftc: DeviceIf {
    fn init_softc(&self, dev: Device, sc: Self::Softc) -> SoftcInit {
        assert!(self as *const Self as *const bindings::driver_t == dev.driver);
        let dev_ptr = dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Self::Softc>();
        unsafe { *sc_ptr = sc };
        SoftcInit(())
    }

    fn get_softc(&self, dev: Device) -> &Self::Softc {
        assert!(self as *const Self as *const bindings::driver_t == dev.driver);
        let dev_ptr = dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Self::Softc>();
        unsafe { sc_ptr.as_ref().unwrap() }
    }

    fn get_softc_as_mut(&self, dev: Device) -> *mut Self::Softc {
        assert!(self as *const Self as *const bindings::driver_t == dev.driver);
        let dev_ptr = dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Self::Softc>();
        sc_ptr
    }
}

impl<T: DeviceIf> HasSoftc for T {}

pub trait DeviceIf {
    type Softc: Sync;

    fn device_probe(&self, dev: Device) -> Result<BusProbe>;
    fn device_attach(&self, dev: Device) -> Result<SoftcInit>;
    fn device_detach(&self, dev: Device) -> Result<()>;
}

pub mod wrappers {
    use super::*;

    pub fn device_get_parent(dev: Device) -> Result<Device> {
        let dev_ptr = dev.as_ptr();
        let res = unsafe { bindings::device_get_parent(dev_ptr) };
        if res.is_null() {
            Err(ENULLPTR)
        } else {
            Ok(Device::new(res))
        }
    }

    pub fn device_set_desc(dev: Device, desc: &'static CStr) {
        let dev_ptr = dev.as_ptr();
        let desc_ptr = desc.as_ptr();
        unsafe { bindings::device_set_desc(dev_ptr, desc_ptr) }
    }

    pub fn device_get_nameunit(dev: &Device) -> &CStr {
        let dev_ptr = dev.as_ptr();
        let name = unsafe { bindings::device_get_nameunit(dev_ptr) };
        unsafe { CStr::from_ptr(name) }
    }

    pub fn device_get_driver(dev: Device) -> *mut bindings::driver_t {
        dev.driver
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Device {
    dev: *mut _device,
    driver: *mut bindings::driver_t,
}

unsafe impl Sync for Device {}

impl Device {
    pub fn new(dev: *mut _device) -> Self {
        // This is called here rather than as-needed based on the assumption
        // that cross-langugage inlining is not available.
        let driver = unsafe { bindings::device_get_driver(dev) };
        Self { dev, driver }
    }

    pub fn as_ptr(&self) -> *mut _device {
        self.dev
    }

    pub fn get_driver(&self) -> *mut bindings::driver_t {
        self.driver
    }
}
