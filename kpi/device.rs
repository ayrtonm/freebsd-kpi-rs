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
use crate::kpi_prelude::*;
use core::any::TypeId;
use core::ffi::{c_int, CStr};
use core::marker::PhantomData;
use core::ops::{Deref, DerefMut};
use core::ptr::drop_in_place;

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

#[derive(Debug)]
pub struct AttachRes(());

impl AsCType<c_int> for ProbeRes {
    fn as_c_type(self) -> c_int {
        self as c_int
    }
}

impl AsRustType<Device> for *mut _device {
    fn as_rust_type(self) -> Device {
        Device::new(self)
    }
}

macro_rules! get_local_softc {
    ($self:expr, $dev:expr) => {
        assert!($self as *const Self as *const bindings::driver_t == $dev.driver);
        let dev_ptr = $dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.byte_add($dev.local_softc).cast();
        unsafe { sc_ptr.as_mut().unwrap() }
    };
}

pub trait DriverIf: Sized {
    type GlobalSoftc;
}

pub trait DeviceIf: DriverIf {
    type Softc: Sync = ();
    // device_probe and device_attach are not used
    type device_probe = ();
    type device_attach = ();
    type device_detach = ();

    fn init_softc(
        &self,
        dev: &mut Device,
        sc: Self::Softc,
    ) -> AttachRes {
        assert!(self as *const Self as *const bindings::driver_t == dev.driver);
        let dev_ptr = dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Self::Softc>();
        unsafe { *sc_ptr = sc };
        AttachRes(())
    }

    fn get_softc(&self, dev: &Device) -> &Self::Softc {
        assert!(self as *const Self as *const bindings::driver_t == dev.driver);
        let dev_ptr = dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Self::Softc>();
        unsafe { sc_ptr.as_ref().unwrap() }
    }

    //fn get_local_softc<T>(&self, dev: &mut Device) -> &mut T {
    //}

    fn device_probe_glue(&self, dev: Device) -> Result<ProbeRes> {
        self.device_probe(&dev)
    }
    fn device_attach_glue(&self, mut dev: Device) -> Result<()> {
        self.device_attach(&mut dev)?;
        Ok(())
    }
    fn device_detach_glue(&self, mut dev: Device) -> Result<()> {
        self.device_detach(&mut dev)?;
        let dev_ptr = dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Self::Softc>();
        unsafe { drop_in_place(sc_ptr) }
        Ok(())
    }
    fn device_probe(&self, dev: &Device) -> Result<ProbeRes>;
    fn device_attach(&self, dev: &mut Device) -> Result<AttachRes>;
    fn device_detach(&self, dev: &mut Device) -> Result<()>;
}

pub mod wrappers {
    use super::*;

    pub fn device_get_parent(dev: &Device) -> Result<Device> {
        let dev_ptr = dev.as_ptr();
        let res = unsafe { bindings::device_get_parent(dev_ptr) };
        if res.is_null() {
            Err(ENULLPTR)
        } else {
            Ok(Device::new(res))
        }
    }

    pub fn device_set_desc(dev: &Device, desc: &'static CStr) {
        let dev_ptr = dev.as_ptr();
        let desc_ptr = desc.as_ptr();
        unsafe { bindings::device_set_desc(dev_ptr, desc_ptr) }
    }

    pub fn device_get_nameunit(dev: &Device) -> &CStr {
        let dev_ptr = dev.as_ptr();
        let name = unsafe { bindings::device_get_nameunit(dev_ptr) };
        unsafe { CStr::from_ptr(name) }
    }

    pub fn device_get_driver(dev: &Device) -> *mut bindings::driver_t {
        dev.driver
    }

}

#[derive(Debug)]
pub struct Device {
    dev: *mut _device,
    driver: *mut bindings::driver_t,
    //local_softc: usize,
}

impl Clone for Device {
    fn clone(&self) -> Self {
        Device {
            dev: self.dev,
            driver: self.driver,
            //local_softc: DO NOT CLONE THIS FIELD!
        }
    }
}

unsafe impl Sync for Device {}

impl Device {
    pub fn new(dev: *mut _device) -> Self {
        // This is called here rather than as-needed based on the assumption
        // that cross-langugage inlining is not available.
        let driver = unsafe {
            bindings::device_get_driver(dev)
        };
        Self {
            dev,
            driver,
        }
    }
}
impl Device {
    pub fn as_ptr(&self) -> *mut _device {
        self.dev
    }

    pub fn get_driver(&self) -> *mut bindings::driver_t {
        self.driver
    }
}
