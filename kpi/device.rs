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
use crate::cell::UniqueState;
use crate::kpi_prelude::*;
use core::ffi::{c_int, CStr};
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

impl AsCType<c_int> for ProbeRes {
    fn as_c_type(self) -> c_int {
        self as c_int
    }
}

impl<S> AsRustType<Device<S>> for *mut _device {
    fn as_rust_type(self) -> Device<S> {
        unsafe { Device::<S>::new(self) }
    }
}

pub trait Softc {
    type BASE; // TODO: add this bound : Sync;

    //fn init_softc(&self, dev: Device) -> Result<Self::BASE>;

    //fn init_softc(&self, dev: &'a mut Device<S>) -> &'a mut Self::BASE {
    //fn get_softc<'a, S>(&'a self, dev: &'a Device<S>) -> &'a Self::BASE {
    //    let dev_ptr = dev.as_ptr();
    //    let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
    //    let sc_ptr = sc_void_ptr.cast::<Self::BASE>();
    //    unsafe { sc_ptr.as_ref().unwrap() }
    //}

    //unsafe fn get_softc_mut<S>(&self, dev: Device<S>) -> &mut Self::BASE {
    //    let dev_ptr = dev.as_ptr();
    //    let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
    //    let sc_ptr = sc_void_ptr.cast::<Self::BASE>();
    //    unsafe { sc_ptr.as_mut().unwrap() }
    //}
}

pub struct Probe(());
pub type Attach = ();
//pub struct Attach(());
pub struct Detach(());
unsafe impl UniqueState for Device<Probe> {}
unsafe impl UniqueState for Device<Attach> {}
unsafe impl UniqueState for Device<Detach> {}

pub trait DeviceIf: Softc {
    fn device_probe_glue(&self, dev: Device<Probe>) -> Result<ProbeRes> {
        self.device_probe(dev)
    }
    fn device_attach_glue(&self, dev: Device<Attach>) -> Result<()> {
        //let sc = self.init_softc(dev)?;
        //unsafe {
        //    *self.get_softc_mut(dev) = sc;
        //}
        self.device_attach(dev)
    }
    fn device_detach_glue(&self, dev: Device<Detach>) -> Result<()> {
        self.device_detach(dev)?;
        //let sc_ptr = self.get_softc(dev) as *const Self::BASE as *mut Self::BASE;
        //unsafe { drop_in_place(sc_ptr) }
        Ok(())
    }
    fn device_probe(&self, dev: Device<Probe>) -> Result<ProbeRes>;
    fn device_attach(&self, dev: Device<Attach>) -> Result<()>;
    fn device_detach(&self, dev: Device<Detach>) -> Result<()>;
}

pub type Device<S = ()> = Ptr<_device, S>;

impl<S> Device<S> {
    pub fn get_parent(&self) -> Result<Device<()>> {
        let dev_ptr = self.as_ptr();
        let res = unsafe { bindings::device_get_parent(dev_ptr) };
        if res.is_null() {
            Err(ENULLPTR)
        } else {
            Ok(unsafe { Device::new(res) })
        }
    }

    pub fn set_desc(&self, desc: &'static CStr) {
        let dev_ptr = self.as_ptr();
        let desc_ptr = desc.as_ptr();
        unsafe { bindings::device_set_desc(dev_ptr, desc_ptr) }
    }

    pub fn get_nameunit(&self) -> &CStr {
        let dev_ptr = self.as_ptr();
        let name = unsafe { bindings::device_get_nameunit(dev_ptr) };
        unsafe { CStr::from_ptr(name) }
    }
}
