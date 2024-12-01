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
use crate::cell::UniqueOwner;
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

impl<S> AsRustType<Device<S>> for *mut _device {
    fn as_rust_type(self) -> Device<S> {
        unsafe { Device::new(self).assume_state() }
    }
}

// This trait may not be implemented for Attach or Probe
pub trait SoftcInit {}
impl SoftcInit for Detach {}
impl SoftcInit for () {}
#[derive(Debug)]
pub struct Probe(());
#[derive(Debug)]
pub struct Attach(());
#[derive(Debug)]
pub struct Detach(());
unsafe impl UniqueOwner for Attach {}
unsafe impl UniqueOwner for Detach {}

impl !UniqueOwner for () {}

pub trait DeviceIf {
    type Softc<S>: Sync;
    // Are 'static only for TypeId::of
    type Probe: 'static = ();
    type Attach: 'static = ();
    type Detach: 'static = ();

    fn device_init_softc(
        &self,
        dev: &Device<Self::Attach>,
        sc: Self::Softc<Self::Attach>,
    ) -> AttachRes {
        let dev_ptr = dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Self::Softc<Self::Attach>>();
        unsafe { *sc_ptr = sc };
        AttachRes(())
    }

    fn device_get_softc<S: SoftcInit>(&self, dev: &Device<S>) -> &Self::Softc<()> {
        let dev_ptr = dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Self::Softc<()>>();
        unsafe { sc_ptr.as_ref().unwrap() }
    }

    fn device_get_softc_with_state<S: SoftcInit>(&self, dev: &mut Device<S>) -> &Self::Softc<S> {
        let dev_ptr = dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Self::Softc<S>>();
        unsafe { sc_ptr.as_ref().unwrap() }
    }

    fn device_probe_glue(&self, dev: Device<Self::Probe>) -> Result<ProbeRes> {
        // TODO: Make this const. blocked on https://github.com/rust-lang/rust/issues/77125
        let probe = TypeId::of::<Self::Probe>();
        let attach = TypeId::of::<Self::Attach>();
        let detach = TypeId::of::<Self::Attach>();
        let dflt = TypeId::of::<()>();
        if probe != dflt {
            assert!(probe != attach);
            assert!(probe != detach);
        }
        if attach != dflt {
            assert!(attach != probe);
            assert!(attach != detach);
        }
        if detach != dflt {
            assert!(detach != probe);
            assert!(detach != attach);
        }
        self.device_probe(&dev)
    }
    fn device_attach_glue(&self, mut dev: Device<Self::Attach>) -> Result<()> {
        self.device_attach(&mut dev)?;
        Ok(())
    }
    fn device_detach_glue(&self, mut dev: Device<Self::Detach>) -> Result<()> {
        self.device_detach(&mut dev)?;
        let dev_ptr = dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Self::Softc<Self::Detach>>();
        unsafe { drop_in_place(sc_ptr) }
        Ok(())
    }
    fn device_probe(&self, dev: &Device<Self::Probe>) -> Result<ProbeRes>;
    fn device_attach(&self, dev: &mut Device<Self::Attach>) -> Result<AttachRes>;
    fn device_detach(&self, dev: &mut Device<Self::Detach>) -> Result<()>;
}

pub mod wrappers {
    use super::*;
    pub fn device_get_parent<S>(dev: &Device<S>) -> Result<Device<()>> {
        let dev_ptr = dev.as_ptr();
        let res = unsafe { bindings::device_get_parent(dev_ptr) };
        if res.is_null() {
            Err(ENULLPTR)
        } else {
            Ok(Device::new(res))
        }
    }

    pub fn device_set_desc<S>(dev: &Device<S>, desc: &'static CStr) {
        let dev_ptr = dev.as_ptr();
        let desc_ptr = desc.as_ptr();
        unsafe { bindings::device_set_desc(dev_ptr, desc_ptr) }
    }

    pub fn device_get_nameunit<S>(dev: &Device<S>) -> &CStr {
        let dev_ptr = dev.as_ptr();
        let name = unsafe { bindings::device_get_nameunit(dev_ptr) };
        unsafe { CStr::from_ptr(name) }
    }
}

#[derive(Debug)]
pub struct Device<S = ()>(*mut _device, PhantomData<S>);

unsafe impl<S> Sync for Device<S> {}

impl Device {
    pub fn new(ptr: *mut _device) -> Self {
        Self(ptr, PhantomData)
    }
}
impl<S> Device<S> {
    pub fn copy_ptr(&self) -> Device {
        Device(self.0, PhantomData)
    }

    pub unsafe fn assume_state<U>(self) -> Device<U> {
        Device(self.0, PhantomData)
    }

    pub fn as_ptr(&self) -> *mut _device {
        self.0
    }
}
