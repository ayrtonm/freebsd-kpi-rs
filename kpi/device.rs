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

pub struct DetachRes<SC>(OutPtr<BorrowCk<SC>>, DropBehavior);

impl AsCType<c_int> for ProbeRes {
    fn as_c_type(self) -> c_int {
        self as c_int
    }
}

impl<SC> AsCType<c_int> for DetachRes<SC> {
    fn as_c_type(self) -> c_int {
        unsafe {
            self.0.set_drop_behavior(self.1);
        }
        // This recursively checks that all BorrowCk'ed fields are available by trying to claim them
        // If a field has outstanding references this will panic. This also drops softc fields that
        // manage memory they allocated (e.g. Vec). Drop is a no-op for most other types and the C
        // KPI is in charge of freeing the memory allocated for the BorrowCk<SC> itself
        unsafe {
            drop_in_place(self.0.as_ptr());
        }
        0
    }
}

impl AsRustType<Device> for *mut _device {
    fn as_rust_type(self) -> Device {
        unsafe { Device::new(self) }
    }
}

pub trait Softc {
    type BASE;
}

pub trait DeviceIf: Softc {
    fn init_softc(&self, dev: Device, initial_value: Self::BASE) -> Result<()> {
        let sc = self.get_borrowck_softc(dev);
        sc.init(initial_value)
    }

    fn get_softc(&self, dev: Device) -> OutPtr<Self::BASE> {
        let sc = self.get_borrowck_softc(dev);
        sc.get()
    }

    fn claim_softc(&self, dev: Device) -> Result<RefMut<Self::BASE>> {
        let sc = self.get_borrowck_softc(dev);
        sc.claim()
    }

    fn release_softc(&self, dev: Device, prev_claim: RefMut<Self::BASE>) -> Result<()> {
        let sc = self.get_borrowck_softc(dev);
        sc.release(prev_claim)
    }

    fn share_softc(&self, dev: Device) -> Result<Ref<Self::BASE>> {
        let sc = self.get_borrowck_softc(dev);
        sc.share()
    }

    #[doc(hidden)]
    fn get_borrowck_softc(&self, dev: Device) -> OutPtr<BorrowCk<Self::BASE>> {
        unsafe {
            dev.get_softc::<BorrowCk<Self::BASE>>()
        }
    }

    fn borrowck_softc(&self, dev: Device) -> DetachRes<Self::BASE> {
        self.drop_softc(dev, DropBehavior::PanicOnDrop)
    }

    /// Sets the behavior of dynamically borrow-checking the softc if panic! is not desired.
    /// BorrowCk fields embedded in the softc must have their drop behavior set separately.
    fn drop_softc(&self, dev: Device, behavior: DropBehavior) -> DetachRes<Self::BASE> {
        DetachRes(self.get_borrowck_softc(dev), behavior)
    }

    fn device_probe(&self, dev: Device) -> Result<ProbeRes>;
    fn device_attach(&self, dev: Device) -> Result<()>;

    /// Clean up before detaching the device. The return value is used to determine the behavior of
    /// a failed softc borrow check and can be created with the `borrowck_softc` or `drop_softc`
    /// methods.
    fn device_detach(&self, dev: Device) -> Result<DetachRes<Self::BASE>>;
}

pub type Device = Ptr<_device>;

impl Device {
    pub fn get_parent(&self) -> Result<Device> {
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

    pub unsafe fn get_softc<SC>(&self) -> OutPtr<SC> {
        let dev_ptr = self.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<SC>();
        unsafe { OutPtr::new(sc_ptr) }
    }
}
