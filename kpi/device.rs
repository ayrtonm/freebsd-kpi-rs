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

use crate::bindings::{device_t, driver_t, kobj_method_t, kobjop_desc};
use crate::prelude::*;
use core::any::TypeId;
use core::ffi::{c_int, CStr};
use core::mem::{offset_of, size_of};
use core::ops::{Deref, DerefMut};
use core::pin::Pin;
use core::ptr::{null_mut, write};

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

impl AsCType<c_int> for BusProbe {
    fn as_c_type(self) -> c_int {
        self as c_int
    }
}

impl AsRustType<Device> for device_t {
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

#[macro_export]
macro_rules! device_init_softc {
    ($dev:expr, $sc:expr) => {{
        use core::pin::Pin;
        use core::ptr::write;
        use $crate::bindings;
        use $crate::device::DevicePointerState;

        assert!($dev.get_ptr_state() == DevicePointerState::Attaching);

        let dev_ptr = $dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };

        let sc_ptr = sc_void_ptr.cast::<Self::Softc>();

        unsafe { write(sc_ptr, $sc) };

        unsafe {
            $dev.set_ptr_state(DevicePointerState::Attached);
        }

        let sc_mut_ref = unsafe { sc_ptr.as_mut().unwrap() };
        unsafe { Pin::new_unchecked(sc_mut_ref) }
    }};
}

#[macro_export]
macro_rules! device_get_softc {
    ($dev:expr) => {{
        $crate::device_get_softc!($dev, Self)
    }};
    ($dev:expr, $driver_ty:ident) => {{
        use core::any::TypeId;
        use core::pin::Pin;
        use $crate::bindings;
        use $crate::device::wrappers::device_get_driver;
        use $crate::device::{DeviceIf, DevicePointerState};

        // Check if the device pointer has a known softc type
        match $dev.get_softc_ty() {
            Some(sc_ty) => {
                // If the device pointer had a softc type just check the TypeId of what we're
                // casting to. This should usually get optimized out.
                assert!(TypeId::of::<<$driver_ty as DeviceIf>::Softc>() == sc_ty);
            }
            None => {
                // If the device pointer had no softc type fall back to checking the device's driver
                let driver = device_get_driver($dev);
                assert!($driver_ty::get_driver() == driver);
            }
        };
        assert!($dev.get_ptr_state() == DevicePointerState::Attached);

        let dev_ptr = $dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<<$driver_ty as DeviceIf>::Softc>();
        let sc_ref = unsafe { sc_ptr.as_ref() };
        let sc_ref = unsafe { sc_ref.unwrap_unchecked() };
        unsafe { Pin::new_unchecked(sc_ref) }
    }};
}

#[macro_export]
macro_rules! device_probe {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            int device_probe(device_t dev)
            with init glue {
                use $crate::device::{Device, DevicePointerState};

                Device::set_ptr_state(&mut dev, DevicePointerState::Unknown);
                // This glue could set the softc ty but a pointer in the probing state can't
                // transition out of that state in rust alone so there's no benefit to setting the
                // type since we can't do anything useful with it.
            }
            rust returns $crate::device::BusProbe
        }
    };
}

#[macro_export]
macro_rules! device_attach {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            int device_attach(device_t dev)
            with init glue {
                use $crate::device::{Device, DeviceIf};
                use core::any::TypeId;

                Device::set_ptr_state(&mut dev, $crate::device::DevicePointerState::Attaching);
                Device::set_softc_ty(&mut dev, TypeId::of::<<$driver_ty as DeviceIf>::Softc>());
            }
            with drop glue {
                assert!(dev.get_ptr_state() == $crate::device::DevicePointerState::Attached);
            }
        }
    };
}

#[macro_export]
macro_rules! device_detach {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            int device_detach(device_t dev)
            with init glue {
                use $crate::device::{Device, DeviceIf, DevicePointerState};
                use core::any::TypeId;

                Device::set_ptr_state(&mut dev, DevicePointerState::Attached);
                Device::set_softc_ty(&mut dev, TypeId::of::<<$driver_ty as DeviceIf>::Softc>());
            }
            with drop glue {
                use $crate::device::IsDriver;
                let sc_ptr = core::ptr::from_ref($crate::device_get_softc!(dev, $driver_ty).get_ref());
                unsafe { core::ptr::drop_in_place(sc_ptr.cast_mut()) }
            }
        }
    };
}

pub trait IsDriver: DeviceIf {
    fn get_driver() -> *const bindings::kobj_class;
}

pub trait DeviceIf {
    type Softc: 'static;

    fn device_probe(dev: Device) -> Result<BusProbe>;
    fn device_attach(dev: Device) -> Result<()>;
    fn device_detach(dev: Device) -> Result<()>;
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

    pub fn device_get_driver(dev: Device) -> *mut driver_t {
        let dev_ptr = dev.as_ptr();
        unsafe { bindings::device_get_driver(dev_ptr) }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DevicePointerState {
    Unknown,
    Attaching,
    Attached,
}

#[derive(Debug, Copy, Clone)]
pub struct Device {
    dev_ptr: device_t,
    ptr_state: DevicePointerState,
    softc_ty: Option<TypeId>,
}

unsafe impl Sync for Device {}
unsafe impl Send for Device {}

impl Device {
    // Creates a Device wrapper from a device_t
    pub fn new(dev_ptr: device_t) -> Self {
        Self {
            dev_ptr,
            ptr_state: DevicePointerState::Unknown,
            softc_ty: None,
        }
    }

    pub fn get_ptr_state(&self) -> DevicePointerState {
        self.ptr_state
    }

    pub unsafe fn set_ptr_state(&mut self, ptr_state: DevicePointerState) {
        self.ptr_state = ptr_state;
    }

    pub fn get_softc_ty(&self) -> Option<TypeId> {
        self.softc_ty
    }

    pub fn set_softc_ty(&mut self, ty: TypeId) {
        self.softc_ty = Some(ty);
    }

    pub fn as_ptr(&self) -> device_t {
        self.dev_ptr
    }
}
