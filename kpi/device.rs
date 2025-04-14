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
    ($dev:expr, $sc:expr) => {
        {
            use $crate::bindings;
            use $crate::device::{DeviceState, DeviceIf};
            use core::pin::Pin;

            let state = $dev.get_state();
            assert!(state == DeviceState::Attaching);

            let dev_ptr = $dev.as_ptr();
            let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
            let sc_ptr = sc_void_ptr.cast::<Option<<Self as DeviceIf>::Softc>>();
            let sc_mut_ref = unsafe { sc_ptr.as_mut().unwrap_unchecked() };
            assert!(sc_mut_ref.is_none());
            *sc_mut_ref = Some($sc);
            unsafe { Pin::new_unchecked(sc_mut_ref.as_mut().unwrap()) }
        }
    };
}

#[macro_export]
macro_rules! device_get_softc {
    ($dev:expr) => {
        {
            use core::pin::Pin;
            use $crate::bindings;
            use $crate::bindings::device_t;
            use $crate::device::{DeviceState, DeviceIf};

            let state = $dev.get_state();
            let dev_ptr = $dev.as_ptr();
            let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
            let sc_ptr = sc_void_ptr.cast::<Option<<Self as DeviceIf>::Softc>>();
            // Omit a check since the pointer returned by C's device_get_softc should never be NULL
            let sc_ref: &Option<<Self as DeviceIf>::Softc> = unsafe { sc_ptr.as_ref().unwrap_unchecked() };

            let init_sc_ref = match state {
                DeviceState::Unknown => unreachable!("cannot call device_get_softc! in device_probe"),
                DeviceState::Attaching => sc_ref.as_ref().unwrap(),
                DeviceState::Attached => unsafe { sc_ref.as_ref().unwrap_unchecked() },
            };
            unsafe { Pin::new_unchecked(init_sc_ref) }
        }
    };
}

#[macro_export]
macro_rules! device_probe {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            int device_probe(device_t dev)
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
                {
                    use $crate::device::{Device, DeviceIf, DeviceState};
                    use $crate::bindings;
                    use core::any::TypeId;
                    use core::ptr::write;

                    // Now that we've started attaching the device set the softc to None

                    // Only needed to help type inference in the next line
                    let dev_ref: &mut Device = &mut dev;
                    let dev_ptr = dev_ref.as_ptr();
                    let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
                    let sc_ptr = sc_void_ptr.cast::<Option<<$driver_ty as DeviceIf>::Softc>>();
                    unsafe { write(sc_ptr, None) };

                    // Set the state of the pointer that's passed to the device_attach impl
                    dev.set_state(DeviceState::Attaching);
                }
            }
            with drop glue {
                {
                    use $crate::bindings;
                    use $crate::device::DeviceIf;

                    let dev_ptr = dev.as_ptr();
                    let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
                    let sc_ptr = sc_void_ptr.cast::<Option<<$driver_ty as DeviceIf>::Softc>>();
                    assert!(unsafe { sc_ptr.as_ref().unwrap_unchecked().is_some() });
                }
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
                {
                    use $crate::device::{Device, DeviceState};

                    // Only needed to help type inference in the next line
                    let dev_ref: &mut Device = &mut dev;
                    dev_ref.set_state(DeviceState::Attached);
                };
            }
            with drop glue {
                {
                    use $crate::bindings;
                    use $crate::device::{Device, DeviceIf};
                    use core::ptr::drop_in_place;

                    let dev_ptr = dev.as_ptr();
                    let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
                    let sc_ptr = sc_void_ptr.cast::<Option<<$driver_ty as DeviceIf>::Softc>>();
                    unsafe { drop_in_place(sc_ptr) };
                };
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
pub enum DeviceState {
    Unknown,
    Attaching,
    Attached,
}

#[derive(Debug, Copy, Clone)]
pub struct Device {
    dev_ptr: device_t,
    state: DeviceState,
}

unsafe impl Sync for Device {}
unsafe impl Send for Device {}

impl Device {
    // Creates a Device wrapper from a device_t
    pub fn new(dev_ptr: device_t) -> Self {
        Self {
            dev_ptr,
            state: DeviceState::Unknown,
        }
    }

    pub fn get_state(&self) -> DeviceState {
        self.state
    }

    pub unsafe fn set_state(&mut self, state: DeviceState) {
        self.state = state;
    }

    pub fn as_ptr(&self) -> device_t {
        self.dev_ptr
    }
}
