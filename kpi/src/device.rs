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
use core::ffi::{c_int, CStr};
use core::ptr::{null_mut};

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

impl AsCType<device_t> for Device {
    fn as_c_type(self) -> device_t {
        self.dev_ptr
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
        use $crate::bindings;
        use $crate::device::{DeviceIf, DeviceState};

        let state = $dev.get_state();
        assert!(state == DeviceState::Attaching);

        let dev_ptr = $dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Option<<Self as DeviceIf>::Softc>>();
        let sc_mut_ref = unsafe { sc_ptr.as_mut().unwrap() };
        assert!(sc_mut_ref.is_none());
        *sc_mut_ref = Some($sc);
        unsafe { $crate::cell::OwnedMutRef::new(sc_mut_ref.as_mut().unwrap(), dev_ptr) }
    }};
}

#[macro_export]
macro_rules! device_get_softc {
    ($dev:expr) => {{
        use $crate::bindings;
        use $crate::device::{DeviceIf, DeviceState};

        let state = $dev.get_state();
        let dev_ptr = $dev.as_ptr();
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = sc_void_ptr.cast::<Option<<Self as DeviceIf>::Softc>>();
        // Omit a check since the pointer returned by C's device_get_softc should never be NULL
        let sc_ref: &Option<<Self as DeviceIf>::Softc> = unsafe { sc_ptr.as_ref().unwrap() };

        let init_sc_ref = match state {
            DeviceState::Unknown => unreachable!("cannot call device_get_softc! in device_probe"),
            DeviceState::Attaching => sc_ref
                .as_ref()
                .expect("must initialize softc using device_init_softc!"),
            DeviceState::Attached => sc_ref.as_ref().unwrap(),
        };
        unsafe { $crate::cell::OwnedRef::new(init_sc_ref, dev_ptr) }
    }};
}

#[macro_export]
macro_rules! device_probe {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            device_probe(dev: device_t) -> int;
            with init glue {
                {
                    use $crate::device::{Device, DeviceState};

                    // Only needed to help type inference in the next line
                    let dev_ref: &mut Device = &mut dev;
                    dev.set_state(DeviceState::Unknown);
                };
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
            device_attach(dev: device_t) -> int;
            with init glue {
                {
                    use $crate::device::{Device, DeviceIf, DeviceState};
                    use $crate::bindings;
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
                    assert!(unsafe { sc_ptr.as_ref().unwrap().is_some() });
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
            device_detach(dev: device_t) -> int;
            with drop glue {
                {
                    use $crate::bindings;
                    use $crate::device::DeviceIf;
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

define_interface! {
    device_shutdown(dev: device_t) -> int;
    device_suspend(dev: device_t) -> int;
    device_resume(dev: device_t) -> int;
    device_quiesce(dev: device_t) -> int;
    device_register(dev: device_t) -> int;
}

pub trait IsDriver: DeviceIf {
    fn get_driver() -> *const bindings::kobj_class;
}

pub trait DeviceIf<State = ()> {
    type Softc: 'static;

    fn device_probe(_dev: Device) -> Result<BusProbe> {
        unimplemented!()
    }
    fn device_attach(_dev: Device) -> Result<()> {
        unimplemented!()
    }
    fn device_detach(_dev: Device) -> Result<()> {
        unimplemented!()
    }
    fn device_shutdown(_dev: Device) -> Result<()> {
        unimplemented!()
    }
    fn device_suspend(_dev: Device) -> Result<()> {
        unimplemented!()
    }
    fn device_resume(_dev: Device) -> Result<()> {
        unimplemented!()
    }
    fn device_quiesce(_dev: Device) -> Result<()> {
        unimplemented!()
    }
    fn device_register(_dev: Device) -> Result<&'static State> {
        unimplemented!()
    }
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

    // FIXME: 'static is wrong here.  It should be OnwedRef<CStr> instead
    pub fn device_get_nameunit(dev: Device) -> &'static CStr {
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
    // TODO: Invert state to default to Attached
    // Creates a Device wrapper from a device_t
    pub fn new(dev_ptr: device_t) -> Self {
        Self {
            dev_ptr,
            state: DeviceState::Attached,
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

#[allow(dead_code)]
#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver;
    use core::mem::MaybeUninit;
    use core::ptr::null_mut;
    use std::ffi::{c_void, CStr};
    use std::println;
    use std::sync::Mutex;
    use std::vec::Vec;

    unsafe impl Send for TestKernel {}
    unsafe impl Sync for TestKernel {}
    trait TestDriver {
        fn probe(&self) -> unsafe extern "C" fn(device_t) -> i32;
        fn attach(&self) -> unsafe extern "C" fn(device_t) -> i32;
        fn detach(&self) -> unsafe extern "C" fn(device_t) -> i32;
    }
    struct TestDevice {
        id: usize,
        driver: usize,
        softc: *mut c_void,
    }
    struct TestKernel {
        devices: Vec<TestDevice>,
        drivers: Vec<&'static dyn TestDriver>,
    }
    impl TestKernel {
        const fn new() -> Self {
            TestKernel {
                devices: Vec::new(),
                drivers: Vec::new(),
            }
        }
        fn new_device(&mut self, driver: usize) -> device_t {
            let id = self.devices.len();
            let softc = null_mut();
            let test_dev = TestDevice { id, softc, driver };
            self.devices.push(test_dev);
            id as device_t
        }
        fn register_driver(&mut self, driver: &'static dyn TestDriver) -> usize {
            let id = self.drivers.len();
            self.drivers.push(driver);
            id
        }
        fn device_test(&mut self, dev_ptr: device_t) {
            let dev = &self.devices[dev_ptr as usize];
            let driver = self.drivers[dev.driver];
            assert_eq!(
                unsafe { driver.probe()(dev_ptr) },
                bindings::BUS_PROBE_DEFAULT
            );
            assert_eq!(unsafe { driver.attach()(dev_ptr) }, 0);
            assert_eq!(unsafe { driver.detach()(dev_ptr) }, 0);
        }
    }
    static TEST_KERNEL: Mutex<TestKernel> = Mutex::new(TestKernel::new());
    pub struct MySoftc {
        dev: Device,
        x: u32,
    }
    #[no_mangle]
    extern "C" fn device_get_softc(_dev_ptr: device_t) -> *mut c_void {
        static mut SOFTC: MaybeUninit<MySoftc> = MaybeUninit::uninit();
        let ptr = &raw mut SOFTC;
        ptr.cast::<c_void>()
    }

    driver!(my_driver, c"mydriver", MyDriver, my_driver_methods,
    INTERFACES {
        device_probe my_driver_probe,
        device_attach my_driver_attach,
        device_detach my_driver_detach,
    });
    impl DeviceIf for MyDriver {
        type Softc = MySoftc;
        fn device_probe(_dev: Device) -> Result<BusProbe> {
            Ok(BUS_PROBE_DEFAULT)
        }
        fn device_attach(dev: Device) -> Result<()> {
            let x = 42;
            println!("{:p}", dev.as_ptr());
            device_init_softc!(dev, MySoftc { dev, x });
            Ok(())
        }
        fn device_detach(dev: Device) -> Result<()> {
            let sc = device_get_softc!(dev);
            assert_eq!(sc.x, 42);
            Ok(())
        }
    }
    impl TestDriver for MyDriver {
        fn probe(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            my_driver_probe
        }
        fn attach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            my_driver_attach
        }
        fn detach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            my_driver_detach
        }
    }
    extern "C" {
        fn my_driver_probe(dev: device_t) -> i32;
        fn my_driver_attach(dev: device_t) -> i32;
        fn my_driver_detach(dev: device_t) -> i32;
    }
    #[no_mangle]
    static device_probe_desc: &'static CStr = c"foo";
    #[no_mangle]
    static device_attach_desc: &'static CStr = c"foo";
    #[no_mangle]
    static device_detach_desc: &'static CStr = c"foo";

    #[test]
    fn get_softc() {
        let mut test_kernel = TEST_KERNEL.lock().unwrap();
        let driver = test_kernel.register_driver(&my_driver);
        let dev = test_kernel.new_device(driver);
        test_kernel.device_test(dev);
    }
}
