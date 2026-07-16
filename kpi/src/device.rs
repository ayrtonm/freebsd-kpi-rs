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

use core::pin::Pin;
use crate::bindings::{device_state_t, device_t, driver_t, _device};
use crate::boxed::Box;
use crate::ffi::Ptr;
use crate::driver::Driver;
use crate::ffi::{ArrayCString, UninitRef};
use crate::kobj::{AsCType, AsRustType};
use crate::prelude::*;
use crate::vec::Vec;
use crate::{ErrCode, define_interface};
use core::ffi::{CStr, c_int};
use core::ptr::{null_mut};
use crate::sync::arc::InnerArc;
use crate::ffi::Ref;

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct Device(Ptr<_device>);

impl Device {
    pub fn new(ptr: *mut _device) -> Self {
        Self(Ptr::new(ptr))
    }

    pub fn as_ptr(&self) -> *mut _device {
        self.0.as_ptr()
    }
}

/// The result of probing a device with a driver.
///
/// This intentionally has no constructors and instead can be created by using the `BUS_PROBE_*`
/// constants in [`crate::prelude`].
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct BusProbe(pub c_int);

impl AsCType<c_int> for BusProbe {
    fn as_c_type(self) -> c_int {
        self.0
    }
}

// Used in device_attach
impl AsRustType<'_, Device> for device_t {
    fn as_rust_type(&self) -> Device {
        Device(Ptr::new(*self))
    }
}

// Allows turning a device_t argument appearing in kobj interfaces into a Pin<&T> to any type. It's
// the responsibility of a kobj trait authors to restrict the Ref to the softc's type or to one of
// its base classes.
impl<'a, T> AsRustType<'a, Ref<'a, T>> for device_t {
    fn as_rust_type(&'a self) -> Ref<'a, T> {
        let void_ptr = unsafe { bindings::device_get_softc(*self) };
        let sc_ptr = void_ptr.cast::<InnerArc<T>>();
        let sc_ref = unsafe { sc_ptr.as_ref().unwrap() };
        Ref(sc_ref)
    }
}

define_interface! {
    in DeviceIf
    fn device_probe(dev: device_t) -> int,
        with desc device_probe_desc
        and typedef device_probe_t;
    fn device_attach(dev: device_t) -> int,
        with desc device_attach_desc
        and typedef device_attach_t,
        with init glue {
            use $crate::bindings;
            use $crate::device::Device;
            use $crate::ffi::UninitRef;
            use $crate::kobj::KobjLayout;
            use core::mem::MaybeUninit;

            // Used to constrain type inference
            let dev: Device = dev;

            let void_ptr = unsafe { bindings::device_get_softc(dev.as_ptr()) };
            type Softc = <SelfType as KobjLayout>::Layout;
            let sc_ptr = void_ptr.cast::<MaybeUninit<Softc>>();
            let sc_ref = unsafe { sc_ptr.as_mut().unwrap() };
            let mut sc_init = false;

            let uninit_sc = unsafe { UninitRef::from_raw(sc_ref, &mut sc_init) };
        },
        with drop glue {
            // drop glue is only called if device_attach succeeded
            if !sc_init {
                device_println!(dev, "Must call .init() on UninitRef<Softc> in device_attach");
                return bindings::ENXIO;
            }
        },
        with prefix args { uninit_sc };
    fn device_detach(dev: device_t) -> int,
        with desc device_detach_desc
        and typedef device_detach_t,
        with drop glue {
            let sc_ptr = &raw const (*dev);
            unsafe { core::ptr::drop_in_place(sc_ptr.cast_mut()) }
        };
    fn device_shutdown(dev: device_t) -> int,
        with desc device_shutdown_desc
        and typedef device_shutdown_t;
    fn device_suspend(dev: device_t) -> int,
        with desc device_suspend_desc
        and typedef device_suspend_t;
    fn device_resume(dev: device_t) -> int,
        with desc device_resume_desc
        and typedef device_resume_t;
    fn device_quiesce(dev: device_t) -> int,
        with desc device_quiesce_desc
        and typedef device_quiesce_t;
    fn device_register(dev: device_t) -> int,
        with desc device_register_desc
        and typedef device_register_t;
}

/// The device interface defined by device_if.m
///
/// This is used to match devices to drivers during autoconfiguration and allow drivers to handle
/// system events such as suspend, resume and shutdown.
///
/// All drivers defined by [`driver!`][crate::driver!] must implement this trait to at least define
/// their softc type. Any methods left unimplemented will just panic if called.
///
/// The softc type associated with each `DeviceIf` impl has the bounds `'static + Sync`. `'static`
/// means any references in the softc must always point to global data. The softc will generally own
/// or share the data it contains rather than use references so this is usually not a problem. The
/// `Sync` bound roughly means that all fields in the softc must be safe to share between multiple
/// threads.

#[diagnostic::on_unimplemented(message = "
Implement the device interface trait and define the softc as follows

```
use kpi::device::DeviceIf;

pub struct {Self}Softc {{ /* softc fields go here */ }}

impl DeviceIf for {Self} {{
    type Softc = {Self}Softc;
}}
```
")]
#[allow(unused_variables)]
pub trait DeviceIf: Driver {
    /// The softc associated with the driver.
    ///
    /// If the driver is a subclass of another, then this must be an appropriate
    /// [`SubClass`][crate::ffi::SubClass].
    type Softc: 'static + Sync;

    /// Used to probe whether the given device is supported by the driver.
    fn device_probe(dev: Device) -> Result<BusProbe> {
        unimplemented!()
    }

    /// Used to initialize a driver.
    ///
    /// All implementations must call [`init`][crate::ffi::UninitRef::init] on the `uninit_sc`
    /// argument before this function returns to avoid a panic at runtime.
    fn device_attach(uninit_sc: UninitRef<Self::Softc>, dev: Device) -> Result<()> {
        unimplemented!()
    }

    /// Used to remove a driver.
    ///
    /// If all explicitly grabbed refcounts to the softc have been released, then the softc's memory
    /// will be freed after this function returns. This includes both the region of memory allocated
    /// for the softc struct as well as any memory accessible to it via layers of indirection. For
    /// example, if a softc struct includes a `Box<T>` field (i.e. a pointer to the heap with
    /// ownership of a `T`) the `T` in the heap will also be freed. This applies recursively through
    /// any number of layers of indirection.
    fn device_detach(sc: Ref<Self::Softc>) -> Result<()> {
        unimplemented!()
    }
    fn device_shutdown(sc: Ref<Self::Softc>) -> Result<()> {
        unimplemented!()
    }
    fn device_suspend(sc: Ref<Self::Softc>) -> Result<()> {
        unimplemented!()
    }
    fn device_resume(sc: Ref<Self::Softc>) -> Result<()> {
        unimplemented!()
    }
    fn device_quiesce(sc: Ref<Self::Softc>) -> Result<()> {
        unimplemented!()
    }
}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;

    gen_newtype! {
        BusProbe as i32,
        BUS_PROBE_SPECIFIC,
        BUS_PROBE_VENDOR,
        BUS_PROBE_DEFAULT,
        BUS_PROBE_LOW_PRIORITY,
        BUS_PROBE_GENERIC,
        BUS_PROBE_HOOVER,
        BUS_PROBE_NOWILDCARD,
    }

    /// Get the softc for a given device
    ///
    /// # Safety
    ///
    /// It is the caller's responsibility to ensure the lifetime of the return value does not extend
    /// past device_detach for the device.
    pub unsafe fn device_get_softc<'a, D: DeviceIf>(dev: Device) -> Pin<&'a D::Softc> {
        let dev_ptr = dev.as_ptr();
        assert_eq!(device_get_driver(dev), <D as Driver>::DRIVER);
        let void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = void_ptr.cast::<D::Softc>();
        let sc_ref = unsafe { sc_ptr.as_ref().unwrap() };
        unsafe { Pin::new_unchecked(sc_ref) }
    }

    pub fn device_claim_softc(dev: Device) {
        unsafe { bindings::device_claim_softc(dev.as_ptr()) }
    }

    pub fn device_get_state(dev: Device) -> device_state_t {
        unsafe { bindings::device_get_state(dev.as_ptr()) }
    }

    pub fn device_get_parent(dev: Device) -> Result<Device> {
        let dev_ptr = dev.as_ptr();
        let res = unsafe { bindings::device_get_parent(dev_ptr) };
        if res.is_null() {
            Err(ENULLPTR)
        } else {
            Ok(Device::new(res))
        }
    }

    pub fn device_get_children(dev: Device) -> Result<Box<[Device], M_TEMP>> {
        let mut devlistp = null_mut();
        let mut devcountp = 0;
        let res = unsafe {
            bindings::device_get_children(dev.as_ptr(), &raw mut devlistp, &raw mut devcountp)
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        let children =
            unsafe { Vec::from_raw(devlistp.cast::<Device>(), devcountp.try_into().unwrap()).into_boxed_slice() };
        Ok(children)
    }

    pub fn device_probe_and_attach(dev: Device) -> Result<()> {
        let dev_ptr = dev.as_ptr();
        let res = unsafe { bindings::device_probe_and_attach(dev_ptr) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn device_set_desc(dev: Device, desc: &'static CStr) {
        let dev_ptr = dev.as_ptr();
        let desc_ptr = desc.as_ptr();
        unsafe { bindings::device_set_desc(dev_ptr, desc_ptr) }
    }

    pub fn device_get_desc(dev: Device) -> ArrayCString {
        let name_ptr = unsafe { bindings::device_get_desc(dev.as_ptr()) };
        assert!(!name_ptr.is_null());
        let name = unsafe { CStr::from_ptr(name_ptr) };
        ArrayCString::new(name)
    }

    /// Returns a copy of the device name
    pub fn device_get_name(dev: Device) -> ArrayCString {
        let name_ptr = unsafe { bindings::device_get_name(dev.as_ptr()) };
        assert!(!name_ptr.is_null());
        let name = unsafe { CStr::from_ptr(name_ptr) };
        ArrayCString::new(name)
    }

    /// Returns a copy of the device name and unit number
    pub fn device_get_nameunit(dev: Device) -> ArrayCString {
        let name_ptr = unsafe { bindings::device_get_nameunit(dev.as_ptr()) };
        assert!(!name_ptr.is_null());
        let name = unsafe { CStr::from_ptr(name_ptr) };
        ArrayCString::new(name)
    }

    pub fn device_get_driver(dev: Device) -> *mut driver_t {
        unsafe { bindings::device_get_driver(dev.as_ptr()) }
    }

    pub fn device_add_child(
        dev: Device,
        name: &'static CStr,
        unit: Option<u32>,
    ) -> Result<Device> {
        let unit = unit.unwrap_or(bindings::DEVICE_UNIT_ANY as u32);
        let child = unsafe { bindings::device_add_child(dev.as_ptr(), name.as_ptr(), unit as i32) };
        if child.is_null() {
            Err(ENULLPTR)
        } else {
            Ok(Device::new(child))
        }
    }
}

#[allow(dead_code, unused)]
#[cfg(test)]
mod tests {
    use super::*;
    use crate::define_driver;
    use crate::ffi::{UninitRef, Ref};
    use crate::tests::{DriverManager, LoudDrop};
    use core::ptr::null_mut;
    use core::sync::atomic::{AtomicPtr, Ordering};
    use std::ffi::{CStr};
    use std::vec::Vec;

    /* These are the drivers that will be used in tests */
    #[repr(C)]
    #[derive(Debug)]
    pub struct TestDriverSoftc {
        dev: Device,
        const_data: u32,
    }
    // This is only used to pipe a Device managed by one driver to another to ensure
    // device_get_softc type checking works as expected. It is unrealistic to do this via a static
    // like this, but this scenario does come up when a driver manages its children's device_t.
    static STASHED_DEVICE: AtomicPtr<_device> = AtomicPtr::new(null_mut());

    impl AnotherDriver {
        fn get_stashed_softc(dev: Device) {
            let test_driver_dev = Device::new(STASHED_DEVICE.load(Ordering::Relaxed));
            let test_driver_sc = unsafe { device_get_softc::<TestDriver>(test_driver_dev) };
            let another_driver_sc = unsafe { device_get_softc::<Self>(dev) };
        }
    }
    impl DeviceIf for TestDriver {
        type Softc = TestDriverSoftc;
        fn device_probe(dev: Device) -> Result<BusProbe> {
            if !ofw_bus_status_okay(dev) {
                println!("test_driver: rejected {dev:x?} as not ok");
                return Err(ENXIO);
            }
            if !ofw_bus_is_compatible(dev, c"device,test_driver") {
                println!("test_driver: rejected {dev:x?} as incompatible");
                return Err(ENXIO);
            }
            device_set_desc(dev, c"test driver");
            println!("test_driver: accepted {dev:x?}");
            Ok(BUS_PROBE_DEFAULT)
        }
        fn device_attach(uninit_sc: UninitRef<Self::Softc>, dev: Device) -> Result<()> {
            let sc = uninit_sc.init(TestDriverSoftc {
                dev,
                const_data: 0xdeadbeef,
            });
            let dev_ptr = sc.dev.as_ptr();
            if ofw_bus_is_compatible(dev, c"another_driver,get_softc") {
                STASHED_DEVICE.store(sc.dev.as_ptr(), Ordering::Relaxed);
            }
            println!("{:x?}", sc);
            Ok(())
        }
        fn device_detach(sc: Ref<Self::Softc>) -> Result<()> {
            assert!(sc.const_data == 0xdeadbeef);
            Ok(())
        }
    }
    define_driver!(
        static test_driver: TestDriver = {
            name: c"test_driver",
        }
        static test_driver_methods = {
            device_probe: test_driver_probe,
            device_attach: test_driver_attach,
            device_detach: test_driver_detach,
        }
        inherit from simplebus_driver,
    );

    #[repr(C)]
    #[derive(Debug)]
    pub struct AnotherDriverSoftc {
        dev: Device,
        loud: LoudDrop,
    }
    impl DeviceIf for AnotherDriver {
        type Softc = AnotherDriverSoftc;
        fn device_probe(dev: Device) -> Result<BusProbe> {
            if !ofw_bus_status_okay(dev) {
                println!("another_driver: rejected {dev:x?} as not ok");
                return Err(ENXIO);
            }
            if !ofw_bus_is_compatible(dev, c"device,another_driver") {
                println!("another_driver: rejected {dev:x?} as incompatible");
                return Err(ENXIO);
            }
            device_set_desc(dev, c"another driver");
            println!("another_driver: accepted {dev:x?}");
            Ok(BUS_PROBE_DEFAULT)
        }
        fn device_attach(uninit_sc: UninitRef<Self::Softc>, dev: Device) -> Result<()> {
            let sc = uninit_sc.init(AnotherDriverSoftc {
                dev,
                loud: LoudDrop,
            });
            println!("attaching another driver");
            // Store a pointer owning a refcount to AnotherDriver's Softc in a TestDriverSoftc for
            // some appropriate device_t. This means that AnotherDriver::device_detach will drop a
            // refcount but will not be able to free the softc (as shown by the LoudDrop Drop impl).
            if ofw_bus_is_compatible(sc.dev, c"another_driver,get_softc") {
                Self::get_stashed_softc(sc.dev);
            }
            Ok(())
        }
        fn device_detach(sc: Ref<Self::Softc>) -> Result<()> {
            Ok(())
        }
    }
    define_driver!(
        static another_driver: AnotherDriver = {
            name: c"another_driver",
        }
        static another_driver_methods = {
            device_probe: another_driver_probe,
            device_attach: another_driver_attach,
            device_detach: another_driver_detach,
        }
    );

    #[test]
    fn normal_flow() {
        let mut m = DriverManager::new();
        m.add_test_device(c"device,test_driver");
        m.add_test_device(c"device,another_driver");
        m.add_test_driver::<TestDriver>();
        m.add_test_driver::<AnotherDriver>();
        m.probe_attach_detach();
    }

    #[test]
    fn reverse_detach() {
        let mut m = DriverManager::new();
        m.add_test_device(c"device,test_driver");
        m.add_test_device(c"device,another_driver");
        m.add_test_driver::<TestDriver>();
        m.add_test_driver::<AnotherDriver>();
        m.probe_all();
        m.attach_all();
        DriverManager::detach_devices(&mut m.devices.iter_mut().rev());
    }

    #[test]
    fn get_softc() {
        let mut m = DriverManager::new();
        m.add_test_device(c"device,test_driver")
            .compat_strs
            .push(c"another_driver,get_softc");
        let dev = m.add_test_device(c"device,another_driver");
        dev.compat_strs.push(c"another_driver,get_softc");
        m.add_test_driver::<TestDriver>();
        m.add_test_driver::<AnotherDriver>();
        m.probe_all();
        m.attach_all();
        DriverManager::detach_devices(&mut m.devices.iter_mut().rev());
    }
}
