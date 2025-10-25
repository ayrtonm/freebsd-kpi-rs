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

use crate::bindings::{device_state_t, device_t, driver_t};
use crate::ffi::{Ptr, RefCountData, RefCounted};
use crate::prelude::*;
use core::ffi::{CStr, c_int};
use core::fmt;
use core::fmt::{Debug, Formatter};

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct BusProbe(c_int);

impl AsCType<c_int> for BusProbe {
    fn as_c_type(self) -> c_int {
        self.0
    }
}

impl Debug for device_t {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let driver = unsafe { bindings::device_get_driver(*self) };
        let driver = if driver.is_null() {
            c"no driver attached"
        } else {
            unsafe { CStr::from_ptr(driver.as_ref().unwrap().name) }
        };
        let desc = unsafe { bindings::device_get_desc(*self) };
        let desc = if desc.is_null() {
            c"no desc set"
        } else {
            unsafe { CStr::from_ptr(desc) }
        };
        f.debug_struct("device_t")
            .field("driver", &driver)
            .field("desc", &desc)
            .finish()
    }
}

#[macro_export]
macro_rules! device_init_softc {
    ($dev:expr, $sc:expr) => {
        device_init_softc::<Self>($dev, $sc)
    };
}

#[macro_export]
macro_rules! device_get_softc {
    ($dev:expr) => {
        device_get_softc::<Self>($dev)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! device_attach {
    ($driver_ty:ident $impl_fn_name:ident) => {
        #[macro_export]
        $crate::export_function! {
            $driver_ty $impl_fn_name
            device_attach(dev: device_t) -> int;
            with drop glue {
                {
                    use $crate::ffi::RefCountData;
                    use $crate::ffi::RefCounted;
                    use $crate::device::DeviceIf;

                    let sc_as_void_ptr = unsafe { bindings::device_get_softc(dev) };
                    let rust_sc_ptr = sc_as_void_ptr.cast::<RefCounted<<$driver_ty as DeviceIf>::Softc>>();

                    let metadata_ptr = RefCounted::metadata_ptr(rust_sc_ptr);
                    let count_ptr = RefCountData::count_ptr(metadata_ptr);
                    let count = unsafe { bindings::refcount_load(count_ptr) };
                    if count == 0 {
                        panic!("`device_init_softc` must be called in `device_attach`");
                    }
                }
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! device_detach {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            device_detach(dev: device_t) -> int;
            with drop glue {
                {
                    let sc_as_void_ptr = unsafe { bindings::device_get_softc(dev) };
                    let rust_sc_ptr = sc_as_void_ptr.cast::<RefCounted<<$driver_ty as DeviceIf>::Softc>>();

                    let metadata_ptr = RefCounted::metadata_ptr(rust_sc_ptr);
                    let count_ptr = RefCountData::count_ptr(metadata_ptr);
                    let last = unsafe {
                        bindings::refcount_release(count_ptr)
                    };
                    if last {
                        let drop_fn = RefCountData::drop_fn(metadata_ptr);
                        unsafe {
                            drop_fn(count_ptr)
                        }
                    }
                };
            }
        }
    };
}

define_interface! {
    device_probe(dev: device_t) -> int;
    device_shutdown(dev: device_t) -> int;
    device_suspend(dev: device_t) -> int;
    device_resume(dev: device_t) -> int;
    device_quiesce(dev: device_t) -> int;
    device_register(dev: device_t) -> int;
}

/// The device interface defined by device_if.m
///
/// This is used to match devices to drivers during autoconfiguration and allow drivers to handle
/// system events such as suspend, resume and shutdown.
#[diagnostic::on_unimplemented(message = "
Implement the device interface trait by adding this where the `driver!` macro was used
pub struct {Self}Softc {{}}
impl DeviceIf for {Self} {{
    type Softc = {Self}Softc;
}}
")]
#[allow(unused_variables)]
pub trait DeviceIf<State = ()> {
    type Softc: 'static + Sync;

    fn device_probe(dev: device_t) -> Result<BusProbe> {
        unimplemented!()
    }
    fn device_attach(dev: device_t) -> Result<()> {
        unimplemented!()
    }
    fn device_detach(dev: device_t) -> Result<()> {
        unimplemented!()
    }
    fn device_shutdown(dev: device_t) -> Result<()> {
        unimplemented!()
    }
    fn device_suspend(dev: device_t) -> Result<()> {
        unimplemented!()
    }
    fn device_resume(dev: device_t) -> Result<()> {
        unimplemented!()
    }
    fn device_quiesce(dev: device_t) -> Result<()> {
        unimplemented!()
    }
    fn device_register(dev: device_t) -> Result<&'static State> {
        unimplemented!()
    }
}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;
    use crate::driver::DriverIf;

    gen_newtype! {
        BusProbe,
        BUS_PROBE_SPECIFIC,
        BUS_PROBE_VENDOR,
        BUS_PROBE_DEFAULT,
        BUS_PROBE_LOW_PRIORITY,
        BUS_PROBE_GENERIC,
        BUS_PROBE_HOOVER,
        BUS_PROBE_NOWILDCARD,
    }

    pub fn device_claim_softc(dev: device_t) {
        unsafe { bindings::device_claim_softc(dev) }
    }
    pub fn device_get_state(dev: device_t) -> device_state_t {
        unsafe { bindings::device_get_state(dev) }
    }

    pub fn device_get_parent(dev: device_t) -> Result<device_t> {
        let res = unsafe { bindings::device_get_parent(dev) };
        if res.as_ptr().is_null() {
            Err(ENULLPTR)
        } else {
            Ok(res)
        }
    }

    pub fn device_set_desc(dev: device_t, desc: &'static CStr) {
        let desc_ptr = desc.as_ptr();
        unsafe { bindings::device_set_desc(dev, desc_ptr) }
    }

    // FIXME: 'static is wrong here.  It should be Ptr<CStr> or something instead
    pub fn device_get_desc(dev: device_t) -> &'static CStr {
        let name = unsafe { bindings::device_get_desc(dev) };
        unsafe { CStr::from_ptr(name) }
    }

    // FIXME: 'static is wrong here.  It should be Ptr<CStr> or something instead
    pub fn device_get_nameunit(dev: device_t) -> &'static CStr {
        let name = unsafe { bindings::device_get_nameunit(dev) };
        unsafe { CStr::from_ptr(name) }
    }

    pub fn device_get_driver(dev: device_t) -> *mut driver_t {
        unsafe { bindings::device_get_driver(dev) }
    }

    pub fn device_init_softc<T: DeviceIf + DriverIf>(dev: device_t, sc: T::Softc) -> Ptr<T::Softc> {
        use core::ptr::write;

        let state = device_get_state(dev);
        if state != bindings::DS_ATTACHING {
            panic!("`device_init_softc!` may only be called in `device_attach`");
        }

        let driver = device_get_driver(dev);
        if driver != T::DRIVER {
            panic!("`device_t` passed to `device_get_softc!` has a softc with a different type");
        }

        let sc_as_void_ptr = unsafe { bindings::device_get_softc(dev) };
        let rust_sc_ptr = sc_as_void_ptr.cast::<RefCounted<T::Softc>>();

        let metadata_ptr = RefCounted::metadata_ptr(rust_sc_ptr);
        let count_ptr = RefCountData::count_ptr(metadata_ptr);

        // Take a snapshot of the count. It must be zero since the softc is zero-initialized.
        // Interrupts are disabled in `device_attach` so it can't change out from under us
        let count = unsafe { bindings::refcount_load(count_ptr) };
        if count != 0 {
            panic!("device_init_softc may not be called twice");
        }
        // Initialize the count to **2** instead of 1. One refcount is for the value this macro
        // returns (which will get released at the end of `device_attach`) and the other is to avoid
        // dropping it between calls to interface functions. It gets dropped in the `device_detach`
        // drop glue.
        unsafe { bindings::refcount_init(count_ptr, 2) };
        let drop_fn_ptr = RefCountData::drop_fn_ptr(metadata_ptr);
        unsafe { write(drop_fn_ptr, T::DROP_FN) };

        // Claim the softc to opt it into refcounting
        device_claim_softc(dev);

        let sc_ptr = RefCounted::get_ptr(rust_sc_ptr);
        unsafe { write(sc_ptr, sc) };
        unsafe { Ptr::from_raw(rust_sc_ptr) }
    }

    pub fn device_get_softc<T: DeviceIf + DriverIf>(dev: device_t) -> Ptr<T::Softc> {
        let state = device_get_state(dev);
        if state != bindings::DS_ATTACHED && state != bindings::DS_ATTACHING {
            panic!(
                "`device_get_softc!` cannot be called during `device_probe` or after `device_detach`"
            );
        }
        let driver = device_get_driver(dev);
        if driver != T::DRIVER {
            panic!("`device_t` passed to `device_get_softc!` has a softc with a different type");
        }

        let sc_as_void_ptr = unsafe { bindings::device_get_softc(dev) };
        let rust_sc_ptr = sc_as_void_ptr.cast::<RefCounted<T::Softc>>();

        let metadata_ptr = RefCounted::metadata_ptr(rust_sc_ptr);
        let count_ptr = RefCountData::count_ptr(metadata_ptr);
        if state == bindings::DS_ATTACHING {
            let count = unsafe { bindings::refcount_load(count_ptr) };
            if count == 0 {
                panic!(
                    "`device_attach must call `device_init_softc!` before calling `device_get_softc`"
                );
            }
        }
        unsafe { bindings::refcount_acquire(count_ptr) };

        unsafe { Ptr::from_raw(rust_sc_ptr) }
    }
}

#[allow(dead_code, unused)]
#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::driver;
    use crate::ffi::Ptr;
    use crate::sync::Mutable;
    use crate::tests::{CDriverFns, DriverManager, LoudDrop};
    use std::ffi::{CStr, c_void};
    use std::vec::Vec;

    /* These are the drivers that will be used in tests */
    // repr(C) isn't strictly necessary but the test harness device_free_softc does rely on
    // device_t being the first field in memory
    #[repr(C)]
    #[derive(Debug)]
    pub struct TestDriverSoftc {
        dev: device_t,
        const_data: u32,
        another_sc: Mutable<Option<Ptr<AnotherDriverSoftc>>>,
    }
    static STASHED_DEVICE: Mutable<device_t> = Mutable::new(device_t::null());
    impl TestDriver {
        // TODO: Fix this buggy test
        //test_driver: rejected device_t { driver: "no driver attached", desc: "no desc set" } as incompatible
        //another_driver: accepted device_t { driver: "no driver attached", desc: "another driver" }
        //refcount: read 0 from 0x7fa3300018b0
        //refcount: initialized count at 0x7fa3300018b0 to 2
        //
        //thread 'device::tests::set_callback' panicked at src/cell.rs:77:13:
        //already borrowed
        //note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
        //refcount: released count from 0x7fa3300018b0
        //
        //thread 'device::tests::set_callback' panicked at library/core/src/panicking.rs:225:5:
        //panic in a function that cannot unwind
        fn set_callback_arg(arg: Ptr<AnotherDriverSoftc>) {
            let sc = device_get_softc!(*STASHED_DEVICE.get_mut());
            *sc.another_sc.get_mut() = Some(arg);
        }
    }
    impl DeviceIf for TestDriver {
        type Softc = TestDriverSoftc;
        fn device_probe(dev: device_t) -> Result<BusProbe> {
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
        fn device_attach(dev: device_t) -> Result<()> {
            let sc = TestDriverSoftc {
                dev,
                const_data: 0xdeadbeef,
                another_sc: Mutable::new(None),
            };
            let sc = device_init_softc!(dev, sc);
            *STASHED_DEVICE.get_mut() = dev;
            println!("{:x?}", sc);
            Ok(())
        }
        fn device_detach(dev: device_t) -> Result<()> {
            let sc = device_get_softc!(dev);
            assert!(sc.const_data == 0xdeadbeef);
            Ok(())
        }
    }
    driver!(test_driver, c"test_driver", TestDriver, test_driver_methods,
            inherit from simplebus_driver,
            INTERFACES {
                device_probe test_driver_probe,
                device_attach test_driver_attach,
                device_detach test_driver_detach,
            }
    );

    // repr(C) isn't strictly necessary but the test harness device_free_softc does rely on
    // device_t being the first field in memory
    #[repr(C)]
    #[derive(Debug)]
    pub struct AnotherDriverSoftc {
        dev: device_t,
        loud: LoudDrop,
    }
    impl DeviceIf for AnotherDriver {
        type Softc = AnotherDriverSoftc;
        fn device_probe(dev: device_t) -> Result<BusProbe> {
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
        fn device_attach(dev: device_t) -> Result<()> {
            let sc = AnotherDriverSoftc {
                dev,
                loud: LoudDrop,
            };
            let sc = device_init_softc!(dev, sc);
            println!("attaching another driver");
            // Store a pointer owning a refcount to AnotherDriver's Softc in a TestDriverSoftc for
            // some appropriate device_t. This means that AnotherDriver::device_detach will drop a
            // refcount but will not be able to free the softc (as shown by the LoudDrop Drop impl).
            if ofw_bus_is_compatible(dev, c"another_driver,set_callback") {
                TestDriver::set_callback_arg(sc);
            }
            Ok(())
        }
        fn device_detach(dev: device_t) -> Result<()> {
            Ok(())
        }
    }
    driver!(another_driver, c"another_driver", AnotherDriver, another_driver_methods,
            INTERFACES {
                device_probe another_driver_probe,
                device_attach another_driver_attach,
                device_detach another_driver_detach,
            }
    );

    // Needed to access Self::DRIVER to impl CDriverFns
    use crate::driver::DriverIf;

    impl CDriverFns for TestDriver {
        fn get_probe(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            test_driver::exported_fns::test_driver_probe
        }
        fn get_attach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            test_driver::exported_fns::test_driver_attach
        }
        fn get_detach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            test_driver::exported_fns::test_driver_detach
        }
        fn softc_size(&self) -> usize {
            self.0.size
        }
        fn get_driver(&self) -> *mut driver_t {
            Self::DRIVER
        }
    }
    impl CDriverFns for AnotherDriver {
        fn get_probe(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            another_driver::exported_fns::another_driver_probe
        }
        fn get_attach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            another_driver::exported_fns::another_driver_attach
        }
        fn get_detach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            another_driver::exported_fns::another_driver_detach
        }
        fn softc_size(&self) -> usize {
            self.0.size
        }
        fn get_driver(&self) -> *mut driver_t {
            Self::DRIVER
        }
    }

    #[test]
    fn normal_flow() {
        let mut m = DriverManager::new();
        m.add_test_device(c"device,test_driver");
        m.add_test_device(c"device,another_driver");
        m.drivers.push(&test_driver);
        m.drivers.push(&another_driver);
        m.probe_attach_detach();
    }

    #[test]
    fn reverse_detach() {
        let mut m = DriverManager::new();
        m.add_test_device(c"device,test_driver");
        m.add_test_device(c"device,another_driver");
        m.drivers.push(&test_driver);
        m.drivers.push(&another_driver);
        m.probe_all();
        m.attach_all();
        DriverManager::detach_devices(&mut m.devices.iter_mut().rev());
    }

    #[test]
    fn set_callback() {
        let mut m = DriverManager::new();
        m.add_test_device(c"device,test_driver");
        let dev = m.add_test_device(c"device,another_driver");
        dev.compat_strs.push(c"another_driver,set_callback");
        m.drivers.push(&test_driver);
        m.drivers.push(&another_driver);
        m.probe_all();
        m.attach_all();
        DriverManager::detach_devices(&mut m.devices.iter_mut().rev());
    }

    // The following have to be in this module for very dumb reasons
    // https://github.com/rust-lang/rust/pull/52234
    driver!(irq_driver, c"irq_driver", IrqDriver, irq_driver_methods,
            INTERFACES {
                device_probe irq_driver_probe,
                device_attach irq_driver_attach,
                device_detach irq_driver_detach,
            }
    );
    impl CDriverFns for IrqDriver {
        fn get_probe(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            irq_driver::exported_fns::irq_driver_probe
        }
        fn get_attach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            irq_driver::exported_fns::irq_driver_attach
        }
        fn get_detach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            irq_driver::exported_fns::irq_driver_detach
        }
        fn softc_size(&self) -> usize {
            self.0.size
        }
        fn get_driver(&self) -> *mut driver_t {
            Self::DRIVER
        }
    }

    driver!(hook_driver, c"hook_driver", HookDriver, hook_driver_methods,
            INTERFACES {
                device_probe hook_driver_probe,
                device_attach hook_driver_attach,
                device_detach hook_driver_detach,
            }
    );
    impl CDriverFns for HookDriver {
        fn get_probe(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            hook_driver::exported_fns::hook_driver_probe
        }
        fn get_attach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            hook_driver::exported_fns::hook_driver_attach
        }
        fn get_detach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            hook_driver::exported_fns::hook_driver_detach
        }
        fn softc_size(&self) -> usize {
            self.0.size
        }
        fn get_driver(&self) -> *mut driver_t {
            Self::DRIVER
        }
    }

    driver!(intc_driver, c"intc_driver", IntcDriver, intc_driver_methods,
            INTERFACES {
                device_probe intc_driver_probe,
                device_attach intc_driver_attach,
                device_detach intc_driver_detach,
            }
    );
    impl CDriverFns for IntcDriver {
        fn get_probe(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            intc_driver::exported_fns::intc_driver_probe
        }
        fn get_attach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            intc_driver::exported_fns::intc_driver_attach
        }
        fn get_detach(&self) -> unsafe extern "C" fn(device_t) -> i32 {
            intc_driver::exported_fns::intc_driver_detach
        }
        fn softc_size(&self) -> usize {
            self.0.size
        }
        fn get_driver(&self) -> *mut driver_t {
            Self::DRIVER
        }
    }
}
