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
use crate::driver::DriverIf;
use crate::ffi::{Ptr, RefCountData, RefCounted, Uninit};
use crate::prelude::*;
use core::ffi::{CStr, c_int};
use core::fmt;
use core::fmt::{Debug, Formatter};

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

/// Calls [`device_get_softc()`] using `Self` as the generic type parameter for the driver.
///
/// This is shorthand for calling `device_get_softc::<Self>(dev)` since that function is mainly
/// expected to be called devices in its own methods. `Self` may not be the correct generic type
/// parameter in cases where one driver's methods the softc of another. In those cases
/// [`device_get_softc()`] should be called directly.
#[macro_export]
macro_rules! device_get_softc {
    ($dev:expr) => {
        $crate::device::device_get_softc::<Self>($dev)
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! device_probe {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            device_probe(dev: device_t) -> int;
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! device_attach {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            device_attach(dev: device_t) -> int;
            with init glue {
                let sc_as_void_ptr = unsafe { bindings::device_get_softc(dev) };
                let sc_ptr = sc_as_void_ptr.cast::<RefCounted<<$driver_ty as DeviceIf>::Softc>>();
                let metadata_ptr = RefCounted::metadata_ptr(sc_ptr);

                let count_ptr = RefCountData::count_ptr(metadata_ptr);
                unsafe { bindings::refcount_init(count_ptr, 1) };
                let drop_fn_ptr = RefCountData::drop_fn_ptr(metadata_ptr);
                unsafe { core::ptr::write(drop_fn_ptr, <$driver_ty as $crate::driver::DriverIf>::DROP_FN) };

                // Claim the softc to opt it into refcounting
                $crate::device::device_claim_softc(dev);

                let uninit_sc = unsafe { $crate::ffi::Uninit::from_raw(sc_ptr) };
            }
            with drop glue {
                assert!(uninit_sc.is_init());
            }
            with prefix args { uninit_sc }
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
            with init glue {
                let sc_as_void_ptr = unsafe { bindings::device_get_softc(dev) };
                let sc_ptr = sc_as_void_ptr.cast::<RefCounted<<$driver_ty as DeviceIf>::Softc>>();
                let _sc = unsafe { sc_ptr.as_ref().unwrap() };
            }
            with drop glue {
                let metadata_ptr = RefCounted::metadata_ptr(sc_ptr);
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
            }
            with prefix args { _sc }
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

/// The device interface defined by device_if.m
///
/// This is used to match devices to drivers during autoconfiguration and allow drivers to handle
/// system events such as suspend, resume and shutdown.
///
/// All drivers defined by [`driver!`][crate::driver] must implement this trait to define at least
/// their softc, [`device_probe`][DeviceIf::device_probe] and
/// [`device_attach`][DeviceIf::device_attach] methods. Any methods left unimplemented will just
/// panic if called.
///
/// The softc type associated with each [`DeviceIf`] impl has the bounds `'static + Sync`. `'static`
/// means any references in the softc must always point to global data. The softc will generally own
/// or share the data it contains rather than use references so this is typically not a problem. The
/// `Sync` bound roughly means that all fields in the softc must be safe to share between multiple
/// threads.
///
/// In contrast to the C equivalent, the softc is passed as an argument to the methods in which the
/// driver is allowed to access it (all except [`device_probe`][DeviceIf::device_probe]). In
/// [`device_attach`][DeviceIf::device_attach] it must be initialized by calling the argument's
/// [`init`][crate::ffi::Uninit::init] method. That returns a reference to the softc (`&RefCounted<Softc>`)
/// which can be used for the duration of the function. If a softc reference is not sufficient (e.g.
/// it needs to be passed to a callback that may be called after the function returns), the
/// [`grab_ref()`][crate::ffi::RefCounted::grab_ref] method can be used to get a pointer to the
/// softc and increase its refcount. Grabbing a refcount ensures that the softc will live as long
/// as necessary. Also note that all kobj interface methods collectively own one refcount to the
/// softc. That is, methods like those in this trait and [`PicIf`] can access the softc without
/// the cost of grabbing and releasing a refcount. The [`device_attach`][DeviceIf::device_attach]
/// and [`device_detach`][DeviceIf::device_detach] methods define when this shared refcount is
/// acquired and released, respectively. After [`device_detach`][DeviceIf::device_detach], this
/// crate's glue code releases a reference which may or may not free all memory owned by the softc
/// depending on whether there are other refcounts which haven't been released.
///
/// For cases where it may be easier to pipe a `device_t` than a softc pointer through an interface
/// [`device_get_softc`][crate::device::device_get_softc()] is also provided to get a softc pointer
/// from the `device_t`. This always grabs a refcount so it should be avoided whenever possible.
#[diagnostic::on_unimplemented(message = "
Implement the device interface trait and define the softc as follows

```
pub struct MyDriverSoftc {{ /* softc fields go here */ }}

impl DeviceIf for {Self} {{
    type Softc = MyDriverSoftc;

    fn device_probe(dev: device_t) -> Result<BusProbe> {{
        /* device_probe impl goes here */
    }}

    fn device_attach(sc: &mut Uninit<MyDriverSoftc>, dev: device_t) -> Result<()> {{
        /* device_attach impl goes here */
    }}
}}
```
")]
#[allow(unused_variables)]
pub trait DeviceIf<State = ()>: DriverIf {
    /// The softc associated with the driver.
    ///
    /// If the driver is a subclass of another, then this must be an appropriate
    /// [`SubClass`][crate::ffi::SubClass].
    type Softc: 'static + Sync;

    /// Used to probe whether the given device is supported by the driver.
    fn device_probe(dev: device_t) -> Result<BusProbe>;

    /// Used to initialize a driver.
    ///
    /// All implementations must call [`init`][crate::ffi::Uninit::init] on the `uninit_sc` argument
    /// before this function returns to avoid a panic at runtime.
    fn device_attach(uninit_sc: &mut Uninit<Self::Softc>, dev: device_t) -> Result<()>;

    /// Used to remove a driver.
    ///
    /// If all explicitly grabbed refcounts to the softc have been released, then the softc's memory
    /// will be freed after this function returns. This includes both the region of memory allocated
    /// for the softc struct as well as any memory accessible to it via layers of indirection. For
    /// example, if a softc struct includes a `Box<T>` field (i.e. a pointer to the heap with
    /// ownership of a `T`) the `T` in the heap will also be freed. This applies recursively through
    /// any number of layers of indirection.
    fn device_detach(sc: &RefCounted<Self::Softc>, dev: device_t) -> Result<()> {
        unimplemented!()
    }
    fn device_shutdown(sc: &RefCounted<Self::Softc>, dev: device_t) -> Result<()> {
        unimplemented!()
    }
    fn device_suspend(sc: &RefCounted<Self::Softc>, dev: device_t) -> Result<()> {
        unimplemented!()
    }
    fn device_resume(sc: &RefCounted<Self::Softc>, dev: device_t) -> Result<()> {
        unimplemented!()
    }
    fn device_quiesce(sc: &RefCounted<Self::Softc>, dev: device_t) -> Result<()> {
        unimplemented!()
    }
    fn device_register(sc: &RefCounted<Self::Softc>, dev: device_t) -> Result<&'static State> {
        unimplemented!()
    }
}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;

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

    /// Get a pointer to a device's softc.
    ///
    /// This function grabs a refcount to a device's softc and returns a [`Ptr`][crate::ffi::Ptr] to
    /// it. The caller must ensure that the generic type parameter `D` is the same type as the
    /// driver which was attached to the device. The caller must also ensure that the device has
    /// been attached and the softc refcount has not dropped to zero. Failure to meet these
    /// requirements will cause a panic at runtime.
    ///
    /// Note since this function is a much heavier operation than device_get_softc in C, the
    /// preferred way to access a softc is through the argument passed to kobj interface methods and
    /// piping those pointers where necessary. This method is mainly provided as a last resort for
    /// when there is no good way to pipe softc pointers to a given context.
    pub fn device_get_softc<D: DeviceIf>(dev: device_t) -> Ptr<D::Softc> {
        let state = device_get_state(dev);
        if state != bindings::DS_ATTACHED {
            panic!("device_get_softc can only be called after device_attach");
        }
        let driver = device_get_driver(dev);
        if driver != D::DRIVER {
            panic!("device_t passed to device_get_softc has a different softc type");
        }

        let sc_as_void_ptr = unsafe { bindings::device_get_softc(dev) };
        let sc_ptr = sc_as_void_ptr.cast::<RefCounted<D::Softc>>();

        let metadata_ptr = RefCounted::metadata_ptr(sc_ptr);
        let count_ptr = RefCountData::count_ptr(metadata_ptr);
        let success = unsafe { bindings::refcount_acquire_if_not_zero(count_ptr) };
        if !success {
            panic!("device_get_softc called after softc was dropped");
        }
        unsafe { Ptr::from_raw(sc_ptr) }
    }
}

#[allow(dead_code, unused)]
#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::driver;
    use crate::sync::Mutable;
    use crate::tests::{DriverManager, LoudDrop};
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
    //static STASHED_DEVICE: Mutable<device_t> = Mutable::new(device_t::null());
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
        fn set_callback_arg(arg: &RefCounted<AnotherDriverSoftc>) {
            //let sc = device_get_softc!(*STASHED_DEVICE.get_mut());
            //*sc.another_sc.get_mut() = Some(arg);
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
        fn device_attach(uninit_sc: &mut Uninit<Self::Softc>, dev: device_t) -> Result<()> {
            let sc = uninit_sc.init(TestDriverSoftc {
                dev,
                const_data: 0xdeadbeef,
                another_sc: Mutable::new(None),
            });
            //*STASHED_DEVICE.get_mut() = dev;
            println!("{:x?}", sc);
            Ok(())
        }
        fn device_detach(sc: &RefCounted<Self::Softc>, dev: device_t) -> Result<()> {
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
        fn device_attach(uninit_sc: &mut Uninit<Self::Softc>, dev: device_t) -> Result<()> {
            let sc = uninit_sc.init(AnotherDriverSoftc {
                dev,
                loud: LoudDrop,
            });
            println!("attaching another driver");
            // Store a pointer owning a refcount to AnotherDriver's Softc in a TestDriverSoftc for
            // some appropriate device_t. This means that AnotherDriver::device_detach will drop a
            // refcount but will not be able to free the softc (as shown by the LoudDrop Drop impl).
            if ofw_bus_is_compatible(dev, c"another_driver,set_callback") {
                TestDriver::set_callback_arg(sc);
            }
            Ok(())
        }
        fn device_detach(sc: &RefCounted<Self::Softc>, dev: device_t) -> Result<()> {
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
    fn set_callback() {
        let mut m = DriverManager::new();
        m.add_test_device(c"device,test_driver");
        let dev = m.add_test_device(c"device,another_driver");
        dev.compat_strs.push(c"another_driver,set_callback");
        m.add_test_driver::<TestDriver>();
        m.add_test_driver::<AnotherDriver>();
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

    driver!(hook_driver, c"hook_driver", HookDriver, hook_driver_methods,
            INTERFACES {
                device_probe hook_driver_probe,
                device_attach hook_driver_attach,
                device_detach hook_driver_detach,
            }
    );

    driver!(intc_driver, c"intc_driver", IntcDriver, intc_driver_methods,
            INTERFACES {
                device_probe intc_driver_probe,
                device_attach intc_driver_attach,
                device_detach intc_driver_detach,
            }
    );
}
