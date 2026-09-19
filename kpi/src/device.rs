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

use crate::bindings::{_device, device_state_t, device_t, driver_t, kobjop_desc};
use crate::boxed::Box;
use crate::driver::Driver;
use crate::ffi::{ArrayCString, Ptr, Ref, SoftcLayout, UninitPtr};
use crate::kobj::{AsCType, AsRustType, rust_driver_marker_desc};
use crate::prelude::*;
use crate::vec::Vec;
use crate::{ErrCode, define_interface};
use core::ffi::{CStr, c_int};
use core::marker::PhantomData;
use core::ptr;
use core::ptr::null_mut;

/// A device which is being managed by a rust driver.
///
/// Device has a lifetime `'a` attached representing the scope for which the device is known to be
/// attached. Functions that require the device is attached for longer than the Device instance may
/// be unsafe and require the caller to justify their usage by explaining how they know the device
/// won't get detached while it's in use.
///
/// Note that rust drivers may have some (or even all) methods written in C so the only thing that
/// that allows you to assume is that the memory layout of the softc matches SoftcLayout<TheSoftc>.
#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct Device<'a>(device_t, PhantomData<&'a ()>);

unsafe impl<'a> Sync for Device<'a> {}
unsafe impl<'a> Send for Device<'a> {}

impl<'a> Device<'a> {
    /// # Safety
    ///
    /// The caller must ensure that the device is managed by a rust driver and will be valid (i.e.
    /// not detached) for the lifetime of the returned value. If the caller does not explicitly
    /// annotate the lifetime and tie it to another reference the lifetime is inferred from context.
    pub unsafe fn new_unchecked(ptr: device_t) -> Self {
        Self(ptr, PhantomData)
    }

    pub fn as_ptr(&self) -> device_t {
        self.0
    }

    pub fn as_static(&self) -> Result<Device<'static>> {
        if !device_is_undetachable(*self) {
            return Err(EDOOFUS);
        }
        let ptr = self.0;
        Ok(Device(ptr, PhantomData))
    }
}

// Cannot be Copy/Clone
#[repr(C)]
#[derive(Debug)]
pub struct BusyDevice(device_t);

unsafe impl Sync for BusyDevice {}
unsafe impl Send for BusyDevice {}

impl BusyDevice {
    pub fn new(dev: Device) -> Self {
        let ptr = dev.0;
        unsafe { bindings::device_busy(ptr) };
        Self(ptr)
    }
}
impl Drop for BusyDevice {
    fn drop(&mut self) {
        unsafe { bindings::device_unbusy(self.0) }
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

// Used in device_probe
impl<'a> AsRustType<'a, Device<'a>> for device_t {
    fn as_rust_type(&'a self) -> Device<'a> {
        // TODO: Make AsRustType unsafe
        // SAFETY: Safety requirements delegated to caller
        unsafe { Device::new_unchecked(*self) }
    }
}

// Used in device_attach
impl<'a, T> AsRustType<'a, UninitPtr<'a, T>> for device_t {
    fn as_rust_type(&'a self) -> UninitPtr<'a, T> {
        let void_ptr = unsafe { bindings::device_get_softc(*self) };
        let sc_ptr = void_ptr.cast::<core::mem::MaybeUninit<SoftcLayout<T>>>();
        let sc_ref = unsafe { sc_ptr.as_mut().unwrap() };
        unsafe { UninitPtr::from_raw(sc_ref, *self) }
    }
}

// Allows turning a device_t argument appearing in kobj interfaces into a Pin<&T> to any type. It's
// the responsibility of a kobj trait authors to restrict the Ref to the softc's type or to one of
// its base classes.
impl<'a, T> AsRustType<'a, Ref<'a, T>> for device_t {
    fn as_rust_type(&'a self) -> Ref<'a, T> {
        let void_ptr = unsafe { bindings::device_get_softc(*self) };
        let sc_ptr = void_ptr.cast::<SoftcLayout<T>>();
        let sc_ref = unsafe { sc_ptr.as_ref().unwrap() };
        unsafe { Ref::from_raw(sc_ref) }
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
            let _: $crate::ffi::UninitPtr<_> = dev;
            let dev_ptr = dev.device().as_ptr();
            let mut init = false;
            dev.set_init_flag(&mut init);
        },
        with drop glue {
            // drop glue is only called if device_attach succeeded
            if !init {
                device_println!(dev_ptr, "Must call .init() on UninitPtr<Softc> in device_attach");
                return bindings::ENXIO;
            }
        };
    fn device_detach(dev: device_t) -> int,
        with desc device_detach_desc
        and typedef device_detach_t,
        with drop glue {
            use $crate::ffi::Ref;

            let (sc_ptr, count_ptr) = Ref::into_raw(dev);
            let last = unsafe { $crate::bindings::refcount_release(count_ptr) };
            if !last {
                let num_refs = unsafe { $crate::bindings::refcount_load(count_ptr) };
                panic!("tried to detach device with {} outstanding softc leases", num_refs);
            }
            unsafe { core::ptr::drop_in_place(sc_ptr) }
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
    /// All implementations must call [`init`][crate::ffi::UninitPtr::init] on the `uninit_sc`
    /// argument before this function returns to avoid a panic at runtime.
    fn device_attach(uninit_sc: UninitPtr<Self::Softc>) -> Result<()> {
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

    pub fn device_get_driver(dev: Device) -> *mut driver_t {
        unsafe { bindings::device_get_driver(dev.as_ptr()) }
    }

    pub fn device_matches_driver<D: DeviceIf>(dev: Device) -> bool {
        ptr::eq(device_get_driver(dev), <D as Driver>::DRIVER)
    }

    /// Checks if the device method table has rust_driver_marker_desc
    ///
    /// This function must be unsafe since it takes a device_t (i.e. struct _device *) and pointers
    /// can safely be fabricated from arbitrary values. Device is a device_t which is known to be
    /// managed by a rust driver so making that the argument type would have been kind of pointless.
    ///
    /// The rust marker is inserted at the front of the method table, but non-rust device won't have
    /// it so this takes O(NumMethods) in the worst-case.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the argument points to a struct _device for the duration of the
    /// function.
    pub unsafe fn device_has_rust_driver(dev_ptr: device_t) -> bool {
        assert!(!dev_ptr.is_null());
        let driver = unsafe { bindings::device_get_driver(dev_ptr) };
        let mut method_ptr = unsafe { (*driver).methods };
        while unsafe { !(*method_ptr).desc.is_null() } {
            let desc = unsafe { (*method_ptr).desc };
            if ptr::eq(desc, rust_driver_marker_desc.0.get()) {
                return true;
            }
            method_ptr = unsafe { method_ptr.add(1) };
        }
        false
    }

    pub fn device_is_undetachable(dev: Device) -> bool {
        let driver = device_get_driver(dev);
        let mut method_ptr = unsafe { (*driver).methods };
        while unsafe { !(*method_ptr).desc.is_null() } {
            let desc = unsafe { (*method_ptr).desc };
            // crate::tests is not part of kernel builds so if cfg!(test) won't work
            #[cfg(test)]
            let detach_addr = &raw mut crate::tests::device_detach_desc as *mut kobjop_desc;
            #[cfg(not(test))]
            let detach_addr = &raw const bindings::device_detach_desc;
            if ptr::eq(desc, detach_addr) {
                return false;
            }
            method_ptr = unsafe { method_ptr.add(1) };
        }
        true
    }

    /// Get a Ref to a device softc.
    ///
    /// Note the existence of the Device ensures that the device won't be detached for its
    /// associated lifetime so the returned Ref has a matching lifetime. To use the softc past that
    /// scope, turn it into a lease using Ref::lease.
    pub fn device_get_softc<'a, D: DeviceIf>(dev: Device<'a>) -> Ref<'a, D::Softc> {
        assert!(device_matches_driver::<D>(dev));
        let void_ptr = unsafe { bindings::device_get_softc(dev.as_ptr()) };
        let sc_ptr = unsafe { void_ptr.cast::<SoftcLayout<D::Softc>>().as_ref().unwrap() };
        unsafe { Ref::from_raw(sc_ptr) }
    }

    /// Get a Ptr to the softc for a device managed by a rust driver.
    ///
    /// Although this function takes the more generic device_t instead of a rust-specific Device,
    /// the device must be managed by a rust driver. If it's not an error is returned.
    ///
    /// The caller should also ensure the generic parameter `D: DeviceIf` for the driver matches
    /// what the device is actually using. Otherwise an error is returned.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the argument points to a valid struct _device and the call to
    /// this function does not race with device_detach. Since this function starts from a device_t
    /// it is not allowed to make many assumptions about the device or driver. This means there is
    /// no reliable way to ensure the device won't be detached while this function runs. Before
    /// returning this function gets a lease to the softc which may catch cases where the caller is
    /// racing with device_detach, but before that there is no guarantee for detachable devices.
    pub unsafe fn device_get_softc_unchecked<D: DeviceIf>(dev_ptr: device_t) -> Ptr<D::Softc> {
        assert!(!dev_ptr.is_null());
        // Required to let this function return a Ref
        // SAFETY: Safety requirements delegated to caller
        assert!(unsafe { device_has_rust_driver(dev_ptr) });
        // SAFETY: Lifetime safety requirements delegated to caller
        let dev = unsafe { Device::new_unchecked(dev_ptr) };

        let sc = device_get_softc::<D>(dev);

        // If device_detach runs after this point it will panic if this Ptr hasn't been dropped
        sc.lease()
    }

    /// Marks the device as busy returning a BusyDevice without an associated lifetime.
    ///
    /// This marks the device, its parent and all grandparent devices as busy preventing them from
    /// being detached. All devices stay busy until the BusyDevice is dropped or destroyed with
    /// device_unbusy. The lack of an associated lifetime in BusyDevice means that it can be stashed
    /// in a global or another softc is desired.
    pub fn device_busy(dev: Device) -> BusyDevice {
        BusyDevice::new(dev)
    }

    pub fn device_unbusy(busy_dev: BusyDevice) {
        drop(busy_dev)
    }

    pub fn device_claim_softc(dev: Device) {
        unsafe { bindings::device_claim_softc(dev.as_ptr()) }
    }

    pub fn device_get_state(dev: Device) -> device_state_t {
        unsafe { bindings::device_get_state(dev.as_ptr()) }
    }

    /// Returns a Device for the parent
    ///
    /// Note that the parent may actually have a longer lifetime since the return value's lifetime
    /// is tied to the argument.
    pub fn device_get_parent<'a>(dev: Device<'a>) -> Result<Device<'a>> {
        let dev_ptr = dev.as_ptr();
        let res = unsafe { bindings::device_get_parent(dev_ptr) };
        if res.is_null() {
            Err(ENULLPTR)
        } else {
            // SAFETY: This returns a Device for the parent with a lifetime tied to the child Device
            // which is fine since the parent must live at least as long as the child. The function
            // signature annotates lifetimes explicitly for clarity, but omitting them gives the
            // same result due to rust's lifetime elision rules.
            Ok(unsafe { Device::new_unchecked(res) })
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
        let children = unsafe {
            Vec::from_raw(devlistp.cast::<Device>(), devcountp.try_into().unwrap())
                .try_into_boxed_slice(M_NOWAIT)?
        };
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

    pub fn device_add_child<'a>(
        dev: Device<'a>,
        name: &'static CStr,
        unit: Option<u32>,
    ) -> Result<Device<'a>> {
        let unit = unit.unwrap_or(bindings::DEVICE_UNIT_ANY as u32);
        let child = unsafe { bindings::device_add_child(dev.as_ptr(), name.as_ptr(), unit as i32) };
        if child.is_null() {
            Err(ENULLPTR)
        } else {
            // TODO: Double check the output lifetime is valid
            Ok(unsafe { Device::new_unchecked(child) })
        }
    }
}

#[allow(dead_code, unused)]
#[cfg(test)]
mod tests {
    use super::*;
    use crate::define_driver;
    use crate::ffi::{Ref, UninitPtr};
    use crate::tests::{DriverManager, LoudDrop};
    use core::ptr::null_mut;
    use core::sync::atomic::{AtomicPtr, Ordering};
    use std::ffi::CStr;
    use std::vec::Vec;

    /* These are the drivers that will be used in tests */
    #[repr(C)]
    #[derive(Debug)]
    pub struct TestDriverSoftc {
        const_data: u32,
    }
    // This is only used to pipe a Device managed by one driver to another to ensure
    // device_get_softc type checking works as expected. It is unrealistic to do this via a static
    // like this, but this scenario does come up when a driver manages its children's device_t.
    static STASHED_DEVICE: AtomicPtr<_device> = AtomicPtr::new(null_mut());

    impl AnotherDriver {
        fn get_stashed_softc(dev: Device) {
            let test_driver_dev = STASHED_DEVICE.load(Ordering::Relaxed);
            let test_driver_sc =
                unsafe { device_get_softc_unchecked::<TestDriver>(test_driver_dev) };
            let another_driver_sc = device_get_softc::<Self>(dev);
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
        fn device_attach(uninit_sc: UninitPtr<Self::Softc>) -> Result<()> {
            let sc = uninit_sc.init(TestDriverSoftc {
                const_data: 0xdeadbeef,
            });
            if ofw_bus_is_compatible(sc.device(), c"another_driver,get_softc") {
                STASHED_DEVICE.store(sc.device().as_ptr(), Ordering::Relaxed);
            }
            if ofw_bus_is_compatible(sc.device(), c"test_driver,check_undetachable") {
                assert!(!device_is_undetachable(sc.device()));
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
        fn device_attach(uninit_sc: UninitPtr<Self::Softc>) -> Result<()> {
            let sc = uninit_sc.init(AnotherDriverSoftc { loud: LoudDrop });
            println!("attaching another driver");
            // Store a pointer owning a refcount to AnotherDriver's Softc in a TestDriverSoftc for
            // some appropriate device_t. This means that AnotherDriver::device_detach will drop a
            // refcount but will not be able to free the softc (as shown by the LoudDrop Drop impl).
            if ofw_bus_is_compatible(sc.device(), c"another_driver,get_softc") {
                Self::get_stashed_softc(sc.device());
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
    pub struct UndetachableDriverSoftc {}
    impl DeviceIf for UndetachableDriver {
        type Softc = UndetachableDriverSoftc;
        fn device_probe(dev: Device) -> Result<BusProbe> {
            if !ofw_bus_status_okay(dev) {
                return Err(ENXIO);
            }
            if !ofw_bus_is_compatible(dev, c"device,undetachable_driver") {
                return Err(ENXIO);
            }
            device_set_desc(dev, c"undetachable driver");
            Ok(BUS_PROBE_DEFAULT)
        }
        fn device_attach(uninit_sc: UninitPtr<UndetachableDriverSoftc>) -> Result<()> {
            let sc = uninit_sc.init(UndetachableDriverSoftc {});
            if ofw_bus_is_compatible(sc.device(), c"undetachable_driver,check_undetachable") {
                assert!(device_is_undetachable(sc.device()));
                // It isn't easy to test !has_rust_driver() w/o really complicating the
                // method_table! macro so only this is tested
                assert!(unsafe { device_has_rust_driver(sc.device().as_ptr()) });
            }
            Ok(())
        }
    }
    define_driver! {
        static undetachable_driver: UndetachableDriver = {
            name: c"undetachable_driver",
        }
        static undetachable_driver_methods = {
            device_probe: undetachable_driver_probe,
            device_attach: undetachable_driver_attach,
            // device_detach intentionally omitted
        }
    }

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

    #[test]
    fn no_detach() {
        let mut m = DriverManager::new();
        m.add_test_device(c"device,undetachable_driver")
            .compat_strs
            .push(c"undetachable_driver,check_undetachable");
        m.add_test_device(c"device,test_driver")
            .compat_strs
            .push(c"test_driver,check_undetachable");
        m.add_test_driver::<UndetachableDriver>();
        m.add_test_driver::<TestDriver>();
        m.probe_all();
        m.attach_all();
    }
}
