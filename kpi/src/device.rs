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
use crate::boxed::Box;
use crate::driver::Driver;
use crate::ffi::{ArrayCString, UninitRef};
use crate::kobj::{AsCType, AsRustType};
use crate::malloc::{Malloc, MallocFlags};
use crate::prelude::*;
use crate::vec::Vec;
use crate::{ErrCode, define_interface};
use core::ffi::{CStr, c_int, c_void};
use core::ops::Range;
use core::ptr::{drop_in_place, null_mut};
use core::sync::atomic::{AtomicPtr, Ordering};

/// Tracks address ranges owned by a softc, allowing KPIs to verify that a reference argument has a
/// lifetime that matches the softc.
///
/// Used by both [`Device`] (for `device_t`) and [`CDev`][crate::cdev::CDev] (for `cdev`).
#[derive(Debug)]
pub struct MemoryRegion {
    // The softc address range. While this could be computed on demand, it's cached to give the rust
    // compiler more opportunities to optimize out bounds checks that can be statically-determined to
    // always pass.
    sc_range: Range<usize>,
    // The head of a linked list of address ranges for each allocation owned by the softc. While
    // the softc may contain allocations not in this list (e.g. by using Box::new instead of
    // Box::new_in_dev) KPIs that require an argument live as long as the softc (e.g.
    // intr_isrc_register) will check this list if the argument is not directly embedded in the
    // softc. If the argument is not found in this list they will panic because at that point there
    // is no way to tell if the argument may be freed before detach.
    allocations_head: AtomicPtr<AllocationRange>,
}

struct AllocationRange {
    range: Range<usize>,
    next: *mut AllocationRange,
    /// Drops the data in place and deallocates the memory.
    drop_fn: unsafe fn(*mut u8, usize),
    /// Number of elements (1 for sized types, slice length for boxed slices).
    elem_count: usize,
}

impl Default for MemoryRegion {
    fn default() -> Self {
        MemoryRegion {
            sc_range: 0..0,
            allocations_head: AtomicPtr::new(null_mut()),
        }
    }
}

impl MemoryManager for MemoryRegion {
    fn region(&self) -> &MemoryRegion {
        self
    }
}

impl MemoryRegion {
    /// Creates a MemoryRegion that considers all addresses in bounds. For testing only.
    #[cfg(test)]
    pub fn test_unchecked() -> Self {
        MemoryRegion {
            sc_range: 0..usize::MAX,
            allocations_head: AtomicPtr::new(null_mut()),
        }
    }

    pub fn new(sc_start: usize, sc_end: usize) -> Self {
        MemoryRegion {
            sc_range: sc_start..sc_end,
            allocations_head: AtomicPtr::new(null_mut()),
        }
    }

    // Not synchronized with add_range. If a driver races add_range against this (e.g. allocating
    // during detach), the new node and its allocation will leak but no use-after-free occurs since
    // the list is only traversed once here and never again.
    pub unsafe fn free_allocation_list(&self) {
        let mut current = self.allocations_head.load(Ordering::Acquire);
        while !current.is_null() {
            let node: Box<AllocationRange> = unsafe { Box::from_raw(current) };
            current = node.next;
            unsafe { (node.drop_fn)(node.range.start as *mut u8, node.elem_count) };
            drop(node);
        }
    }

    pub fn in_bounds<T>(&self, t: &T) -> bool {
        let t_ptr = t as *const T;
        let t_start = t_ptr.addr();
        let t_end = t_start + size_of::<T>();
        if t_start >= self.sc_range.start && t_end <= self.sc_range.end {
            return true;
        }
        let mut current = self.allocations_head.load(Ordering::Acquire);
        while !current.is_null() {
            let node = unsafe { &*current };
            if t_start >= node.range.start && t_end <= node.range.end {
                return true;
            }
            current = node.next;
        }
        false
    }

    pub fn add_box_range<T, M: Malloc>(&self, b: &Box<T, M>, flags: MallocFlags) {
        let b_start = b.0.as_ptr().addr();
        let b_end = b_start + size_of::<T>();
        unsafe fn drop_box<T, M: Malloc>(ptr: *mut u8, _len: usize) {
            unsafe {
                drop_in_place(ptr.cast::<T>());
                free(ptr.cast::<c_void>(), M::malloc_type());
            }
        }
        self.add_range(b_start..b_end, drop_box::<T, M>, 1, flags);
    }

    pub fn add_boxed_slice_range<T, M: Malloc>(&self, b: &Box<[T], M>, flags: MallocFlags) {
        let len = b.len();
        let b_start = b.0.as_ptr().addr();
        let b_end = b_start + size_of::<T>() * len;
        unsafe fn drop_boxed_slice<T, M: Malloc>(ptr: *mut u8, len: usize) {
            unsafe {
                let slice = core::ptr::slice_from_raw_parts_mut(ptr.cast::<T>(), len);
                drop_in_place(slice);
                free(ptr.cast::<c_void>(), M::malloc_type());
            }
        }
        self.add_range(b_start..b_end, drop_boxed_slice::<T, M>, len, flags);
    }

    fn add_range(
        &self,
        range: Range<usize>,
        drop_fn: unsafe fn(*mut u8, usize),
        elem_count: usize,
        flags: MallocFlags,
    ) {
        let node: Box<AllocationRange> = Box::new(
            AllocationRange {
                range,
                next: null_mut(),
                drop_fn,
                elem_count,
            },
            flags,
        );
        let node_ptr = Box::into_raw(node);
        loop {
            let old_head = self.allocations_head.load(Ordering::Acquire);
            unsafe {
                (*node_ptr).next = old_head;
            }
            let res = self.allocations_head.compare_exchange_weak(
                old_head,
                node_ptr,
                Ordering::Release,
                Ordering::Relaxed,
            );
            if res.is_ok() {
                return;
            } else {
                continue;
            }
        }
    }
}

/// Trait for types that own a [`MemoryRegion`], allowing KPIs and allocation functions to work
/// with both [`Device`] and [`CDev`][crate::cdev::CDev].
#[diagnostic::on_unimplemented(message = "You probably meant to pass in `&sc.dev` here")]
pub trait MemoryManager {
    fn region(&self) -> &MemoryRegion;
}

/// A pointer to a device which is aware of the address ranges owned by the device driver's softc.
///
/// This explicitly does not impl Copy or Clone because all compare-and-swap operations for a given
/// device must operate on the same address.
#[derive(Debug)]
pub struct Device {
    ptr: device_t,
    region: MemoryRegion,
}

impl Device {
    pub fn as_ptr(&self) -> device_t {
        self.ptr
    }
}

impl Drop for Device {
    fn drop(&mut self) {
        unsafe { self.region.free_allocation_list() }
    }
}

impl MemoryManager for Device {
    fn region(&self) -> &MemoryRegion {
        &self.region
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

// Allows turning device_t arguments appearing in kobj interfaces into a Ref to any type. It's the
// responsibility of a kobj trait authors to restrict the Ref to the softc's type or to one of its
// base classes.
impl<'a, T> AsRustType<&'a T> for device_t {
    fn as_rust_type(self) -> &'a T {
        let void_ptr = unsafe { bindings::device_get_softc(self) };
        let sc_ptr = void_ptr.cast::<T>();
        unsafe { sc_ptr.as_ref().unwrap() }
    }
}

// Should only be called once per device_t during init since it assumes there are no allocations.
impl AsRustType<Device> for device_t {
    fn as_rust_type(self) -> Device {
        let sc = unsafe { bindings::device_get_softc(self) };
        let driver = unsafe { bindings::device_get_driver(self) };
        // Equals core::mem::size_of::<SoftcTy>() which includes padding
        let sc_size = unsafe { (*driver).size };
        let sc_start = sc.addr();
        let sc_end = sc_start + sc_size;
        Device {
            ptr: self,
            region: MemoryRegion::new(sc_start, sc_end),
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! device_attach {
    (get_typedef) => {
        device_attach_t
    };
    (get_desc) => {
        device_attach_desc
    };
    ($driver_ty:ident $driver_sym:ident $impl_fn_name:ident) => {
        $crate::define_c_function! {
            $driver_ty $driver_sym $impl_fn_name in DeviceIf as
            fn device_attach(dev: device_t) -> int;
            with init glue {
                use $crate::bindings;
                use $crate::ffi::UninitRef;
                use $crate::kobj::KobjLayout;
                use $crate::device::Device;

                let dev: Device = dev;
                let dev_ptr = dev.as_ptr();
                let mut sc_init = false;
                let void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
                let sc_ptr = void_ptr.cast::<<$driver_ty as KobjLayout>::Layout>();

                let uninit_sc = unsafe { UninitRef::from_raw(sc_ptr, &mut sc_init) };
            }
            with drop glue {
                // drop glue is only called if device_attach succeeded
                if !sc_init {
                    device_println!(dev_ptr, "Must call .init() on UninitRef<Softc> in device_attach");
                    return bindings::ENXIO;
                }
            }
            with prefix args { uninit_sc }
        }
    };
}

define_interface! {
    in DeviceIf
    fn device_probe(dev: device_t) -> int,
        with desc device_probe_desc
        and typedef device_probe_t;
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
    fn device_probe(dev: device_t) -> Result<BusProbe> {
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
    fn device_detach(sc: &Self::Softc) -> Result<()> {
        unimplemented!()
    }
    fn device_shutdown(sc: &Self::Softc) -> Result<()> {
        unimplemented!()
    }
    fn device_suspend(sc: &Self::Softc) -> Result<()> {
        unimplemented!()
    }
    fn device_resume(sc: &Self::Softc) -> Result<()> {
        unimplemented!()
    }
    fn device_quiesce(sc: &Self::Softc) -> Result<()> {
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

    pub fn device_get_softc<D: DeviceIf>(dev: &Device) -> &D::Softc {
        let dev_ptr = dev.as_ptr();
        assert_eq!(device_get_driver(dev_ptr), <D as Driver>::DRIVER);
        let void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        let sc_ptr = void_ptr.cast::<D::Softc>();
        unsafe { sc_ptr.as_ref().unwrap() }
    }

    pub fn device_claim_softc(dev: &Device) {
        unsafe { bindings::device_claim_softc(dev.as_ptr()) }
    }
    pub fn device_get_state(dev: &Device) -> device_state_t {
        unsafe { bindings::device_get_state(dev.as_ptr()) }
    }

    pub fn device_get_parent(dev: &Device) -> Result<device_t> {
        let res = unsafe { bindings::device_get_parent(dev.as_ptr()) };
        if res.as_ptr().is_null() {
            Err(ENULLPTR)
        } else {
            Ok(res)
        }
    }

    pub fn device_get_children(dev: &Device) -> Result<Box<[device_t], M_TEMP>> {
        let mut devlistp = null_mut();
        let mut devcountp = 0;
        let res = unsafe {
            bindings::device_get_children(dev.as_ptr(), &raw mut devlistp, &raw mut devcountp)
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        let children =
            unsafe { Vec::from_raw(devlistp, devcountp.try_into().unwrap()).into_boxed_slice() };
        Ok(children)
    }

    pub fn device_probe_and_attach(dev: device_t) -> Result<()> {
        let res = unsafe { bindings::device_probe_and_attach(dev) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn device_set_desc(dev: device_t, desc: &'static CStr) {
        let desc_ptr = desc.as_ptr();
        unsafe { bindings::device_set_desc(dev, desc_ptr) }
    }

    pub fn device_get_desc(dev: &Device) -> ArrayCString {
        let name_ptr = unsafe { bindings::device_get_desc(dev.as_ptr()) };
        assert!(!name_ptr.is_null());
        let name = unsafe { CStr::from_ptr(name_ptr) };
        ArrayCString::new(name)
    }

    /// Returns a copy of the device name
    pub fn device_get_name(dev: &Device) -> ArrayCString {
        let name_ptr = unsafe { bindings::device_get_name(dev.as_ptr()) };
        assert!(!name_ptr.is_null());
        let name = unsafe { CStr::from_ptr(name_ptr) };
        ArrayCString::new(name)
    }

    /// Returns a copy of the device name and unit number
    pub fn device_get_nameunit(dev: &Device) -> ArrayCString {
        let name_ptr = unsafe { bindings::device_get_nameunit(dev.as_ptr()) };
        assert!(!name_ptr.is_null());
        let name = unsafe { CStr::from_ptr(name_ptr) };
        ArrayCString::new(name)
    }

    pub fn device_get_driver(dev: device_t) -> *mut driver_t {
        unsafe { bindings::device_get_driver(dev) }
    }

    pub fn device_add_child(
        dev: &Device,
        name: &'static CStr,
        unit: Option<u32>,
    ) -> Result<device_t> {
        let unit = unit.unwrap_or(bindings::DEVICE_UNIT_ANY as u32);
        let child = unsafe { bindings::device_add_child(dev.as_ptr(), name.as_ptr(), unit as i32) };
        if child.as_ptr().is_null() {
            Err(ENULLPTR)
        } else {
            Ok(child)
        }
    }
}

#[allow(dead_code, unused)]
#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver;
    use crate::ffi::UninitRef;
    use crate::tests::{DriverManager, LoudDrop};
    use core::ptr::null_mut;
    use core::sync::atomic::{AtomicPtr, Ordering};
    use std::ffi::{CStr, c_void};
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
    static STASHED_DEVICE: AtomicPtr<Device> = AtomicPtr::new(null_mut());

    impl AnotherDriver {
        fn get_stashed_softc(dev: &Device) {
            let test_driver_dev = unsafe { &*STASHED_DEVICE.load(Ordering::Relaxed) };
            let test_driver_sc = device_get_softc::<TestDriver>(test_driver_dev);
            let another_driver_sc = device_get_softc::<Self>(dev);
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
        fn device_attach(uninit_sc: UninitRef<Self::Softc>, dev: Device) -> Result<()> {
            let sc = uninit_sc.init(TestDriverSoftc {
                dev,
                const_data: 0xdeadbeef,
            });
            let dev_ptr = sc.dev.as_ptr();
            if ofw_bus_is_compatible(dev_ptr, c"another_driver,get_softc") {
                STASHED_DEVICE.store(&sc.dev as *const Device as *mut Device, Ordering::Relaxed);
            }
            println!("{:x?}", sc);
            Ok(())
        }
        fn device_detach(sc: &Self::Softc) -> Result<()> {
            assert!(sc.const_data == 0xdeadbeef);
            Ok(())
        }
    }
    driver!(test_driver, c"test_driver", TestDriver,
            test_driver_methods = {
                device_probe test_driver_probe,
                device_attach test_driver_attach,
                device_detach test_driver_detach,
            },
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
        fn device_attach(uninit_sc: UninitRef<Self::Softc>, dev: Device) -> Result<()> {
            let sc = uninit_sc.init(AnotherDriverSoftc {
                dev,
                loud: LoudDrop,
            });
            println!("attaching another driver");
            let dev_ptr = sc.dev.as_ptr();
            // Store a pointer owning a refcount to AnotherDriver's Softc in a TestDriverSoftc for
            // some appropriate device_t. This means that AnotherDriver::device_detach will drop a
            // refcount but will not be able to free the softc (as shown by the LoudDrop Drop impl).
            if ofw_bus_is_compatible(dev_ptr, c"another_driver,get_softc") {
                Self::get_stashed_softc(&sc.dev);
            }
            Ok(())
        }
        fn device_detach(sc: &Self::Softc) -> Result<()> {
            Ok(())
        }
    }
    driver!(another_driver, c"another_driver", AnotherDriver,
            another_driver_methods = {
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
