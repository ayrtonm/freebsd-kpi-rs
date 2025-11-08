/*-
 * spdx-license-identifier: bsd-2-clause
 *
 * copyright (c) 2025 ayrton muñoz
 * all rights reserved.
 *
 * redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * this software is provided by the author and contributors ``as is'' and
 * any express or implied warranties, including, but not limited to, the
 * implied warranties of merchantability and fitness for a particular purpose
 * are disclaimed.  in no event shall the author or contributors be liable
 * for any direct, indirect, incidental, special, exemplary, or consequential
 * damages (including, but not limited to, procurement of substitute goods
 * or services; loss of use, data, or profits; or business interruption)
 * however caused and on any theory of liability, whether in contract, strict
 * liability, or tort (including negligence or otherwise) arising in any way
 * out of the use of this software, even if advised of the possibility of
 * such damage.
 */

#![allow(dead_code)]

use crate::bindings;
use crate::bindings::{
    device_attach_desc, device_attach_t, device_detach_desc, device_detach_t, device_probe_desc,
    device_probe_t, device_state_t, device_t, driver_filter_t, driver_intr_t, driver_t,
    intr_config_hook, intr_irq_filter_t, kobjop_desc, resource, u_int,
};
use crate::driver::DriverIf;
use core::mem::transmute;
use std::ffi::{CStr, CString, c_void};
use std::ptr::{null, null_mut};
use std::sync::atomic::{AtomicPtr, AtomicU32, Ordering};
use std::vec::Vec;
use std::{println, vec};

/*
 * This is part of the test harness, which effectively does what the kernel should do from the
 * perspective of the KPI crate
 */
pub struct TestDevice {
    pub dev: device_t, /* This is just the address of the TestDevice once it's inserted in the DriverManager's Vec */
    pub id: usize,
    pub is_ok: bool,
    pub assigned_driver: Option<*mut driver_t>,
    pub state: device_state_t,
    pub softc: *mut c_void,
    pub compat_strs: Vec<&'static CStr>,
    filter: Option<(driver_filter_t, *mut c_void)>,
    handler: Option<(driver_intr_t, *mut c_void)>,
    pic_root: Option<(intr_irq_filter_t, *mut c_void)>,
    desc: Option<CString>,
}

// We only support testing a single config hook since config_intrhook_establish's function signature
// lacks a way to access a device_t or driver_t
static CONFIG_HOOK: AtomicPtr<intr_config_hook> = AtomicPtr::new(null_mut());

impl driver_t {
    fn get_softc_size(driver: *mut Self) -> usize {
        unsafe { (*driver).size }
    }
    fn get_interface_fn(
        driver: *mut Self,
        interface_fn_desc: *mut kobjop_desc,
    ) -> Option<unsafe extern "C" fn()> {
        let mut res = None;

        let class = unsafe { &*driver };
        let mut method_ptr = class.methods;

        while unsafe { (*method_ptr).func.is_some() } {
            let desc = unsafe { (*method_ptr).desc };
            if desc == interface_fn_desc {
                res = unsafe { (*method_ptr).func };
                break;
            }
            method_ptr = unsafe { method_ptr.add(1) };
        }

        res
    }
    fn get_probe_fn(driver: *mut Self) -> device_probe_t {
        let func = Self::get_interface_fn(driver, &raw mut device_probe_desc);
        unsafe { transmute(func) }
    }
    fn get_attach_fn(driver: *mut Self) -> device_attach_t {
        let func = Self::get_interface_fn(driver, &raw mut device_attach_desc);
        unsafe { transmute(func) }
    }
    fn get_detach_fn(driver: *mut Self) -> device_detach_t {
        let func = Self::get_interface_fn(driver, &raw mut device_detach_desc);
        unsafe { transmute(func) }
    }
}
pub struct DriverManager {
    pub devices: Vec<TestDevice>,
    pub drivers: Vec<*mut driver_t>,
    dev_counter: usize,
}

impl DriverManager {
    pub fn new() -> Self {
        Self {
            devices: Vec::new(),
            drivers: Vec::new(),
            dev_counter: 0,
        }
    }

    pub fn add_test_driver<D: DriverIf>(&mut self) {
        self.drivers.push(D::DRIVER);
    }

    pub fn add_test_device(&mut self, driver_compat: &'static CStr) -> &mut TestDevice {
        self.dev_counter += 1;
        let id = self.dev_counter;
        self.devices.push(TestDevice {
            dev: device_t::null(),
            id,
            is_ok: true,
            assigned_driver: None,
            state: bindings::DS_NOTPRESENT,
            softc: null_mut(),
            compat_strs: vec![driver_compat],
            filter: None,
            handler: None,
            pic_root: None,
            desc: None,
        });
        let device = self.devices.last_mut().unwrap();
        device.dev = device_t::new(device as *mut TestDevice as _);
        device
    }

    pub fn probe_attach_detach(&mut self) {
        self.probe_all();
        self.attach_all();
        self.detach_all();
    }

    pub fn probe_all(&mut self) {
        assert!(!self.drivers.is_empty());
        for dev in &mut self.devices {
            for driver in &self.drivers {
                let probe_fn = driver_t::get_probe_fn(*driver).unwrap();
                let res = unsafe { probe_fn(dev.dev) };
                if res == bindings::BUS_PROBE_DEFAULT {
                    dev.assigned_driver = Some(*driver);
                    dev.state = bindings::DS_ALIVE;
                    let mut softc: Vec<u8> = Vec::new();
                    for _ in 0..driver_t::get_softc_size(*driver) {
                        softc.push(0u8);
                    }
                    dev.softc = softc.leak().as_ptr().cast_mut().cast::<c_void>();
                    break;
                }
            }
        }
    }

    pub fn attach_all(&mut self) {
        for dev in &mut self.devices {
            if let Some(driver) = dev.assigned_driver {
                dev.state = bindings::DS_ATTACHING;
                let attach_fn = driver_t::get_attach_fn(driver).unwrap();
                let res = unsafe { attach_fn(dev.dev) };
                if res != 0 {
                    dev.is_ok = false;
                } else {
                    dev.state = bindings::DS_ATTACHED;
                }
            }
        }
    }

    pub fn detach_all(&mut self) {
        Self::detach_devices(self.devices.iter_mut());
    }

    pub fn detach_devices<'a>(devs: impl Iterator<Item = &'a mut TestDevice>) {
        for dev in devs {
            if let Some(driver) = dev.assigned_driver {
                if dev.is_ok {
                    let detach_fn = driver_t::get_detach_fn(driver).unwrap();
                    let res = unsafe { detach_fn(dev.dev) };
                    if res != 0 {
                        dev.is_ok = false;
                    } else {
                        dev.state = bindings::DS_NOTPRESENT;
                    }
                }
            }
        }
    }

    pub fn trigger_irq(&self, dev: device_t) {
        for candidate in &self.devices {
            if candidate.dev.as_ptr() != dev.as_ptr() {
                continue;
            }
            let (func, arg) = candidate.filter.expect("haven't called `bus_setup_intr`");
            let mut invoke_handler = true;
            if let Some(func) = func {
                let res = unsafe { func(arg) };
                if res == bindings::FILTER_HANDLED {
                    invoke_handler = false;
                }
            }
            if invoke_handler {
                let (handler, arg) = candidate
                    .handler
                    .expect("`bus_setup_intr` didn't set up handler");
                if let Some(handler) = handler {
                    unsafe { handler(arg) }
                }
            }
        }
    }

    pub fn trigger_pic_root(&self, dev: device_t) {
        for candidate in &self.devices {
            if candidate.dev.as_ptr() != dev.as_ptr() {
                continue;
            }
            let (func, arg) = candidate.pic_root.unwrap();
            let _res = unsafe { (func.unwrap())(arg) };
        }
    }

    pub fn trigger_config_hooks(&self) {
        let mut hook_ptr = CONFIG_HOOK.load(Ordering::Relaxed);
        while !hook_ptr.is_null() {
            let hook = unsafe { std::ptr::read(hook_ptr) };
            let func = hook.ich_func.unwrap();
            let arg = hook.ich_arg;
            unsafe {
                func(arg);
            }
            hook_ptr = CONFIG_HOOK.load(Ordering::Relaxed);
        }
    }
}

#[derive(Debug)]
pub struct LoudDrop;
impl Drop for LoudDrop {
    fn drop(&mut self) {
        println!("dropping an instance of LoudDrop at {:p}", self);
    }
}

#[derive(Debug)]
pub struct NoDrop;
impl Drop for NoDrop {
    fn drop(&mut self) {
        panic!("dropping an instance of NoDrop at {:p}", self);
    }
}

fn test_dev<'a>(dev: device_t) -> &'a TestDevice {
    unsafe { dev.as_ptr().cast::<TestDevice>().as_ref().unwrap() }
}
fn test_dev_mut<'a>(dev: device_t) -> &'a mut TestDevice {
    unsafe { dev.as_ptr().cast::<TestDevice>().as_mut().unwrap() }
}

mod unmangled_fns {
    use super::*;
    use crate::bindings::malloc_type;
    use core::ffi::{c_int, c_void};
    use core::ptr::null_mut;

    unsafe extern "C" {
        fn aligned_alloc(align: usize, size: usize) -> *mut c_void;
    }

    #[unsafe(no_mangle)]
    extern "C" fn malloc_aligned(
        size: usize,
        align: usize,
        _ty: *mut malloc_type,
        _flags: c_int,
    ) -> *mut c_void {
        unsafe { aligned_alloc(align, size) }
    }
    #[unsafe(no_mangle)]
    extern "C" fn refcount_init__extern(count_ptr: *mut u_int, init: u_int) {
        println!("refcount: initialized count at {count_ptr:p} to {init:}");
        unsafe {
            AtomicU32::from_ptr(count_ptr).store(init, Ordering::Relaxed);
        }
    }

    #[unsafe(no_mangle)]
    extern "C" fn refcount_load__extern(count_ptr: *mut u_int) -> u_int {
        let count = unsafe { AtomicU32::from_ptr(count_ptr).load(Ordering::Relaxed) };
        println!("refcount: read {count:} from {count_ptr:p}");
        count
    }

    #[unsafe(no_mangle)]
    extern "C" fn refcount_release__extern(count_ptr: *mut u_int) -> bool {
        let last = unsafe { AtomicU32::from_ptr(count_ptr).fetch_sub(1, Ordering::Relaxed) == 1 };
        println!(
            "refcount: released {}count from {count_ptr:p}",
            if last { "last " } else { "" }
        );
        last
    }

    #[unsafe(no_mangle)]
    extern "C" fn refcount_acquire__extern(count_ptr: *mut u_int) {
        unsafe {
            AtomicU32::from_ptr(count_ptr).fetch_add(1, Ordering::Relaxed);
        }
        println!("refcount: grabbed count from {count_ptr:p}");
    }

    #[unsafe(no_mangle)]
    extern "C" fn device_get_state(dev: device_t) -> device_state_t {
        test_dev(dev).state
    }

    #[unsafe(no_mangle)]
    extern "C" fn device_claim_softc(_dev: device_t) {}

    #[unsafe(no_mangle)]
    extern "C" fn device_get_softc(dev: device_t) -> *mut c_void {
        test_dev(dev).softc
    }

    #[unsafe(no_mangle)]
    extern "C" fn device_free_softc(sc: *mut c_void) {
        // Assuming the first field is a device_t, dereference it
        let dev = unsafe { *sc.cast::<device_t>() };
        let driver = test_dev(dev).assigned_driver.unwrap();
        // Reconstitute the Vec<u8> to free the memory when it gets dropped
        let _softc = unsafe {
            Vec::<u8>::from_raw_parts(
                test_dev(dev).softc.cast::<u8>(),
                driver_t::get_softc_size(driver),
                driver_t::get_softc_size(driver),
            )
        };
    }

    #[unsafe(no_mangle)]
    extern "C" fn device_get_driver(dev: device_t) -> *mut driver_t {
        test_dev(dev).assigned_driver.unwrap_or(null_mut())
    }

    #[unsafe(no_mangle)]
    extern "C" fn ofw_bus_is_compatible(dev: device_t, compat_str: *const i8) -> bool {
        for candidate in &test_dev(dev).compat_strs {
            if *candidate == unsafe { CStr::from_ptr(compat_str) } {
                return true;
            }
        }
        false
    }

    #[unsafe(no_mangle)]
    extern "C" fn ofw_bus_status_okay(dev: device_t) -> i32 {
        test_dev(dev).is_ok as i32
    }

    #[unsafe(no_mangle)]
    extern "C" fn device_set_desc(dev: device_t, desc: *const i8) {
        unsafe {
            let cstr = CStr::from_ptr(desc.cast_mut());
            test_dev_mut(dev).desc = Some(CString::from(cstr));
        }
    }

    #[unsafe(no_mangle)]
    extern "C" fn device_get_desc<'a>(dev: device_t) -> *const i8 {
        match test_dev(dev).desc.as_ref() {
            Some(cstring) => cstring.as_c_str().as_ptr(),
            None => null(),
        }
    }

    #[unsafe(no_mangle)]
    extern "C" fn bus_setup_intr(
        dev: device_t,
        _r: *mut resource,
        _flags: c_int,
        filter: driver_filter_t,
        handler: driver_intr_t,
        arg: *mut c_void,
        _cookiep: *mut *mut c_void,
    ) -> c_int {
        test_dev_mut(dev).filter = Some((filter, arg));
        test_dev_mut(dev).handler = Some((handler, arg));
        0
    }

    #[unsafe(no_mangle)]
    extern "C" fn bus_teardown_intr(
        _dev: device_t,
        _r: *mut resource,
        _cookie: *mut c_void,
    ) -> i32 {
        0
    }

    #[unsafe(no_mangle)]
    extern "C" fn config_intrhook_establish(hook: *mut intr_config_hook) -> i32 {
        CONFIG_HOOK.store(hook, Ordering::Relaxed);
        0
    }

    #[unsafe(no_mangle)]
    extern "C" fn config_intrhook_disestablish(_hook: *mut intr_config_hook) {
        CONFIG_HOOK.store(null_mut(), Ordering::Relaxed);
    }
    #[unsafe(no_mangle)]
    extern "C" fn intr_pic_claim_root(
        dev: device_t,
        _arg2: isize,
        filter: intr_irq_filter_t,
        arg: *mut c_void,
        _root: u32,
    ) -> c_int {
        test_dev_mut(dev).pic_root = Some((filter, arg));
        0
    }

    #[unsafe(no_mangle)]
    static mut M_DEVBUF: () = ();

    #[unsafe(no_mangle)]
    static mut device_probe_desc: () = ();

    #[unsafe(no_mangle)]
    static mut device_attach_desc: () = ();

    #[unsafe(no_mangle)]
    static mut device_detach_desc: () = ();
}

define_stub_syms! {
    simplebus_driver
    //refcount_release__extern
    //refcount_init__extern
    //refcount_load__extern
    //refcount_acquire__extern
    //device_free_softc
}
