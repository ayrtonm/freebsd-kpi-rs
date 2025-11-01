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

use crate::ErrCode;
use crate::bindings::{
    device_t, ich_func_t, intr_config_hook, intr_irq_filter_t, intr_irqsrc, intr_map_data,
    intr_map_data_fdt, pcell_t, trapframe,
};
use crate::bus::{Filter, Resource};
use crate::ffi::{OwnedPtr, Ptr, RefCountData, RefCounted, SubClass};
use crate::ofw::XRef;
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::{CStr, c_int, c_void};
use core::mem::transmute;
use core::ptr::null_mut;
use core::sync::atomic::{AtomicBool, AtomicPtr, Ordering};

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IntrRoot(c_int);

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IntrIsrcf(c_int);

impl<'a, T> AsRustType<&'a mut IrqSrc<T>> for *mut intr_irqsrc {
    fn as_rust_type(self) -> &'a mut IrqSrc<T> {
        unsafe { IrqSrc::from_base_ptr(self) }
    }
}

impl AsRustType<MapData> for *mut intr_map_data {
    fn as_rust_type(self) -> MapData {
        MapData::new(self)
    }
}

impl AsRustType<IntrRoot> for u32 {
    fn as_rust_type(self) -> IntrRoot {
        IntrRoot(self as i32)
    }
}

pub type IrqFilter<T> = extern "C" fn(&'static T) -> Filter;

define_interface! {
    pic_setup_intr(dev: device_t, isrc: *mut intr_irqsrc, res: *mut resource, data: *mut intr_map_data) -> int;
    pic_teardown_intr(dev: device_t, isrc: *mut intr_irqsrc, res: *mut resource, data: *mut intr_map_data) -> int;
    pic_enable_intr(dev: device_t, isrc: *mut intr_irqsrc);
    pic_disable_intr(dev: device_t, isrc: *mut intr_irqsrc);
    pic_post_filter(dev: device_t, isrc: *mut intr_irqsrc);
    pic_post_ithread(dev: device_t, isrc: *mut intr_irqsrc);
    pic_pre_ithread(dev: device_t, isrc: *mut intr_irqsrc);
    pic_bind_intr(dev: device_t, isrc: *mut intr_irqsrc) -> int;
    pic_init_secondary(dev: device_t, root: u32);
    pic_ipi_send(dev: device_t, isrc: *mut intr_irqsrc, cpus: cpuset_t, ipi: u32);
    pic_map_intr(dev: device_t, data: *mut intr_map_data, isrcp: *mut *mut intr_irqsrc) -> int,
        with init glue {
            // Store the pointer passed to the extern "C" function so we can reference it later
            let mut c_isrcp = isrcp;

            let mut x = None;
            // Shadow the variable we pass to the rust method
            let mut isrcp = &mut x;
        },
        with drop glue {
            use $crate::ffi::SubClass;
            match isrcp {
                Some(x) => {
                    // Write the subclass base pointer to the pointer passed to the extern "C" function
                    *c_isrcp = SubClass::as_base_ptr(x);
                },
                None => { return EDOOFUS; },
            }
        };
    pic_ipi_setup(dev: device_t, ipi: u32, isrcp: *mut *mut intr_irqsrc) -> int,
        with init glue {
            let mut c_isrcp = isrcp;

            let mut x = None;
            let mut isrcp = &mut x;
        },
        with drop glue {
            use $crate::ffi::SubClass;
            match isrcp {
                Some(x) => {
                    *c_isrcp = SubClass::as_base_ptr(x);
                },
                None => { return EDOOFUS; },
            }
        };
}

pub type IrqSrc<T> = SubClass<intr_irqsrc, T>;

/// The PIC interface defined by pic_if.m
#[diagnostic::on_unimplemented(message = "
Implement the PIC interface trait by adding this where the `driver!` macro was used
pub struct {Self}IrqSrcFields {{ ... }}
impl PicIf for {Self} {{
    type IrqSrcFields = {Self}IrqSrcFields;
}}
")]
#[allow(unused_variables)]
pub trait PicIf: DeviceIf {
    type IrqSrcFields;
    fn pic_setup_intr(
        sc: &RefCounted<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
        res: Resource,
        data: MapData,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_teardown_intr(
        sc: &RefCounted<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
        res: Resource,
        data: MapData,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_map_intr<'a>(
        sc: &'a RefCounted<Self::Softc>,
        dev: device_t,
        data: MapData,
        isrcp: &mut Option<&'a IrqSrc<Self::IrqSrcFields>>,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_enable_intr(
        sc: &RefCounted<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) {
        unimplemented!()
    }
    fn pic_disable_intr(
        sc: &RefCounted<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) {
        unimplemented!()
    }
    fn pic_post_filter(
        sc: &RefCounted<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) {
        unimplemented!()
    }
    fn pic_post_ithread(
        sc: &RefCounted<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) {
        unimplemented!()
    }
    fn pic_pre_ithread(
        sc: &RefCounted<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) {
        unimplemented!()
    }
    fn pic_bind_intr(
        sc: &RefCounted<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_init_secondary(sc: &RefCounted<Self::Softc>, dev: device_t, root: IntrRoot) {
        unimplemented!()
    }
    fn pic_ipi_setup<'a>(
        sc: &'a RefCounted<Self::Softc>,
        dev: device_t,
        ipi: u32,
        isrcp: &mut Option<&'a IrqSrc<Self::IrqSrcFields>>,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_ipi_send(
        sc: &RefCounted<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
        cpus: bindings::cpuset_t,
        ipi: u32,
    ) {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum MapData {
    ACPI,
    FDT(MapDataFDT),
    GPIO,
    MSI,
    Unknown,
}

#[derive(Debug)]
pub struct MapDataFDT(*const intr_map_data_fdt);

impl MapDataFDT {
    pub fn cells(&self) -> &[pcell_t] {
        let fdt_data_ref = unsafe { self.0.as_ref().unwrap() };
        unsafe { fdt_data_ref.cells.as_slice(fdt_data_ref.ncells as usize) }
    }
}

impl MapData {
    pub fn new(map_data_ptr: *mut intr_map_data) -> Self {
        let ty = unsafe { (*map_data_ptr).type_ };
        match ty {
            bindings::INTR_MAP_DATA_ACPI => Self::ACPI,
            bindings::INTR_MAP_DATA_FDT => {
                let fdt_data_ptr = map_data_ptr as *const intr_map_data_fdt;
                Self::FDT(MapDataFDT(fdt_data_ptr))
            }
            bindings::INTR_MAP_DATA_GPIO => Self::GPIO,
            bindings::INTR_MAP_DATA_MSI => Self::MSI,
            _ => Self::Unknown,
        }
    }
}

#[derive(Debug)]
pub struct ConfigHook {
    c_hook: UnsafeCell<intr_config_hook>,
    init: AtomicBool,
    metadata_ptr: AtomicPtr<RefCountData>,
}

// TODO: How much do I care about this given that it only is really active while attaching devices
// and before interrupts are enabled? Maybe use device_get_state to validate it's not used
// afterwards and remove the Mutable wrapper on the ctx field
unsafe impl Sync for ConfigHook {}

pub type ConfigHookFn<T> = extern "C" fn(&RefCounted<T>);

impl ConfigHook {
    pub fn new() -> Self {
        let c_hook = UnsafeCell::new(intr_config_hook::default());
        Self {
            c_hook,
            init: AtomicBool::new(false),
            metadata_ptr: AtomicPtr::new(null_mut()),
        }
    }

    pub fn init<T>(&self, func_arg: ConfigHookFn<T>, arg: Ptr<T>) {
        let func = unsafe { transmute::<Option<ConfigHookFn<T>>, ich_func_t>(Some(func_arg)) };
        // TODO: where does the refcount get recreated?
        let (arg_ptr, metadata_ptr) = OwnedPtr::into_raw_parts(arg);

        let c_hook = self.c_hook.get();
        unsafe { (*c_hook).ich_func = func };
        unsafe { (*c_hook).ich_arg = arg_ptr.cast::<c_void>() };
        self.metadata_ptr
            .store(metadata_ptr.unwrap(), Ordering::Relaxed);
        self.init.store(true, Ordering::Relaxed);
    }
}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;

    gen_newtype! {
        IntrRoot,
        INTR_ROOT_IRQ,
        INTR_ROOT_FIQ,
    }

    gen_newtype! {
        IntrIsrcf,
        INTR_ISRCF_IPI,
        INTR_ISRCF_PPI,
        INTR_ISRCF_BOUND,
    }
    pub fn intr_isrc_register<T>(
        isrc: &'static IrqSrc<T>,
        dev: device_t,
        flags: Option<IntrIsrcf>,
        fmt_str: &'static CStr,
        arg0: &'static CStr,
        arg1: u32,
        arg2: u32,
    ) -> Result<()> {
        let flags = flags.map(|f| f.0 as u32).unwrap_or(0);
        let isrc_ptr = SubClass::as_base_ptr(&isrc);
        let res = unsafe {
            bindings::intr_isrc_register(
                isrc_ptr,
                dev,
                flags,
                fmt_str.as_ptr(),
                arg0.as_ptr(),
                arg1,
                arg2,
            )
        };
        if res == 0 {
            Ok(())
        } else {
            Err(ErrCode::from(res))
        }
    }

    pub fn intr_pic_register(dev: device_t, xref: XRef) -> Result<()> {
        let pic = unsafe { bindings::intr_pic_register(dev, xref.0 as _) };
        if pic.is_null() {
            Err(ENULLPTR)
        } else {
            // TODO: Return the pic pointer?
            Ok(())
        }
    }

    pub fn intr_pic_claim_root<T>(
        dev: device_t,
        xref: XRef,
        filter_arg: IrqFilter<T>,
        arg: &'static T,
        root: IntrRoot,
    ) -> Result<()> {
        let leaked_arg = arg as *const T;
        let leaked_ptr = leaked_arg.cast_mut().cast::<c_void>();
        let xref = xref.0 as isize;
        let filter =
            unsafe { transmute::<Option<IrqFilter<T>>, intr_irq_filter_t>(Some(filter_arg)) };
        let res =
            unsafe { bindings::intr_pic_claim_root(dev, xref, filter, leaked_ptr, root.0 as u32) };
        if res == 0 {
            Ok(())
        } else {
            Err(ErrCode::from(res))
        }
    }

    pub fn intr_ipi_pic_register(dev: device_t, priority: u32) -> Result<()> {
        let res = unsafe { bindings::intr_ipi_pic_register(dev, priority) };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }

    pub fn intr_isrc_dispatch<T>(isrc: &'static IrqSrc<T>, tf: *mut trapframe) -> c_int {
        let isrc_ptr = SubClass::as_base_ptr(isrc);
        unsafe { bindings::intr_isrc_dispatch(isrc_ptr, tf) }
    }

    pub fn intr_ipi_dispatch(ipi: u32) {
        unsafe { bindings::intr_ipi_dispatch(ipi) }
    }

    pub fn config_intrhook_establish<H: OwnedPtr<ConfigHook>>(hook: H) -> Result<()> {
        if !hook.init.load(Ordering::Relaxed) {
            // User didn't call ConfigHook::init
            return Err(EDOOFUS);
        }
        let res = unsafe { bindings::config_intrhook_establish(hook.c_hook.get()) };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }

    pub fn config_intrhook_disestablish(hook: &ConfigHook) {
        unsafe { bindings::config_intrhook_disestablish(hook.c_hook.get()) };

        unsafe {
            RefCountData::release_ref(hook.metadata_ptr.load(Ordering::Relaxed));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::tests::{HookDriver, IntcDriver, hook_driver, intc_driver};
    use crate::device::{BusProbe, DeviceIf};
    use crate::ffi::Uninit;
    use crate::tests::{DriverManager, LoudDrop};

    #[repr(C)]
    pub struct HookSoftc {
        dev: device_t,
        hook: ConfigHook,
        loud: LoudDrop,
    }
    impl DeviceIf for HookDriver {
        type Softc = HookSoftc;
        fn device_probe(dev: device_t) -> Result<BusProbe> {
            if !ofw_bus_is_compatible(dev, c"intr,hook_driver") {
                return Err(ENXIO);
            }
            Ok(BUS_PROBE_DEFAULT)
        }
        fn device_attach(uninit_sc: &mut Uninit<Self::Softc>, dev: device_t) -> Result<()> {
            let hook = ConfigHook::new();
            let loud = LoudDrop;
            let sc = uninit_sc.init(HookSoftc { dev, hook, loud });
            sc.hook.init(HookDriver::deferred_attach, sc.grab_ref());
            config_intrhook_establish(project!(sc->hook)).unwrap();
            Ok(())
        }
        fn device_detach(_sc: &RefCounted<Self::Softc>, _dev: device_t) -> Result<()> {
            Ok(())
        }
    }

    impl HookDriver {
        extern "C" fn deferred_attach(sc: &RefCounted<HookSoftc>) {
            println!("called config hook rust function/deferred_attach");
            config_intrhook_disestablish(&sc.hook);
        }
    }

    #[test]
    fn run_hook() {
        let mut m = DriverManager::new();
        m.add_test_device(c"intr,hook_driver");
        m.drivers.push(&hook_driver);
        m.probe_all();
        m.attach_all();
        m.trigger_config_hooks();
        m.detach_all();
    }

    #[repr(C)]
    #[derive(Debug)]
    pub struct IntcSoftc {
        dev: device_t,
    }
    impl DeviceIf for IntcDriver {
        type Softc = IntcSoftc;
        fn device_probe(dev: device_t) -> Result<BusProbe> {
            if !ofw_bus_is_compatible(dev, c"intr,intc_driver") {
                return Err(ENXIO);
            }
            Ok(BUS_PROBE_DEFAULT)
        }
        fn device_attach(uninit_sc: &mut Uninit<Self::Softc>, dev: device_t) -> Result<()> {
            let sc = uninit_sc.init(IntcSoftc { dev });
            intr_pic_claim_root(
                dev,
                XRef(0),
                IntcDriver::handle_irq,
                sc.leak_ref(),
                INTR_ROOT_IRQ,
            )
        }
        fn device_detach(_sc: &RefCounted<Self::Softc>, _dev: device_t) -> Result<()> {
            Ok(())
        }
    }

    impl IntcDriver {
        extern "C" fn handle_irq(sc: &'static IntcSoftc) -> Filter {
            println!("invoked irq handler {sc:x?}");
            FILTER_HANDLED
        }
    }

    #[test]
    fn run_intc() {
        let mut m = DriverManager::new();
        let dev = m.add_test_device(c"intr,intc_driver").dev;
        m.drivers.push(&intc_driver);
        m.probe_all();
        m.attach_all();
        m.trigger_pic_root(dev);
        m.detach_all();
    }
}
