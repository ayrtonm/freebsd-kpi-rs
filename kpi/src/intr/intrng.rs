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
    device_t, intr_irq_filter_t, intr_irqsrc, intr_map_data, intr_map_data_fdt, pcell_t, trapframe,
};
use crate::bus::{Filter, Resource};
use crate::ffi::SubClass;
use crate::ofw::XRef;
use crate::prelude::*;
use crate::sync::arc::Arc;
use core::ffi::{CStr, c_int, c_void};
use core::mem::transmute;

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IntrRoot(c_int);

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IntrIsrcf(c_int);

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

define_dev_interface! {
    in PicIf
    fn pic_setup_intr(dev: device_t, isrc: *mut intr_irqsrc, res: *mut resource, data: *mut intr_map_data) -> int;
    fn pic_teardown_intr(dev: device_t, isrc: *mut intr_irqsrc, res: *mut resource, data: *mut intr_map_data) -> int;
    fn pic_enable_intr(dev: device_t, isrc: *mut intr_irqsrc);
    fn pic_disable_intr(dev: device_t, isrc: *mut intr_irqsrc);
    fn pic_post_filter(dev: device_t, isrc: *mut intr_irqsrc);
    fn pic_post_ithread(dev: device_t, isrc: *mut intr_irqsrc);
    fn pic_pre_ithread(dev: device_t, isrc: *mut intr_irqsrc);
    fn pic_bind_intr(dev: device_t, isrc: *mut intr_irqsrc) -> int;
    fn pic_init_secondary(dev: device_t, root: u32);
    fn pic_ipi_send(dev: device_t, isrc: *mut intr_irqsrc, cpus: cpuset_t, ipi: u32);
    fn pic_map_intr(dev: device_t, data: *mut intr_map_data, isrcp: *mut *mut intr_irqsrc) -> int,
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
    fn pic_ipi_setup(dev: device_t, ipi: u32, isrcp: *mut *mut intr_irqsrc) -> int,
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
#[allow(unused_variables)]
pub trait PicIf: DeviceIf {
    type IrqSrcFields;
    fn pic_setup_intr(
        sc: ArcRef<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
        res: Resource,
        data: MapData,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_teardown_intr(
        sc: ArcRef<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
        res: Resource,
        data: MapData,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_map_intr<'a>(
        sc: ArcRef<'a, Self::Softc>,
        dev: device_t,
        data: MapData,
        isrcp: &mut Option<&'a IrqSrc<Self::IrqSrcFields>>,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_enable_intr(
        sc: ArcRef<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) {
        unimplemented!()
    }
    fn pic_disable_intr(
        sc: ArcRef<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) {
        unimplemented!()
    }
    fn pic_post_filter(
        sc: ArcRef<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) {
        unimplemented!()
    }
    fn pic_post_ithread(
        sc: ArcRef<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) {
        unimplemented!()
    }
    fn pic_pre_ithread(
        sc: ArcRef<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) {
        unimplemented!()
    }
    fn pic_bind_intr(
        sc: ArcRef<Self::Softc>,
        dev: device_t,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_init_secondary(sc: &Arc<Self::Softc>, dev: device_t, root: IntrRoot) {
        unimplemented!()
    }
    fn pic_ipi_setup<'a>(
        sc: ArcRef<'a, Self::Softc>,
        dev: device_t,
        ipi: u32,
        isrcp: &mut Option<&'a IrqSrc<Self::IrqSrcFields>>,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_ipi_send(
        sc: ArcRef<Self::Softc>,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::{BusProbe, DeviceIf};
    use crate::driver;
    use crate::sync::arc::{Arc, UninitArc};
    use crate::tests::DriverManager;

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
        fn device_attach(uninit_sc: UninitArc<Self::Softc>, dev: device_t) -> Result<()> {
            let sc = uninit_sc.init(IntcSoftc { dev });
            intr_pic_claim_root(
                dev,
                XRef(0),
                IntcDriver::handle_irq,
                Arc::leak(sc.into_arc()),
                INTR_ROOT_IRQ,
            )
        }
        fn device_detach(_sc: Arc<Self::Softc>, _dev: device_t) -> Result<()> {
            Ok(())
        }
    }

    #[repr(C)]
    #[derive(Debug)]
    pub struct IntcIrqSrc {
        x: u32,
    }

    impl PicIf for IntcDriver {
        type IrqSrcFields = IntcIrqSrc;

        fn pic_setup_intr(
            _sc: ArcRef<Self::Softc>,
            _dev: device_t,
            isrc: &mut IrqSrc<Self::IrqSrcFields>,
            _res: Resource,
            _data: MapData,
        ) -> Result<()> {
            println!("called setup intr {isrc:x?}");
            assert_eq!(isrc.x, 0xdeadbeef);
            Ok(())
        }
    }

    impl IntcDriver {
        extern "C" fn handle_irq(sc: &'static IntcSoftc) -> Filter {
            println!("invoked irq handler {sc:x?}");
            FILTER_HANDLED
        }
    }

    driver!(intc_driver, c"intc_driver", IntcDriver,
            intc_driver_methods = {
                device_probe intc_driver_probe,
                device_attach intc_driver_attach,
                device_detach intc_driver_detach,
                pic_setup_intr intc_setup_intr,
            }
    );

    #[test]
    fn run_intc() {
        let mut m = DriverManager::new();
        let dev = m.add_test_device(c"intr,intc_driver").dev;
        m.add_test_driver::<IntcDriver>();
        m.probe_all();
        m.attach_all();
        m.trigger_pic_root(dev);
        m.detach_all();
    }

    #[test]
    fn run_setup_intr() {
        let mut m = DriverManager::new();
        let _dev = m.add_test_device(c"intr,intc_driver").dev;
        m.add_test_driver::<IntcDriver>();
        m.probe_all();
        m.attach_all();
        // SAFETY: The irqsrc is ABI-compatible with SubClass<intr_irqsrc, u32> since the PicIf impl
        // has an IrqSrcFields that is ABI-compatible with a u32.
        unsafe {
            m.setup_intr();
        }
    }
}
