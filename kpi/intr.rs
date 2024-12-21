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

use crate::bindings::{
    intr_irqsrc, intr_map_data, intr_map_data_fdt, intr_pic, pcell_t, trapframe,
};
use crate::bus::Resource;
use crate::device::Device;
use crate::kpi_prelude::*;
use crate::ofw::XRef;
use core::ffi::{c_int, c_void, CStr};
use core::mem::transmute;

enum_c_macros! {
    #[repr(i32)]
    #[derive(Copy, Clone, Debug)]
    #[allow(nonstandard_style)]
    pub enum IntrRoot {
        INTR_ROOT_IRQ,
        INTR_ROOT_FIQ,
    }
}

enum_c_macros! {
    #[repr(i32)]
    #[derive(Copy, Clone, Debug)]
    #[allow(nonstandard_style)]
    pub enum FilterRes {
        FILTER_STRAY,
        FILTER_HANDLED,
        FILTER_SCHEDULE_THREAD,
    }
}

impl<'a, T> AsRustType<&'a mut SubClass<IrqSrc, T>> for *mut intr_irqsrc {
    fn as_rust_type(self) -> &'a mut SubClass<IrqSrc, T> {
        unsafe { SubClass::from_base(self) }
    }
}

impl AsRustType<MapData> for *mut intr_map_data {
    fn as_rust_type(self) -> MapData {
        MapData::new(self)
    }
}

impl<'a> AsRustType<&'a mut *mut intr_irqsrc> for *mut *mut intr_irqsrc {
    fn as_rust_type(self) -> &'a mut *mut intr_irqsrc {
        unsafe { self.as_mut().unwrap() }
    }
}

pub type IrqSrc = intr_irqsrc;

impl<T> SubClass<IrqSrc, T> {
    pub fn isrc_dispatch(&self, tf: *mut trapframe) -> c_int {
        unsafe { bindings::intr_isrc_dispatch(SubClass::get_base_ptr(self), tf) }
    }
}

pub trait PicIf: DriverIf {
    type Irq;

    fn pic_setup_intr(
        &self,
        dev: Device,
        isrc: &mut SubClass<IrqSrc, Self::Irq>,
        res: Resource,
        data: MapData,
    ) -> Result<()>;
    fn pic_map_intr(&self, dev: Device, data: MapData, isrcp: &mut *mut IrqSrc) -> Result<()>;
    fn pic_teardown_intr(&self, dev: Device, isrc: &SubClass<IrqSrc, Self::Irq>, res: Resource, data: MapData) -> Result<()>;

    fn pic_disable_intr(&self, dev: Device, isrc: &SubClass<IrqSrc, Self::Irq>);
    fn pic_enable_intr(&self, dev: Device, isrc: &SubClass<IrqSrc, Self::Irq>);

    fn pic_post_filter(&self, dev: Device, isrc: &SubClass<IrqSrc, Self::Irq>);
    fn pic_pre_ithread(&self, dev: Device, isrc: &SubClass<IrqSrc, Self::Irq>);
    fn pic_post_ithread(&self, dev: Device, isrc: &SubClass<IrqSrc, Self::Irq>);
}

    //fn isrc_register(
    //    &self,
    //    dev: Device,
    //    isrc: &SubClass<IrqSrc, Self::Irq>,
    //    flags: i32,
    //    fmt_str: &CStr,
    //    arg0: &CStr,
    //    arg1: u32,
    //    arg2: u32,
    //) -> Result<()> {
    //    let res = unsafe {
    //        bindings::intr_isrc_register(
    //            SubClass::get_base_ptr(&isrc),
    //            dev.as_ptr(),
    //            flags as u32,
    //            fmt_str.as_ptr(),
    //            arg0.as_ptr(),
    //            arg1,
    //            arg2,
    //        )
    //    };
    //    if res != 0 {
    //        Err(ErrCode::from(res))
    //    } else {
    //        Ok(())
    //    }
    //}

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

pub struct Pic {
    dev: Device,
    xref: XRef,
    pic: *const intr_pic,
}

impl Device {
    //pub fn pic_register(&self, xref: XRef) -> Result<Pic> {
    //    let dev_ptr = self.as_ptr();
    //    let pic = unsafe { bindings::intr_pic_register(dev_ptr, xref.0 as _) };
    //    if pic.is_null() {
    //        Err(ENULLPTR)
    //    } else {
    //        Ok(Pic {
    //            dev: *self,
    //            xref,
    //            pic,
    //        })
    //    }
    //}

    pub fn ipi_pic_register(&self, priority: u32) -> Result<()> {
        let dev_ptr = self.as_ptr();
        let res = unsafe { bindings::intr_ipi_pic_register(dev_ptr, priority) };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }
}

type RawFilter = unsafe extern "C" fn(*mut c_void) -> i32;
type Filter<P> = extern "C" fn(P) -> FilterRes;

impl Pic {
    // TODO: Pin pointer
    pub fn claim_root<SC>(
        &mut self,
        filter: Filter<*mut SC>,
        arg: *mut SC,
        root: IntrRoot,
    ) -> Result<()> {
        let filter = unsafe { transmute(filter) };
        let arg = arg.cast();
        self.claim_root_internal(filter, arg, root)
    }

    pub fn claim_root_internal(
        &mut self,
        filter: RawFilter,
        arg: *mut c_void,
        root: IntrRoot,
    ) -> Result<()> {
        let dev_ptr = self.dev.as_ptr();
        let xref = self.xref.0 as isize;
        let res =
            unsafe { bindings::intr_pic_claim_root(dev_ptr, xref, Some(filter), arg, root as u32) };
        if res == 0 {
            Ok(())
        } else {
            Err(ErrCode::from(res))
        }
    }

    //pub fn isrc_dispatch(isrc: &mut IrqSrc, tf: *mut trapframe) -> Result<()> {
    //    let res = unsafe { bindings::intr_isrc_dispatch(isrc, tf) };
    //    if res == 0 {
    //        Ok(())
    //    } else {
    //        Err(ErrCode::from(res))
    //    }
    //}
}
