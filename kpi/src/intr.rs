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
    _device, intr_irqsrc, intr_map_data, intr_map_data_fdt, intr_pic, pcell_t, trapframe,
};
use crate::bus::{Filter, Resource};
use crate::cell::{ExtendedRef, SubClass};
use crate::device::Device;
use crate::ofw::XRef;
use crate::prelude::*;
use crate::ErrCode;
use core::ffi::{c_int, c_void, CStr};
use core::mem::transmute;
use core::ops::Deref;

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
    pub enum IntrIsrcf {
        INTR_ISRCF_IPI,
        INTR_ISRCF_PPI,
        INTR_ISRCF_BOUND,
    }
}

impl<'a, T> AsRustType<&'a mut IrqSrc<T>> for *mut intr_irqsrc {
    fn as_rust_type(self) -> &'a mut IrqSrc<T> {
        unsafe { IrqSrc::from_base(self) }
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

#[macro_export]
macro_rules! pic_setup_intr {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            pic_setup_intr(dev: device_t, isrc: *mut intr_irqsrc, res: *mut resource, data: *mut intr_map_data) -> int;
        }
    };
}

pub type IrqSrc<T> = SubClass<intr_irqsrc, T>;

pub trait PicIf {
    type DriverIrq;
    fn pic_setup_intr(dev: Device, isrc: &mut IrqSrc<Self::DriverIrq>, res: Resource, data: MapData) -> Result<()> {
        unimplemented!()
    }
}
//impl<T> SubClass<IrqSrc, T> {
//    pub fn isrc_dispatch(&self, tf: *mut trapframe) -> c_int {
//        unsafe { bindings::intr_isrc_dispatch(SubClass::get_base_ptr(self), tf) }
//    }
//}

//pub trait PicIf {
//    type Irq;
//
//    fn pic_setup_intr(
//        &self,
//        dev: Device,
//        isrc: &mut SubClass<IrqSrc, Self::Irq>,
//        res: Resource,
//        data: MapData,
//    ) -> Result<()>;
//    fn pic_map_intr(&self, dev: Device, data: MapData, isrcp: &mut *mut IrqSrc) -> Result<()>;
//    fn pic_teardown_intr(
//        &self,
//        dev: Device,
//        isrc: &SubClass<IrqSrc, Self::Irq>,
//        res: Resource,
//        data: MapData,
//    ) -> Result<()>;
//
//    fn pic_disable_intr(&self, dev: Device, isrc: &SubClass<IrqSrc, Self::Irq>);
//    fn pic_enable_intr(&self, dev: Device, isrc: &SubClass<IrqSrc, Self::Irq>);
//
//    fn pic_post_filter(&self, dev: Device, isrc: &SubClass<IrqSrc, Self::Irq>);
//    fn pic_pre_ithread(&self, dev: Device, isrc: &SubClass<IrqSrc, Self::Irq>);
//    fn pic_post_ithread(&self, dev: Device, isrc: &SubClass<IrqSrc, Self::Irq>);
//}

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

pub mod wrappers {
    use super::*;
    use core::pin::Pin;

    pub fn intr_isrc_register<T>(
        isrc: ExtendedRef<IrqSrc<T>, _device>,
        dev: Device,
        flags: Option<IntrIsrcf>,
        fmt_str: &'static CStr,
        arg0: &'static CStr,
        arg1: u32,
        arg2: u32,
    ) -> Result<()> {
        let dev_ptr = dev.as_ptr();
        if isrc.get_owner() != dev_ptr {
            return Err(EDOOFUS);
        }
        let flags = flags.map(|f| f as u32).unwrap_or(0);
        let isrc_ptr = SubClass::get_base_ptr(&isrc);
        let res = unsafe {
            bindings::intr_isrc_register(
                isrc_ptr,
                dev_ptr,
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

    pub fn intr_pic_register(dev: Device, xref: XRef) -> Result<()> {
        let dev_ptr = dev.as_ptr();
        let pic = unsafe { bindings::intr_pic_register(dev_ptr, xref.0 as _) };
        if pic.is_null() {
            Err(ENULLPTR)
        } else {
            // TODO: Return the pic pointer?
            Ok(())
        }
    }

    pub fn intr_pic_claim_root<T>(
        dev: Device,
        xref: XRef,
        filter: extern "C" fn(&T) -> i32,
        arg: &ExtendedRef<T, _device>,
        root: IntrRoot,
    ) -> Result<()> {
        let dev_ptr = dev.as_ptr();
        if arg.get_owner() != dev_ptr {
            return Err(EDOOFUS);
        }
        let arg_ptr = arg.deref() as *const T;
        let xref = xref.0 as isize;
        let filter = unsafe { transmute(filter) };
        let res = unsafe {
            bindings::intr_pic_claim_root(
                dev_ptr,
                xref,
                Some(filter),
                arg_ptr.cast::<c_void>().cast_mut(),
                root as u32,
            )
        };
        if res == 0 {
            Ok(())
        } else {
            Err(ErrCode::from(res))
        }
    }

    pub fn intr_ipi_pic_register(dev: Device, priority: u32) -> Result<()> {
        let dev_ptr = dev.as_ptr();
        let res = unsafe { bindings::intr_ipi_pic_register(dev_ptr, priority) };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }

    pub fn intr_isrc_dispatch<T>(isrc: &IrqSrc<T>, tf: *mut trapframe) -> c_int {
        let isrc_ptr = SubClass::get_base_ptr(&isrc);
        unsafe {
            bindings::intr_isrc_dispatch(isrc_ptr, tf)
        }
    }
}
