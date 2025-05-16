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
    intr_config_hook, intr_irqsrc, intr_map_data, intr_map_data_fdt, pcell_t, trapframe,
};
use crate::bus::{Filter, Resource};
use crate::cell::{OwnedMutRef, OwnedPtr, OwnedRef, OwnedVar, SubClass};
use crate::device::Device;
use crate::ofw::XRef;
use crate::prelude::*;
use crate::ErrCode;
use core::ffi::{c_int, c_void, CStr};
use core::mem::{transmute, MaybeUninit};
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

impl AsRustType<IntrRoot> for u32 {
    fn as_rust_type(self) -> IntrRoot {
        IntrRoot::try_from(self as i32).unwrap()
    }
}

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
}

#[macro_export]
macro_rules! pic_map_intr {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            pic_map_intr(dev: device_t, data: *mut intr_map_data, isrcp: *mut *mut intr_irqsrc) -> int;
            with init glue {
                use $crate::cell::OwnedPtr;
                // Create a dummy OwnedPtr so we can init the mutable reference we pass in to the
                // rust method. It could instead be a &mut MaybeUninit but that adds needless
                // complexity to PicIf.
                let mut x = OwnedPtr::<_, false>::null();
                // Store the pointer passed to the extern "C" function so we can reference it later
                let mut c_isrcp = isrcp;
                // Shadow the variable we pass to the rust method
                let mut isrcp = &mut x;
            }
            with drop glue {
                use $crate::cell::{SubClass, OwnedVar};
                let dev_ptr = dev.as_ptr();
                // Check the shadoweded variable's owner
                // This also implicitly checks that the user actually initialized it because
                // otherwise the owner would be null
                if isrcp.get_owner() != dev_ptr {
                    return EDOOFUS;
                }
                // Write the subclass base pointer to the pointer passed to the extern "C" function
                *c_isrcp = SubClass::base_ptr(x.get_var_ptr());
            }
        }
    };
}

#[macro_export]
macro_rules! pic_ipi_setup {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            pic_ipi_setup(dev: device_t, ipi: u32, isrcp: *mut *mut intr_irqsrc) -> int;
            with init glue {
                use $crate::cell::OwnedPtr;
                let mut x = OwnedPtr::<_, false>::null();
                let mut c_isrcp = isrcp;
                let mut isrcp = &mut x;
            }
            with drop glue {
                use $crate::cell::{SubClass, OwnedVar};
                let dev_ptr = dev.as_ptr();
                if isrcp.get_owner() != dev_ptr {
                    return EDOOFUS;
                }
                *c_isrcp = SubClass::base_ptr(x.get_var_ptr());
            }
        }
    };
}

pub type IrqSrc<T> = SubClass<intr_irqsrc, T>;

pub trait PicIf {
    type IrqSrcFields;
    fn pic_setup_intr(
        dev: Device,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
        res: Resource,
        data: MapData,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_teardown_intr(
        dev: Device,
        isrc: &mut IrqSrc<Self::IrqSrcFields>,
        res: Resource,
        data: MapData,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_map_intr(
        dev: Device,
        data: MapData,
        isrcp: &mut OwnedPtr<IrqSrc<Self::IrqSrcFields>>,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_enable_intr(dev: Device, isrc: &mut IrqSrc<Self::IrqSrcFields>) {
        unimplemented!()
    }
    fn pic_disable_intr(dev: Device, isrc: &mut IrqSrc<Self::IrqSrcFields>) {
        unimplemented!()
    }
    fn pic_post_filter(dev: Device, isrc: &mut IrqSrc<Self::IrqSrcFields>) {
        unimplemented!()
    }
    fn pic_post_ithread(dev: Device, isrc: &mut IrqSrc<Self::IrqSrcFields>) {
        unimplemented!()
    }
    fn pic_pre_ithread(dev: Device, isrc: &mut IrqSrc<Self::IrqSrcFields>) {
        unimplemented!()
    }
    fn pic_bind_intr(dev: Device, isrc: &mut IrqSrc<Self::IrqSrcFields>) -> Result<()> {
        unimplemented!()
    }
    fn pic_init_secondary(dev: Device, root: IntrRoot) {
        unimplemented!()
    }
    fn pic_ipi_setup(
        dev: Device,
        ipi: u32,
        isrcp: &mut OwnedPtr<IrqSrc<Self::IrqSrcFields>>,
    ) -> Result<()> {
        unimplemented!()
    }
    fn pic_ipi_send(
        dev: Device,
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
pub struct ConfigHook<T: 'static> {
    c_hook: intr_config_hook,
    pub func: fn(OwnedRef<T>) -> Result<()>,
    pub arg: OwnedPtr<T>,
}

impl<T> ConfigHook<T> {
    pub fn new() -> Self {
        let mut c_hook = intr_config_hook::default();
        c_hook.ich_func = Some(Self::config_hook_thunk);
        Self {
            c_hook,
            func: Self::default_thunk,
            arg: OwnedPtr::null(),
        }
    }

    fn default_thunk(arg: OwnedRef<T>) -> Result<()> {
        unreachable!("")
    }
    extern "C" fn config_hook_thunk(config_hook: *mut c_void) {
        let config_hook_ptr = config_hook.cast::<Self>();
        let config_hook_ref = unsafe { config_hook_ptr.as_ref().unwrap() };
        let arg = unsafe { config_hook_ref.arg.assume_can_reference() };
        (config_hook_ref.func)(arg).unwrap();
    }
}

pub mod wrappers {
    use super::*;
    use core::pin::Pin;

    //pub fn intr_isrc_register<'a, T, P: OwnedVar<'a, IrqSrc<T>>>(
    //    isrc: P,//OwnedRef<IrqSrc<T>>,
    pub fn intr_isrc_register<T>(
        isrc: OwnedPtr<IrqSrc<T>>,
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
        let isrc_ptr = SubClass::base_ptr(isrc.get_var_ptr());
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

    pub fn intr_pic_claim_root<T, P: OwnedVar<T>>(
        dev: Device,
        xref: XRef,
        filter: extern "C" fn(&T) -> Filter,
        arg: P,
        root: IntrRoot,
    ) -> Result<()> {
        let dev_ptr = dev.as_ptr();
        if arg.get_owner() != dev_ptr {
            return Err(EDOOFUS);
        }
        let arg_ptr = arg.get_var_ptr().cast::<c_void>();
        let xref = xref.0 as isize;
        let filter = unsafe { transmute(filter) };
        let res = unsafe {
            bindings::intr_pic_claim_root(dev_ptr, xref, Some(filter), arg_ptr, root as u32)
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
        let isrc_ptr = SubClass::get_base_ptr(isrc);
        unsafe { bindings::intr_isrc_dispatch(isrc_ptr, tf) }
    }

    pub fn intr_ipi_dispatch(ipi: u32) {
        unsafe { bindings::intr_ipi_dispatch(ipi) }
    }

    pub fn config_intrhook_establish<T>(hook: &mut OwnedMutRef<ConfigHook<T>>) -> Result<()> {
        hook.c_hook.ich_arg = hook.get_var_ptr().cast::<c_void>();
        if hook.arg.get_var_ptr().is_null() {
            return Err(EDOOFUS);
        }
        if hook.func == ConfigHook::<T>::default_thunk {
            return Err(EDOOFUS);
        }
        // The argument in c_hook will live long enough only if the owner of the OwnedRef passed to ConfigHook is a device (because of when the callback is scheduled for)
        let res = unsafe {
            bindings::config_intrhook_establish(
                &hook.c_hook as *const intr_config_hook as *mut intr_config_hook,
            )
        };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }
}
