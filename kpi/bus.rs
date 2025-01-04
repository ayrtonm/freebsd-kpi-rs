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

use crate::prelude::*;
use crate::ErrCode;
use crate::bindings::{bus_size_t, resource, resource_spec, RF_ACTIVE};
use crate::device::Device;
use crate::vec::Vec;
use core::ffi::{c_int, c_void};
use core::mem::transmute;
use core::ptr::{addr_of_mut, null_mut};
use core::pin::Pin;

enum_c_macros! {
    #[repr(i32)]
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    #[allow(nonstandard_style)]
    pub enum SysRes {
        SYS_RES_MEMORY,
        SYS_RES_IRQ,
    }
}

// TODO: This belongs in intr.rs after sorting out the intrng feature
enum_c_macros! {
    #[repr(i32)]
    #[derive(Copy, Clone, Debug)]
    #[allow(nonstandard_style)]
    pub enum Filter {
        FILTER_STRAY,
        FILTER_HANDLED,
        FILTER_SCHEDULE_THREAD,
    }
}

type RawFilterFn = Option<unsafe extern "C" fn(*mut c_void) -> i32>;
type RawHandler = Option<unsafe extern "C" fn(*mut c_void)>;

type FilterFn<T> = Option<extern "C" fn(&T) -> Filter>;
type Handler<T> = Option<extern "C" fn(&T)>;

impl AsRustType<Resource> for *mut resource {
    fn as_rust_type(self) -> Resource {
        Resource {
            res: self,
            rid: None,
            ty: None,
        }
    }
}

#[derive(Debug)]
pub struct Resource {
    res: *mut resource,
    // bus_alloc_resouce writes rid out to a pointer so let's make this public
    // TODO: it's currently Option for the PicIf interface
    pub rid: Option<c_int>,
    ty: Option<SysRes>,
}

unsafe impl Sync for Resource {}

#[derive(Debug)]
pub struct Register<const START: bus_size_t = 0, const SIZE: bus_size_t = { bus_size_t::MAX }> {
    res: *mut resource,
}

#[derive(Debug)]
pub struct VarRegister {
    res: *mut resource,
    start: bus_size_t,
    size: bus_size_t,
}

#[derive(Debug)]
pub struct ResourceSpec {
    ty: SysRes,
    rid: c_int,
    //flags: i32,
}

impl ResourceSpec {
    pub const fn new(ty: SysRes, rid: c_int) -> Self {
        Self { ty, rid }
    }
}

fn bus_setup_intr_internal(
    dev: Device,
    irq: &Resource,
    flags: u32,
    filter: RawFilterFn,
    handler: RawHandler,
    arg: *mut c_void,
    intrhand: *mut *mut c_void,
) -> Result<()> {
    let dev_ptr = dev.as_ptr();
    let res_ptr = irq.res;
    let res = unsafe {
        bindings::bus_setup_intr(
            dev_ptr,
            res_ptr,
            flags as c_int,
            filter,
            handler,
            arg,
            intrhand,
        )
    };
    if res != 0 {
        Err(ErrCode::from(res))
    } else {
        Ok(())
    }
}

impl<D: HasSoftc> BusIfWrappers for D {}

pub trait BusIfWrappers: HasSoftc {
    fn bus_setup_intr(
        &self,
        dev: Device,
        irq: &Resource,
        flags: u32,
        filter: FilterFn<Self::Softc>,
        handler: Handler<Self::Softc>,
        arg: Pin<&Self::Softc>,
        intrhand: *mut *mut c_void,
    ) -> Result<()> {
        assert!(self as *const Self as *const bindings::driver_t == dev.get_driver());
        let filter = unsafe { transmute(filter) };
        let handler = unsafe { transmute(handler) };
        let arg = arg.get_ref() as *const Self::Softc as *const c_void as *mut c_void;
        bus_setup_intr_internal(dev, irq, flags, filter, handler, arg, intrhand)
    }
}

pub mod wrappers {
    use super::*;

    pub fn bus_alloc_resource(
        dev: Device,
        ty: SysRes,
        mut rid: c_int,
    ) -> Result<Resource> {
        let dev_ptr = dev.as_ptr();
        // TODO: as u32 needed because bindgen flag makes macros default to signed, but RF_ACTIVE is
        // a bitfield. Ideally there'd be a heuristic for choosing signedness in cases like this
        let res = unsafe {
            bindings::bus_alloc_resource(
                dev_ptr,
                ty as c_int,
                &mut rid,
                0,
                !0, /* this is bitwise neg */
                1,
                RF_ACTIVE as u32,
            )
        };
        if res.is_null() {
            Err(ENULLPTR)
        } else {
            Ok(Resource {
                res,
                rid: Some(rid),
                ty: Some(ty),
            })
        }
    }

    pub fn bus_alloc_resources<const N: usize>(
        dev: Device,
        spec: [ResourceSpec; N],
    ) -> Result<[Resource; N]> {
        let dev_ptr = dev.as_ptr();

        #[repr(C)]
        struct NullTerminated<const N: usize> {
            list: [resource_spec; N],
            null: resource_spec,
        }

        let list = spec.map(|s| resource_spec {
            type_: s.ty as c_int,
            rid: s.rid,
            flags: RF_ACTIVE,
        });
        let mut spec = NullTerminated {
            list,
            null: resource_spec {
                type_: -1,
                rid: 0,
                flags: 0,
            },
        };
        let mut resp: [*mut resource; N] = [null_mut(); N];
        let res = unsafe {
            bindings::bus_alloc_resources(dev_ptr, addr_of_mut!(spec).cast(), resp.as_mut_ptr())
        };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            let mut ret: [Resource; N] = resp.map(|r| Resource {
                res: r,
                rid: None,
                ty: None,
            });
            for n in 0..N {
                ret[n].rid = Some(spec.list[n].rid);
                ret[n].ty = spec.list[n].type_.try_into().ok();
            }
            Ok(ret)
        }
    }
}

struct Window {
    start: bus_size_t,
    end: bus_size_t,
}


pub struct RegisterBuilder<const N: usize> {
    res: *mut resource,
    claimed_windows: [Option<Window>; N],
    claimed: usize,
}

impl Resource {
    pub fn split_resource<const N: usize>(self) -> Result<RegisterBuilder<N>> {
        if self.ty != Some(SYS_RES_MEMORY) {
            return Err(EDOOFUS);
        }
        Ok(RegisterBuilder {
            res: self.res,
            claimed_windows: [const { None }; N],
            claimed: 0,
        })
    }

    pub fn take_register(self) -> Result<Register> {
        if self.ty != Some(SYS_RES_MEMORY) {
            return Err(EDOOFUS);
        }
        Ok(Register { res: self.res })
    }
}

impl<const N: usize> RegisterBuilder<N> {
    fn is_claimed(&self, start: bus_size_t, size: bus_size_t) -> bool {
        let end = start + size;
        for claimed in &self.claimed_windows {
            match claimed {
                Some(window) => {
                    if window.start <= end && start <= window.end {
                        return true;
                    }
                },
                None => return false,
            }
        }
        panic!("Attempted to claim more than {N} registers")
    }

    pub fn take_var_register(&mut self, start: bus_size_t, size: bus_size_t) -> Result<VarRegister> {
        if self.is_claimed(start, size) {
            return Err(EDOOFUS);
        }
        let end = start + size;
        self.claimed_windows[self.claimed] = Some(Window { start, end });
        self.claimed += 1;
        Ok(VarRegister { res: self.res, start, size })
    }

    pub fn take_register<const START: bus_size_t, const SIZE: bus_size_t>(
        &mut self,
    ) -> Result<Register<START, SIZE>> {
        if self.is_claimed(START, SIZE) {
            return Err(EDOOFUS);
        }
        let end = START + SIZE;
        self.claimed_windows[self.claimed] = Some(Window { start: START, end });
        self.claimed += 1;
        Ok(Register { res: self.res })
    }
}

#[cfg(target_arch = "aarch64")]
impl<const START: bus_size_t, const SIZE: bus_size_t> Register<START, SIZE> {
    fn assert_precond(offset: bus_size_t) {
        assert!(START <= offset);
        assert!(offset < START + SIZE);
    }

    pub fn read_4(&mut self, offset: bus_size_t) -> u32 {
        Self::assert_precond(offset);
        unsafe {
            let tag = (*self.res).r_bustag;
            let handle = (*self.res).r_bushandle;
            let f = (*tag).bs_r_4.unwrap();
            f((*tag).bs_cookie, handle, offset)
        }
    }

    pub fn write_4(&mut self, offset: bus_size_t, value: u32) {
        Self::assert_precond(offset);
        unsafe {
            let tag = (*self.res).r_bustag;
            let handle = (*self.res).r_bushandle;
            let f = (*tag).bs_w_4.unwrap();
            f((*tag).bs_cookie, handle, offset, value)
        }
    }

    pub fn read_8(&mut self, offset: bus_size_t) -> u64 {
        Self::assert_precond(offset);
        unsafe {
            let tag = (*self.res).r_bustag;
            let handle = (*self.res).r_bushandle;
            let f = (*tag).bs_r_8.unwrap();
            f((*tag).bs_cookie, handle, offset)
        }
    }

    pub fn write_8(&mut self, offset: bus_size_t, value: u64) {
        Self::assert_precond(offset);
        unsafe {
            let tag = (*self.res).r_bustag;
            let handle = (*self.res).r_bushandle;
            let f = (*tag).bs_w_8.unwrap();
            f((*tag).bs_cookie, handle, offset, value)
        }
    }
}
