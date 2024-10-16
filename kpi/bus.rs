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

use crate::bindings::{bus_size_t, resource, resource_spec, RF_ACTIVE};
use crate::device::Device;
use crate::intr::FilterRes;
use crate::{bindings, AsRustType, ErrCode, PointsTo, Ptr, Ref, Result};
use alloc::vec::Vec;
use core::ffi::{c_int, c_void};
use core::mem::transmute;
use core::ptr::{addr_of_mut, null_mut};

enum_c_macros! {
    #[repr(i32)]
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    #[allow(nonstandard_style)]
    pub enum SysRes {
        SYS_RES_MEMORY,
        SYS_RES_IRQ,
    }
}

type RawFilter = Option<unsafe extern "C" fn(*mut c_void) -> i32>;
type RawHandler = Option<unsafe extern "C" fn(*mut c_void)>;

type Filter<T> = Option<extern "C" fn(T) -> FilterRes>;
type Handler<T> = Option<extern "C" fn(T)>;

impl AsRustType<Resource> for *mut resource {
    fn as_rust_type(self) -> Resource {
        Resource {
            res: unsafe { Ptr::new(self) },
            rid: None,
            claimed_windows: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Resource {
    res: Ptr<resource>,
    // bus_alloc_resouce writes rid out to a pointer so let's make this public
    // TODO: it's currently Option for the PicIf interface
    pub rid: Option<c_int>,
    // TODO: Should this be fixed-size?
    claimed_windows: Vec<Window>,
}

#[derive(Debug)]
struct Window {
    start: bus_size_t,
    end: bus_size_t,
}

#[derive(Debug)]
pub struct Register<const START: bus_size_t = 0, const SIZE: bus_size_t = { bus_size_t::MAX }> {
    res: Ptr<resource>,
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

impl Device {
    pub fn bus_alloc_resource(&mut self, ty: SysRes, mut rid: c_int) -> Result<Resource> {
        let dev_ptr = self.as_ptr();
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
            Err(ErrCode::ENULLPTR)
        } else {
            let res = unsafe { Ptr::new(res) };
            Ok(Resource {
                res,
                rid: Some(rid),
                claimed_windows: Vec::new(),
            })
        }
    }

    pub fn bus_alloc_resources<const N: usize>(
        &mut self,
        spec: [ResourceSpec; N],
    ) -> Result<[Resource; N]> {
        let dev_ptr = self.as_ptr();

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
                res: unsafe { Ptr::new(r) },
                rid: None,
                claimed_windows: Vec::new(),
            });
            for n in 0..N {
                ret[n].rid = Some(spec.list[n].rid);
            }
            Ok(ret)
        }
    }

    pub fn bus_setup_intr_internal(
        &mut self,
        irq: Resource,
        flags: u32,
        filter: RawFilter,
        handler: RawHandler,
        arg: *mut c_void,
        intrhand: *mut *mut c_void,
    ) -> Result<()> {
        let dev_ptr = self.as_ptr();
        let res_ptr = irq.res.as_ptr();
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

    pub fn bus_setup_intr<SC, P: PointsTo<SC>>(
        &mut self,
        irq: Resource,
        flags: u32,
        filter: Filter<P>,
        handler: Handler<P>,
        arg: P,
        intrhand: *mut *mut c_void,
    ) -> Result<()> {
        let filter = unsafe { transmute(filter) };
        let handler = unsafe { transmute(handler) };
        let arg = arg.as_ptr().cast();
        self.bus_setup_intr_internal(irq, flags, filter, handler, arg, intrhand)
    }
}

// TODO: these should take &mut Resource to avoid races with shared mmio
impl Resource {
    pub fn whole_register(self) -> Register {
        Register { res: self.res }
    }

    pub fn take_register<const START: bus_size_t, const SIZE: bus_size_t>(
        &mut self,
    ) -> Result<Register<START, SIZE>> {
        let end = START + SIZE;
        for claimed in &mut self.claimed_windows {
            if (end > claimed.start) || (START < claimed.end) {
                return Err(ErrCode::EDOOFUS);
            }
        }
        self.claimed_windows.push(Window { start: START, end });
        Ok(Register { res: self.res })
    }
}

impl<const START: bus_size_t, const SIZE: bus_size_t> Register<START, SIZE> {
    fn assert_precond(offset: bus_size_t) {
        assert!(START <= offset);
        assert!(offset < START + SIZE);
    }

    pub fn read_4(&mut self, offset: bus_size_t) -> u32 {
        Self::assert_precond(offset);
        let tag = unsafe { get_field!(self.res, r_bustag).read() };
        let handle = unsafe { get_field!(self.res, r_bushandle).read() };
        unsafe {
            let f = (*tag).bs_r_4.unwrap();
            f((*tag).bs_cookie, handle, offset)
        }
    }

    pub fn write_4(&mut self, offset: bus_size_t, value: u32) {
        Self::assert_precond(offset);
        let tag = unsafe { get_field!(self.res, r_bustag).read() };
        let handle = unsafe { get_field!(self.res, r_bushandle).read() };
        unsafe {
            let f = (*tag).bs_w_4.unwrap();
            f((*tag).bs_cookie, handle, offset, value)
        }
    }

    pub fn read_8(&mut self, offset: bus_size_t) -> u64 {
        Self::assert_precond(offset);
        let tag = unsafe { get_field!(self.res, r_bustag).read() };
        let handle = unsafe { get_field!(self.res, r_bushandle).read() };
        unsafe {
            let f = (*tag).bs_r_8.unwrap();
            f((*tag).bs_cookie, handle, offset)
        }
    }

    pub fn write_8(&mut self, offset: bus_size_t, value: u64) {
        Self::assert_precond(offset);
        let tag = unsafe { get_field!(self.res, r_bustag).read() };
        let handle = unsafe { get_field!(self.res, r_bushandle).read() };
        unsafe {
            let f = (*tag).bs_w_8.unwrap();
            f((*tag).bs_cookie, handle, offset, value)
        }
    }
}
