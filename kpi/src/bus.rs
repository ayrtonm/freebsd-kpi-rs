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
use core::ffi::{c_int, c_void};
use core::mem::transmute;
use core::ptr::{addr_of_mut, null_mut};
use core::pin::Pin;
use core::ops::BitOr;

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

#[repr(C)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ResFlags(i32);

impl BitOr<ResFlags> for ResFlags {
    type Output = ResFlags;

    fn bitor(self, rhs: ResFlags) -> ResFlags {
        Self(self.0 | rhs.0)
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

unsafe impl Sync for Register {}

#[derive(Copy, Clone, Debug)]
struct Bounds {
    start: bus_size_t,
    end: bus_size_t,
}

#[derive(Debug)]
pub struct Register {
    res: *mut resource,
    bounds: Option<Bounds>,
}

impl Register {
    pub fn new(res: Resource) -> Result<Self> {
        if res.ty != Some(SYS_RES_MEMORY) {
            return Err(EDOOFUS);
        }
        Ok(Register { res: res.res, bounds: None })
    }

    pub fn as_ptr(&self) -> *mut resource {
        self.res
    }

    pub fn assert_allowed(&self, offset: bus_size_t) {
        if let Some(bounds) = self.bounds {
            assert!(bounds.start <= offset);
            assert!(offset < bounds.end);
        }
    }
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

impl<D: IsDriver> BusIfWrappers for D {}

pub trait BusIfWrappers: IsDriver {
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
        let driver = device_get_driver(dev);
        assert!(self as *const Self as *const bindings::driver_t == driver);
        let filter = unsafe { transmute(filter) };
        let handler = unsafe { transmute(handler) };
        let arg = arg.get_ref() as *const Self::Softc as *const c_void as *mut c_void;
        bus_setup_intr_internal(dev, irq, flags, filter, handler, arg, intrhand)
    }
}

pub struct RegisterBuilder<const N: usize> {
    res: *mut resource,
    claimed_windows: [Option<Bounds>; N],
    num_claimed: usize,
}

impl Resource {
    pub fn split_registers<const N: usize>(self) -> Result<RegisterBuilder<N>> {
        if self.ty != Some(SYS_RES_MEMORY) {
            return Err(EDOOFUS);
        }
        Ok(RegisterBuilder {
            res: self.res,
            claimed_windows: [const { None }; N],
            num_claimed: 0,
        })
    }
}

impl<const N: usize> RegisterBuilder<N> {
    fn is_claimed(&self, start: bus_size_t, size: bus_size_t) -> bool {
        // TODO: check overflow behavior
        let end = start + size;
        for claimed in &self.claimed_windows {
            match claimed {
                Some(window) => {
                    // Check if an existing window overlaps with the arguments. This assumes that
                    // bounds are well-formed which they must be since we calculate end from the
                    // window size
                    if window.start <= end && start <= window.end {
                        return true;
                    }
                },
                None => {
                    // When we hit an empty slot we know there bounds are not claimed
                    return false
                },
            }
        }
        panic!("Attempted to claim more than {N} registers")
    }

    pub fn take_register(&mut self, start: bus_size_t, size: bus_size_t) -> Result<Register> {
        if self.is_claimed(start, size) {
            return Err(EDOOFUS);
        }
        let end = start + size;
        let bounds = Some(Bounds { start, end });
        self.claimed_windows[self.num_claimed] = bounds;
        self.num_claimed += 1;
        Ok(Register { res: self.res, bounds })
    }
}

pub mod wrappers {
    use super::*;
    use core::ops::DerefMut;

    typesafe_c_macros! {
        ResFlags,
        RF_ACTIVE
    }

    pub fn bus_alloc_resource(
        dev: Device,
        ty: SysRes,
        mut rid: c_int,
        flags: ResFlags,
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
                flags.0 as u32,
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
            flags: RF_ACTIVE.0,
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

    macro_rules! bus_n {
        ($read_fn:ident, $read_impl:ident, $write_fn:ident, $write_impl:ident, $ty:ty) => {
            #[macro_export]
            macro_rules! $read_fn {
                ($reg:expr, $offset:expr) => {
                    {
                        use core::any::{Any, TypeId};
                        let _: $crate::bindings::bus_size_t = $offset;
                        let reg_ref = &mut $reg;
                        reg_ref.assert_allowed($offset);
                        unsafe {
                            $crate::bindings::$read_impl(reg_ref.as_ptr(), $offset)
                        }
                    }
                };
            }
            #[macro_export]
            macro_rules! $write_fn {
                ($reg:expr, $offset:expr, $value:expr) => {
                    {
                        let _: $crate::bindings::bus_size_t = $offset;
                        let _: $ty = $value;
                        let reg_ref = &mut $reg;
                        reg_ref.assert_allowed($offset);
                        unsafe {
                            $crate::bindings::$write_impl(reg_ref.as_ptr(), $offset, $value)
                        }
                    }
                };
            }
            // Needed because macro_export expanded from macro cannot be referenced by full-path in
            // prelude module
            pub use $read_fn;
            pub use $write_fn;
        };
    }
    bus_n!(bus_read_1, rust_bindings_bus_read_1, bus_write_1, rust_bindings_bus_write_1, u8);
    bus_n!(bus_read_2, rust_bindings_bus_read_2, bus_write_2, rust_bindings_bus_write_2, u16);
    bus_n!(bus_read_4, rust_bindings_bus_read_4, bus_write_4, rust_bindings_bus_write_4, u32);
    bus_n!(bus_read_8, rust_bindings_bus_read_8, bus_write_8, rust_bindings_bus_write_8, u64);
}

