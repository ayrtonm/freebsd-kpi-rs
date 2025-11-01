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
use crate::bindings::{bus_size_t, device_t, resource, resource_spec};
use crate::ffi::{Ptr, RefCountData, RefCounted};
use crate::prelude::*;
use core::ffi::{c_int, c_void};
use core::mem::transmute;
use core::ops::BitOr;
use core::ptr::{addr_of_mut, null_mut};
use core::sync::atomic::{AtomicPtr, Ordering};

pub mod dma;

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SysRes(c_int);

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Filter(c_int);

#[repr(C)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ResFlags(i32);

impl BitOr<ResFlags> for ResFlags {
    type Output = ResFlags;

    fn bitor(self, rhs: ResFlags) -> ResFlags {
        Self(self.0 | rhs.0)
    }
}

pub type RawFilterFn = Option<unsafe extern "C" fn(*mut c_void) -> i32>;
type RawHandler = Option<unsafe extern "C" fn(*mut c_void)>;

pub type FilterFn<T> = Option<extern "C" fn(&RefCounted<T>) -> Filter>;
type Handler<T> = Option<extern "C" fn(&RefCounted<T>)>;

impl AsRustType<Resource> for *mut resource {
    fn as_rust_type(self) -> Resource {
        Resource {
            res: self,
            rid: None,
            ty: None,
        }
    }
}

impl AsCType<*mut resource> for Resource {
    fn as_c_type(self) -> *mut resource {
        self.res
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

// Sync is explicitly not implemented for Register

impl Register {
    pub fn new(res: Resource) -> Result<Self> {
        if res.ty != Some(SYS_RES_MEMORY) {
            return Err(EDOOFUS);
        }
        Ok(Register {
            res: res.res,
            bounds: None,
        })
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

#[derive(Debug, Default)]
pub struct Irq {
    res: *mut resource,
    cookie: *mut c_void,
    metadata_ptr: AtomicPtr<RefCountData>,
}

unsafe impl Sync for Irq {}

#[derive(Debug)]
pub struct ResourceSpec {
    ty: SysRes,
    rid: c_int,
}

impl ResourceSpec {
    pub const fn new(ty: SysRes, rid: c_int) -> Self {
        Self { ty, rid }
    }
}

pub struct RegisterBuilder<const N: usize> {
    res: *mut resource,
    claimed_windows: [Option<Bounds>; N],
    num_claimed: usize,
}

impl Resource {
    pub fn as_irq(self) -> Result<Irq> {
        if self.ty != Some(SYS_RES_IRQ) {
            return Err(EDOOFUS);
        }
        Ok(Irq {
            res: self.res,
            cookie: null_mut(),
            metadata_ptr: AtomicPtr::new(null_mut()),
        })
    }

    pub fn as_register(self) -> Result<Register> {
        if self.ty != Some(SYS_RES_MEMORY) {
            return Err(EDOOFUS);
        }
        Ok(Register {
            res: self.res,
            bounds: None,
        })
    }

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
                    if window.start < end && start < window.end {
                        return true;
                    }
                }
                None => {
                    // When we hit an empty slot we know there bounds are not claimed
                    return false;
                }
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
        Ok(Register {
            res: self.res,
            bounds,
        })
    }
}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;
    use crate::ffi::OwnedPtr;
    use bindings::{bus_space_handle_t, bus_space_tag_t};

    gen_newtype! {
        SysRes,
        SYS_RES_MEMORY,
        SYS_RES_IRQ,
    }

    gen_newtype! {
        Filter,
        FILTER_STRAY,
        FILTER_HANDLED,
        FILTER_SCHEDULE_THREAD,
    }

    gen_newtype! {
        ResFlags,
        RF_ACTIVE,
        RF_SHAREABLE,
        RF_UNMAPPED
    }

    pub fn bus_setup_intr<T>(
        dev: device_t,
        irq: &Irq,
        flags: u32,
        filter_arg: FilterFn<T>,
        handler_arg: Handler<T>,
        arg: Ptr<T>,
    ) -> Result<()> {
        if filter_arg.is_none() && handler_arg.is_none() {
            return Err(EDOOFUS);
        }
        // SAFETY: These types are ABI-compatible
        let filter = unsafe { transmute::<FilterFn<T>, RawFilterFn>(filter_arg) };
        // SAFETY: These types are ABI-compatible
        let handler = unsafe { transmute::<Handler<T>, RawHandler>(handler_arg) };

        // TODO: is this right?
        // arg_ptr is stored in the struct Irq then the Ptr is reconstructed and dropped in
        // bus_teardown_intr
        let (arg_ptr, metadata_ptr) = OwnedPtr::into_raw_parts(arg);
        irq.metadata_ptr
            .store(metadata_ptr.unwrap(), Ordering::Relaxed);
        let cookiep = &raw const irq.cookie;

        let res = unsafe {
            bindings::bus_setup_intr(
                dev,
                irq.res,
                flags as c_int,
                filter,
                handler,
                arg_ptr.cast::<c_void>(),
                cookiep.cast_mut(),
            )
        };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }

    pub fn bus_teardown_intr(dev: device_t, irq: &Irq) -> Result<()> {
        let res = unsafe { bindings::bus_teardown_intr(dev, irq.res, irq.cookie) };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            let metadata_ptr = irq.metadata_ptr.load(Ordering::Relaxed);
            // SAFETY: The refcount was owned by the Ptr<T> passed in to and broken down to its raw
            // pars in bus_setup_intr
            unsafe {
                RefCountData::release_ref(metadata_ptr);
            }
            Ok(())
        }
    }
    pub fn rman_get_bustag(res: &Resource) -> bus_space_tag_t {
        unsafe { bindings::rman_get_bustag(res.res) }
    }

    pub fn rman_get_bushandle(res: &Resource) -> bus_space_handle_t {
        unsafe { bindings::rman_get_bushandle(res.res) }
    }

    pub fn bus_alloc_resource_any(
        dev: device_t,
        ty: SysRes,
        mut rid: c_int,
        flags: ResFlags,
    ) -> Result<Resource> {
        // TODO: as u32 needed because bindgen flag makes macros default to signed, but RF_ACTIVE is
        // a bitfield. Ideally there'd be a heuristic for choosing signedness in cases like this
        let res = unsafe {
            bindings::bus_alloc_resource(
                dev,
                ty.0,
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
        dev: device_t,
        spec: [ResourceSpec; N],
    ) -> Result<[Resource; N]> {
        #[repr(C)]
        struct NullTerminated<const N: usize> {
            list: [resource_spec; N],
            null: resource_spec,
        }

        let list = spec.map(|s| resource_spec {
            type_: s.ty.0,
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
            bindings::bus_alloc_resources(dev, addr_of_mut!(spec).cast(), resp.as_mut_ptr())
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
                ret[n].ty = Some(SysRes(spec.list[n].type_));
            }
            Ok(ret)
        }
    }

    macro_rules! bus_n {
        ($read_fn:ident, $read_impl:ident, $write_fn:ident, $write_impl:ident, $ty:ty) => {
            #[macro_export]
            macro_rules! $read_fn {
                ($reg:expr, $offset:expr) => {{
                    let _: $crate::bindings::bus_size_t = $offset;
                    let reg_ref = &mut $reg;
                    reg_ref.assert_allowed($offset);
                    unsafe { $crate::bindings::$read_impl(reg_ref.as_ptr(), $offset) }
                }};
            }
            #[macro_export]
            macro_rules! $write_fn {
                ($reg:expr, $offset:expr, $value:expr) => {{
                    let _: $crate::bindings::bus_size_t = $offset;
                    let _: $ty = $value;
                    let reg_ref = &mut $reg;
                    reg_ref.assert_allowed($offset);
                    unsafe { $crate::bindings::$write_impl(reg_ref.as_ptr(), $offset, $value) }
                }};
            }
            // Needed because macro_export expanded from macro cannot be referenced by full-path in
            // prelude module
            pub use {$read_fn, $write_fn};
        };
    }
    bus_n!(
        bus_read_1,
        rust_bindings_bus_read_1,
        bus_write_1,
        rust_bindings_bus_write_1,
        u8
    );
    bus_n!(
        bus_read_2,
        rust_bindings_bus_read_2,
        bus_write_2,
        rust_bindings_bus_write_2,
        u16
    );
    bus_n!(
        bus_read_4,
        rust_bindings_bus_read_4,
        bus_write_4,
        rust_bindings_bus_write_4,
        u32
    );
    bus_n!(
        bus_read_8,
        rust_bindings_bus_read_8,
        bus_write_8,
        rust_bindings_bus_write_8,
        u64
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::tests::{IrqDriver, irq_driver};
    use crate::device::{BusProbe, DeviceIf};
    use crate::ffi::Uninit;
    use crate::tests::{DriverManager, LoudDrop};

    #[repr(C)]
    #[derive(Debug)]
    pub struct IrqSoftc {
        dev: device_t,
        irq: Irq,
        loud: LoudDrop,
    }

    impl IrqDriver {
        fn setup(dev: device_t, sc: &RefCounted<IrqSoftc>, filter: bool, handler: bool) {
            let filter = if filter {
                Some(IrqDriver::filter as _)
            } else {
                None
            };
            let handler = if handler {
                Some(IrqDriver::handler as _)
            } else {
                None
            };
            bus_setup_intr(dev, &sc.irq, 0, filter, handler, sc.grab_ref()).unwrap();
        }
    }
    impl DeviceIf for IrqDriver {
        type Softc = IrqSoftc;
        fn device_probe(dev: device_t) -> Result<BusProbe> {
            if !ofw_bus_is_compatible(dev, c"bus,irq_driver") {
                return Err(ENXIO);
            }
            Ok(BUS_PROBE_DEFAULT)
        }
        fn device_attach(uninit_sc: &mut Uninit<Self::Softc>, dev: device_t) -> Result<()> {
            let sc = uninit_sc.init(IrqSoftc {
                dev,
                irq: Irq::default(),
                loud: LoudDrop,
            });
            assert_eq!(
                bus_setup_intr(dev, &sc.irq, 0, None, None, sc.grab_ref()),
                Err(EDOOFUS)
            );
            if ofw_bus_is_compatible(dev, c"irq_driver,set_both") {
                IrqDriver::setup(dev, &sc, true, true);
            }
            if ofw_bus_is_compatible(dev, c"irq_driver,set_filter") {
                IrqDriver::setup(dev, &sc, true, false);
            }
            if ofw_bus_is_compatible(dev, c"irq_driver,set_handler") {
                IrqDriver::setup(dev, &sc, false, true);
            }
            Ok(())
        }
        fn device_detach(sc: &RefCounted<Self::Softc>, dev: device_t) -> Result<()> {
            if ofw_bus_is_compatible(dev, c"irq_driver,teardown_intr") {
                bus_teardown_intr(dev, &sc.irq).unwrap();
            }
            Ok(())
        }
    }

    impl IrqDriver {
        extern "C" fn filter(sc: &RefCounted<IrqSoftc>) -> Filter {
            println!("called filter");
            if ofw_bus_is_compatible(sc.dev, c"irq_driver,filter_handled") {
                FILTER_HANDLED
            } else {
                FILTER_SCHEDULE_THREAD
            }
        }
        extern "C" fn handler(sc: &RefCounted<IrqSoftc>) {
            println!("called handler {sc:x?}");
        }
    }

    #[test]
    fn filter_only() {
        let mut m = DriverManager::new();
        let dev = m.add_test_device(c"bus,irq_driver");
        dev.compat_strs.push(c"irq_driver,set_filter");
        let dev = dev.dev;
        m.drivers.push(&irq_driver);
        m.probe_all();
        m.attach_all();
        m.trigger_irq(dev);
        m.detach_all();
    }

    #[test]
    fn handler_only() {
        let mut m = DriverManager::new();
        let dev = m.add_test_device(c"bus,irq_driver");
        dev.compat_strs.push(c"irq_driver,set_handler");
        let dev = dev.dev;
        m.drivers.push(&irq_driver);
        m.probe_all();
        m.attach_all();
        m.trigger_irq(dev);
        m.detach_all();
    }

    #[test]
    fn filter_handler() {
        let mut m = DriverManager::new();

        // Set up a device that will do everything in the filter
        let d1 = m.add_test_device(c"bus,irq_driver");
        d1.compat_strs.push(c"irq_driver,set_both");
        d1.compat_strs.push(c"irq_driver,teardown_intr");
        let d1 = d1.dev;

        m.drivers.push(&irq_driver);
        m.probe_all();
        m.attach_all();

        m.trigger_irq(d1);

        m.detach_all();
    }

    #[test]
    fn filter_handler_resched() {
        let mut m = DriverManager::new();

        // Set up a device that will do everything in the filter
        let d1 = m.add_test_device(c"bus,irq_driver");
        d1.compat_strs.push(c"irq_driver,set_both");
        d1.compat_strs.push(c"irq_driver,filter_handled");
        d1.compat_strs.push(c"irq_driver,teardown_intr");
        let d1 = d1.dev;

        // Set up another device that will schedule the handler from the filter
        let d2 = m.add_test_device(c"bus,irq_driver");
        d2.compat_strs.push(c"irq_driver,set_both");
        d2.compat_strs.push(c"irq_driver,teardown_intr");
        let d2 = d2.dev;

        m.drivers.push(&irq_driver);
        m.probe_all();
        m.attach_all();

        m.trigger_irq(d1);
        m.trigger_irq(d2);

        m.detach_all();
    }

    #[test]
    fn filter_handler_two_irqs() {
        let mut m = DriverManager::new();

        // Set up a device that will do everything in the filter
        let d1 = m.add_test_device(c"bus,irq_driver");
        d1.compat_strs.push(c"irq_driver,set_both");
        d1.compat_strs.push(c"irq_driver,filter_handled");
        d1.compat_strs.push(c"irq_driver,teardown_intr");
        let d1 = d1.dev;

        // Set up another device that will schedule the handler from the filter
        let d2 = m.add_test_device(c"bus,irq_driver");
        d2.compat_strs.push(c"irq_driver,set_both");
        d2.compat_strs.push(c"irq_driver,teardown_intr");
        let d2 = d2.dev;

        m.drivers.push(&irq_driver);
        m.probe_all();
        m.attach_all();

        m.trigger_irq(d1);
        m.trigger_irq(d2);

        m.trigger_irq(d1);
        m.trigger_irq(d2);

        m.detach_all();
    }

    #[test]
    fn sequential_setup() {
        let mut m = DriverManager::new();
        let dev = m.add_test_device(c"bus,irq_driver");
        dev.compat_strs.push(c"irq_driver,set_filter");
        dev.compat_strs.push(c"irq_driver,set_handler");
        let dev = dev.dev;
        m.drivers.push(&irq_driver);
        m.probe_all();
        m.attach_all();
        m.trigger_irq(dev);
        m.detach_all();
    }
}
