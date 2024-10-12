use crate::bindings::{bus_size_t, resource, resource_spec, RF_ACTIVE};
use crate::device::Device;
use crate::intr::FilterRes;
use crate::{AsRustType, bindings, ErrCode, PointsTo, Ref, Result, UniqRef};
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
            res: self,
            rid: None,
        }
    }
}

#[derive(Debug)]
pub struct Resource {
    res: *mut resource,
    rid: Option<c_int>,
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
            Ok(Resource { res, rid: Some(rid) })
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
            let mut ret: [Resource; N] = resp.map(|r| Resource { res: r, rid: None });
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
        let res = unsafe {
            bindings::bus_setup_intr(
                dev_ptr,
                irq.res,
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
    pub fn read_4(&self, offset: bus_size_t) -> u32 {
        unsafe {
            let tag = (*self.res).r_bustag;
            let handle = (*self.res).r_bushandle;
            let f = (*tag).bs_r_4.unwrap();
            f((*tag).bs_cookie, handle, offset)
        }
    }

    pub fn write_4(&self, offset: bus_size_t, value: u32) {
        unsafe {
            let tag = (*self.res).r_bustag;
            let handle = (*self.res).r_bushandle;
            let f = (*tag).bs_w_4.unwrap();
            f((*tag).bs_cookie, handle, offset, value)
        }
    }

    pub fn read_8(&self, offset: bus_size_t) -> u64 {
        unsafe {
            let tag = (*self.res).r_bustag;
            let handle = (*self.res).r_bushandle;
            let f = (*tag).bs_r_8.unwrap();
            f((*tag).bs_cookie, handle, offset)
        }
    }

    pub fn write_8(&self, offset: bus_size_t, value: u64) {
        unsafe {
            let tag = (*self.res).r_bustag;
            let handle = (*self.res).r_bushandle;
            let f = (*tag).bs_w_8.unwrap();
            f((*tag).bs_cookie, handle, offset, value)
        }
    }
}
