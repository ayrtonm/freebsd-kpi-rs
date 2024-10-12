use crate::bus::Resource;
use crate::bindings::{pcell_t, intr_irqsrc, intr_map_data, intr_map_data_fdt, intr_pic, trapframe};
use crate::device::Device;
use crate::ofw::XRef;
use crate::{bindings, AsRustType, ErrCode, PointsTo, Ptr, Ref, Result , SharedValue, SuperClass};
use core::marker::PhantomData;
use core::ops::Deref;
use core::cell::UnsafeCell;
use core::ffi::{c_int, c_void, CStr};
use core::mem::{transmute, MaybeUninit};

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

impl<'a, T> AsRustType<&'a SuperClass<IrqSrc, T>> for *mut intr_irqsrc {
    fn as_rust_type(self) -> &'a SuperClass<IrqSrc, T> {
        unsafe {
            SuperClass::from_base(self)
        }
    }
}

impl AsRustType<MapData> for *mut intr_map_data {
    fn as_rust_type(self) -> MapData {
        MapData(Ptr::new(self))
    }
}

impl AsRustType<*mut Ptr<IrqSrc>> for *mut *mut intr_irqsrc {
    fn as_rust_type(self) -> *mut Ptr<IrqSrc> {
        self.cast()
    }
}

pub type IrqSrc = intr_irqsrc;

impl<T> SuperClass<IrqSrc, T> {
    pub fn isrc_dispatch(&self, tf: *mut trapframe) -> c_int {
        unsafe {
            bindings::intr_isrc_dispatch(self.base_ptr(), tf)
        }
    }
}

pub trait PicIf<T> {
    fn isrc_register(&self, dev: Device, isrc: &SuperClass<IrqSrc, T>, flags: i32, desc: &CStr) -> Result<()> {
        let res = unsafe {
            bindings::intr_isrc_register(isrc.base_ptr(), dev.as_ptr(), flags as u32, desc.as_ptr())
        };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }
    fn pic_setup_intr(&self, dev: Device, isrc: &SuperClass<IrqSrc, T>, res: Resource, data: MapData) -> Result<()>;
    //fn pic_map_intr(&self, dev: Device, data: MapData, isrcp: *mut Ptr<IrqSrc<T>>) -> Result<()>;
    //fn pic_teardown_intr(&self, dev: Device, isrc: Ptr<IrqSrc<T>>, res: Resource, data: MapData) -> Result<()>;

    fn pic_disable_intr(&self, dev: Device, isrc: &SuperClass<IrqSrc, T>);
    fn pic_enable_intr(&self, dev: Device, isrc: &SuperClass<IrqSrc, T>);

    //fn pic_post_filter(&self, dev: Device, isrc: IrqSrc);
    //fn pic_post_ithread(&self, dev: Device, isrc: IrqSrc);
    //fn pic_pre_ithread(&self, dev: Device, isrc: IrqSrc);
}

#[derive(Debug)]
#[repr(transparent)]
pub struct MapData(Ptr<intr_map_data>);

#[derive(Debug)]
pub enum MapDataContents {
    ACPI,
    FDT(Ref<intr_map_data_fdt>),
    GPIO,
    MSI,
    Unknown,
}

impl Ref<intr_map_data_fdt> {
    pub fn cells(&self) -> &[pcell_t] {
        unsafe {
            self.cells.as_slice(self.ncells as usize)
        }
    }
}

impl MapData {
    pub fn get_contents(&self) -> MapDataContents {
        let map_data_ptr = self.0.as_ptr();
        let ty = unsafe { (*map_data_ptr).type_ };
        match ty {
            bindings::INTR_MAP_DATA_ACPI => MapDataContents::ACPI,
            bindings::INTR_MAP_DATA_FDT => {
                let fdt_data_ptr = Ptr::new(map_data_ptr.cast());
                // this should be fine if the C side doesn't touch the fdt data for the lifetime of the Ref
                let fdt_data_ref = unsafe {
                    fdt_data_ptr.can_ref()
                };
                MapDataContents::FDT(fdt_data_ref)
            }
            bindings::INTR_MAP_DATA_GPIO => MapDataContents::GPIO,
            bindings::INTR_MAP_DATA_MSI => MapDataContents::MSI,
            _ => MapDataContents::Unknown,
        }
    }
}

pub struct Pic {
    dev: Device,
    xref: XRef,
    pic: *const intr_pic,
}

impl Device {
    pub fn pic_register(&self, xref: XRef) -> Result<Pic> {
        let dev_ptr = self.as_ptr();
        let pic = unsafe { bindings::intr_pic_register(dev_ptr, xref.0 as _) };
        if pic.is_null() {
            Err(ErrCode::ENULLPTR)
        } else {
            Ok(Pic {
                dev: *self,
                xref,
                pic,
            })
        }
    }

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
type Filter<T> = extern "C" fn(T) -> FilterRes;

impl Pic {
    pub fn claim_root<SC, P: PointsTo<SC>>(
        &mut self,
        filter: Filter<P>,
        arg: P,
        root: IntrRoot,
    ) -> Result<()> {
        let filter = unsafe { transmute(filter) };
        let arg = arg.as_ptr().cast();
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
