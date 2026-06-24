/*-
 * Copyright (c) 2024 John Baldwin <jhb@FreeBSD.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#![no_std]

use core::cell::UnsafeCell;
use core::ffi::{c_int, c_uint, c_void};
use core::marker::PhantomData;
use core::mem::MaybeUninit;
use core::ptr::{NonNull, null_mut};
use kpi::ErrCode;
use kpi::kobj::AsCType;
use kpi::prelude::*;

//static ECHO_CDEVSW: bindings::cdevsw = bindings::cdevsw {
//    d_version: bindings::D_VERSION,
//    d_name: c"echo".as_ptr(),
//
//    d_read: echodev_read,
//    ..
//};

pub struct EchoDevSoftc {}

#[repr(C)]
pub struct EchoDev(UnsafeCell<bindings::cdevsw>);
unsafe impl Sync for EchoDev {}

impl CDevSw for EchoDev {
    type Softc = EchoDevSoftc;
    fn on_read(dev: *mut bindings::cdev, uio: UioRef, ioflag: c_int) -> Result<()> {
        //if uio.offset() >= 512 {
        //    return Ok(())
        //}
        let todo = uio.remaining_bytes();
        let mut buf = [0; 32];
        uiomove_read(&mut buf, uio)?;
        Ok(())
    }
    //fn on_read(dev: &
}

impl Module for EchoDev {
    fn on_load() -> Result<()> {
        let mut args = MakeDevArgs::new();
        args.0.mda_flags = bindings::MAKEDEV_WAITOK | bindings::MAKEDEV_CHECKNAME;
        //args.0.mda_devsw = &raw mut ECHO_CDEVSW;
        // TODO: constrain this to <EchoDev as CDevSw>::Softc
        //args.0.mda_si_drv1 = sc;
        let mut outp = null_mut();
        let res = unsafe { bindings::make_dev_s(&raw mut args.0, &raw mut outp, c"echo".as_ptr()) };
        // TODO: stash the resulting out pointer
        Ok(())
    }

    fn on_unload() -> Result<()> {
        unsafe {
            //bindings::destroy_dev(
        }
        Ok(())
    }
}

//cdevsw! {
//    EchoDev, D_VERSION, c"echo",
// TODO: The following expands to the extern "C" glue that defines an unmangled echodev_read which
// calls <EchoDev as CDevSw>::on_read. The cdevsw! macro will encode the C function type signatures.
//    on_read: echodev_read,
//    on_write: echodev_write,
//}

extern "C" fn echodev_read(dev: *mut bindings::cdev, uio: *mut bindings::uio, iof: c_int) -> c_int {
    let uio_ref = unsafe { UioRef::new(&uio) };
    let res = <EchoDev as CDevSw>::on_read(dev, uio_ref, iof);
    match res {
        Ok(()) => 0,
        Err(e) => e.as_c_type(),
    }
}
static ECHO_CDEVSW: EchoDev = EchoDev(UnsafeCell::new(unsafe {
    let mut res: bindings::cdevsw = MaybeUninit::zeroed().assume_init();
    res.d_version = bindings::D_VERSION;
    res.d_name = c"echo".as_ptr();
    res.d_read = Some(echodev_read);
    res
}));

//module!(EchoDev, echodev_modevent);

// Everything below this should be in the kpi crate

pub trait CDevSw {
    type Softc: 'static + Sync;
    fn on_read(dev: *mut bindings::cdev, uio: UioRef, ioflag: c_int) -> Result<()> {
        unimplemented!()
    }
}

pub struct UioRef<'a>(NonNull<bindings::uio>, PhantomData<&'a bindings::uio>);

impl<'a> UioRef<'a> {
    pub unsafe fn new(ptr: &'a *mut bindings::uio) -> Self {
        Self(NonNull::new(*ptr).unwrap(), PhantomData)
    }

    pub fn offset(&self) -> usize {
        unsafe { self.0.read().uio_offset.try_into().unwrap() }
    }

    pub fn remaining_bytes(&self) -> usize {
        unsafe { self.0.read().uio_resid.try_into().unwrap() }
    }

    pub fn is_read(&self) -> bool {
        let flags = unsafe { self.0.read().uio_rw };
        if flags == bindings::UIO_READ {
            true
        } else {
            assert!(flags == bindings::UIO_WRITE);
            false
        }
    }
}

pub fn uiomove_read(buf: &mut [u8], uio_ref: UioRef) -> Result<()> {
    if !uio_ref.is_read() {
        return Err(EDOOFUS);
    }
    let res = unsafe {
        bindings::uiomove(
            buf.as_mut_ptr().cast::<c_void>(),
            buf.len() as i32,
            uio_ref.0.as_ptr(),
        )
    };
    if res != 0 {
        Err(ErrCode::from(res))
    } else {
        Ok(())
    }
}

pub fn uiomove_write(buf: &[u8], uio_ref: UioRef) -> Result<()> {
    if uio_ref.is_read() {
        return Err(EDOOFUS);
    }
    let res = unsafe {
        bindings::uiomove(
            buf.as_ptr().cast_mut().cast::<c_void>(),
            buf.len() as i32,
            uio_ref.0.as_ptr(),
        )
    };
    if res != 0 {
        Err(ErrCode::from(res))
    } else {
        Ok(())
    }
}

// Wrapper may not be necessary when this is in the kpi crate
pub struct MakeDevArgs(bindings::make_dev_args);

impl MakeDevArgs {
    pub fn new() -> Self {
        let mut args = bindings::make_dev_args::default();
        args.mda_size = size_of::<bindings::make_dev_args>();
        Self(args)
    }
}

pub trait Module {
    fn on_load() -> Result<()> {
        Ok(())
    }

    fn on_unload() -> Result<()> {
        Ok(())
    }
}

// This will be exapnded from module!
#[unsafe(no_mangle)]
extern "C" fn echodev_modevent(m: bindings::module_t, ty: c_int, data: *mut c_void) -> c_int {
    let res = match ty as c_uint {
        bindings::MOD_LOAD => <EchoDev as Module>::on_load(),
        bindings::MOD_UNLOAD => <EchoDev as Module>::on_unload(),
        _ => Err(ENOTSUP),
    };
    match res {
        Ok(()) => 0,
        Err(e) => e.as_c_type(),
    }
}
