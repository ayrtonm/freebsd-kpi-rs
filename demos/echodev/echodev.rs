/*-
 * Copyright (c) 2024 John Baldwin <jhb@FreeBSD.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#![no_std]

use kpi::prelude::*;
use kpi::kobj::AsCType;
use core::ffi::{c_void, c_int, c_uint};
use core::ptr::null_mut;

//static ECHO_CDEVSW: bindings::cdevsw = bindings::cdevsw {
//    d_version: bindings::D_VERSION,
//    d_name: c"echo".as_ptr(),
//
//    d_read: echodev_read,
//    ..
//};

pub struct EchoDevSoftc {
}

pub struct EchoDev;

impl CDevSw for EchoDev {
    type Softc = EchoDevSoftc;
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
        let res = unsafe {
            bindings::make_dev_s(&raw mut args.0, &raw mut outp, c"echo".as_ptr())
        };
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

//module!(EchoDev, echodev_modevent);

// Everything below this should be in the kpi crate

pub trait CDevSw {
    type Softc: 'static + Sync;
    fn on_read(dev: *mut bindings::cdev, uio: *mut bindings::uio, ioflag: c_int) -> Result<()> {
        unimplemented!()
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
