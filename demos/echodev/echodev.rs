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
use kpi::module::Module;
use core::ptr::{NonNull, null_mut};
use kpi::ErrCode;
use kpi::prelude::*;
use kpi::{define_module,define_cdev};
use kpi::cdev::{UioRef, CDevSw};

pub struct EchoDevSoftc {}

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
    fn on_load(data: *mut c_void) -> Result<()> {
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

    fn on_unload(data: *mut c_void) -> Result<()> {
        unsafe {
            //bindings::destroy_dev(
        }
        Ok(())
    }
}

define_cdev! {
    EchoDev, c"echo",
    on_read: echodev_read,
}

define_module!(EchoDev, echodev_modevent);

// Everything below this should be in the kpi crate

// Wrapper may not be necessary when this is in the kpi crate
pub struct MakeDevArgs(bindings::make_dev_args);

impl MakeDevArgs {
    pub fn new() -> Self {
        let mut args = bindings::make_dev_args::default();
        args.mda_size = size_of::<bindings::make_dev_args>();
        Self(args)
    }
}
