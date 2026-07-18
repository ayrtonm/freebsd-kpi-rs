/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2026 Ayrton Muñoz
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

use crate::boxed::Box;
use crate::define_interface;
use crate::ffi::Ptr;
use crate::kobj::AsRustType;
use crate::malloc::{Malloc, MallocType};
use crate::misc::Thread;
use crate::prelude::*;
use core::any::TypeId;
use core::ffi::{CStr, c_int, c_void};
use core::marker::PhantomData;
use core::ptr::NonNull;
use crate::ffi::{Loan, LoanLayout, Lease};
use crate::bindings::cdev;

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct CDev<'a>(*mut cdev, PhantomData<&'a ()>);

impl<'a> CDev<'a> {
    pub fn new(ptr: *mut cdev) -> Self {
        Self(ptr, PhantomData)
    }
}

pub trait CDevSwInternal {
    fn get_cdevsw_ptr(&self) -> *mut bindings::cdevsw;
}

#[allow(unused_variables)]
pub trait CDevSw: CDevSwInternal {
    type Softc: 'static + Sync;

    fn d_open(sc: Loan<Self::Softc>, fflag: i32, devtype: i32, td: Thread) -> Result<()> {
        unimplemented!()
    }
    fn d_close(sc: Loan<Self::Softc>, fflag: i32, devtype: i32, td: Thread) -> Result<()> {
        unimplemented!()
    }
    fn d_read(sc: Loan<Self::Softc>, uio: UioRef, ioflag: c_int) -> Result<()> {
        unimplemented!()
    }
    fn d_write(sc: Loan<Self::Softc>, uio: UioRef, ioflag: c_int) -> Result<()> {
        unimplemented!()
    }
}

define_interface! {
    in CDevSw
    fn d_open(dev: *mut bindings::cdev, fflag: i32, devtype: i32, td: *mut bindings::thread) -> i32;
    fn d_close(dev: *mut bindings::cdev, fflag: i32, devtype: i32, td: *mut bindings::thread) -> i32;
    fn d_read(dev: *mut bindings::cdev, uio: *mut bindings::uio, iof: i32) -> i32;
    fn d_write(dev: *mut bindings::cdev, uio: *mut bindings::uio, iof: i32) -> i32;
}

pub struct MakeDevArgs<T, M: Malloc> {
    pub flags: i32,
    pub uid: i32,
    pub gid: i32,
    pub mode: i32,
    pub name: &'static CStr,
    sc: Box<LoanLayout<T>, M>,
    size: usize,
    cdevsw_ptr: *mut bindings::cdevsw,
}

impl<T, M: Malloc> MakeDevArgs<T, M> {
    pub fn into_raw(self) -> (bindings::make_dev_args, &'static CStr) {
        let mut args = bindings::make_dev_args::default();
        args.mda_size = self.size;
        args.mda_flags = self.flags;
        args.mda_uid = self.uid.try_into().unwrap();
        args.mda_gid = self.gid.try_into().unwrap();
        args.mda_mode = self.mode;
        args.mda_si_drv1 = Box::into_raw(self.sc).cast::<c_void>();
        // Record the softc's allocator so destroy_dev can free it later.
        args.mda_si_drv2 = M::malloc_type().as_raw().cast::<c_void>();
        args.mda_devsw = self.cdevsw_ptr;
        (args, self.name)
    }
}

#[macro_export]
macro_rules! define_cdev {
    (
        static $cdevsw_name:ident: $cdev_ty:ident = {
            d_name: $driver_name:expr,
            $($trait_fn:ident: $unmangled_name:ident,)*
        }
    ) => {
        #[repr(C)]
        pub struct $cdev_ty(core::cell::UnsafeCell<$crate::bindings::cdevsw>);
        unsafe impl Sync for $cdev_ty {}
        impl $crate::cdev::CDevSwInternal for $cdev_ty {
            fn get_cdevsw_ptr(&self) -> *mut bindings::cdevsw {
                self.0.get()
            }
        }

        static $cdevsw_name: $cdev_ty = $cdev_ty(core::cell::UnsafeCell::new({
            use $crate::bindings::cdevsw;

            let mut res: cdevsw = unsafe { MaybeUninit::zeroed().assume_init() };
            res.d_version = $crate::bindings::D_VERSION;
            let _ty_ck: &'static core::ffi::CStr = $driver_name;
            res.d_name = $driver_name.as_ptr();
            $(res.$trait_fn = Some($unmangled_name);)*
            res
        }));
        $($crate::$trait_fn!($cdev_ty $unmangled_name);)*
    };
}

impl<'a, T> AsRustType<'a, Loan<'a, T>, T> for *mut cdev {
    fn as_rust_type(&'a self) -> Loan<'a, T> {
        let dev = *self;
        let sc_ptr = unsafe { (*dev).si_drv1 };
        let res = unsafe { sc_ptr.cast::<LoanLayout<T>>().as_ref().unwrap() };
        Loan(res)
    }
}

#[derive(Copy, Clone)]
pub struct UioRef<'a>(NonNull<bindings::uio>, PhantomData<&'a bindings::uio>);

impl<'a> AsRustType<'a, UioRef<'a>> for *mut bindings::uio {
    fn as_rust_type(&'a self) -> UioRef<'a> {
        UioRef(NonNull::new(*self).unwrap(), PhantomData)
    }
}

impl<'a> UioRef<'a> {
    pub fn offset(&self) -> usize {
        unsafe { self.0.read().uio_offset.try_into().unwrap() }
    }

    pub fn resid(&self) -> usize {
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

    pub fn is_write(&self) -> bool {
        !self.is_read()
    }
}

#[doc(hidden)]
pub mod wrappers {
    use super::*;
    use crate::ErrCode;
    use core::ffi::c_void;
    use core::ptr::null_mut;

    /// Initializes a [`MakeDevArgs`] for the given cdevsw (as declared by
    /// [`define_cdev!`][crate::define_cdev]), taking ownership of the boxed softc.
    pub fn make_dev_args_init<D: CDevSw, M: Malloc>(
        sc: Box<LoanLayout<D::Softc>, M>,
        dev: &'static D,
    ) -> MakeDevArgs<D::Softc, M> {
        MakeDevArgs {
            sc,
            // Follows make_dev_args_init's behavior
            size: core::mem::size_of::<bindings::make_dev_args>(),
            flags: 0,
            uid: 0,
            gid: 0,
            mode: 0,
            name: c"",
            cdevsw_ptr: dev.get_cdevsw_ptr(),
        }
    }

    pub fn make_dev_s<T: 'static, M: Malloc>(
        args: MakeDevArgs<T, M>,
    ) -> Result<Lease<T>>
    {
        let mut outp = null_mut();
        let (mut args_raw, name) = args.into_raw();
        let sc_ptr = args_raw.mda_si_drv1.cast::<LoanLayout<T>>();
        let res = unsafe { bindings::make_dev_s(&raw mut args_raw, &raw mut outp, name.as_ptr()) };
        if res != 0 {
            // Reclaim the boxed softc that into_raw moved into mda_si_drv1 so it is dropped
            // and freed.
            drop(unsafe { Box::<LoanLayout<T>, M>::from_raw(sc_ptr) });
            return Err(ErrCode::from(res));
        }
        // Record the cdev so destroy_dev can find it later.
        unsafe { (*sc_ptr).set_cdev(outp) };
        let sc_loan = Loan(unsafe { sc_ptr.as_ref().unwrap() });
        Ok(sc_loan.lease())
    }

    /// Destroys the character device created by [`make_dev_s`] and frees its softc.
    ///
    /// The softc is freed with the allocator recorded in the cdev by `make_dev_s`. Panics if any
    /// other `Lease` to the softc is still outstanding.
    pub fn destroy_dev<T: 'static>(sc: Lease<T>) {
        let dev = sc.0.cdev();
        // The allocator the softc was boxed with, recorded in si_drv2 by make_dev_s.
        let mtype = MallocType::from_raw(unsafe { (*dev).si_drv2.cast() });
        // Blocks until all threads have left this driver's cdevsw callbacks, so no new Loans
        // can be created from the cdev afterwards.
        unsafe { bindings::destroy_dev(dev) };
        // Release our lease and the device's original reference, then free the softc.
        unsafe { sc.release_and_free(mtype) };
    }

    pub fn uiomove_read(buf: &mut [u8], uio_ref: UioRef) -> Result<()> {
        if uio_ref.is_write() {
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
}
