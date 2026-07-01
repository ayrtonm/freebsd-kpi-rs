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
use crate::device::{MemoryManager, MemoryRegion};
use crate::ffi::Ptr;
use crate::malloc::Malloc;
use crate::prelude::*;
use core::any::TypeId;
use core::ffi::{CStr, c_int, c_void};
use core::marker::PhantomData;
use core::mem::size_of;
use crate::define_interface;
use crate::kobj::AsRustType;
use core::ptr::NonNull;
use crate::misc::Thread;

#[allow(non_camel_case_types)]
pub struct cdev_t(Ptr<bindings::cdev>, Option<TypeId>);

impl cdev_t {
    pub const fn new() -> Self {
        Self(Ptr::null(), None)
    }
}

impl Default for cdev_t {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Default)]
pub struct CDev {
    ptr: cdev_t,
    region: MemoryRegion,
}

impl CDev {
    pub fn as_raw_ptr(&self) -> *mut bindings::cdev {
        self.ptr.0.as_ptr()
    }
}

impl Drop for CDev {
    fn drop(&mut self) {
        unsafe { self.region.free_allocation_list() }
    }
}

impl MemoryManager for CDev {
    fn region(&self) -> &MemoryRegion {
        &self.region
    }
}

pub trait CDevSwInternal {
    fn get_cdevsw_ptr() -> *mut bindings::cdevsw;
}

#[allow(unused_variables)]
pub trait CDevSw: CDevSwInternal {
    type Softc: 'static + Sync;
    type MallocType: Malloc;

    fn on_open(sc: &Self::Softc, fflag: i32, devtype: i32, td: Thread) -> Result<()> {
        unimplemented!()
    }
    fn on_close(sc: &Self::Softc, fflag: i32, devtype: i32, td: Thread) -> Result<()> {
        unimplemented!()
    }
    fn on_read(sc: &Self::Softc, uio: UioRef, ioflag: c_int) -> Result<()> {
        unimplemented!()
    }
    fn on_write(sc: &Self::Softc, uio: UioRef, ioflag: c_int) -> Result<()> {
        unimplemented!()
    }
    fn make_dev_args_init(
        sc: Box<Self::Softc, Self::MallocType>,
    ) -> MakeDevArgs<Self::Softc, Self::MallocType> {
        MakeDevArgs {
            sc,
            // Follows make_dev_args_init's behavior
            size: size_of::<bindings::make_dev_args>(),
            flags: 0,
            uid: 0,
            gid: 0,
            mode: 0,
            name: c"",
            cdevsw_ptr: Self::get_cdevsw_ptr(),
        }
    }

    fn destroy_dev(dev: cdev_t) {
        assert!(dev.1.unwrap() == TypeId::of::<Self::Softc>());
        let sc_ptr = unsafe { (*dev.0.as_ptr()).si_drv1 };
        unsafe { bindings::destroy_dev(dev.0.as_ptr()) };
        let sc: Box<Self::Softc, Self::MallocType> =
            unsafe { Box::from_raw(sc_ptr.cast::<Self::Softc>()) };
        drop(sc);
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! cdev_field_for_trait_fn {
    ($res:ident, on_read) => { $res.d_read };
    ($res:ident, on_write) => { $res.d_write };
    ($res:ident, on_open) => { $res.d_open };
    ($res:ident, on_close) => { $res.d_close };
}

define_interface! {
    in CDevSw
    fn on_open(dev: *mut bindings::cdev, fflag: i32, devtype: i32, td: *mut bindings::thread) -> i32;
    fn on_close(dev: *mut bindings::cdev, fflag: i32, devtype: i32, td: *mut bindings::thread) -> i32;
    fn on_read(dev: *mut bindings::cdev, uio: *mut bindings::uio, iof: i32) -> i32;
    fn on_write(dev: *mut bindings::cdev, uio: *mut bindings::uio, iof: i32) -> i32;
}

pub struct MakeDevArgs<T, M: Malloc> {
    pub flags: i32,
    pub uid: i32,
    pub gid: i32,
    pub mode: i32,
    pub name: &'static CStr,
    sc: Box<T, M>,
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
        args.mda_devsw = self.cdevsw_ptr;
        (args, self.name)
    }
}

#[macro_export]
macro_rules! define_cdev {
    (
        $cdev_ty:ident, $driver_name:expr, $cdevsw_name:ident,
        $($trait_fn:ident: $unmangled_name:ident,)*
    ) => {
        #[repr(C)]
        pub struct $cdev_ty(core::cell::UnsafeCell<$crate::bindings::cdevsw>);
        unsafe impl Sync for $cdev_ty {}
        impl $crate::cdev::CDevSwInternal for $cdev_ty {
            fn get_cdevsw_ptr() -> *mut bindings::cdevsw {
                $cdevsw_name.0.get()
            }
        }

        static $cdevsw_name: $cdev_ty = $cdev_ty(core::cell::UnsafeCell::new({
            use $crate::bindings::cdevsw;

            let mut res: cdevsw = unsafe { MaybeUninit::zeroed().assume_init() };
            res.d_version = $crate::bindings::D_VERSION;
            let _ty_ck: &'static core::ffi::CStr = $driver_name;
            res.d_name = $driver_name.as_ptr();
            $($crate::cdev_field_for_trait_fn!(res, $trait_fn) = Some($unmangled_name);)*
            res
        }));
        $($crate::$trait_fn!($cdev_ty x $unmangled_name);)*
    };
}

impl<'a, T> AsRustType<'a, &'a T, T> for *mut bindings::cdev {
    fn as_rust_type(&'a self) -> &'a T {
        let dev = *self;
        let sc_ptr = unsafe { (*dev).si_drv1 };
        unsafe { sc_ptr.cast::<T>().as_ref().unwrap() }
    }
}

#[derive(Copy, Clone)]
pub struct UioRef<'a>(NonNull<bindings::uio>, PhantomData<&'a bindings::uio>);

impl<'a> AsRustType<'a, UioRef<'a>> for *mut bindings::uio {
    fn as_rust_type(&'a self) -> UioRef {
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

    pub fn make_dev_s<T: 'static, M: Malloc, F>(
        args: MakeDevArgs<T, M>,
        sc_init: F,
    ) -> Result<cdev_t>
    where
        F: Fn(&mut T, CDev),
    {
        let mut outp = null_mut();
        let (mut args_raw, name) = args.into_raw();
        let sc_ptr = args_raw.mda_si_drv1.cast::<T>();
        let res = unsafe { bindings::make_dev_s(&raw mut args_raw, &raw mut outp, name.as_ptr()) };
        if res != 0 {
            // FIXME: This leaks the softc
            return Err(ErrCode::from(res));
        }
        let sc_mut_ref = unsafe { sc_ptr.as_mut().unwrap() };
        let raw_ptr = Ptr::new(outp);
        let sc_start = sc_ptr.addr();
        let sc_end = sc_start + size_of::<T>();
        let res = cdev_t(raw_ptr, Some(TypeId::of::<T>()));
        let dev = CDev {
            ptr: cdev_t(raw_ptr, Some(TypeId::of::<T>())),
            region: MemoryRegion::new(sc_start, sc_end),
        };
        sc_init(sc_mut_ref, dev);
        Ok(res)
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
