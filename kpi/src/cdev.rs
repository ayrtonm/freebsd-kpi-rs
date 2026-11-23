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

use crate::bindings::cdev;
use crate::define_interface;
use crate::ffi::{Ptr, Softc};
use crate::kobj::AsRustType;
use crate::prelude::*;
use crate::proc::Thread;
use core::ffi::{CStr, c_int, c_void};
use core::sync::atomic::Ordering;

define_wrapper_type!(CDev, cdev);

impl CDev {
    define_getter!(si_drv1, *mut c_void);
    define_getter!(si_drv2, *mut c_void);
}

pub trait CDevSwInternal {
    fn get_cdevsw_ptr(&self) -> *mut bindings::cdevsw;
}

#[allow(unused_variables)]
pub trait CDevSw: CDevSwInternal {
    type Softc: 'static + Sync;

    fn d_open(sc: &Softc<Self::Softc>, fflag: i32, devtype: i32, td: &Thread) -> Result<()> {
        unimplemented!()
    }
    fn d_close(sc: &Softc<Self::Softc>, fflag: i32, devtype: i32, td: &Thread) -> Result<()> {
        unimplemented!()
    }
    fn d_read(sc: &Softc<Self::Softc>, uio: &Uio, ioflag: c_int) -> Result<()> {
        unimplemented!()
    }
    fn d_write(sc: &Softc<Self::Softc>, uio: &Uio, ioflag: c_int) -> Result<()> {
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

pub struct MakeDevArgs<T: 'static> {
    pub flags: i32,
    pub uid: i32,
    pub gid: i32,
    pub mode: i32,
    pub name: &'static CStr,
    sc: Ptr<T>,
    size: usize,
    cdevsw_ptr: *mut bindings::cdevsw,
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
            use $cdevsw_name::*;

            let mut res: cdevsw = unsafe { core::mem::MaybeUninit::zeroed().assume_init() };
            res.d_version = $crate::bindings::D_VERSION;
            let _ty_ck: &'static core::ffi::CStr = $driver_name;
            res.d_name = $driver_name.as_ptr();
            $(res.$trait_fn = Some($unmangled_name);)*
            res
        }));

        #[allow(nonstandard_style)]
        mod $cdevsw_name {
            use $crate::*;
            use super::$cdev_ty;
            use $crate::cdev::CDevSw;
            $($trait_fn!($cdev_ty $unmangled_name);)*
        }
    };
}

impl<'a, T> AsRustType<'a, &'a Softc<T>, T> for *mut cdev {
    fn as_rust_type(&'a self) -> &'a Softc<T> {
        let dev = *self;
        let sc_ptr = unsafe { (*dev).si_drv1 };
        unsafe { sc_ptr.cast::<Softc<T>>().as_ref().unwrap() }
    }
}

define_wrapper_type!(Uio, uio);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SegFlag {
    UserSpace,
    SysSpace,
    NoCopy,
}

impl Uio {
    define_getter!(uio_offset, i64);
    define_getter!(uio_resid, force into usize);
    define_getter!(uio_rw, u32);
    define_getter!(uio_td, *mut crate::bindings::thread);
    define_getter!(uio_segflg, u32);
    define_getter!(uio_iov, *mut crate::bindings::iovec);
    define_getter!(uio_iovcnt, force into usize);

    pub fn thread(&self) -> Option<&Thread> {
        let ptr = self.uio_td();
        if ptr.is_null() {
            None
        } else {
            unsafe { Some(ptr.cast::<Thread>().as_ref().unwrap()) }
        }
    }

    pub fn segment_flag(&self) -> Result<SegFlag> {
        match self.uio_segflg() {
            bindings::UIO_USERSPACE => Ok(SegFlag::UserSpace),
            bindings::UIO_SYSSPACE => Ok(SegFlag::SysSpace),
            bindings::UIO_NOCOPY => Ok(SegFlag::NoCopy),
            _ => Err(EINVAL),
        }
    }

    pub fn is_buffer_to_iov(&self) -> bool {
        self.uio_rw() == bindings::UIO_READ
    }

    pub fn is_iov_to_buffer(&self) -> bool {
        self.uio_rw() == bindings::UIO_WRITE
    }
}

#[doc(hidden)]
pub mod wrappers {
    use super::*;
    use crate::ErrCode;
    use core::ffi::c_void;
    use core::ptr::null_mut;

    /// Initializes a [`MakeDevArgs`] for the given cdevsw (as declared by [`define_cdev!`][crate::define_cdev]).
    pub fn make_dev_args_init<D: CDevSw>(
        sc: Ptr<D::Softc>,
        dev: &'static D,
    ) -> MakeDevArgs<D::Softc> {
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

    pub fn make_dev_s<T: 'static>(args: MakeDevArgs<T>) -> Result<Ptr<T>> {
        let mut outp = null_mut();

        let mut args_raw = bindings::make_dev_args::default();
        args_raw.mda_size = args.size;
        args_raw.mda_flags = args.flags;
        args_raw.mda_uid = args.uid.try_into().unwrap();
        args_raw.mda_gid = args.gid.try_into().unwrap();
        args_raw.mda_mode = args.mode;
        let (sc_ptr, _count_ptr) = Ptr::into_raw(args.sc);
        args_raw.mda_si_drv1 = sc_ptr.cast::<c_void>();
        args_raw.mda_devsw = args.cdevsw_ptr;

        let res =
            unsafe { bindings::make_dev_s(&raw mut args_raw, &raw mut outp, args.name.as_ptr()) };
        if res != 0 {
            // Reclaim the softc Ptr that into_raw moved into mda_si_drv1 so it is dropped
            drop(unsafe { Ptr::from_raw(sc_ptr) });
            return Err(ErrCode::from(res));
        }
        let sc = unsafe { Ptr::from_raw(sc_ptr) };
        sc.cdev.store(outp, Ordering::Relaxed);
        let res = sc.acquire_ptr();
        // This is si_drv1's refcount so don't release it
        Ptr::into_raw(sc);
        Ok(res)
    }

    /// Destroys the character device created by [`make_dev_s`] and frees its softc.
    ///
    /// The softc is freed with the allocator recorded in the cdev by `make_dev_s`. Panics if any
    /// other `Ptr` to the softc is still outstanding.
    pub fn destroy_dev<T: 'static>(sc: Ptr<T>) {
        let dev = sc.cdev();

        // Blocks until all threads have left this driver's cdevsw callbacks, so no new Refs
        // can be created from the cdev afterwards.
        unsafe { bindings::destroy_dev(dev.as_ptr()) };

        // Release the arg's refcount and the char device's original reference, then free the softc.
        let (_sc_ptr, count_ptr) = Softc::as_raw(&sc);

        let last = unsafe { bindings::refcount_release(count_ptr) };
        assert!(!last);
        Ptr::free(sc);
    }

    pub fn uiomove_read(buf: &mut [u8], uio_ref: &Uio) -> Result<()> {
        if uio_ref.is_buffer_to_iov() {
            return Err(EDOOFUS);
        }
        let res = unsafe {
            bindings::uiomove(
                buf.as_mut_ptr().cast::<c_void>(),
                buf.len() as i32,
                uio_ref.as_ptr(),
            )
        };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }

    pub fn uiomove_write(buf: &[u8], uio_ref: &Uio) -> Result<()> {
        if uio_ref.is_iov_to_buffer() {
            return Err(EDOOFUS);
        }
        let res = unsafe {
            bindings::uiomove(
                buf.as_ptr().cast_mut().cast::<c_void>(),
                buf.len() as i32,
                uio_ref.as_ptr(),
            )
        };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        }
    }
}

#[allow(dead_code)]
#[cfg(test)]
mod tests {
    use super::*;

    pub struct TestCDevSoftc;

    impl CDevSw for TestCDev {
        type Softc = TestCDevSoftc;
    }

    define_cdev! {
        static TEST_CDEVSW: TestCDev = {
            d_name: c"test_cdev",
            d_read: test_cdev_read,
            d_write: test_cdev_write,
        }
    }
}
