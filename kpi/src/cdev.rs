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

use crate::prelude::*;
use core::ffi::c_int;
use core::ptr::NonNull;
use core::marker::PhantomData;

pub trait CDevSw {
    type Softc: 'static + Sync;
    fn on_read(dev: *mut bindings::cdev, uio: UioRef, ioflag: c_int) -> Result<()> { unimplemented!() }
    fn on_write(dev: *mut bindings::cdev, uio: UioRef, ioflag: c_int) -> Result<()> { unimplemented!() }
}

#[doc(hidden)]
#[macro_export]
macro_rules! cdev_field_for_trait_fn {
    ($res:ident, on_read) => { $res.d_read };
    ($res:ident, on_write) => { $res.d_write };
}

#[doc(hidden)]
#[macro_export]
macro_rules! c_fn_for_cdev {
    ($unmangled_name:ident, $cdev_ty:ident, $on_read_or_on_write:ident) => {
        extern "C" fn $unmangled_name(
            dev: *mut $crate::bindings::cdev,
            uio: *mut $crate::bindings::uio,
            iof: core::ffi::c_int
        ) -> core::ffi::c_int {
            let uio_ref = unsafe { $crate::cdev::UioRef::new(&uio) };
            let res = <$cdev_ty as $crate::cdev::CDevSw>::$on_read_or_on_write(dev, uio_ref, iof);
            use $crate::kobj::AsCType;
            match res {
                Ok(()) => 0,
                Err(e) => e.as_c_type(),
            }
        }
    };
}

#[macro_export]
macro_rules! define_cdev {
    (
        $cdev_ty:ident, $driver_name:expr,
        $($trait_fn:ident: $unmangled_name:ident,)*
    ) => {
        #[repr(C)]
        pub struct $cdev_ty(core::cell::UnsafeCell<$crate::bindings::cdevsw>);
        unsafe impl Sync for $cdev_ty {}

        static FOO: $cdev_ty = $cdev_ty(core::cell::UnsafeCell::new({
            use $crate::bindings::cdevsw;

            let mut res: cdevsw = unsafe { MaybeUninit::zeroed().assume_init() };
            res.d_version = $crate::bindings::D_VERSION;
            let _ty_ck: &'static core::ffi::CStr = $driver_name;
            res.d_name = $driver_name.as_ptr();
            $($crate::cdev_field_for_trait_fn!(res, $trait_fn) = Some($unmangled_name);)*
            res
        }));
        $($crate::c_fn_for_cdev!($unmangled_name, $cdev_ty, $trait_fn))*;
    };
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

    pub fn is_write(&self) -> bool { !self.is_read() }
}

#[doc(hidden)]
pub mod wrappers {
    use super::*;
    use core::ffi::c_void;
    use crate::ErrCode;

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
