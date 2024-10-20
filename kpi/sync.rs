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

use crate::bindings::{mtx, LO_INITIALIZED, MTX_DEF, MTX_SPIN};
use crate::kpi_prelude::*;
use core::cell::UnsafeCell;
use core::ffi::CStr;
use core::ops::{Deref, DerefMut};
use core::ptr::null_mut;

#[derive(Debug)]
pub struct Mutex<T, const SPINS: bool = false> {
    inner: FFICell<mtx>,
    data: UnsafeCell<T>,
}

pub type SpinLock<T> = Mutex<T, true>;

pub struct MutexGuard<'a, T, const SPINS: bool> {
    lock: &'a Mutex<T, SPINS>,
}

impl<T, const SPINS: bool> Deref for MutexGuard<'_, T, SPINS> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.lock.data.get() }
    }
}

impl<T, const SPINS: bool> DerefMut for MutexGuard<'_, T, SPINS> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.lock.data.get() }
    }
}

impl<T, const SPINS: bool> Mutex<T, SPINS> {
    pub fn uninit(t: T) -> Self {
        Self {
            inner: FFICell::zeroed(),
            data: UnsafeCell::new(t),
        }
    }
}

impl<T, const SPINS: bool> RefMut<Mutex<T, SPINS>> {
    pub fn init(&mut self, name: &CStr, kind: Option<&CStr>) {
        let name_ptr = name.as_ptr();
        let kind_ptr = match kind {
            Some(k) => k.as_ptr(),
            None => null_mut(),
        };
        let inner_ptr = self.inner.get_out_ptr();
        let inner_lock_ptr = get_field!(inner_ptr, mtx_lock).as_ptr();

        let variant = if SPINS { MTX_SPIN } else { MTX_DEF };
        unsafe { bindings::_mtx_init(inner_lock_ptr, name_ptr, kind_ptr, variant) }
    }
}

impl<T, const SPINS: bool> Mutex<T, SPINS> {
    pub fn is_init(&self) -> bool {
        let inner_ptr = self.inner.get_out_ptr();
        let lock_obj_ptr = get_field!(inner_ptr, lock_object);
        let lo_flags_ptr = unsafe { get_field!(lock_obj_ptr, lo_flags).assume_init() };
        let lo_flags = unsafe { lo_flags_ptr.read() };
        (lo_flags & LO_INITIALIZED as u32) != 0
    }

    #[track_caller]
    pub fn lock(&self) -> Result<MutexGuard<'_, T, SPINS>> {
        if !self.is_init() {
            return Err(EDOOFUS);
        }
        let inner_ptr = self.inner.get_out_ptr();
        let inner_lock_ptr = get_field!(inner_ptr, mtx_lock).as_ptr();
        if SPINS {
            unsafe {
                bindings::__mtx_lock_spin_flags(inner_lock_ptr, 0, c"".as_ptr(), 0);
            }
        } else {
            unsafe {
                bindings::__mtx_lock_flags(inner_lock_ptr, 0, c"".as_ptr(), 0);
            }
        }
        Ok(MutexGuard { lock: self })
    }
}

impl<T, const SPINS: bool> Drop for MutexGuard<'_, T, SPINS> {
    fn drop(&mut self) {
        let inner_ptr = self.lock.inner.get_out_ptr();
        let inner_lock_ptr = get_field!(inner_ptr, mtx_lock).as_ptr();
        if SPINS {
            unsafe {
                bindings::__mtx_unlock_spin_flags(inner_lock_ptr, 0, c"".as_ptr(), 0);
            }
        } else {
            unsafe {
                bindings::__mtx_unlock_flags(inner_lock_ptr, 0, c"".as_ptr(), 0);
            }
        }
    }
}
