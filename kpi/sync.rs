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

use crate::prelude::*;
use crate::bindings::{mtx, LO_INITIALIZED, MTX_DEF, MTX_SPIN};
use crate::allocator::KernelAllocator;
use core::cell::UnsafeCell;
use core::ffi::CStr;
use core::ops::{Deref, DerefMut};
use core::ptr::null_mut;
use core::mem::MaybeUninit;

#[derive(Debug)]
pub struct Mutex<T, const SPINS: bool = false> {
    inner_ptr: *mut mtx,
    allocator: KernelAllocator,
    data: UnsafeCell<T>,
}

pub type SpinLock<T> = Mutex<T, true>;

unsafe impl<T, const SPINS: bool> Sync for Mutex<T, SPINS> {}

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
    pub fn new(t: T, name: &CStr, kind: Option<&CStr>, flags: KernelAllocator) -> Self {
        let name_ptr = name.as_ptr();
        let kind_ptr = match kind {
            Some(k) => k.as_ptr(),
            None => null_mut(),
        };
        let inner = Box::new_in(MaybeUninit::<mtx>::zeroed(), flags);

        let (inner_ptr, allocator) = Box::into_raw_with_allocator(inner);

        let inner_ptr = inner_ptr as *mut mtx;

        let variant = if SPINS { MTX_SPIN } else { MTX_DEF };
        unsafe {
            let inner_lock_ptr = &raw mut (*inner_ptr).mtx_lock;
            bindings::_mtx_init(inner_lock_ptr, name_ptr, kind_ptr, variant);
        }

        Self {
            inner_ptr,
            allocator,
            data: UnsafeCell::new(t),
        }
    }

    #[track_caller]
    pub fn lock(&self) -> MutexGuard<'_, T, SPINS> {
        let inner_lock_ptr = unsafe { &raw mut (*self.inner_ptr).mtx_lock };
        if SPINS {
            unsafe {
                bindings::__mtx_lock_spin_flags(inner_lock_ptr, 0, c"".as_ptr(), 0);
            }
        } else {
            unsafe {
                bindings::__mtx_lock_flags(inner_lock_ptr, 0, c"".as_ptr(), 0);
            }
        }
        MutexGuard { lock: self }
    }
}

impl<T, const SPINS: bool> MutexGuard<'_, T, SPINS> {
    pub fn unlock(self) {
        let inner_lock_ptr = unsafe { &raw mut (*self.lock.inner_ptr).mtx_lock };
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
impl<T, const SPINS: bool> Drop for MutexGuard<'_, T, SPINS> {
    fn drop(&mut self) {
        let inner_lock_ptr = unsafe { &raw mut (*self.lock.inner_ptr).mtx_lock };
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

impl<T, const SPINS: bool> Drop for Mutex<T, SPINS> {
    fn drop(&mut self) {
        let inner = unsafe { Box::from_raw_in(self.inner_ptr, self.allocator) };
        core::mem::drop(inner);
    }
}
