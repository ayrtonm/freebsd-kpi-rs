/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2025 Ayrton Muñoz
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

use crate::ErrCode;
use crate::bindings::{MTX_DEF, MTX_SPIN, mtx};
use crate::prelude::*;
use crate::sync::Mutable;
use core::cell::UnsafeCell;
use core::ffi::{CStr, c_int, c_void};
use core::mem::drop;
use core::ops::{Deref, DerefMut};
use core::ptr::null_mut;

pub struct MutexGuard<'a, T> {
    lock: &'a Mutex<T>,
}

impl<T> Deref for MutexGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.lock.data.get().as_ref().unwrap() }
    }
}

impl<T> DerefMut for MutexGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.lock.data.get().as_mut().unwrap() }
    }
}

impl<T> Mutable<T> for Mutex<T> {
    type Guard<'a>
        = MutexGuard<'a, T>
    where
        T: 'a;

    fn data_ptr(&self) -> *mut T {
        self.data.get()
    }

    fn get_mut(&self) -> Self::Guard<'_> {
        mtx_lock(self)
    }
}

pub struct SpinLockGuard<'a, T> {
    lock: &'a SpinLock<T>,
}

impl<T> Deref for SpinLockGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.lock.data.get().as_ref().unwrap() }
    }
}

impl<T> DerefMut for SpinLockGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.lock.data.get().as_mut().unwrap() }
    }
}

impl<T> Mutable<T> for SpinLock<T> {
    type Guard<'a>
        = SpinLockGuard<'a, T>
    where
        T: 'a;

    fn data_ptr(&self) -> *mut T {
        self.data.get()
    }

    fn get_mut(&self) -> Self::Guard<'_> {
        mtx_lock_spin(self)
    }
}

#[doc(hidden)]
#[derive(Debug, Default)]
pub struct MtxCommon {
    inner: UnsafeCell<mtx>,
}

impl MtxCommon {
    pub fn new() -> Self {
        let inner = UnsafeCell::new(mtx::default());
        Self { inner }
    }
}

unsafe impl<T: Send> Send for Mutex<T> {}
unsafe impl<T: Send> Sync for Mutex<T> {}

unsafe impl<T: Send> Send for SpinLock<T> {}
unsafe impl<T: Send> Sync for SpinLock<T> {}

#[derive(Debug, Default)]
pub struct Mutex<T> {
    mtx_impl: MtxCommon,
    data: UnsafeCell<T>,
}

impl<T> Mutex<T> {
    pub fn new(t: T) -> Self {
        let mtx_impl = MtxCommon::new();
        Self {
            mtx_impl,
            data: UnsafeCell::new(t),
        }
    }

    pub fn data_ptr(&self) -> *mut T {
        self.data.get()
    }

    pub fn get_mut(&mut self) -> &mut T {
        unsafe { self.data_ptr().as_mut().unwrap() }
    }
}

#[derive(Debug, Default)]
pub struct SpinLock<T> {
    mtx_impl: MtxCommon,
    data: UnsafeCell<T>,
}

impl<T> SpinLock<T> {
    pub fn new(t: T) -> Self {
        let mtx_impl = MtxCommon::new();
        Self {
            mtx_impl,
            data: UnsafeCell::new(t),
        }
    }

    pub fn data_ptr(&self) -> *mut T {
        self.data.get()
    }
}

pub trait Lockable {
    const SPINS: bool;
    #[doc(hidden)]
    fn get_impl_mut(&self) -> &MtxCommon;
}

impl<T> Lockable for Mutex<T> {
    const SPINS: bool = false;
    fn get_impl_mut(&self) -> &MtxCommon {
        &self.mtx_impl
    }
}

impl<T> Lockable for SpinLock<T> {
    const SPINS: bool = true;
    fn get_impl_mut(&self) -> &MtxCommon {
        &self.mtx_impl
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct MtxFlags(pub(crate) c_int);

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;

    gen_newtype! {
        MtxFlags as i32,
        MTX_RECURSE,
        MTX_QUIET,
        MTX_NOWITNESS,
        MTX_DUPOK,
        MTX_NOPROFILE,
        MTX_NEW,
    }

    pub fn mtx_init<M: Lockable>(
        //dev: &Device,
        lock: &M,
        name: &'static CStr,
        kind: Option<&'static CStr>,
        flags: Option<MtxFlags>,
    ) {
        // TODO: Re-enable bounds check
        //assert!(dev.in_bounds(lock));
        let name_ptr = name.as_ptr();
        let kind_ptr = match kind {
            Some(k) => k.as_ptr(),
            None => null_mut(),
        };
        let variant = if M::SPINS { MTX_SPIN } else { MTX_DEF };
        let flags = match flags {
            Some(flags) => flags.0,
            None => 0,
        };
        let mtx_impl = lock.get_impl_mut();
        let mtx_ptr = mtx_impl.inner.get();
        let mtx_lock_ptr = unsafe { &raw mut (*mtx_ptr).mtx_lock };
        unsafe {
            // TODO: The cast was added recently and should probably be fixed on the C side
            bindings::_mtx_init(
                mtx_lock_ptr.cast::<usize>(),
                name_ptr,
                kind_ptr,
                variant | flags,
            );
        }
    }

    pub fn mtx_lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
        let mtx_impl = &mutex.mtx_impl;

        let mtx_ptr = mtx_impl.inner.get();
        let mtx_lock_ptr = unsafe { &raw mut (*mtx_ptr).mtx_lock };
        unsafe {
            bindings::__mtx_lock_flags(mtx_lock_ptr.cast::<usize>(), 0, c"".as_ptr(), 0);
        };
        MutexGuard { lock: mutex }
    }
    pub fn mtx_lock_spin<T>(mutex: &SpinLock<T>) -> SpinLockGuard<'_, T> {
        let mtx_impl = &mutex.mtx_impl;

        let mtx_ptr = mtx_impl.inner.get();
        let mtx_lock_ptr = unsafe { &raw mut (*mtx_ptr).mtx_lock };
        unsafe {
            bindings::__mtx_lock_spin_flags(mtx_lock_ptr.cast::<usize>(), 0, c"".as_ptr(), 0);
        };
        SpinLockGuard { lock: mutex }
    }

    pub fn mtx_sleep<'a, T, C>(
        chan: &C,
        guard: MutexGuard<'a, T>,
        pri: c_int,
        wmesg: &CStr,
        timo: i32,
    ) -> (MutexGuard<'a, T>, Result<()>) {
        let lock = guard.lock;
        let mtx_ptr = lock.mtx_impl.inner.get();
        let lock_obj_ptr = unsafe { &raw mut (*mtx_ptr).lock_object };

        core::mem::forget(guard);

        let res = unsafe {
            bindings::_sleep(
                (chan as *const C).cast::<c_void>(),
                lock_obj_ptr,
                pri,
                wmesg.as_ptr(),
                bindings::tick_sbt * timo as i64,
                0,
                bindings::C_HARDCLOCK,
            )
        };

        let new_guard = MutexGuard { lock };

        let result = if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        };

        (new_guard, result)
    }

    pub fn mtx_sleep_spin<'a, T, C>(
        chan: &C,
        guard: SpinLockGuard<'a, T>,
        wmesg: &CStr,
        timo: i32,
    ) -> (SpinLockGuard<'a, T>, Result<()>) {
        let lock = guard.lock;
        let mtx_ptr = lock.mtx_impl.inner.get();

        core::mem::forget(guard);

        let res = unsafe {
            bindings::msleep_spin_sbt(
                (chan as *const C).cast::<c_void>(),
                mtx_ptr,
                wmesg.as_ptr(),
                bindings::tick_sbt * timo as i64,
                0,
                bindings::C_HARDCLOCK,
            )
        };

        let new_guard = SpinLockGuard { lock };

        let result = if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        };

        (new_guard, result)
    }

    pub fn mtx_unlock<T>(guard: MutexGuard<T>) {
        drop(guard)
    }

    pub fn mtx_unlock_spin<T>(guard: SpinLockGuard<T>) {
        drop(guard)
    }
}

impl<T> Drop for MutexGuard<'_, T> {
    fn drop(&mut self) {
        let mtx_impl = &self.lock.mtx_impl;

        let mtx_ptr = mtx_impl.inner.get();
        let mtx_lock_ptr = unsafe { &raw mut (*mtx_ptr).mtx_lock };
        unsafe {
            bindings::__mtx_unlock_flags(mtx_lock_ptr.cast::<usize>(), 0, c"".as_ptr(), 0);
        };
    }
}

impl<T> Drop for SpinLockGuard<'_, T> {
    fn drop(&mut self) {
        let mtx_impl = &self.lock.mtx_impl;

        let mtx_ptr = mtx_impl.inner.get();
        let mtx_lock_ptr = unsafe { &raw mut (*mtx_ptr).mtx_lock };
        unsafe {
            bindings::__mtx_unlock_spin_flags(mtx_lock_ptr.cast::<usize>(), 0, c"".as_ptr(), 0);
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_mutex() {
        let lock = Mutex::new(4u32);
        mtx_init(&lock, c"", None, None);
        let mut x = mtx_lock(&lock);
        *x += 1;
        mtx_unlock(x);
    }

    #[test]
    fn basic_spinlock() {
        let lock = SpinLock::new(4u32);
        mtx_init(&lock, c"", None, None);
        let mut x = mtx_lock_spin(&lock);
        *x += 1;
        mtx_unlock_spin(x);
    }

    // These are just basic tests to make sure the guards work as expected
    #[unsafe(no_mangle)]
    extern "C" fn _mtx_init() {}
    #[unsafe(no_mangle)]
    extern "C" fn __mtx_lock_flags() {}
    #[unsafe(no_mangle)]
    extern "C" fn __mtx_unlock_flags() {}
    #[unsafe(no_mangle)]
    extern "C" fn __mtx_lock_spin_flags() {}
    #[unsafe(no_mangle)]
    extern "C" fn __mtx_unlock_spin_flags() {}
}
