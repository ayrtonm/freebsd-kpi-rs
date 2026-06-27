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

use crate::ErrCode;
use crate::bindings::sx;
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::{CStr, c_int, c_void};
use core::mem::drop;
use core::ops::{Deref, DerefMut};

pub struct SxSharedGuard<'a, T> {
    lock: &'a SxLock<T>,
}

impl<T> Deref for SxSharedGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.lock.data.get().as_ref().unwrap() }
    }
}

impl<T> Drop for SxSharedGuard<'_, T> {
    fn drop(&mut self) {
        let sx_ptr = self.lock.inner.get();
        unsafe {
            bindings::_sx_sunlock(sx_ptr, c"".as_ptr(), 0);
        }
    }
}

pub struct SxExclusiveGuard<'a, T> {
    lock: &'a SxLock<T>,
}

impl<T> Deref for SxExclusiveGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.lock.data.get().as_ref().unwrap() }
    }
}

impl<T> DerefMut for SxExclusiveGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.lock.data.get().as_mut().unwrap() }
    }
}

impl<T> Drop for SxExclusiveGuard<'_, T> {
    fn drop(&mut self) {
        let sx_ptr = self.lock.inner.get();
        unsafe {
            bindings::_sx_xunlock(sx_ptr, c"".as_ptr(), 0);
        }
    }
}

unsafe impl<T: Send + Sync> Send for SxLock<T> {}
unsafe impl<T: Send + Sync> Sync for SxLock<T> {}

#[derive(Debug, Default)]
pub struct SxLock<T> {
    inner: UnsafeCell<sx>,
    data: UnsafeCell<T>,
}

impl<T> SxLock<T> {
    pub fn new(t: T) -> Self {
        Self {
            inner: UnsafeCell::new(sx::default()),
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

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;
    use crate::device::MemoryManager;

    pub fn sx_init<R: MemoryManager>(lock: &SxLock<impl Sized>, owner: &R, name: &'static CStr) {
        assert!(owner.region().in_bounds(lock), "SxLock not in device-owned memory");
        let name_ptr = name.as_ptr();
        let sx_ptr = lock.inner.get();
        unsafe {
            bindings::sx_init_flags(sx_ptr, name_ptr, 0);
        }
    }

    pub fn sx_slock<T>(lock: &SxLock<T>) -> SxSharedGuard<'_, T> {
        let sx_ptr = lock.inner.get();
        unsafe {
            bindings::_sx_slock(sx_ptr, 0, c"".as_ptr(), 0);
        }
        SxSharedGuard { lock }
    }

    pub fn sx_xlock<T>(lock: &SxLock<T>) -> SxExclusiveGuard<'_, T> {
        let sx_ptr = lock.inner.get();
        unsafe {
            bindings::_sx_xlock(sx_ptr, 0, c"".as_ptr(), 0);
        }
        SxExclusiveGuard { lock }
    }

    pub fn sx_sleep<'a, T, C>(
        chan: &C,
        guard: SxExclusiveGuard<'a, T>,
        pri: c_int,
        wmesg: &CStr,
        timo: i32,
    ) -> (SxExclusiveGuard<'a, T>, Result<()>) {
        let lock = guard.lock;
        let sx_ptr = lock.inner.get();
        let lock_obj_ptr = unsafe { &raw mut (*sx_ptr).lock_object };

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

        let new_guard = SxExclusiveGuard { lock };

        let result = if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(())
        };

        (new_guard, result)
    }

    pub fn sx_sunlock<T>(guard: SxSharedGuard<T>) {
        drop(guard)
    }

    pub fn sx_xunlock<T>(guard: SxExclusiveGuard<T>) {
        drop(guard)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_sx_exclusive() {
        let lock = SxLock::new(4u32);
        sx_init(&lock, c"test");
        let mut x = sx_xlock(&lock);
        *x += 1;
        sx_xunlock(x);
    }

    #[test]
    fn basic_sx_shared() {
        let lock = SxLock::new(4u32);
        sx_init(&lock, c"test");
        let x = sx_slock(&lock);
        assert_eq!(*x, 4);
        sx_sunlock(x);
    }

    #[unsafe(no_mangle)]
    extern "C" fn sx_init_flags() {}
    #[unsafe(no_mangle)]
    extern "C" fn _sx_slock() {}
    #[unsafe(no_mangle)]
    extern "C" fn _sx_xlock() {}
    #[unsafe(no_mangle)]
    extern "C" fn _sx_sunlock() {}
    #[unsafe(no_mangle)]
    extern "C" fn _sx_xunlock() {}
}
