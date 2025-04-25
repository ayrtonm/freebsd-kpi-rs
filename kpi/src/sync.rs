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

use crate::bindings::{mtx, LO_INITIALIZED, MTX_DEF, MTX_SPIN, u_int};
use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::CStr;
use core::mem::{drop, forget, MaybeUninit};
use core::ops::{Deref, DerefMut};
use core::marker::PhantomData;
use core::ptr::{null_mut, drop_in_place, NonNull};
use core::pin::Pin;

#[derive(Debug)]
struct MtxCommon {
    inner: mtx,
    init: bool,
}

pub struct MutexGuard<'a, T> {
    lock: &'a Mutex<T>
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

impl MtxCommon {
    pub fn new() -> Self {
        let inner = unsafe { MaybeUninit::<mtx>::zeroed().assume_init() };
        Self {
            inner,
            init: false,
        }
    }
}

unsafe impl<T: Send> Sync for Mutex<T> {}

unsafe impl<T: Send> Sync for SpinLock<T> {}


#[derive(Debug)]
pub struct Mutex<T> {
    mtx_impl: MtxCommon,
    data: UnsafeCell<T>,
}

impl<T> Mutex<T> {
    pub fn new(t: T) -> Self {
        Self {
            mtx_impl: MtxCommon::new(),
            data: UnsafeCell::new(t),
        }
    }
}

#[derive(Debug)]
pub struct SpinLock<T> {
    mtx_impl: MtxCommon,
    data: UnsafeCell<T>,
}

impl<T> SpinLock<T> {
    pub fn new(t: T) -> Self {
        Self {
            mtx_impl: MtxCommon::new(),
            data: UnsafeCell::new(t),
        }
    }
}

pub mod wrappers {
    use super::*;

    pub trait HasMtx {
        const SPINS: bool;
        fn get_impl_mut(&mut self) -> &mut MtxCommon;
    }
    impl<T> HasMtx for Mutex<T> {
        const SPINS: bool = false;
        fn get_impl_mut(&mut self) -> &mut MtxCommon {
            &mut self.mtx_impl
        }
    }
    impl<T> HasMtx for SpinLock<T> {
        const SPINS: bool = true;
        fn get_impl_mut(&mut self) -> &mut MtxCommon {
            &mut self.mtx_impl
        }
    }

    pub fn mtx_init<M: HasMtx>(mutex: Pin<&mut M>, name: &'static CStr, kind: Option<&'static CStr>) {
        let name_ptr = name.as_ptr();
        let kind_ptr = match kind {
            Some(k) => k.as_ptr(),
            None => null_mut(),
        };
        let variant = if M::SPINS { MTX_SPIN } else { MTX_DEF };
        let mtx_ref: &mut M = unsafe { mutex.get_unchecked_mut() };
        let mtx_impl = mtx_ref.get_impl_mut();
        mtx_impl.init = true;
        let inner_lock_ptr = &raw mut mtx_impl.inner.mtx_lock;
        unsafe {
            // TODO: The cast was added recently and should probably be fixed on the C side
            bindings::_mtx_init(inner_lock_ptr.cast::<usize>(), name_ptr, kind_ptr, variant);
        }
    }

    pub fn mtx_lock<T>(mutex: &Mutex<T>) -> MutexGuard<T> {
        // This ensures that the Mutex was previously pinned
        assert!(mutex.mtx_impl.init);
        let inner_lock_ptr = &raw const mutex.mtx_impl.inner.mtx_lock;
        unsafe {
            bindings::__mtx_lock_flags(inner_lock_ptr.cast::<usize>().cast_mut(), 0, c"".as_ptr(), 0);
        };
        MutexGuard {
            lock: mutex,
        }
    }
    pub fn mtx_lock_spin<T>(mutex: &SpinLock<T>) -> SpinLockGuard<T> {
        // This ensures that the Mutex was previously pinned
        assert!(mutex.mtx_impl.init);
        let inner_lock_ptr = &raw const mutex.mtx_impl.inner.mtx_lock;
        unsafe {
            bindings::__mtx_lock_spin_flags(inner_lock_ptr.cast::<usize>().cast_mut(), 0, c"".as_ptr(), 0);
        };
        SpinLockGuard {
            lock: mutex,
        }
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
        let inner_lock_ptr = &raw const self.lock.mtx_impl.inner.mtx_lock;
        unsafe {
            bindings::__mtx_unlock_flags(inner_lock_ptr.cast::<usize>().cast_mut(), 0, c"".as_ptr(), 0);
        };
    }
}

impl<T> Drop for SpinLockGuard<'_, T> {
    fn drop(&mut self) {
        let inner_lock_ptr = &raw const self.lock.mtx_impl.inner.mtx_lock;
        unsafe {
            bindings::__mtx_unlock_spin_flags(inner_lock_ptr.cast::<usize>().cast_mut(), 0, c"".as_ptr(), 0);
        };
    }
}

#[repr(C)]
struct InternalArc<T> {
    count: u_int,
    t: T,
}

#[repr(C)]
#[derive(Debug)]
pub struct Arc<T, M: MallocType>(NonNull<InternalArc<T>>, PhantomData<M>);

impl<T, M: MallocType> Arc<T, M> {
    pub fn try_new(t: T, flags: MallocFlags) -> Result<Self> {
        let count = 0;
        let internal_arc = InternalArc { count, t };
        let mut boxed_arc: Box<InternalArc<T>, M> = Box::try_new(internal_arc, flags)?;
        let ptr = NonNull::new(boxed_arc.as_ptr()).unwrap();
        let count_ptr = &raw mut boxed_arc.count;
        unsafe {
            bindings::rust_bindings_refcount_init(count_ptr, 1)
        };
        forget(boxed_arc);
        Ok(Self(ptr, PhantomData))
    }

    fn get_count_ptr(&self) -> *mut u_int {
        self.0.as_ptr().cast::<u_int>()
    }

    fn snapshot_refcount(&self) -> u_int {
        let count_ptr = self.get_count_ptr();
        unsafe {
            bindings::rust_bindings_refcount_load(count_ptr)
        }
    }
}

impl<T, M: MallocType> Clone for Arc<T, M> {
    fn clone(&self) -> Self {
        let count_ptr = self.get_count_ptr();
        unsafe {
            bindings::rust_bindings_refcount_acquire(count_ptr)
        };
        Self(self.0, PhantomData)
    }
}

unsafe impl<T: Sync + Send, M: MallocType> Send for Arc<T, M> {}
unsafe impl<T: Sync + Send, M: MallocType> Sync for Arc<T, M> {}

impl<T, M: MallocType> Deref for Arc<T, M> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe {
            &self.0.as_ref().t
        }
    }
}

impl<T, M: MallocType> Drop for Arc<T, M> {
    fn drop(&mut self) {
        let count_ptr = self.get_count_ptr();
        let last = unsafe {
            bindings::rust_bindings_refcount_release(count_ptr)
        };
        if last {
            let ptr = &raw mut self.0;
            unsafe {
                drop_in_place::<Box<InternalArc<T>, M>>(ptr.cast());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // FIXME: These are totally thread-unsafe functions which are only used for tests
    #[no_mangle]
    fn rust_bindings_refcount_init(count: *mut u_int, value: u_int) {
        unsafe {
            *count = value;
        }
    }
    #[no_mangle]
    fn rust_bindings_refcount_load(count: *mut u_int) -> u_int {
        unsafe {
            *count
        }
    }
    #[no_mangle]
    fn rust_bindings_refcount_acquire(count: *mut u_int) -> u_int {
        unsafe {
            let old = *count;
            *count += 1;
            old
        }
    }
    #[no_mangle]
    fn rust_bindings_refcount_release(count: *mut u_int) -> bool {
        unsafe {
            *count -= 1;
            *count == 0
        }
    }
    #[test]
    fn arc() {
        let x: Arc<u32, M_DEVBUF> = Arc::try_new(42, M_NOWAIT).unwrap();
        assert_eq!(x.snapshot_refcount(), 1);
        {
            let y = x.clone();
            assert_eq!(y.snapshot_refcount(), 2);
            assert_eq!(x.snapshot_refcount(), 2);
        }
        assert_eq!(x.snapshot_refcount(), 1);
        let z = x.clone();
        assert_eq!(z.snapshot_refcount(), 2);
        assert_eq!(x.snapshot_refcount(), 2);
        drop(x);
        assert_eq!(z.snapshot_refcount(), 1);
    }
}
