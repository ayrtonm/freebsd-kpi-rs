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

use crate::bindings::{device_t, u_int, cdev};
use crate::device::Device;
use crate::cdev::CDev;
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::fmt::{Debug, Formatter};
use core::marker::PhantomData;
use core::mem::{MaybeUninit, forget};
use core::ops::Deref;
use core::sync::atomic::{AtomicUsize, Ordering};
use core::pin::Pin;
use core::{fmt, ptr};
use crate::boxed::Box;
use crate::malloc::{Malloc, MallocFlags};

/// The kernel object a `LoanLayout` is attached to.
///
/// `repr(C)` guarantees the `Uninit` discriminant is zero so that zero-initialized memory (e.g. a
/// newbus-allocated softc) is a valid `Owner`.
#[repr(C)]
enum Owner {
    Uninit,
    Device(device_t),
    CDev(*mut cdev),
}

#[repr(C)]
pub struct LoanLayout<T> {
    t: MaybeUninit<T>,
    owner: Owner,
    count: UnsafeCell<u_int>,
}

impl<T> LoanLayout<T> {
    pub fn new(t: T) -> Self {
        let mut res = Self {
            t: MaybeUninit::new(t),
            owner: Owner::Uninit,
            count: UnsafeCell::new(0),
        };
        let count_ptr = UnsafeCell::raw_get(&raw mut res.count );
        // This is just an address-insensitive atomic write
        unsafe { bindings::refcount_init(count_ptr, 1) };
        res
    }

    pub(crate) fn set_cdev(&mut self, dev: *mut cdev) {
        self.owner = Owner::CDev(dev);
    }

    /// Panics if this `LoanLayout` is not attached to a cdev.
    pub(crate) fn cdev(&self) -> *mut cdev {
        match self.owner {
            Owner::CDev(dev) => dev,
            _ => panic!("LoanLayout is not attached to a cdev"),
        }
    }

    /// Panics if this `LoanLayout` is not attached to a device_t.
    pub(crate) fn device(&self) -> device_t {
        match self.owner {
            Owner::Device(dev) => dev,
            _ => panic!("LoanLayout is not attached to a device"),
        }
    }
}

/// A unique pointer to an uninitialized, externally-managed object.
pub struct Uninit<'a, T>(&'a mut LoanLayout<T>, Option<&'a mut bool>);

impl<'a, T> Uninit<'a, T> {
    pub unsafe fn from_raw(ptr: &'a mut LoanLayout<T>, dev: device_t) -> Self {
        ptr.owner = Owner::Device(dev);
        Self(ptr, None)
    }

    pub fn set_init_flag(&mut self, flag: &'a mut bool) {
        self.1 = Some(flag);
    }

    pub fn device(&self) -> Device<'_> {
        Device::new(self.0.device())
    }

    /// Initialize the externally-managed object to `t` and return a pinned reference to the pointee
    pub fn init(self, t: T) -> Loan<'a, T> {
        self.0.t.write(t);
        self.1.map(|init| *init = true);
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        unsafe { bindings::refcount_init(count_ptr, 1) };
        Loan(unsafe { inner_ptr.as_ref().unwrap() }, PhantomData)
    }
}

/// A pointer that may be opted into refcounting if requested.
///
/// `M` records the allocator the backing `LoanLayout<T>` was allocated with, so that a
/// [`Lease`] created from this loan knows how to free it. It defaults to `()` for loans whose
/// leases never free their pointee.
#[repr(C)]
pub struct Loan<'a, T: 'static, M = ()>(
    pub(crate) &'a LoanLayout<T>,
    pub(crate) PhantomData<M>,
);

impl<'a, T, M> Loan<'a, T, M> {
    pub unsafe fn map_unchecked<U: ?Sized, F>(self, f: F) -> Pin<&'a U>
    where F: FnOnce(&T) -> &U {
        unsafe { Pin::new_unchecked(f(self.0.t.assume_init_ref())) }
    }

    pub unsafe fn from_raw(ptr: &'a LoanLayout<T>) -> Self {
        Self(ptr, PhantomData)
    }

    pub fn into_raw(self) -> (*mut T, *mut u_int) {
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        let t_ptr = self.0.t.as_ptr().cast_mut();
        (t_ptr, count_ptr)
    }

    pub fn device(&self) -> Device<'_> {
        Device::new(self.0.device())
    }

    pub fn cdev(&self) -> CDev<'_> {
        CDev::new(self.0.cdev())
    }

    pub fn lease(&self) -> Lease<T, M> {
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        unsafe { bindings::refcount_acquire(count_ptr) };
        Lease(unsafe { inner_ptr.as_ref().unwrap() }, PhantomData)
    }
}

impl<'a, T: 'static + Debug, M> Debug for Loan<'a, T, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0.t, f)
    }
}

impl<'a, T, M> Copy for Loan<'a, T, M> {}

impl<'a, T, M> Clone for Loan<'a, T, M> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<'a, T, M> Deref for Loan<'a, T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.t.assume_init_ref() }
    }
}

/// A pointer to a refcounted object.
///
/// `M` records the allocator the backing `LoanLayout<T>` was allocated with, so
/// [`release_and_free`][Self::release_and_free] can free it without naming the allocator again.
/// It defaults to `()` for leases that never free their pointee.
#[repr(C)]
pub struct Lease<T: 'static, M = ()>(
    pub(crate) &'static LoanLayout<T>,
    pub(crate) PhantomData<M>,
);

impl<T, M> Lease<T, M> {
    pub unsafe fn map_unchecked<U: ?Sized, F>(&self, f: F) -> Pin<&U>
    where F: FnOnce(&T) -> &U {
        unsafe { Pin::new_unchecked(f(self.0.t.assume_init_ref())) }
    }

    pub fn device(&self) -> Device<'_> {
        Device::new(self.0.device())
    }

    pub fn cdev(&self) -> CDev<'_> {
        CDev::new(self.0.cdev())
    }

    pub fn lease(&self) -> Self {
        Loan::<T, M>(self.0, PhantomData).lease()
    }

    pub fn into_raw(self) -> (*mut T, *mut u_int) {
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        let t_ptr = self.0.t.as_ptr().cast_mut();
        forget(self);
        (t_ptr, count_ptr)
    }

    /// Releases this lease along with the original reference created by [`LoanLayout::new`], then
    /// frees the allocation, dropping `T`.
    ///
    /// Panics if any other lease is still outstanding.
    ///
    /// # Safety
    ///
    /// The `LoanLayout<T>` must have been allocated via `Box::<LoanLayout<T>, M>::into_raw` and no
    /// other reference to it may be created after this call.
    pub(crate) unsafe fn release_and_free(self)
    where
        M: Malloc,
    {
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        forget(self);
        // Release this lease, then the original reference from `LoanLayout::new`, which must be
        // the last one.
        unsafe { bindings::refcount_release(count_ptr) };
        let last = unsafe { bindings::refcount_release(count_ptr) };
        assert!(last, "LoanLayout still leased in release_and_free");
        // `MaybeUninit` never runs `T`'s destructor on its own, so drop it explicitly before
        // freeing the allocation.
        unsafe { (*inner_ptr).t.assume_init_drop() };
        drop(unsafe { Box::<LoanLayout<T>, M>::from_raw(inner_ptr) });
    }
}

impl<T, M> Drop for Lease<T, M> {
    fn drop(&mut self) {
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        let last = unsafe { bindings::refcount_release(count_ptr) };
        assert!(!last);
    }
}

impl<T, M> Deref for Lease<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.t.assume_init_ref() }
    }
}
const UNINIT: usize = usize::MAX;
const REVOKED: usize = usize::MAX - 1;

/// A `Lease<T>` that can be revoked
///
/// This is narrower than a general reader-writer lock: the only value ever stored is a
/// `Lease<T>`, it is set at most once via [`init`][Self::init], read any number of times
/// concurrently via [`get`][Self::get], and released at most once via [`revoke`][Self::revoke].
/// `revoke` does not block waiting for readers to finish — it panics if called while any
/// [`LeaseGuard`] is still outstanding.
pub struct LeaseSlot<T: 'static, M = ()> {
    lease: UnsafeCell<MaybeUninit<Lease<T, M>>>,
    // UNINIT = never initialized, REVOKED = permanently emptied, otherwise the number of
    // outstanding `LeaseGuard`s (0 meaning initialized with no active readers).
    state: AtomicUsize,
}

unsafe impl<T: Sync, M> Sync for LeaseSlot<T, M> {}
unsafe impl<T: Sync + Send, M> Send for LeaseSlot<T, M> {}

impl<T, M> Default for LeaseSlot<T, M> {
    fn default() -> Self {
        LeaseSlot::uninit()
    }
}

impl<T, M> LeaseSlot<T, M> {
    pub const fn uninit() -> Self {
        Self {
            lease: UnsafeCell::new(MaybeUninit::uninit()),
            state: AtomicUsize::new(UNINIT),
        }
    }

    /// Sets the leased value.
    ///
    /// Panics if called more than once.
    pub fn init(&self, lease: Lease<T, M>) {
        if self
            .state
            .compare_exchange(UNINIT, 0, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            panic!("LeaseSlot already initialized");
        }
        unsafe { (*self.lease.get()).write(lease) };
    }

    /// Borrows the leased value.
    ///
    /// Panics if it hasn't been initialized yet or has already been revoked.
    pub fn get(&self) -> LeaseGuard<'_, T, M> {
        loop {
            let cur = self.state.load(Ordering::Acquire);
            if cur == UNINIT || cur == REVOKED {
                panic!("LeaseSlot not initialized or already revoked");
            }
            if self
                .state
                .compare_exchange_weak(cur, cur + 1, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                let lease = unsafe { (*self.lease.get()).assume_init_ref() };
                return LeaseGuard {
                    lease,
                    state: &self.state,
                };
            }
        }
    }

    /// Drops the leased value, releasing its refcount.
    ///
    /// Panics if it isn't currently initialized with zero outstanding readers (i.e. if called
    /// before `init`, more than once, or while a [`LeaseGuard`] is still alive).
    pub fn clear(&self) {
        drop(self.take());
    }

    /// Takes the leased value out of the slot, transferring ownership to the caller.
    ///
    /// Panics if it isn't currently initialized with zero outstanding readers (i.e. if called
    /// before `init`, more than once, or while a [`LeaseGuard`] is still alive).
    pub fn take(&self) -> Lease<T, M> {
        if self
            .state
            .compare_exchange(0, REVOKED, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            panic!(
                "LeaseSlot: cannot revoke -- not initialized, already revoked, or readers active"
            );
        }
        // The successful CAS above guarantees exclusive access: no guards are outstanding and
        // `get` can never hand out a new reference once REVOKED is published.
        unsafe { (*self.lease.get()).assume_init_read() }
    }
}

pub struct LeaseGuard<'a, T: 'static, M = ()> {
    lease: &'a Lease<T, M>,
    state: &'a AtomicUsize,
}

impl<'a, T: 'static, M> LeaseGuard<'a, T, M> {
    pub fn lease(&self) -> Lease<T, M> {
        self.lease.lease()
    }

    pub fn device(&self) -> Device<'_> {
        self.lease.device()
    }

    pub fn cdev(&self) -> CDev<'_> {
        self.lease.cdev()
    }
}

impl<'a, T: 'static, M> Deref for LeaseGuard<'a, T, M> {
    type Target = T;

    fn deref(&self) -> &T {
        self.lease.deref()
    }
}

impl<'a, T, M> Drop for LeaseGuard<'a, T, M> {
    fn drop(&mut self) {
        self.state.fetch_sub(1, Ordering::Release);
    }
}
