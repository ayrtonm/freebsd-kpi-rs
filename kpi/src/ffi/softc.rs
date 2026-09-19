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
use crate::bindings::{_device, cdev, device_t, u_int};
use crate::cdev::CDev;
use crate::device::Device;
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::fmt::{Debug, Formatter};
use core::mem::{MaybeUninit, forget};
use core::ops::Deref;
use core::pin::Pin;
use core::ptr::NonNull;
use core::sync::atomic::{AtomicUsize, Ordering};
use core::{fmt, ptr};

/// The layout of the softc that Ref and Ptr point to.
///
/// This determines the memory layout of all driver and char device softc managed by rust drivers.
/// It uses repr(C) and the driver-defined T is intentionally placed first to allow using rust
/// drivers as subclasses of existing C drivers.
///
/// This must be pub since define_driver! uses it for the KobjLayout trait impl, but it's not useful
/// to users so it's marked as doc(hidden).
#[doc(hidden)]
#[repr(C)]
pub struct SoftcLayout<T> {
    // This is the softc type specified by a driver or char device. It must be first to support
    // subclass drivers. Note there may be padding between the end of the softc to ensure the next
    // field is aligned to 8 bytes.
    inner: T,
    // The type in these Option<T>s must be non-null so NULL is used as a niche value to represent
    // None. That means the following two fields are each the size of void*.
    dev: Option<NonNull<_device>>,
    cdev: Option<NonNull<cdev>>,
    // The u_int must be behind an UnsafeCell since it's modified while behind a shared reference.
    // It avoids data races by using the atomic C KPI refcount_* functions. This is a u32 so there
    // should be at least a u32 of padding after it.
    count: UnsafeCell<u_int>,
}

impl<T> SoftcLayout<T> {
    // TODO: This is only pub to suport the echodev demo. Ideally SoftcLayout would not be exposed
    // outside this crate at all.
    pub fn new(t: T) -> Self {
        let mut res = Self {
            inner: t,
            dev: None,
            cdev: None,
            count: UnsafeCell::new(0),
        };
        let count_ptr = UnsafeCell::raw_get(&raw mut res.count);
        // This is just an address-insensitive atomic write
        unsafe { bindings::refcount_init(count_ptr, 1) };
        res
    }

    pub fn set_cdev(&mut self, dev: *mut cdev) {
        assert!(self.cdev.is_none());
        self.cdev = Some(NonNull::new(dev).unwrap());
    }

    /// Panics if this `SoftcLayout` is not attached to a cdev.
    pub fn cdev(&self) -> *mut cdev {
        match self.cdev {
            Some(nonnull_cdev) => nonnull_cdev.as_ptr(),
            None => panic!("softc does not have an associated *mut cdev"),
        }
    }

    /// Panics if this `SoftcLayout` is not attached to a device_t.
    pub fn device(&self) -> device_t {
        match self.dev {
            Some(nonnull_dev) => nonnull_dev.as_ptr(),
            None => panic!("softc does not have an associated device_t"),
        }
    }
}

/// A pointer to an uninitialized softc with no aliases.
///
/// This struct also carries a mutable reference to a bool so the glue code creating the UninitPtr can
/// later see whether the init method was called or not.
///
/// The second field is wrapped in Option rather than just &mut bool because UninitPtr is created from
/// an AsRustType impl which cannot grab references to locals on the stack frame of device_attach.
/// This kludge means that the glue code must call set_init_flag before handing it off to a driver.
pub struct UninitPtr<'a, T>(&'a mut MaybeUninit<SoftcLayout<T>>, Option<&'a mut bool>);

impl<'a, T> UninitPtr<'a, T> {
    pub(crate) unsafe fn from_raw(
        sc_ref: &'a mut MaybeUninit<SoftcLayout<T>>,
        dev: device_t,
    ) -> Self {
        // Get a pointer to the SoftcLayout on the heap from the MaybeUninit<SoftcLayout<T>> reference
        let sc_ptr: *mut SoftcLayout<T> = sc_ref.as_mut_ptr();
        // SAFETY: Since the softc has not been initialized we can't create a mutable reference to
        // the entire thing. Instead we'll just write directly to the fields that need to be set
        // here. An unsynchronized write is safe here since there are it has no aliases (the ptr arg
        // was a mutable reference).
        unsafe {
            (*sc_ptr).dev = Some(NonNull::new(dev).unwrap());
            // If a softc has both a device_t and a cdev pointer, the device_t is always initialized
            // first since newbus allocates the device softc. That means there should be no case
            // where this was already initialized to Some. We have to initialize it to soundly
            // create references to the entire SoftcLayout so None is the correct value here.
            (*sc_ptr).cdev = None;
        }
        Self(sc_ref, None)
    }

    // Used for the second field kludge described in UninitPtr's doc comment.
    // Must be public since it's called by KPI glue code in the driver .rlib's. Marked doc(hidden)
    // because it should not be called explicitly by the driver.
    #[doc(hidden)]
    pub fn set_init_flag(&mut self, flag: &'a mut bool) {
        *flag = false;
        self.1 = Some(flag);
    }

    pub fn device(&self) -> Device<'_> {
        // We still can't make a reference to the entire SoftcLayout<T> so calling SoftcLayout::device
        // is not an option to get a device_t.
        let sc_ptr: *const SoftcLayout<T> = self.0.as_ptr();
        // SAFETY: `dev` was initialized in `from_raw`.
        let dev = unsafe { (*sc_ptr).dev };
        match dev {
            Some(nonnull_dev) => {
                // SAFETY: The lifetime of the Device matches the UninitPtr borrow
                unsafe { Device::new_unchecked(nonnull_dev.as_ptr()) }
            }
            None => unreachable!(),
        }
    }

    // TODO: Consider removing this. It may have been needed for apple aic since there is no
    // function to undo pic_claim_root
    pub fn device_as_static(&self) -> Result<Device<'static>> {
        self.device().as_static()
    }

    /// Initialize the softc to `t` and return a Ref<T> pointer.
    ///
    /// The returned pointer may only be used for the lifetime of the UninitPtr it was created from.
    /// The KPI glue sets the UninitPtr lifetime parameter using a local on the device_attach stack
    /// frame so in practical terms this means that trying to stash the Ref in a global or
    /// equivalent (e.g. another softc) is a compile-time error.
    pub fn init(self, t: T) -> Ref<'a, T> {
        // Get a pointer to the SoftcLayout on the heap from the MaybeUninit<SoftcLayout<T>> reference
        let sc_ptr = self.0.as_mut_ptr();

        unsafe {
            (*sc_ptr).inner = t;
        }
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*sc_ptr).count });

        // This points to the heap, but this is just an address-insensitive atomic write anyway
        unsafe { bindings::refcount_init(count_ptr, 1) };

        match self.1 {
            Some(init_flag) => *init_flag = true,
            // This means there was a bug in the KPI glue
            None => unreachable!(),
        }
        // All fields are now initialized since `dev` was written in `from_raw` and `inner` and
        // `count` were written above.
        Ref(unsafe { self.0.assume_init_ref() })
    }
}

// TODO: document the part about Ref<T> being explicitly pinning and how proj!(&sc.field) can give a Pin<&Field>
/// A pointer to a softc passed in to kobj methods.
///
/// This is the equivalent of calling `device_get_softc` from a kobj method in C. In rust the
/// Ref<Softc> is created by the KPI glue and passed in to the trait method representing a kobj
/// method as an argument. Functionally it behaves like a `&Softc` argument.
///
/// Softc pointers passed to kobj methods do not need to be refcounted since the caller in C ensures
/// that the pointee will not be freed for the duration of the function being called. However, if a
/// softc is passed as a callback argument to a safe rust function there must be some way to ensure
/// the callback won't access the softc after it's freed. To do this the `sc.lease()` can be used to
/// create a Ptr<T> pointer to the same softc. Like Arc<T> in the standard library, this
/// increments a refcount embedded in the softc and dropping it decrements the refcount. Unlike
/// Arc<T> the refcount cannot be used to extend the lifetime of the softc past device_detach or
/// destroy_dev. The caller is responsible for dropping all Ptr<T>s created before that point
/// otherwise the KPI glue will panic when it tries to free the softc. In practical terms this means
/// if a callback was registered with a Ptr, the corresponding unregister function must be called.
#[repr(C)]
pub struct Ref<'a, T: 'static>(&'a SoftcLayout<T>);

impl<'a, T> Ref<'a, T> {
    // Only intended to be used by the proj! macro.
    #[doc(hidden)]
    pub unsafe fn map_unchecked<U: ?Sized, F>(self, f: F) -> Pin<&'a U>
    where
        F: FnOnce(&T) -> &U,
    {
        unsafe { Pin::new_unchecked(f(&self.0.inner)) }
    }

    // TODO: document safety reqs (on heap, anything else?)
    pub unsafe fn from_raw(ptr: &'a SoftcLayout<T>) -> Self {
        Self(ptr)
    }

    pub fn into_raw(self) -> (*mut T, *mut u_int) {
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        let t_ptr = ptr::from_ref(&self.0.inner).cast_mut();
        (t_ptr, count_ptr)
    }

    pub fn device(&self) -> Device<'_> {
        // SAFETY: The lifetime of the return value is tied to the Ref borrow (&self)
        unsafe { Device::new_unchecked(self.0.device()) }
    }

    pub fn cdev(&self) -> CDev<'_> {
        // SAFETY: The lifetime of the return value is tied to the Ref borrow (&self)
        unsafe { CDev::new_unchecked(self.0.cdev()) }
    }

    /// Increments the refcount and returns a new Ptr<T> pointing to the softc.
    ///
    /// Dropping the Ptr<T> decrements the refcount
    pub fn lease(&self) -> Ptr<T> {
        let inner_ptr = ptr::from_ref(self.0).cast_mut();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        unsafe { bindings::refcount_acquire(count_ptr) };
        Ptr(NonNull::from_ref(self.0))
    }
}

impl<'a, T: 'static + Debug> Debug for Ref<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0.inner, f)
    }
}

impl<'a, T> Copy for Ref<'a, T> {}

impl<'a, T> Clone for Ref<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'a, T> Deref for Ref<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0.inner
    }
}

#[repr(C)]
pub struct Ptr<T: 'static>(pub(crate) NonNull<SoftcLayout<T>>);

impl<T> Ptr<T> {
    pub unsafe fn map_unchecked<U: ?Sized, F>(&self, f: F) -> Pin<&U>
    where
        F: FnOnce(&T) -> &U,
    {
        unsafe { Pin::new_unchecked(f(self.deref())) }
    }

    /// Get a Device that owns the softc.
    ///
    /// The Device may only be used for the lifetime of the Ptr. Attempting to use it after
    /// passing on ownership of the Ptr somewhere else is a compile-time error. Calling this on a
    /// softc owned by a char device will panic.
    pub fn device(&self) -> Device<'_> {
        // SAFETY: The pointee is freed in device_detach, but the KPI glue for it panics if there is
        // an outstanding softc Ptr when it's ready to free it. The return value lifetime is tied
        // to the Ptr borrow.
        let dev_ptr = unsafe { self.0.as_ref().device() };
        // SAFETY: The lifetime of the return value is tied to the Ptr borrow (&self)
        unsafe { Device::new_unchecked(dev_ptr) }
    }

    /// Get a CDev that owns the softc.
    ///
    /// The CDev may only be used for the lifetime of the Ptr. Attempting to use it after
    /// passing on ownership of the Ptr somewhere else is a compile-time error. Calling this on a
    /// softc owned by a device driver will panic.
    pub fn cdev(&self) -> CDev<'_> {
        // SAFETY: The pointee is freed in device_detach, but the KPI glue for it panics if there is
        // an outstanding softc Ptr when it's ready to free it. The return value lifetime is tied
        // to the Ptr borrow.
        unsafe { CDev::new_unchecked(self.0.as_ref().cdev()) }
    }

    pub fn lease(&self) -> Self {
        // SAFETY: The pointee is freed in device_detach, but the KPI glue for it panics if there is
        // an outstanding softc Ptr when it's ready to free it.
        Ref(unsafe { self.0.as_ref() }).lease()
    }

    pub fn as_ref(&self) -> Ref<'_, T> {
        Ref(unsafe { self.0.as_ref() })
    }

    pub fn into_raw(lease: Self) -> (*mut T, *mut u_int) {
        let inner_ptr = lease.0.as_ptr();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        let t_ptr = unsafe { &raw mut (*lease.0.as_ptr()).inner };
        forget(lease);
        (t_ptr, count_ptr)
    }

    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        Self(NonNull::new(ptr.cast::<SoftcLayout<T>>()).unwrap())
    }
}

impl<T> Drop for Ptr<T> {
    fn drop(&mut self) {
        let inner_ptr = self.0.as_ptr();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        let last = unsafe { bindings::refcount_release(count_ptr) };
        assert!(!last);
    }
}

impl<T> Deref for Ptr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY: The pointee is freed in device_detach, but the KPI glue for it panics if there is
        // an outstanding softc Ptr when it's ready to free it. The return value lifetime is tied
        // to the Ptr borrow.
        unsafe { &self.0.as_ref().inner }
    }
}
const UNINIT: usize = usize::MAX;
const REVOKED: usize = usize::MAX - 1;

/// A `Ptr<T>` that can be revoked
///
/// This is narrower than a general reader-writer lock: the only value ever stored is a
/// `Ptr<T>`, it is set at most once via [`init`][Self::init], read any number of times
/// concurrently via [`get`][Self::get], and released at most once via [`revoke`][Self::revoke].
/// `revoke` does not block waiting for readers to finish — it panics if called while any
/// [`PtrGuard`] is still outstanding.
pub struct PtrSlot<T: 'static> {
    lease: UnsafeCell<MaybeUninit<Ptr<T>>>,
    // UNINIT = never initialized, REVOKED = permanently emptied, otherwise the number of
    // outstanding `PtrGuard`s (0 meaning initialized with no active readers).
    state: AtomicUsize,
}

unsafe impl<T: Sync> Sync for PtrSlot<T> {}
unsafe impl<T: Sync + Send> Send for PtrSlot<T> {}

impl<T> Default for PtrSlot<T> {
    fn default() -> Self {
        PtrSlot::uninit()
    }
}

impl<T> PtrSlot<T> {
    pub const fn uninit() -> Self {
        Self {
            lease: UnsafeCell::new(MaybeUninit::uninit()),
            state: AtomicUsize::new(UNINIT),
        }
    }

    /// Sets the leased value.
    ///
    /// Panics if called more than once.
    pub fn init(&self, lease: Ptr<T>) {
        if self
            .state
            .compare_exchange(UNINIT, 0, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            panic!("PtrSlot already initialized");
        }
        unsafe { (*self.lease.get()).write(lease) };
    }

    /// Borrows the leased value.
    ///
    /// Panics if it hasn't been initialized yet or has already been revoked.
    pub fn get(&self) -> PtrGuard<'_, T> {
        self.try_get()
            .expect("PtrSlot not initialized or already revoked")
    }

    /// Borrows the leased value, returning `None` if uninit or revoked.
    pub fn try_get(&self) -> Option<PtrGuard<'_, T>> {
        loop {
            let cur = self.state.load(Ordering::Acquire);
            if cur == UNINIT || cur == REVOKED {
                return None;
            }
            if self
                .state
                .compare_exchange_weak(cur, cur + 1, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                let lease = unsafe { (*self.lease.get()).assume_init_ref() };
                return Some(PtrGuard {
                    lease,
                    state: &self.state,
                });
            }
        }
    }

    /// Drops the leased value, releasing its refcount.
    ///
    /// Panics if it isn't currently initialized with zero outstanding readers (i.e. if called
    /// before `init`, more than once, or while a [`PtrGuard`] is still alive).
    pub fn clear(&self) {
        drop(self.take());
    }

    /// Takes the leased value out of the slot, transferring ownership to the caller.
    ///
    /// Panics if it isn't currently initialized with zero outstanding readers (i.e. if called
    /// before `init`, more than once, or while a [`PtrGuard`] is still alive).
    pub fn take(&self) -> Ptr<T> {
        if self
            .state
            .compare_exchange(0, REVOKED, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            panic!(
                "PtrSlot: cannot revoke -- not initialized, already revoked, or readers active"
            );
        }
        // The successful CAS above guarantees exclusive access: no guards are outstanding and
        // `get` can never hand out a new reference once REVOKED is published.
        unsafe { (*self.lease.get()).assume_init_read() }
    }
}

pub struct PtrGuard<'a, T: 'static> {
    lease: &'a Ptr<T>,
    state: &'a AtomicUsize,
}

impl<'a, T: 'static> PtrGuard<'a, T> {
    pub fn lease(&self) -> Ptr<T> {
        self.lease.lease()
    }

    pub fn device(&self) -> Device<'_> {
        self.lease.device()
    }

    pub fn cdev(&self) -> CDev<'_> {
        self.lease.cdev()
    }
}

impl<'a, T: 'static> Deref for PtrGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.lease.deref()
    }
}

impl<'a, T> Drop for PtrGuard<'a, T> {
    fn drop(&mut self) {
        self.state.fetch_sub(1, Ordering::Release);
    }
}
