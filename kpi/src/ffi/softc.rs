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

use crate::bindings::{_device, cdev, device_t, u_int};
use crate::cdev::CDev;
use crate::device::Device;
use crate::prelude::*;
use core::cell::UnsafeCell;
use crate::boxed::Box;
use crate::malloc::{Malloc, MallocFlags};
use core::fmt::{Debug, Formatter};
use core::mem::{MaybeUninit, forget};
use core::ops::{Deref, DerefMut};
use core::pin::Pin;
use core::ptr::NonNull;
use core::{fmt, ptr};
use crate::ffi::PinProject;

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
pub struct Softc<T> {
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

impl<T> Softc<T> {
    pub fn new<M: Malloc>(t: T, flags: MallocFlags) -> Box<Self, M> {
        Softc::try_new(t, flags).unwrap()
    }

    pub fn try_new<M: Malloc>(t: T, flags: MallocFlags) -> Result<Box<Self, M>> {
        let mut res = Box::try_new(Self {
            inner: t,
            dev: None,
            cdev: None,
            count: UnsafeCell::new(0),
        }, flags)?;
        let count_ptr = UnsafeCell::raw_get(&raw mut res.count);
        // This is just an address-insensitive atomic write
        unsafe { bindings::refcount_init(count_ptr, 1) };
        Ok(res)
    }

    pub fn set_cdev(&mut self, dev: *mut cdev) {
        assert!(self.cdev.is_none());
        self.cdev = Some(NonNull::new(dev).unwrap());
    }

    /// Panics if this `Softc` is not attached to a cdev.
    pub fn cdev(&self) -> CDev<'_> {
        let ptr = match self.cdev {
            Some(nonnull_cdev) => nonnull_cdev.as_ptr(),
            None => panic!("softc does not have an associated *mut cdev"),
        };
        // SAFETY: The lifetime of the return value is tied to the Ref borrow (&self)
        unsafe { CDev::new_unchecked(ptr) }
    }

    /// Panics if this `Softc` is not attached to a device_t.
    pub fn device(&self) -> Device<'_> {
        let ptr = match self.dev {
            Some(nonnull_dev) => nonnull_dev.as_ptr(),
            None => panic!("softc does not have an associated device_t"),
        };
        // SAFETY: The lifetime of the return value is tied to the Ref borrow (&self)
        unsafe { Device::new_unchecked(ptr) }
    }

    pub fn get_ptr(&self) -> Ptr<T> {
        let count_ptr = self.count.get();
        unsafe { bindings::refcount_acquire(count_ptr) };
        Ptr(NonNull::from_ref(self))
    }

    pub fn as_pin(&self) -> Pin<&T> {
        unsafe { Pin::new_unchecked(&self.inner) }
    }

    pub fn project(&self) -> T::ProjHelper<'_>
    where T: PinProject {
        self.as_pin().project()
    }

    pub fn as_raw(&self) -> (*mut T, *mut u_int) {
        let t_ptr = ptr::from_ref(&self.inner).cast_mut();
        (t_ptr, self.count.get())
    }
}

impl<'a, T: 'static + Debug> Debug for Softc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl<T> Deref for Softc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Softc<T> {
    fn deref_mut(&mut self) -> &mut <Self as Deref>::Target {
        &mut self.inner
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
pub struct UninitPtr<'a, T>(&'a mut MaybeUninit<Softc<T>>);

impl<'a, T> UninitPtr<'a, T> {
    pub(crate) unsafe fn from_raw(
        sc_ref: &'a mut MaybeUninit<Softc<T>>,
        dev: device_t,
    ) -> Self {
        // Get a pointer to the Softc on the heap from the MaybeUninit<Softc<T>> reference
        let sc_ptr: *mut Softc<T> = sc_ref.as_mut_ptr();
        // SAFETY: Since the softc has not been initialized we can't create a mutable reference to
        // the entire thing. Instead we'll just write directly to the fields that need to be set
        // here. An unsynchronized write is safe here since there are it has no aliases (the ptr arg
        // was a mutable reference).
        unsafe {
            (*sc_ptr).dev = Some(NonNull::new(dev).unwrap());
            // If a softc has both a device_t and a cdev pointer, the device_t is always initialized
            // first since newbus allocates the device softc. That means there should be no case
            // where this was already initialized to Some. We have to initialize it to soundly
            // create references to the entire Softc so None is the correct value here.
            (*sc_ptr).cdev = None;
        }
        Self(sc_ref)
    }

    pub fn device(&self) -> Device<'_> {
        // We still can't make a reference to the entire Softc<T> so calling Softc::device
        // is not an option to get a device_t.
        let sc_ptr: *const Softc<T> = self.0.as_ptr();
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

    /// Initialize the softc to `t` and return a Ref<T> pointer.
    ///
    /// The returned pointer may only be used for the lifetime of the UninitPtr it was created from.
    /// The KPI glue sets the UninitPtr lifetime parameter using a local on the device_attach stack
    /// frame so in practical terms this means that trying to stash the Ref in a global or
    /// equivalent (e.g. another softc) is a compile-time error.
    pub fn init(self, t: T) -> &'a Softc<T> {
        // Get a pointer to the Softc on the heap from the MaybeUninit<Softc<T>> reference
        let sc_ptr = self.0.as_mut_ptr();

        unsafe {
            (*sc_ptr).inner = t;
        }
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*sc_ptr).count });

        // This points to the heap, but this is just an address-insensitive atomic write anyway
        unsafe { bindings::refcount_init(count_ptr, 1) };

        // All fields are now initialized since `dev` was written in `from_raw` and `inner` and
        // `count` were written above.
        unsafe { self.0.assume_init_ref() }
    }
}

#[repr(C)]
pub struct Ptr<T: 'static>(pub(crate) NonNull<Softc<T>>);

impl<T> Ptr<T> {
    /// Get a Device that owns the softc.
    ///
    /// The Device may only be used for the lifetime of the Ptr. Attempting to use it after
    /// passing on ownership of the Ptr somewhere else is a compile-time error. Calling this on a
    /// softc owned by a char device will panic.
    pub fn device(&self) -> Device<'_> {
        // SAFETY: The pointee is freed in device_detach, but the KPI glue for it panics if there is
        // an outstanding softc Ptr when it's ready to free it. The return value lifetime is tied
        // to the Ptr borrow.
        unsafe { self.0.as_ref().device() }
        // SAFETY: The lifetime of the return value is tied to the Ptr borrow (&self)
        //unsafe { Device::new_unchecked(dev_ptr) }
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
        unsafe { self.0.as_ref().cdev() }
    }

    pub fn get_ptr(&self) -> Self {
        // SAFETY: The pointee is freed in device_detach, but the KPI glue for it panics if there is
        // an outstanding softc Ptr when it's ready to free it.
        unsafe { self.0.as_ref().get_ptr() }
    }

    pub fn as_pin(&self) -> Pin<&T> {
        unsafe { Pin::new_unchecked(&self.0.as_ref().inner) }
    }

    pub fn project(&self) -> T::ProjHelper<'_>
    where T: PinProject {
        self.as_pin().project()
    }

    pub fn into_raw(lease: Self) -> (*mut T, *mut u_int) {
        let inner_ptr = lease.0.as_ptr();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*inner_ptr).count });
        let t_ptr = unsafe { &raw mut (*lease.0.as_ptr()).inner };
        forget(lease);
        (t_ptr, count_ptr)
    }

    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        Self(NonNull::new(ptr.cast::<Softc<T>>()).unwrap())
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

unsafe impl<T: Sync + Send> Sync for Ptr<T> {}
unsafe impl<T: Sync + Send> Send for Ptr<T> {}
