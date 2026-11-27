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

use crate::bindings::{_device, cdev, device_t, malloc_type, u_int};
use crate::boxed::Box;
use crate::cdev::CDev;
use crate::device::Device;
use crate::ffi::{Refcounted, PinProject};
use crate::malloc::{Malloc, MallocFlags, MallocType};
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::c_void;
use core::fmt::{Debug, Formatter};
use core::marker::PhantomData;
use core::mem::{MaybeUninit, forget};
use core::ops::Deref;
use core::pin::Pin;
use core::ptr::{NonNull, drop_in_place, null_mut};
use core::sync::atomic::{AtomicPtr, Ordering};
use core::{fmt, ptr};

unsafe impl<T: 'static> Refcounted for Softc<T> {
    fn acquire_ref(&self) {
        let count_ptr = self.count.get();
        unsafe { bindings::refcount_acquire(count_ptr) };
    }
    unsafe fn release_ref(sc: NonNull<Self>) -> bool {
        let sc_ptr = sc.as_ptr();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*sc_ptr).count });
        let last = unsafe { bindings::refcount_release(count_ptr) };
        if !last {
            return last;
        }
        let mtype = unsafe { (*sc_ptr).mtype };
        match mtype {
            Some(mtype) => unsafe {
                drop_in_place(sc_ptr);
                free(sc_ptr.cast::<c_void>(), MallocType::from_raw(mtype.as_ptr()));
            },
            // device_detach glue owns a reference
            None => unreachable!("released the last referene to a newbus softc"),
        }
        last
    }
}

/// The layout of the softc that Ptr and Ptr point to.
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
    // None. That means the following fields are each the size of void *.
    dev: Option<NonNull<_device>>,
    // This can change after init without synchronization so it must be atomic
    pub(crate) cdev: AtomicPtr<cdev>,
    pub(crate) mtype: Option<NonNull<malloc_type>>,
    // The u_int must be behind an UnsafeCell since it's modified while behind a shared reference.
    // It avoids data races by using the atomic C KPI refcount_* functions. This is a u32 so there
    // should be at least a u32 of padding after it.
    count: UnsafeCell<u_int>,
}

unsafe impl<T: Sync> Sync for Softc<T> {}
unsafe impl<T: Send> Send for Softc<T> {}

impl<T> Softc<T> {
    pub fn alloc<M: Malloc>(t: T, mtype: M, flags: MallocFlags) -> Ptr<Softc<T>> {
        Softc::try_alloc(t, mtype, flags).unwrap()
    }

    pub fn try_alloc<M: Malloc>(t: T, mtype: M, flags: MallocFlags) -> Result<Ptr<Softc<T>>> {
        let sc = Box::try_new(
            Self {
                inner: t,
                dev: None,
                cdev: AtomicPtr::new(null_mut()),
                mtype: Some(NonNull::new(M::malloc_type().as_raw()).unwrap()),
                count: UnsafeCell::new(0),
            },
            mtype,
            flags,
        )?;
        let sc_ptr = Box::into_raw(sc);
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*sc_ptr).count });
        // This is just an address-insensitive atomic write
        unsafe { bindings::refcount_init(count_ptr, 1) };
        let res = unsafe { Ptr::<Softc<T>>::from_raw(sc_ptr) };
        Ok(res)
    }

    /// Panics if this `Softc` is not attached to a cdev.
    pub fn cdev(&self) -> &CDev {
        let ptr = self.cdev.load(Ordering::Acquire);
        if ptr.is_null() {
            panic!("softc does not have an associated *mut cdev");
        }
        // SAFETY: The lifetime of the return value is tied to the Ptr borrow (&self)
        unsafe { ptr.cast::<CDev>().as_ref().unwrap() }
    }

    /// Panics if this `Softc` is not attached to a device_t.
    pub fn device(&self) -> &Device {
        let ptr = match self.dev {
            Some(nonnull_dev) => nonnull_dev.as_ptr(),
            None => panic!("softc does not have an associated device_t"),
        };
        // SAFETY: The lifetime of the return value is tied to the Ptr borrow (&self)
        unsafe { ptr.cast::<Device>().as_ref().unwrap() }
    }

    pub fn acquire_ptr(&self) -> Ptr<Softc<T>> {
        let count_ptr = self.count.get();
        unsafe { bindings::refcount_acquire(count_ptr) };
        Ptr(NonNull::from_ref(self), PhantomData)
    }

    pub fn as_pin(&self) -> Pin<&T> {
        unsafe { Pin::new_unchecked(&self.inner) }
    }

    pub fn project(&self) -> T::ProjHelper<'_>
    where
        T: PinProject,
    {
        self.as_pin().project()
    }

    pub fn as_raw(sc: &Self) -> (*mut T, *mut u_int) {
        let t_ptr = ptr::from_ref(&sc.inner).cast_mut();
        (t_ptr, sc.count.get())
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

/// An uninitialized softc.
#[repr(C)]
pub struct Uninit<T>(UnsafeCell<MaybeUninit<Softc<T>>>);

impl<T> Uninit<T> {
    pub(crate) unsafe fn from_raw<'a>(sc_ptr: *mut Softc<T>, dev: device_t) -> &'a Self {
        // SAFETY: Since the softc has not been initialized we can't create a reference to the
        // entire thing. Instead we'll just write directly to the fields that need to be set here.
        // The caller ensures the softc has no aliases.
        unsafe {
            (*sc_ptr).dev = Some(NonNull::new(dev).unwrap());
            // If a softc has both a device_t and a cdev pointer, the device_t is always initialized
            // first since newbus allocates the device softc. That means there should be no case
            // where this was already initialized to Some. We have to initialize it to soundly
            // create references to the entire Softc so None is the correct value here.
            (*sc_ptr).cdev = AtomicPtr::new(null_mut());
            (*sc_ptr).mtype = None;
            sc_ptr.cast::<Self>().as_ref().unwrap()
        }
    }

    pub fn device(&self) -> &Device {
        // We still can't make a reference to the entire Softc<T> so calling Softc::device
        // is not an option to get a device_t.
        let sc_ptr: *const Softc<T> = self.0.get().cast();
        // SAFETY: `dev` was initialized in `from_raw`.
        let dev = unsafe { (*sc_ptr).dev };
        match dev {
            Some(nonnull_dev) => {
                // SAFETY: The lifetime of the Device matches the Uninit borrow
                unsafe { nonnull_dev.as_ptr().cast::<Device>().as_ref().unwrap() }
            }
            None => unreachable!(),
        }
    }

    /// Initialize the softc to `t`.
    ///
    /// Panics if the softc was already initialized.
    pub fn init(&self, t: T) -> &Softc<T> {
        let sc_ptr: *mut Softc<T> = self.0.get().cast();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw mut (*sc_ptr).count });

        // newbus zeroes the softc so count is only nonzero after init. Uninit is !Sync so this
        // can't race.
        assert!(unsafe { *count_ptr } == 0, "softc was already initialized");

        unsafe {
            let inner_ptr = &raw mut (*sc_ptr).inner;
            // Use the write method to avoid calling drop on the old (zeroed) softc value
            inner_ptr.write(t);
        }

        // This points to the heap, but this is just an address-insensitive atomic write anyway
        unsafe { bindings::refcount_init(count_ptr, 1) };

        // All fields are now initialized since `dev` was written in `from_raw` and `inner` and
        // `count` were written above.
        unsafe { sc_ptr.as_ref().unwrap() }
    }

    #[doc(hidden)]
    pub fn is_init(&self) -> bool {
        let sc_ptr: *mut Softc<T> = self.0.get().cast();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw const (*sc_ptr).count });
        unsafe { bindings::refcount_load(count_ptr) != 0 }
    }
}

impl<T: 'static> Ptr<Softc<T>> {
    pub fn into_raw_with_count(t: Self) -> (*mut T, *mut u_int) {
        let ptr = t.0.as_ptr();
        let count_ptr = UnsafeCell::raw_get(unsafe { &raw const (*ptr).count });
        forget(t);
        (ptr.cast::<T>(), count_ptr)
    }

    pub fn as_pin(&self) -> Pin<&T> {
        unsafe { Pin::new_unchecked(&self.0.as_ref().inner) }
    }

    pub fn project(&self) -> T::ProjHelper<'_>
    where
        T: PinProject,
    {
        self.as_pin().project()
    }
}

#[repr(C)]
pub struct Ptr<T: Refcounted + 'static>(pub(crate) NonNull<T>, PhantomData<T>);

impl<T: Refcounted> Ptr<T> {
    pub fn new(t: &T) -> Self {
        t.acquire_ref();
        Self(NonNull::from(t), PhantomData)
    }

    pub fn free(t: Self) {
        let ptr = t.0;
        forget(t);
        let last = unsafe { T::release_ref(ptr) };
        assert!(last, "Freed a softc with other references still held");
    }

    pub fn as_raw(t: &Self) -> *mut T {
        t.0.as_ptr()
    }

    pub fn into_raw(t: Self) -> *mut T {
        let ptr = t.0.as_ptr();
        forget(t);
        ptr
    }

    pub unsafe fn from_raw(ptr: *mut T) -> Self {
        Self(NonNull::new(ptr).unwrap(), PhantomData)
    }
}

impl<T: Refcounted> Drop for Ptr<T> {
    fn drop(&mut self) {
        unsafe { T::release_ref(self.0) };
    }
}

impl<T: Refcounted> Deref for Ptr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY: The pointee is freed in device_detach, but the KPI glue for it panics if there is
        // an outstanding softc Ptr when it's ready to free it. The return value lifetime is tied
        // to the Ptr borrow.
        unsafe { self.0.as_ref() }
    }
}

unsafe impl<T: Refcounted + Sync + Send> Sync for Ptr<T> {}
unsafe impl<T: Refcounted + Sync + Send> Send for Ptr<T> {}

#[repr(C)]
pub struct NullablePtr<T: Refcounted + 'static>(pub(crate) AtomicPtr<T>);

impl<T: Refcounted> NullablePtr<T> {
    pub const fn new() -> NullablePtr<T> {
        NullablePtr(AtomicPtr::new(null_mut()))
    }

    pub fn take(&self) -> Option<Ptr<T>> {
        let ptr = self.0.swap(null_mut(), Ordering::Acquire);
        if ptr.is_null() {
            return None;
        }
        Some(unsafe { Ptr::from_raw(ptr) })
    }

    pub fn swap(&self, ptr: Ptr<T>) -> Option<Ptr<T>> {
        let new_ptr = Ptr::into_raw(ptr);
        let old_ptr = self.0.swap(new_ptr, Ordering::AcqRel);
        if old_ptr.is_null() {
            return None;
        }
        Some(unsafe { Ptr::from_raw(old_ptr) })
    }
}

impl<T: Refcounted> Drop for NullablePtr<T> {
    fn drop(&mut self) {
        let _ptr = self.take();
    }
}
