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

use crate::bindings;
use crate::bindings::{_device, device_t, u_int};
use crate::boxed::Box;
use crate::malloc::MallocType;
use crate::prelude::*;
use crate::ErrCode;
use core::any::TypeId;
use core::cell::UnsafeCell;
use core::ffi::{c_void, CStr};
use core::mem::{forget, offset_of};
use core::ops::{Deref, DerefMut, Drop};
use core::ptr::{drop_in_place, null_mut};
use core::sync::atomic::{AtomicBool, Ordering};
use core::sync::atomic::{AtomicU16, AtomicU32, AtomicU64, AtomicU8};

/// A value borrow-checked at runtime
#[derive(Debug)]
pub struct Checked<T> {
    t: UnsafeCell<T>,
    borrowed: AtomicBool,
}

unsafe impl<T> Sync for Checked<T> {}

impl<T> Checked<T> {
    pub fn new(t: T) -> Self {
        Self {
            t: UnsafeCell::new(t),
            borrowed: AtomicBool::new(false),
        }
    }

    pub fn get_mut(&self) -> CheckedRef<'_, T> {
        if !self
            .borrowed
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            panic!("already borrowed");
        }
        CheckedRef {
            value: self.t.get(),
            borrowed: &self.borrowed,
        }
    }
}

pub struct CheckedRef<'b, T: 'b + ?Sized> {
    value: *mut T,
    borrowed: &'b AtomicBool,
}

impl<T: ?Sized> Deref for CheckedRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.value.as_ref().unwrap() }
    }
}

impl<T: ?Sized> DerefMut for CheckedRef<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.value.as_mut().unwrap() }
    }
}

impl<T: ?Sized> Drop for CheckedRef<'_, T> {
    fn drop(&mut self) {
        self.borrowed.store(false, Ordering::Relaxed);
    }
}

/// A struct containing a base class B and extra fields F.
///
/// The definition assumes that the base class is shared between Rust and C but places no
/// restriction on the extra fields so it may be possible to create references to them.
#[repr(C)]
#[derive(Debug)]
pub struct SubClass<B, F> {
    // This doesn't need to be first for all uses of subclasses (e.g. pic_if.m) but it does for some
    // (e.g. simplebus subclass drivers)
    base: UnsafeCell<B>,
    sub: F,
}

impl<B: Default, F> SubClass<B, F> {
    pub fn new(sub: F) -> Self {
        Self {
            base: UnsafeCell::new(B::default()),
            sub,
        }
    }
}

impl<B, F> SubClass<B, F> {
    pub const fn new_with_base(base: B, sub: F) -> Self {
        Self {
            base: UnsafeCell::new(base),
            sub,
        }
    }

    pub fn get_base(&self) -> &B {
        unsafe { self.base.get().as_ref().unwrap() }
    }

    pub fn get_base_mut(&mut self) -> &mut B {
        unsafe { self.base.get().as_mut().unwrap() }
    }

    pub fn base_ptr(ptr: *mut Self) -> *mut B {
        assert!(offset_of!(Self, base) == 0);
        ptr.cast()
    }

    pub fn get_base_ptr(sub: &Self) -> *mut B {
        sub.base.get()
    }

    pub unsafe fn from_base<'a>(ptr: *mut B) -> &'a mut Self {
        assert!(offset_of!(Self, base) == 0);
        ptr.cast::<Self>().as_mut().unwrap()
    }
}

impl<B, F> Deref for SubClass<B, F> {
    type Target = F;

    fn deref(&self) -> &Self::Target {
        &self.sub
    }
}

impl<B, F> DerefMut for SubClass<B, F> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.sub
    }
}

pub trait OwnedVar<T: ?Sized, O: ?Sized = _device> {
    fn get_var_ptr(&self) -> *mut T;
    fn get_owner(&self) -> *mut O;
}

impl<T: ?Sized, M: MallocType> OwnedVar<T, T> for Box<T, M> {
    fn get_var_ptr(&self) -> *mut T {
        self.as_ptr()
    }
    fn get_owner(&self) -> *mut T {
        self.get_var_ptr()
    }
}

impl<T, const HOLDS_REFCOUNT: bool> OwnedVar<T> for OwnedPtr<T, HOLDS_REFCOUNT> {
    fn get_var_ptr(&self) -> *mut T {
        self.ptr
    }
    fn get_owner(&self) -> device_t {
        self.owner
    }
}

impl<'a, T: ?Sized> OwnedVar<T> for OwnedRef<'a, T> {
    fn get_var_ptr(&self) -> *mut T {
        let ptr = self.borrow as *const T;
        ptr.cast_mut()
    }
    fn get_owner(&self) -> device_t {
        self.owner
    }
}

impl<'a, T: ?Sized> OwnedVar<T> for &'a OwnedRef<'a, T> {
    fn get_var_ptr(&self) -> *mut T {
        let ptr = self.borrow as *const T;
        ptr.cast_mut()
    }
    fn get_owner(&self) -> device_t {
        self.owner
    }
}

impl<'a, T: ?Sized> OwnedVar<T> for OwnedMutRef<'a, T> {
    fn get_var_ptr(&self) -> *mut T {
        let ptr = self.borrow as *const T;
        ptr.cast_mut()
    }
    fn get_owner(&self) -> device_t {
        self.owner
    }
}

impl<'a, T: ?Sized> OwnedVar<T> for &'a OwnedMutRef<'a, T> {
    fn get_var_ptr(&self) -> *mut T {
        let ptr = self.borrow as *const T;
        ptr.cast_mut()
    }
    fn get_owner(&self) -> device_t {
        self.owner
    }
}

/// A pointer to a variable known to live as long as some device's softc.
///
///
#[repr(C)]
#[derive(Debug)]
pub struct OwnedPtr<T = (), const HOLDS_REFCOUNT: bool = false> {
    ptr: *mut T,
    owner: device_t,
    refcount: *mut u_int,
    drop_fn: Option<unsafe extern "C" fn(*mut c_void)>,
}

impl<T> OwnedPtr<T, false> {
    /// Grab a refcount to the softc returning a new `OwnedPtr` that can be used like a reference.
    pub fn refcount_this(&self) -> OwnedPtr<T, true> {
        unsafe { bindings::rust_bindings_refcount_acquire(self.refcount) };
        OwnedPtr {
            ptr: self.ptr,
            owner: self.owner,
            refcount: self.refcount,
            drop_fn: self.drop_fn,
        }
    }

    pub unsafe fn assume_can_reference(&self) -> OwnedRef<T> {
        OwnedRef {
            borrow: unsafe { self.ptr.as_ref().unwrap() },
            owner: self.owner,
            refcount: self.refcount,
            drop_fn: self.drop_fn.unwrap(),
        }
    }
}

impl<T> OwnedPtr<T, true> {
    /// Grab a reference to the pointee to allow using it as a regular reference.
    ///
    /// The caller must have previously taken a refcount to the pointee's softc using
    /// `.refcount_this()` and cannot have erased the pointer's generic parameter.
    pub fn as_ref(&self) -> OwnedRef<T> {
        assert!(TypeId::of::<T>() != TypeId::of::<()>());
        OwnedRef {
            borrow: unsafe { self.ptr.as_ref().unwrap() },
            owner: self.owner,
            refcount: self.refcount,
            drop_fn: self.drop_fn.unwrap(),
        }
    }
}

impl<T, const HOLDS_REFCOUNT: bool> OwnedPtr<T, HOLDS_REFCOUNT> {
    pub fn null() -> Self {
        Self {
            ptr: null_mut(),
            owner: null_mut(),
            refcount: null_mut(),
            drop_fn: None,
        }
    }

    pub fn erase_type(self) -> OwnedPtr<(), HOLDS_REFCOUNT> {
        let res = OwnedPtr {
            ptr: self.ptr.cast::<()>(),
            owner: self.owner,
            refcount: self.refcount,
            drop_fn: self.drop_fn,
        };
        forget(self);
        res
    }
}

impl<T, const HOLDS_REFCOUNT: bool> Clone for OwnedPtr<T, HOLDS_REFCOUNT> {
    /// Explicitly create a copy of the pointer.
    ///
    /// To use the new pointer as a reference the caller must call `.refcount_this()` regardless of
    /// whether the old pointer already did or not.
    fn clone(&self) -> Self {
        if HOLDS_REFCOUNT {
            unsafe { bindings::rust_bindings_refcount_acquire(self.refcount) };
        }
        Self {
            ptr: self.ptr,
            owner: self.owner,
            refcount: self.refcount,
            drop_fn: self.drop_fn,
        }
    }
}

impl<T, const HOLDS_REFCOUNT: bool> Drop for OwnedPtr<T, HOLDS_REFCOUNT> {
    fn drop(&mut self) {
        if !HOLDS_REFCOUNT {
            return;
        }
        if self.refcount.is_null() {
            return;
        }
        let last = unsafe { bindings::rust_bindings_refcount_release(self.refcount) };
        if last {
            let sc_void_ptr = unsafe { bindings::device_get_softc(self.owner) };
            // Call the softc's destructors
            unsafe { (self.drop_fn.unwrap())(sc_void_ptr) };
            // Free the softc
            unsafe { bindings::device_free_softc(sc_void_ptr) };
        };
    }
}

/// A reference to a variable known to live as long as some device's softc.
///
/// The variable is usually the device softc itself, a subset of it or something referenced by the
/// softc. It can be used like a regular `&T` reference and that is the recommended usage if
/// possible. Some KPIs take a pointer which must live at least as long as some device is running so
/// their rust wrappers take `OwnedRef`. Given an `OwnedRef` to some variable `x` you can create an
/// `OwnedRef` to a field in `x` with `project!(x.field)`. In the future this "projection" operation
/// may change to `x->field` (see the
/// [field projection v2 RFC](<https://github.com/rust-lang/rfcs/pull/3735>)).
///
/// # Example
///
/// ```
/// use kpi::cell::OwnedRef;
///
/// struct Foo {
///     some_field: u32,
/// }
///
/// // This function takes a struct `Foo` named `x` that is known to live as long as some device's
/// // softc.
/// fn foo(x: OwnedRef<Foo>) {
///     // Passing a `&u32` reference to a local variable will not compile since `bar`'s argument
///     // must be owned by a device_t.
///     let local_var: u32 = 4;
///     // bar(&local_var);
///
///     // Taking a reference to the `OwnedRef`'s field will also return a regular regular `&u32`
///     // and not compile. This behavior is caused by the `Deref` impl on `OwnedRef` and is done
///     // for convenience since most APIs should not care if a reference to a variable is owned by
///     // a device or not.
///     let field_ref: &u32 = &x.some_field;
///     // bar(field_ref);
///
///     // To call bar we need to "project" the `OwnedRef` `x` to the field we want to reference.
///     let owned_field_ref: OwnedRef<u32> = x.project(|v| x.some_field);
///     bar(owned_field_ref);
///
///     // For convenience the `project!` macro does the same but just takes a reference instead of
///     // a function as argument. Also the types on the right hand side may generally be omitted
///     // since they can usually be inferred.
///     let other_owned_field_ref = project!(x.some_field);
///     bar(other_owned_field_ref);
/// }
///
/// // This function takes a reference to a u32 and may need to do something which requires that the
/// // reference lives as long as the device softc (e.g. pass it back to some kernel API) so it
/// // takes an `OwnedRef<u32>`.
/// fn bar(y: OwnedRef<u32>) {
///     // Will print the argument as if it were a `u32`
///     println!("{y}");
/// }
///
/// ```
#[repr(C)]
#[derive(Debug)]
pub struct OwnedRef<'a, T: ?Sized + 'static> {
    borrow: &'a T,
    owner: device_t,
    refcount: *mut u_int,
    // TODO: This can be derived from the refcount pointer by putting it in the softc
    drop_fn: unsafe extern "C" fn(*mut c_void),
}

/// A mutable reference to a variable known to be owned by a device
#[repr(C)]
#[derive(Debug)]
pub struct OwnedMutRef<'a, T: ?Sized> {
    borrow: &'a mut T,
    owner: device_t,
    refcount: *mut u_int,
    drop_fn: unsafe extern "C" fn(*mut c_void),
}

impl<'a, T: ?Sized> OwnedRef<'a, T> {
    /// Create an `OwnedRef` with the given owner.
    ///
    /// # Safety
    ///
    /// Only allowed if the thing being borrowed lives as long as the owner's softc.
    pub unsafe fn new(
        borrow: &'a T,
        owner: device_t,
        refcount: *mut u_int,
        drop_fn: unsafe extern "C" fn(*mut c_void),
    ) -> Self {
        Self {
            borrow,
            owner,
            refcount,
            drop_fn,
        }
    }

    /// Create an `OwnedRef` from a subset of another.
    pub fn project<U: ?Sized, F: FnOnce(&T) -> &U>(&self, f: F) -> OwnedRef<U> {
        // SAFETY: The function `f` enforces that its return value is a shorter than or equal to the
        // lifetime of `self.borrow` so the new borrow `f(self.borrow)` lives at least as long as
        // the old `OwnedRef`.
        unsafe { OwnedRef::new(f(self.borrow), self.owner, self.refcount, self.drop_fn) }
    }
}

/// This increments the softc refcount by one. Dropping the returned `OwnedPtr` will decrement
/// the refcount by one. Regardless of the refcount the earliest the softc will be freed is
/// after calling device_detach with the device.
/// This is useful for storing the thing being borrowed somewhere that lives across calls to
/// driver interface functions.

impl<'a, T> OwnedRef<'a, T> {
    /// Erase the lifetime of the `OwnedRef` creating an `OwnedPtr`.
    ///
    /// This is purely a type system transformation for pointers in kernel interfaces that need a
    /// lifetime longer than the one returned by `device_get_softc!`.
    pub fn erase_lifetime(&self) -> OwnedPtr<T, false> {
        let ptr = self.borrow as *const T;
        OwnedPtr {
            ptr: ptr.cast_mut(),
            owner: self.owner,
            refcount: self.refcount,
            drop_fn: Some(self.drop_fn),
        }
    }
}

impl<'a, T: ?Sized> OwnedMutRef<'a, T> {
    pub unsafe fn new(
        borrow: &'a mut T,
        owner: device_t,
        refcount: *mut u_int,
        drop_fn: unsafe extern "C" fn(*mut c_void),
    ) -> Self {
        Self {
            borrow,
            owner,
            refcount,
            drop_fn,
        }
    }

    pub fn project<U: ?Sized, F: FnOnce(&T) -> &U>(&self, f: F) -> OwnedRef<U> {
        unsafe { OwnedRef::new(f(self.borrow), self.owner, self.refcount, self.drop_fn) }
    }

    pub fn project_mut<U: ?Sized, F: FnOnce(&mut T) -> &mut U>(&mut self, f: F) -> OwnedMutRef<U> {
        unsafe { OwnedMutRef::new(f(self.borrow), self.owner, self.refcount, self.drop_fn) }
    }
}

impl<'a, T> OwnedMutRef<'a, T> {
    pub fn erase_lifetime(&self) -> OwnedPtr<T, false> {
        let ptr = self.borrow as *const T as *mut T;
        OwnedPtr {
            ptr,
            owner: self.owner,
            refcount: self.refcount,
            drop_fn: Some(self.drop_fn),
        }
    }
}

impl<'a, T: ?Sized> Deref for OwnedRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.borrow
    }
}

impl<'a, T: ?Sized> Deref for OwnedMutRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.borrow
    }
}

impl<'a, T: ?Sized> DerefMut for OwnedMutRef<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.borrow
    }
}

pub trait Sleepable {
    fn as_ptr(&self) -> *mut c_void;
}
impl<T> Sleepable for Checked<T> {
    fn as_ptr(&self) -> *mut c_void {
        self.t.get().cast::<c_void>()
    }
}
impl Sleepable for AtomicU8 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}
impl Sleepable for AtomicU16 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}
impl Sleepable for AtomicU32 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}
impl Sleepable for AtomicU64 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}

pub mod wrappers {
    use super::*;

    pub fn tsleep<T: Sleepable>(chan: &T, priority: i32, wmesg: &CStr, timo: i32) -> Result<()> {
        let chan_ptr = chan.as_ptr();
        let wmesg_ptr = wmesg.as_ptr();
        let res = unsafe {
            bindings::_sleep(
                chan_ptr,
                null_mut(),
                priority,
                wmesg_ptr,
                bindings::tick_sbt * timo as i64,
                0,
                bindings::C_HARDCLOCK,
            )
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn wakeup<T: Sleepable>(chan: &T) {
        let chan_ptr = chan.as_ptr();
        unsafe { bindings::wakeup(chan_ptr) }
    }
}
