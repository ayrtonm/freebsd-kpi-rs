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

//! Utilities related to FFI with C.

use crate::bindings;
use crate::bindings::u_int;
use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use crate::vec::Vec;
use core::cell::UnsafeCell;
use core::ffi::CStr;
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::mem::{forget, offset_of};
use core::ops::{Deref, DerefMut, Drop};
use core::ptr::{null_mut, write};

/// Metadata for refcounting Ptr and FatPtr
#[repr(C)]
pub struct RefCountData {
    drop_fn: unsafe fn(*mut u_int),
    count: u_int,
}

impl RefCountData {
    /// Projects the metadata pointer to a pointer to its count field
    pub fn count_ptr(metadata_ptr: *mut Self) -> *mut u_int {
        unsafe { &raw mut (*metadata_ptr).count }
    }

    /// Get the drop function
    pub fn drop_fn(metadata_ptr: *mut Self) -> unsafe fn(*mut u_int) {
        // SAFETY: The drop function of the RefCountData should not change once it's initialized
        unsafe { *Self::drop_fn_ptr(metadata_ptr) }
    }

    pub fn drop_fn_ptr(metadata_ptr: *mut Self) -> *mut unsafe fn(*mut u_int) {
        unsafe { &raw mut (*metadata_ptr).drop_fn }
    }

    /// Releases the reference, possibly calling the drop callback in the process.
    ///
    /// # Safety
    ///
    /// The caller must ensure that it conceptually owns a refcount to the pointee. For example,
    /// while a `Ptr<T>` can be used to obtain multiple `*mut RefCountData` pointers it may only
    /// release_ref once since it only owns one refcount.
    pub unsafe fn release_ref(metadata_ptr: *mut Self) {
        let count_ptr = Self::count_ptr(metadata_ptr);
        let last = unsafe { bindings::refcount_release(count_ptr) };
        if last {
            let drop_fn = unsafe { (*metadata_ptr).drop_fn };
            unsafe { (drop_fn)(count_ptr) };
        }
    }

    pub fn count_offset() -> usize {
        offset_of!(Self, count)
    }
}

/// An atomically recounted `T` on the heap.
///
/// [`RefCounted<T>`] implements [`Deref<Target = T>`][`Deref`] which allows it to be transparently
/// used like a `T`. This includes both accessing fields and methods the normal dot syntax (e.g.
/// `refcounted_t.some_field_on_t`). This is how it's primarily intended to be used and it exposes
/// very few methods itself.
///
/// # Invariants
///
/// All instances of this type are heap-allocated. For this reason it does not expose a public
/// constructor outside this crate. Instead `&RefCounted<T>` are provided by some traits and
/// methods in this crate. A consequence of the [`RefCounted<T>`] struct itself being on the heap is
/// that this crate always provides access via references. Hitting borrow checker issues due to the
/// lifetime associated with these references can usually be fixed by calling its
/// [`grab_ref`][RefCounted::grab_ref] method.
///
/// # Projection
///
/// Sometimes it may be useful to get a pointer to a field and pass the assurances that the initial
/// pointer type provided on to the new pointer. In this case [`RefCounted<T>`] owns a refcount
/// ensuring that the `T` lives at least as long as the reference. Doing a "pointer projection" to
/// one of `T`'s fields would then create a new pointer to that field which owns some refcount which
/// will ensure the field lives as long as necessary. Since it's difficult to know ahead of time
/// which fields will have projections created, projecting a `RefCounted<T>` uses the `T`'s refcount
/// for the field as well. This means that the entire `T` will live as long as the new field pointer
/// is alive. To instead use a dedicated refcount for a part of the `T` the solution would be to
/// make the field an `Arc` (which comes at the cost of a level of indirection). This operation is
/// currently done using the [`project!`] macro which also documents it in more detail.
///
/// # [`RefCounted`] vs [`Arc`][crate::sync::arc::Arc]
///
/// One might ask why provide both [`RefCounted`] and [`Arc`][crate::sync::arc::Arc] (i.e.
/// atomic-refcounted) if they're both for thread-safe refcounted types? Ultimately it boils down to
/// differences in the memory layout of these types and the different capabilities they provide. In
/// [`RefCounted<T>`] the `T` is stored first while [`Arc<T>`][crate::sync::arc::Arc] stores it
/// last. Storing it first allows easier interop when `T` is a subclass of another type. For example
/// a `RefCounted<SubClass<Base, SubFields>>` has the same layout as this C struct.
///
/// ```c
/// struct RefCounted {
///     struct SubClass {
///         struct Base { ... } base;
///         struct SubFields { ... } sub_fields;
///     };
///     struct RefCountData metadata;
///     bool init;
/// };
/// ```
///
/// This layout makes it trivial to turn a `struct Base *ptr` received from some C API into a
/// `&RefCounted<SubClass<Base, SubFields>>` in rust without adding/subtracting any pointer offsets.
/// The downside of this layout is that the size of `T` must be known at compile-time (i.e. has the
/// implicit bound [`Sized`]). Since [`Arc`][crate::sync::arc::Arc] has `T` at the end it opts it
/// out of this bound (i.e. allows `T: ?Sized`). This means you can create things like `Arc<[u32]>`
/// and other types where `T` has its size determined at runtime.
#[repr(C)]
pub struct RefCounted<T> {
    // This field must be first to support subclass drivers like simplebus and nvme
    t: T,
    metadata: RefCountData,
}

impl<T> RefCounted<T> {
    #[doc(hidden)]
    pub fn metadata_offset() -> usize {
        offset_of!(Self, metadata)
    }

    #[doc(hidden)]
    pub fn t_ptr(ptr: *mut Self) -> *mut T {
        unsafe { &raw mut (*ptr).t }
    }

    #[doc(hidden)]
    pub fn metadata_ptr(ptr: *mut Self) -> *mut RefCountData {
        unsafe { &raw mut (*ptr).metadata }
    }

    /// Grabs a refcount and returns a [`Ptr<T>`] which logically owns it.
    pub fn grab_ref(&self) -> Ptr<T> {
        Ptr::new(self)
    }

    /// Grabs a refcount, leaks it and returns a reference with a static lifetime.
    ///
    /// Note that leaking the refcount prevents the `T` from ever being freed.
    pub fn leak_ref(&self) -> &'static T {
        let ptr = self.grab_ref();
        let res = ptr.deref() as *const T;
        forget(ptr);
        unsafe { res.as_ref().unwrap() }
    }
}

// Allows using a `RefCounted<T>` like a `&T`
impl<'a, T> Deref for RefCounted<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.t
    }
}

// Format the `RefCounted<T>` as if it were a `&T`.
impl<T: Debug> Debug for RefCounted<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

/// A pointer to a `T` which owns a refcount for it.
///
/// [`Ptr<T>`] can be created by calling the [`grab_ref`][RefCounted::grab_ref] method on a
/// [`RefCounted`] reference. Other methods, such as
/// [`device_get_softc`][crate::device::device_get_softc()] may also return [`Ptr<T>`]s. Dropping a
/// [`Ptr<T>`] releases the refcount it owns.
///
/// [`Ptr<T>`] implements [`Deref<Target = T>`][`Deref`] which allows it to be transparently used
/// like a `T`. This includes both accessing fields and methods the normal dot syntax (e.g.
/// `ptr_t.some_field_on_t`). This is how it's primarily intended to be used and it exposes very few
/// methods itself.
#[repr(C)]
pub struct Ptr<T>(*mut RefCounted<T>);

impl<T> Ptr<T> {
    /// Grabs an additional refcount to a `T` and creates a new `Ptr<T>`.
    fn new(rc_t: &RefCounted<T>) -> Self {
        let count_ptr = &raw const rc_t.metadata.count;
        // SAFETY: The pointer is derived from the `&RefCounted<T>` arg passed in to this function.
        // acquiring a refcount is thread-safe so this may happen concurrently with other operations
        unsafe { bindings::refcount_acquire(count_ptr.cast_mut()) };
        let ptr = rc_t as *const RefCounted<T>;
        Self(ptr.cast_mut())
    }

    /// Creates a [`Ptr`] from a pointer to a [`RefCounted<T>`] without grabbing a refcount.
    ///
    /// # Safety
    ///
    /// The caller must ensure that there is at least one refcount that the [`Ptr`] can logically
    /// own.
    pub(crate) unsafe fn from_raw(ptr: *mut RefCounted<T>) -> Self {
        Self(ptr)
    }

    pub fn grab_ref(&self) -> Self {
        self.clone()
    }

    pub fn leak_ref(&self) -> &'static T {
        unsafe { self.0.as_ref().unwrap().leak_ref() }
    }
}

// Allows using a `Ptr<T>` just like a `&T`.
impl<'a, T> Deref for Ptr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY: The `Ptr<T>` owns a refcount to the `T` so it will not get dropped while the
        // `Ptr<T>` is alive.
        unsafe { &self.0.as_ref().unwrap().t }
    }
}

// Format the `Ptr<T>` as if it were a `&T`.
impl<T: Debug> Debug for Ptr<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

// Explicitly clone the `Ptr<T>` grabbing a new refcount in the process.
impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        let metadata_ptr = RefCounted::metadata_ptr(self.0);
        let count_ptr = RefCountData::count_ptr(metadata_ptr);
        unsafe { bindings::refcount_acquire(count_ptr) };
        Self(self.0)
    }
}

// Dropping the pointer releases the refcount to the `T` that it owns.
impl<T> Drop for Ptr<T> {
    fn drop(&mut self) {
        let metadata_ptr = RefCounted::metadata_ptr(self.0);
        unsafe { RefCountData::release_ref(metadata_ptr) }
    }
}

// SAFETY: `SyncPtr` is intended for cases where `Sync` is intentionally desired on the pointer
unsafe impl<T: Sync> Sync for Ptr<T> {}

/// A pointer type implementing `Sync`.
///
/// This is useful for creating pointer types that are expected to be shared between threads without
/// explicit synchronization.
#[repr(C)]
#[derive(Debug)]
pub struct SyncPtr<T>(pub(crate) *mut T);

impl<T> Default for SyncPtr<T> {
    fn default() -> Self {
        Self(null_mut())
    }
}

// Allows explicitly cloning a `SyncPtr` just like a regular raw pointer
impl<T> Clone for SyncPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

// Allows implicitly copying a `SyncPtr` just like a raw pointer
impl<T> Copy for SyncPtr<T> {}

impl<T> SyncPtr<T> {
    /// Creates a new null `SyncPtr`
    pub const fn null() -> Self {
        Self(null_mut())
    }

    /// Creates a new `SyncPtr` from a raw pointer.
    pub const fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }

    /// Get a raw pointer for the `SyncPtr`
    pub fn as_ptr(self) -> *mut T {
        self.0
    }

    pub fn is_null(&self) -> bool {
        self.0.is_null()
    }
}

// SAFETY: `SyncPtr` is intended for cases where `Sync` is intentionally desired on the pointer
unsafe impl<T> Sync for SyncPtr<T> {}
unsafe impl<T> Send for SyncPtr<T> {}

impl Debug for SyncPtr<RefCountData> {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

pub enum CString<const N: usize = 16> {
    Small([u8; N]),
    Heap(Vec<u8>),
}

impl<const N: usize> CString<N> {
    pub fn new(msg: &str, ty: MallocType, flags: MallocFlags) -> Result<Self> {
        let mut buf = Vec::try_with_capacity(msg.as_bytes().len() + 1, ty, flags).unwrap();
        for &b in msg.as_bytes() {
            buf.try_push(b);
        }
        buf.try_push(0);
        Ok(Self::Heap(buf))
    }

    pub fn as_c_str(&self) -> &CStr {
        match self {
            Self::Small(_) => todo!(""),
            Self::Heap(s) => unsafe { CStr::from_ptr(s.as_ptr().cast()) },
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Small(s) => unsafe { CStr::from_ptr(s.as_ptr().cast()).count_bytes() },
            Self::Heap(s) => s.len() - 1,
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        match self {
            Self::Small(s) => &s.as_slice()[0..self.len()],
            Self::Heap(s) => s.as_slice(),
        }
    }
}

impl<const N: usize> From<[u8; N]> for CString<N> {
    fn from(array: [u8; N]) -> Self {
        Self::Small(array)
    }
}

/// A struct containing a base class `B` followed by extra fields `F`.
///
/// This type assumes the base class is shared with C code and is primarily intended to be used for
/// interfacing with existing KPIs which require a subclass of a C type. If that is not necessary
/// this type should be avoided.
///
/// `SubClass<B, F>` has a `Deref<Target = F>` impl to allow using it like an `F`. For example if
/// `F` is a struct with fields named `foo` and `bar`, a subclass allows transparently accessing
/// them using `my_subclass.foo` and `my_subclass.bar`.
///
/// Accessing the base class fields requires using the [`base!`] macro which uses C-like syntax for
/// reading, writing and getting the address of fields.
///
/// ```
/// let baz_addr: *mut BazTy = base!(&my_subclass->baz);
/// let old_baz_value = unsafe { base!(my_subclass->baz) };
/// unsafe { base!(my_subclass->baz) = new_baz_value; };
///
/// ```
///
/// For example if `B` is a struct with a field named `baz`, a subclass allows getting its address using `base!(&my_subclass->baz)` and accessing it using `unsafe { base!(my_subclass->baz) = baz_value };` or
/// `let baz_value = unsafe { base!(my_subclass->baz) };`. Since the base class is assumed to be
/// shared with C code it requires the user to manually ensure that the field is not being accessed
/// concurrently by other threads.
#[repr(C)]
#[derive(Default)]
pub struct SubClass<B, F> {
    // This needs to be first for some uses of subclass (e.g. simplebus subclass drivers)
    base_class: UnsafeCell<B>,
    // Extra fields last allows making them unsized
    sub_fields: F,
}

impl<B, F: Debug> Debug for SubClass<B, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("SubClass")
            .field("base_class", &..)
            .field("sub_fields", &self.sub_fields)
            .finish()
    }
}

// SAFETY: &SubClass provides no direct access to the &UnsafeCell<B>
unsafe impl<B, F: Sync> Sync for SubClass<B, F> {}

impl<B: Default, F> SubClass<B, F> {
    pub fn new(sub_fields: F) -> Self {
        Self::new_with_base(B::default(), sub_fields)
    }
}
impl<B, F> SubClass<B, F> {
    pub const fn new_with_base(base: B, sub_fields: F) -> Self {
        Self {
            base_class: UnsafeCell::new(base),
            sub_fields,
        }
    }

    pub fn as_base_ptr(sub: &Self) -> *mut B {
        sub.base_class.get()
    }

    pub unsafe fn from_base_ptr<'a>(ptr: *mut B) -> &'a Self {
        unsafe { ptr.cast::<Self>().as_ref().unwrap() }
    }

    pub unsafe fn from_base_ptr_mut<'a>(ptr: *mut B) -> &'a mut Self {
        unsafe { ptr.cast::<Self>().as_mut().unwrap() }
    }
}

impl<B, F> Deref for SubClass<B, F> {
    type Target = F;

    fn deref(&self) -> &Self::Target {
        &self.sub_fields
    }
}

impl<B, F> DerefMut for SubClass<B, F> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.sub_fields
    }
}

pub struct UninitPtr<T>(*mut RefCounted<T>);

impl<T> UninitPtr<T> {
    pub unsafe fn from_raw(ptr: *mut RefCounted<T>) -> Self {
        Self(ptr)
    }

    pub fn init(self, t: T) -> UniquePtr<T> {
        let t_ptr = RefCounted::t_ptr(self.0);
        unsafe { write(t_ptr, t) };
        let metadata_ptr = RefCounted::metadata_ptr(self.0);
        let count_ptr = RefCountData::count_ptr(metadata_ptr);
        unsafe { bindings::refcount_init(count_ptr, 1) };
        UniquePtr(self.0)
    }
}

pub struct UniquePtr<T>(*mut RefCounted<T>);

impl<T> UniquePtr<T> {
    pub fn weak_ref(&self) -> WeakPtr<T> {
        let rc_t = unsafe { self.0.as_ref().unwrap() };
        let ptr = Ptr::new(rc_t);
        let weak = WeakPtr(ptr.0);
        forget(ptr);
        weak
    }

    pub fn grab_ref(self) -> Ptr<T> {
        let rc_t = unsafe { self.0.as_ref().unwrap() };
        Ptr::new(rc_t)
    }

    pub fn leak_ref(self) -> &'static T {
        self.grab_ref().leak_ref()
    }
}

impl<T> Deref for UniquePtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.0.as_ref().unwrap().t }
    }
}

impl<T> DerefMut for UniquePtr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut self.0.as_mut().unwrap().t }
    }
}

// Format the `UniquePtr<T>` as if it were a `&T`.
impl<T: Debug> Debug for UniquePtr<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

pub struct WeakPtr<T>(*mut RefCounted<T>);

// A pointer that can be treated as a &RefCounted<T> after device_attach
pub trait SharedPtr<T> {
    fn into_raw_parts(ptr: Self) -> (*mut RefCounted<T>, *mut RefCountData);
}

impl<T> SharedPtr<T> for Ptr<T> {
    fn into_raw_parts(ptr: Self) -> (*mut RefCounted<T>, *mut RefCountData) {
        let metadata_ptr = RefCounted::metadata_ptr(ptr.0);
        let rc_t_ptr = ptr.0;
        forget(ptr);
        (rc_t_ptr, metadata_ptr)
    }
}

impl<T> SharedPtr<T> for WeakPtr<T> {
    fn into_raw_parts(ptr: Self) -> (*mut RefCounted<T>, *mut RefCountData) {
        let metadata_ptr = RefCounted::metadata_ptr(ptr.0);
        let rc_t_ptr = ptr.0;
        forget(ptr);
        (rc_t_ptr, metadata_ptr)
    }
}
