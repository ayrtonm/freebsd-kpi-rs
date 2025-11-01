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

use crate::bindings::u_int;
use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use crate::vec::Vec;
use crate::{bindings, project};
use core::cell::UnsafeCell;
use core::ffi::CStr;
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::mem::{MaybeUninit, forget, offset_of};
use core::ops::{Deref, DerefMut, Drop};
use core::ptr::{null_mut, write};

#[doc(hidden)]
pub trait Projectable<T, U> {
    type Projection;
    type ProjectFn;

    fn project(self, f: Self::ProjectFn) -> Self::Projection;
}

impl<'a, T, U> Projectable<T, U> for &'a RefCounted<T> {
    type Projection = FatPtr<U>;
    type ProjectFn = fn(&T) -> &U;
    /// Projects the `&RefCounted<T>` to one of its fields or through layers of indirection.
    fn project(self, f: Self::ProjectFn) -> Self::Projection {
        Projectable::project(self.grab_ref(), f)
    }
}

impl<T, U> Projectable<T, U> for Ptr<T> {
    type Projection = FatPtr<U>;
    type ProjectFn = fn(&T) -> &U;
    /// Projects the pointer to one of its fields or through layers of indirection.
    ///
    /// Given a type `T` with a field of type `U` projection takes a pointer to a `T` and adds an
    /// offset to create a pointer to its `U` field. This operation consumes the `Ptr<T>` without
    /// releasing the refcount and transfers ownership of it to the `FatPtr<U>`. Note that
    /// `FatPtr` is has the memory layout of two pointers in a struct so it may not be used in all
    /// interfaces that accept a `Ptr` which has the same layout as a single pointer in C.
    fn project(self, f: Self::ProjectFn) -> Self::Projection {
        let u_ptr = f(self.deref()) as *const U;
        let metadata_ptr = Ptr::metadata_ptr(&self);
        forget(self);
        FatPtr {
            ptr: u_ptr.cast_mut(),
            metadata_ptr,
        }
    }
}

impl<T, U> Projectable<T, U> for FatPtr<T> {
    type Projection = FatPtr<U>;
    type ProjectFn = fn(&T) -> &U;

    /// Projects the pointer to one of its fields or through layers of indirection.
    ///
    /// Given a type `T` with a field of type `U` projection takes a pointer to a `T` and adds an
    /// offset to create a pointer to its `U` field. This operation consumes the `FatPtr<T>`
    /// without releasing the refcount and transfers ownership of it to the new `FatPtr<U>`.
    fn project(self, f: Self::ProjectFn) -> Self::Projection {
        let u_ptr = f(self.deref()) as *const U;
        let metadata_ptr = self.metadata_ptr;
        forget(self);
        FatPtr {
            ptr: u_ptr.cast_mut(),
            metadata_ptr,
        }
    }
}

/// A pointer with ownership, potentially shared, of its pointee
///
/// This is currently used to support interfaces that may use either a Ptr or a FatPtr. It may also
/// be extended to support `Box<T>` and `Arc<T>` with `T: Sized`.
///
/// # Implementation Safety
///
/// Taking `Self` by value in these methods must not `Drop` the pointee.
pub unsafe trait OwnedPtr<T>: Sized + Deref<Target = T> {
    /// Break down a pointer into its raw parts leaking a refcount if necessary.
    ///
    /// This leaks any refcount held by the pointer so the caller must ensure the refcount
    /// will get released eventually to avoid leaking the pointee.
    fn into_raw_parts(ptr: Self) -> (*mut T, Option<*mut RefCountData>);

    /// Leak the pointer returning a static reference.
    ///
    /// If `Self` owned a refcount to the pointee, this method irreversibly leaks it.
    fn leak(ptr: Self) -> &'static T {
        let (t_ptr, _metadata_ptr) = Self::into_raw_parts(ptr);
        unsafe { t_ptr.as_ref().unwrap() }
    }
}

// SAFETY: The Ptr<T> owns a refcount to the T and `into_raw_parts` calls `forget` to avoid dropping
// the `T`.
unsafe impl<T> OwnedPtr<T> for Ptr<T> {
    fn into_raw_parts(ptr: Self) -> (*mut T, Option<*mut RefCountData>) {
        let t_ptr = ptr.deref() as *const T;
        let res = (t_ptr.cast_mut(), Some(Ptr::metadata_ptr(&ptr)));
        // Avoid calling Ptr's Drop impl
        forget(ptr);
        res
    }
}

// SAFETY: The FatPtr<T> owns a refcount to the T and `into_raw_parts` calls `forget` to avoid
// dropping the `T`.
unsafe impl<T> OwnedPtr<T> for FatPtr<T> {
    fn into_raw_parts(ptr: Self) -> (*mut T, Option<*mut RefCountData>) {
        let res = (ptr.ptr, Some(ptr.metadata_ptr));
        // Avoid calling FatPtr's Drop impl
        forget(ptr);
        res
    }
}

/// Metadata for refcounting Ptr and FatPtr
#[repr(C)]
pub struct RefCountData {
    pub drop_fn: unsafe fn(*mut u_int),
    pub count: u_int,
}

impl RefCountData {
    /// Projects the metadata pointer to a pointer to its count field
    pub fn count_ptr(metadata_ptr: *mut Self) -> *mut u_int {
        project!(&raw mut metadata_ptr->count)
    }

    /// Get the drop function
    pub fn drop_fn(metadata_ptr: *mut Self) -> unsafe fn(*mut u_int) {
        // SAFETY: The drop function of the RefCountData should not change once it's initialized
        unsafe { *Self::drop_fn_ptr(metadata_ptr) }
    }

    pub fn drop_fn_ptr(metadata_ptr: *mut Self) -> *mut unsafe fn(*mut u_int) {
        project!(&raw mut metadata_ptr->drop_fn)
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
}

/// The layout of the Ptr pointee
///
/// This type intentionally does not expose a constructor outside this module to enforce the
/// invariant that any `RefCounted<T>` which has its address taken is heap-allocated. The `metadata`
/// is intentionally kept private to ensure the `Deref` impl for `RefCounted<T>` works as expected
/// for `T` with a field named `metadata`. The intended way to use a `&RefCounted<T>` is just like a
/// `&T` would be used with the addition that `CPtr`s (and potentially extending its lifetime
/// because of the additional refcount).
#[repr(C)]
pub struct RefCounted<T> {
    // This field must be first to support subclass drivers like simplebus and nvme
    pub(crate) t: T,
    metadata: RefCountData,
    init: bool,
}

impl<T> RefCounted<T> {
    pub fn metadata_offset() -> usize {
        offset_of!(Self, metadata)
    }
    pub fn get_ptr(ptr: *mut Self) -> *mut T {
        project!(&raw mut ptr->t)
    }
    pub fn metadata_ptr(ptr: *mut Self) -> *mut RefCountData {
        project!(&raw mut ptr->metadata)
    }
    pub fn grab_ref(&self) -> Ptr<T> {
        Ptr::new(self)
    }
    pub fn leak_ref(&self) -> &'static T {
        OwnedPtr::leak(self.grab_ref())
    }
}

// Allows using a `RefCounted<T>` like a `&T`
impl<'a, T> Deref for RefCounted<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.t
    }
}

// Allows using a `RefCounted<T>` like a `&mut T`
impl<'a, T> DerefMut for RefCounted<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.t
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
/// Dropping the `Ptr<T>` releases the refcount it owns.
#[repr(C)]
pub struct Ptr<T>(*mut RefCounted<T>);

impl<T> Ptr<T> {
    /// Grabs an additional refcount to a `T` and creates a new `Ptr<T>`.
    pub fn new(rc_t: &RefCounted<T>) -> Self {
        let count_ptr = &raw const rc_t.metadata.count;
        // SAFETY: The pointer is derived from the `&RefCounted<T>` arg passed in to this function.
        // acquiring a refcount is thread-safe so this may happen concurrently with other operations
        unsafe { bindings::refcount_acquire(count_ptr.cast_mut()) };
        let ptr = rc_t as *const RefCounted<T>;
        Self(ptr.cast_mut())
    }

    /// Creates a `Ptr` from a pointer to a `RefCounted<T>` without grabbing a refcount.
    ///
    /// # Safety
    ///
    /// The caller must ensure that there is at least one refcount that the `Ptr` can conceptually
    /// own.
    pub unsafe fn from_raw(ptr: *mut RefCounted<T>) -> Self {
        Self(ptr)
    }

    fn metadata_ptr(ptr: &Self) -> *mut RefCountData {
        RefCounted::metadata_ptr(ptr.0)
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
        let metadata_ptr = Ptr::metadata_ptr(&self);
        let count_ptr = RefCountData::count_ptr(metadata_ptr);
        unsafe { bindings::refcount_acquire(count_ptr) };
        Self(self.0)
    }
}

// Dropping the pointer releases the refcount to the `T` that it owns.
impl<T> Drop for Ptr<T> {
    fn drop(&mut self) {
        let metadata_ptr = Ptr::metadata_ptr(&self);
        unsafe { RefCountData::release_ref(metadata_ptr) }
    }
}

/// A projected pointer to a `T` which owns a refcount for it.
///
/// While this has the same functionality as `Ptr<T>` it has a different memory layout so not all
/// interfaces support using projected pointers. More specifically `FatPtr<T>` has the layout of
///
/// ```c
/// struct proj_ptr_t {
///     T *t;
///     RefCountData *metadata;
/// };
/// ```
///
/// This is required because projection uses the refcount from the original pointer for the
/// projected pointer so we cannot rely on the memory layout of the pointee as a way to get a
/// pointer to the refcount metadata. For example given a type `T` with fields of types `U` and `V`,
/// if we create projected pointers for each field they would have different offsets from the
/// refcount in the original `RefCounted<T>`. Pointer projection may also go through a level of
/// indirection (i.e. projecting a `Ptr<T>` to a `Box<U>` field in the `T` may produce a pointer in
/// a completely part of the heap) which is another reason why a separate refcount metadata pointer
/// may be necessary.
///
/// Projected pointers may be projected further producing a new `FatPtr` with a different type and
/// the same layout.
#[repr(C)]
pub struct FatPtr<T> {
    ptr: *mut T,
    metadata_ptr: *mut RefCountData,
}

impl<T> FatPtr<T> {
    /// Creates a `FatPtr` from a pointer to a `T` without grabbing a refcount.
    ///
    /// # Safety
    ///
    /// The caller must ensure that there is at least one refcount that the `FatPtr` can
    /// conceptually own.
    pub unsafe fn from_raw(ptr: *mut T, metadata_ptr: *mut RefCountData) -> Self {
        Self { ptr, metadata_ptr }
    }
}

// Allows using a `FatPtr<T>` like a `&T`
impl<'a, T> Deref for FatPtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.ptr.as_ref().unwrap() }
    }
}

// Format the `FatPtr<T>` as if it were a `&T`.
impl<T: Debug> Debug for FatPtr<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

// Explicitly clone the `FatPtr<T>` grabbing a new refcount in the process.
impl<T> Clone for FatPtr<T> {
    fn clone(&self) -> Self {
        let metadata_ptr = self.metadata_ptr;
        let count_ptr = RefCountData::count_ptr(metadata_ptr);
        unsafe { bindings::refcount_acquire(count_ptr) };
        Self {
            ptr: self.ptr,
            metadata_ptr,
        }
    }
}

// Dropping the pointer releases the refcount to the `T` that it owns.
impl<T> Drop for FatPtr<T> {
    fn drop(&mut self) {
        unsafe { RefCountData::release_ref(self.metadata_ptr) }
    }
}

/// A pointer wrapper implementing `Sync`.
///
/// This is useful for creating pointers to types that intentionally are expected to be shared
/// between threads without explicit synchronization.
#[repr(C)]
pub struct SyncPtr<T>(pub(crate) *mut T);

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
}

// SAFETY: `SyncPtr` is intended for cases where `Sync` is intentionally desired on the pointer
unsafe impl<T> Sync for SyncPtr<T> {}

pub struct CString(Vec<u8>);

impl CString {
    pub fn new(msg: &str, ty: MallocType, flags: MallocFlags) -> Result<Self> {
        let mut buf = Vec::try_with_capacity(msg.as_bytes().len() + 1, ty, flags).unwrap();
        for &b in msg.as_bytes() {
            buf.try_push(b);
        }
        buf.try_push(0);
        Ok(Self(buf))
    }

    pub fn as_c_str(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.0.as_ptr().cast()) }
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

    pub unsafe fn from_base_ptr<'a>(ptr: *mut B) -> &'a mut Self {
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

#[repr(C)]
pub struct Uninit<T>(MaybeUninit<RefCounted<T>>);

impl<T> Uninit<T> {
    pub unsafe fn from_raw<'a>(ptr: *mut RefCounted<T>) -> &'a mut Self {
        unsafe { ptr.cast::<Self>().as_mut().unwrap() }
    }

    pub fn init(&mut self, t: T) -> &RefCounted<T> {
        let ref_t_ptr = self.0.as_mut_ptr();
        let t_ptr = unsafe { &raw mut (*ref_t_ptr).t };
        unsafe { write(t_ptr, t) };
        unsafe {
            self.0.assume_init_mut().init = true;
            self.0.assume_init_ref()
        }
    }

    pub fn is_init(&self) -> bool {
        let ref_t_ptr = self.0.as_ptr();
        let init_ptr = unsafe { &raw const (*ref_t_ptr).init };
        unsafe { *init_ptr }
    }
}
