//! FreeBSD KPI pointers
//!
//! Pointers in rust (i.e. `*const T` and `*mut T`) require unsafe blocks around dereferences
//! because the language doesn't enforce properties that make it valid to use them in different
//! scenarios. This module provides ABI-compatible types for pointers which are known to have some
//! of these properties. This allows downstream code (i.e. the rest of the crate and drivers) to
//! depend on the type system for enforcing properties of pointers and restricting how they may be
//! used with unsafe functions that have a more limited set of requirements than the native *mut T.
//!
//! Unless noted otherwise we assume pointers received from the FreeBSD KPI always:
//! - are non-null unless that is explicitly used to signal an error condition
//! - have pointees with fixed addresses unless explicitly noted otherwise
//! - point to a region of memory big enough to hold a `T`
//! - point to a region of memory with proper alignment for `T`
//!
//! `OutPtr<T>` represents pointers with these properties. Note that the pointee is not assumed to
//! be initialized. Also uninitialized pointees should not be assumed to be zeroed out. As the name
//! implies, this type of pointer is typically used for C functions to return "out parameters".
//! Since the pointee may be uninitialized writing is allowed, but reading is not. The pointee may
//! have other pointers so writes must potentially be synchronized.
//!
//! `Ptr<T>` has these additional assumptions:
//! - the pointee has a valid value for `T`. This means that it must be initialized to a valid value
//!   and any changes map it to another valid value.
//!
//! By ensuring this holds and calling the unsafe `OutPtr::assume_init` we get `Ptr<T>`. Since the
//! pointee is initialized both reading and writing are allowed. The pointee may still have other
//! pointers so both types of accesses must potentially be synchronized.
//!
//! `Ref<T>` has the additional assumption:
//! - shared references (i.e. `&T`) to the pointee may be created
//!
//! TODO: Finish documenting Ref and RefMut and how they differ from &T and &mut T
//!
//! If we're sure nothing else will write to the pointee
//!
//! `RefMut<T>` has the additional assumption:
//! - mutable references (i.e. `&mut T`) to the pointee may be created

use core::ffi::c_void;
use core::mem::MaybeUninit;
use core::ops::{Deref, DerefMut};

pub trait PointsTo<T> {
    fn as_ptr(&self) -> *mut T;
    fn as_type_erased_ptr(&self) -> *mut c_void {
        self.as_ptr().cast()
    }
}

/// A pointer received from the C KPI.
///
/// The memory it points to is not assumed to be initialized and may have other pointers.
#[derive(Debug)]
#[repr(transparent)]
pub struct OutPtr<T>(*mut T);

impl<T> Clone for OutPtr<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T> Copy for OutPtr<T> {}

impl<T> PointsTo<T> for OutPtr<T> {
    fn as_ptr(&self) -> *mut T {
        self.0
    }
}

impl<T> OutPtr<T> {
    // SAFETY: The caller must ensure `ptr` came directly from the C KPI to ensure the size and
    // alignment requirements of the pointee. The caller must also ensure that the C KPI this came
    // from does not use NULL as an error condition.
    pub unsafe fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }

    // SAFETY: The caller must synchronize this call with other pointers to `self.`
    pub unsafe fn write(&mut self, t: T) {
        *self.0 = t;
    }

    // SAFETY: The caller must ensure the pointee has a valid value for `T` before calling this.
    pub unsafe fn assume_init(self) -> Ptr<T> {
        Ptr(self.0)
    }

    #[doc(hidden)]
    pub fn get_field_helper<U, F>(self, f: F) -> OutPtr<U>
    where
        F: FnOnce(*mut T) -> *mut U,
    {
        OutPtr(f(self.0))
    }
}

/// A pointer received from the C KPI with the extra assumption that the pointee is initialized.
///
/// The pointee may have other pointers.
#[derive(Debug)]
#[repr(transparent)]
pub struct Ptr<T>(*mut T);

impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T> Copy for Ptr<T> {}

impl<T> PointsTo<T> for Ptr<T> {
    fn as_ptr(&self) -> *mut T {
        self.0
    }
}

impl<T> Ptr<T> {
    // SAFETY: The caller must ensure `ptr` came directly from the C KPI to ensure the size and
    // alignment requirements of the pointee. The caller must also ensure that the C KPI this came
    // from does not use NULL as an error condition. Additionally the pointee must be initialized to
    // a valid value.
    pub unsafe fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }

    // TODO: the safety condition here is just synchronizing with other pointers to the pointee, but
    // this also must not break the first two properties about FreeBSD KPI pointers mentioned above.
    // That means calling `write` on a Ptr<Ptr<T>> with any value other than its current value is
    // not allowed. Ideally there would be a way to catch this w/o using specialization
    pub unsafe fn write(&mut self, t: T) {
        *self.0 = t;
    }

    pub unsafe fn read(&self) -> T {
        core::ptr::read(self.0 as *const T)
    }

    pub unsafe fn allows_ref(self) -> Ref<T> {
        Ref(self.0)
    }

    pub unsafe fn allows_mut_ref(self) -> RefMut<T> {
        RefMut(self.0)
    }

    #[doc(hidden)]
    pub fn get_field_helper<U, F>(self, f: F) -> Ptr<U>
    where
        F: FnOnce(*mut T) -> *mut U,
    {
        Ptr(f(self.0))
    }

    pub fn as_out_ptr(self) -> OutPtr<T> {
        OutPtr(self.0)
    }
}

impl<T> Ptr<Ptr<T>> {
    pub fn flatten(self) -> Ptr<T> {
        // SAFETY: The outer `Ptr` is assumed to have a pointee with a fixed address meaning that
        // nothing can write to the outer pointer. Since the outer pointer has no writers, we can
        // read without synchronization. That gives us the inner `Ptr` which satisfies the safety
        // conditions for `Ptr<T>` by construction.
        unsafe {
            self.read()
        }
    }
}

/// A pointer received from the C KPI which may be turned into a reference.
#[derive(Debug)]
#[repr(transparent)]
pub struct Ref<T>(*mut T);

impl<T> Ref<T> {
    pub unsafe fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }

    #[doc(hidden)]
    pub fn get_field_helper<U, F>(self, f: F) -> Ref<U>
    where
        F: FnOnce(&T) -> &U,
    {
        let new_ptr = f(self.deref()) as *const U;
        Ref(new_ptr as *mut U)
    }

    // just for documentation
    pub fn map_ref<U, F>(self, f: F) -> Ref<U>
    where
        F: FnOnce(&T) -> &U,
    {
        self.get_field_helper(f)
    }
}

impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T> Copy for Ref<T> {}

impl<T> PointsTo<T> for Ref<T> {
    fn as_ptr(&self) -> *mut T {
        self.0
    }
}

impl<T> Deref for Ref<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref().unwrap() }
    }
}

/// A pointer received from the C KPI which may be turned into a mutable reference.
#[derive(Debug)]
#[repr(transparent)]
pub struct RefMut<T>(*mut T);

impl<T> RefMut<T> {
    pub unsafe fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }

    pub fn share(self) -> Ref<T> {
        Ref(self.0)
    }

    #[doc(hidden)]
    pub fn get_field_helper<U, F>(&mut self, f: F) -> RefMut<U>
    where
        F: FnOnce(*mut T) -> *mut U,
    {
        RefMut(f(self.0))
    }
}

impl<T> PointsTo<T> for RefMut<T> {
    fn as_ptr(&self) -> *mut T {
        self.0
    }
}

impl<T> Deref for RefMut<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref().unwrap() }
    }
}

impl<T> DerefMut for RefMut<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut().unwrap() }
    }
}
