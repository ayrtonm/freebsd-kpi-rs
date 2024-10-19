use core::ops::{Deref, DerefMut};
use core::mem::MaybeUninit;

pub trait PointsTo<T> {
    fn as_ptr(&self) -> *mut T;
}

// FreeBSD Rust KPI
//
// Rust requires unsafe blocks around pointer accesses because it does not enforce some aspects of
// their validity. Since this crate receives pointers from the C KPI we can assume some of these
// aspects hold and be precise about the aspects we can't assume as true. This allows code in
// downstream crates (i.e. drivers) to depend on the type system for checking the aspects of pointer
// validity.
//
// We assume pointers received from the C KPI are always:
// - non-null unless that is explicitly used to signal an error condition
// - points to a region of memory big enough to hold `T`
// - points to a region of memory with the proper alignment for `T`

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

impl<T> OutPtr<MaybeUninit<T>> {
    // Cast away the MaybeUninit wrapper without assuming the pointee is initialized.
    pub fn flatten(self) -> OutPtr<T> {
        OutPtr(self.0.cast())
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
