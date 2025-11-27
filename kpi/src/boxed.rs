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

//! The `Box<T>` type for heap allocation.

use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use core::cmp::PartialEq;
use core::ffi::c_void;
use core::fmt::{Debug, Formatter};
use core::mem::{forget, size_of};
use core::ops::{Deref, DerefMut};
use core::ptr::{NonNull, drop_in_place};
use core::{fmt, slice};

/// To avoid adding an extra, generic malloc_type parameter on Box<T> things on the heap store the
/// malloc_type they were allocated with on the heap. This is then used when free is called when the
/// boxed thing is dropped. This means that the layout of a heap-allocated type is equivalent to
/// ```c
/// struct boxed_thing {
///     malloc_type *ty;
///     T thing;
/// };
/// ```
/// While the extra pointer in the heap adds per allocation overhead this avoids the cognitive
/// overhead of another generic parameter on Box<T>.
#[repr(C)]
pub(crate) struct BoxedThing<T: ?Sized> {
    ty: MallocType,
    // Must be the last field since its size might not be known at compile-time
    pub(crate) thing: T,
}

/// A pointer to something on the heap.
///
/// When a `Box<T>` is dropped, the T on the heap is deallocated. The memory layout of this type is
/// equivalent to
/// ```c
/// void *ptr;
/// ```
/// though the pointee on the heap has extra metadata preceeding the T.
#[repr(C)]
pub struct Box<T: ?Sized>(NonNull<BoxedThing<T>>);

// impl Deref to allow using a Box<T> like a T transparently
impl<T: ?Sized> Deref for Box<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        let boxed_thing_ref = unsafe { self.0.as_ref() };
        &boxed_thing_ref.thing
    }
}

impl<T: ?Sized> DerefMut for Box<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let boxed_thing_mut_ref = unsafe { self.0.as_mut() };
        &mut boxed_thing_mut_ref.thing
    }
}

impl<T: Debug + ?Sized> Debug for Box<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Use Box<T>'s Deref impl since it returns a &T
        Debug::fmt(self.deref(), f)
    }
}

impl<T: PartialEq> PartialEq for Box<T> {
    fn eq(&self, other: &Self) -> bool {
        // Use Box<T>'s Deref impl since it returns a &T. This means that if two heap variables have
        // the same value but were allocated with different `malloc_type`s this will evaluate to
        // true.
        PartialEq::eq(self.deref(), other)
    }
}

impl<T: Eq> Eq for Box<T> {}

unsafe impl<T: Sync + ?Sized> Sync for Box<T> {}

impl<T: ?Sized> Drop for Box<T> {
    fn drop(&mut self) {
        // Box<T>'s Deref impl returns the &T but we need the *mut malloc_type so call as_ref
        // directly.
        let ty = Box::malloc_type(&self);
        let boxed_thing_ptr = self.0.as_ptr();
        let thing_ptr = unsafe { &raw mut (*boxed_thing_ptr).thing };
        // Drop anything that T owns
        unsafe { drop_in_place(thing_ptr) };
        // Deallocate the memory for BoxedThing<T>
        unsafe { free(boxed_thing_ptr.cast::<c_void>(), ty) }
    }
}

impl<T> Box<T> {
    pub fn new(t: T, ty: MallocType, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_new(t, ty, flags).unwrap()
    }

    pub fn try_new(t: T, ty: MallocType, flags: MallocFlags) -> Result<Self> {
        if flags.contains(M_NOWAIT) && flags.contains(M_WAITOK) {
            return Err(EDOOFUS);
        };
        let boxed_t_size = size_of::<BoxedThing<T>>();
        let boxed_t_align = align_of::<BoxedThing<T>>();
        let void_ptr = malloc_aligned(boxed_t_size, boxed_t_align, ty, flags);
        match NonNull::new(void_ptr) {
            Some(nonnull_void_ptr) => {
                let boxed_thing_ptr = nonnull_void_ptr.cast::<BoxedThing<T>>();
                unsafe { boxed_thing_ptr.write(BoxedThing { ty, thing: t }) };
                Ok(Self(boxed_thing_ptr))
            }
            None => Err(ENOMEM),
        }
    }
}

impl<T: ?Sized> Box<T> {
    pub(crate) fn leak<'a>(b: Self) -> &'a mut BoxedThing<T> {
        let mut ptr = b.0;
        forget(b);
        unsafe { ptr.as_mut() }
    }

    pub(crate) unsafe fn from_raw(raw: NonNull<BoxedThing<T>>) -> Self {
        Self(raw)
    }

    pub(crate) fn malloc_type(b: &Self) -> MallocType {
        unsafe { b.0.as_ref().ty }
    }
}

impl<'a, T> IntoIterator for &'a mut Box<[T]> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;
    fn into_iter(self) -> <&'a mut Box<[T]> as IntoIterator>::IntoIter {
        // Rely on DerefMut<[T]> to use core::slice impl
        self.iter_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::LoudDrop;

    #[test]
    fn boxed() {
        let value = 42;
        let x: Box<u64> = Box::try_new(value, M_DEVBUF, M_NOWAIT).unwrap();
        assert_eq!(*x, value);
        println!("{x:?}");
    }

    #[test]
    fn bad_flags() {
        let x = Box::try_new(0xdeadbeefu32, M_DEVBUF, M_NOWAIT | M_WAITOK);
        assert!(x == Err(EDOOFUS));
    }

    #[test]
    fn oom() {
        let x = Box::try_new(111u8, M_DEVBUF, M_USE_RESERVE);
        assert!(x == Err(ENOMEM));
    }

    #[test]
    fn round_trip_drop() {
        let y = Box::try_new(LoudDrop, M_DEVBUF, M_NOWAIT).unwrap();
        let y_ref = Box::leak(y);
        let nonnull_y = NonNull::from_ref(y_ref);
        let _new_y = unsafe { Box::from_raw(nonnull_y) };
    }
}
