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

use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use core::cmp::PartialEq;
use core::ffi::c_void;
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::marker::PhantomData;
use core::mem::size_of;
use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;
use core::slice;

// Fields are pub(crate) for easy conversion from Vec
#[repr(C)]
pub struct Box<T: ?Sized, M: MallocType>(pub(crate) NonNull<T>, pub(crate) PhantomData<M>);

impl<T: Debug + ?Sized, M: MallocType> Debug for Box<T, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

impl<T: PartialEq, M: MallocType> PartialEq for Box<T, M> {
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(self.deref(), other)
    }
}

impl<T: Eq, M: MallocType> Eq for Box<T, M> {}

unsafe impl<T: Sync, M: MallocType> Sync for Box<T, M> {}

impl<T: ?Sized, M: MallocType> Drop for Box<T, M> {
    fn drop(&mut self) {
        free(self.as_ptr().cast::<c_void>(), M::TYPE)
    }
}

impl<T, M: MallocType> Box<T, M> {
    pub fn try_new(t: T, flags: MallocFlags) -> Result<Self> {
        let void_ptr = malloc(size_of::<T>(), M::TYPE, flags);
        match NonNull::new(void_ptr) {
            Some(nonnull_void_ptr) => {
                let t_ptr = nonnull_void_ptr.cast::<T>();
                unsafe { t_ptr.write(t) };
                Ok(Self(t_ptr, PhantomData))
            }
            None => Err(ENOMEM),
        }
    }
}

impl<T: ?Sized, M: MallocType> Box<T, M> {
    pub(crate) fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }
}

impl<T: ?Sized, M: MallocType> Deref for Box<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<T: ?Sized, M: MallocType> DerefMut for Box<T, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

impl<'a, T, M: MallocType> IntoIterator for &'a mut Box<[T], M> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;
    fn into_iter(self) -> <&'a mut Box<[T], M> as IntoIterator>::IntoIter {
        // Rely on DerefMut<[T]> to use core::slice impl
        self.iter_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bindings::malloc_type;
    use std::vec::Vec;

    #[no_mangle]
    static mut M_DEVBUF: () = ();

    // FIXME: current test uses system malloc/free which ignores all args except first. Create a
    // wrapper to interpose these and check other args
    //#[no_mangle]
    //fn malloc(size: usize, ty: *mut malloc_type, flags: MallocFlags) -> *mut c_void {
    //    let buf = Vec::<u8>::with_capacity(size).leak();
    //    buf.as_mut_ptr().cast::<c_void>()
    //}
    //#[no_mangle]
    //fn free(ptr: *mut c_void, ty: *mut malloc_type) {
    //}

    #[test]
    fn boxed() {
        let value = 42;
        let x: Box<u32, M_DEVBUF> = Box::try_new(value, M_NOWAIT /* ignored in tests */).unwrap();
        let mut y: Box<u32, M_DEVBUF> =
            Box::try_new(value, M_NOWAIT /* ignored in tests */).unwrap();
        assert_eq!(x, y);
        *y += 1;
        assert_ne!(x, y);
    }
}
