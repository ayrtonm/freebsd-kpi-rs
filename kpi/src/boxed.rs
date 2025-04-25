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
use crate::prelude::*;
use crate::malloc::{MallocFlags, MallocType};
use core::mem::size_of;
use core::ptr::NonNull;
use core::marker::PhantomData;
use core::ops::{Deref, DerefMut};

#[repr(C)]
#[derive(Debug)]
pub struct Box<T, M: MallocType>(NonNull<T>, PhantomData<M>);

unsafe impl<T: Sync, M: MallocType> Sync for Box<T, M> {}

impl<T, M: MallocType> Box<T, M> {
    pub fn try_new(t: T, flags: MallocFlags) -> Result<Self> {
        let void_ptr = malloc(size_of::<T>(), M::TYPE, flags);
        match NonNull::new(void_ptr) {
            Some(nonnull_void_ptr) => {
                let t_ptr = nonnull_void_ptr.cast::<T>();
                unsafe {
                    t_ptr.write(t)
                };
                Ok(Self(t_ptr, PhantomData))
            }
            None => Err(ENOMEM),
        }
    }

    pub fn new(t: T, flags: MallocFlags) -> Self {
        Self::try_new(t, flags).unwrap()
    }

    pub(crate) fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }
}

impl<T, M: MallocType> Deref for Box<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            self.0.as_ref()
        }
    }
}

impl<T, M: MallocType> DerefMut for Box<T, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            self.0.as_mut()
        }
    }
}
