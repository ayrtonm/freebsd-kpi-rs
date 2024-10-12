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
use crate::bindings::{malloc_type, M_DEVBUF, M_NOWAIT, M_WAITOK, M_ZERO};
use core::alloc::{AllocError, Allocator, GlobalAlloc, Layout};
use core::ffi::c_int;
use core::ptr::{addr_of_mut, NonNull};

#[derive(Debug, Copy, Clone)]
pub struct KernelAllocator<const F: c_int> {
    ty: *mut malloc_type,
}

impl<const F: c_int> KernelAllocator<F> {
    pub const fn new(ty: *mut malloc_type) -> Self {
        Self { ty }
    }
}

impl<const F: c_int> KernelAllocator<F> {
    unsafe fn malloc(&self, layout: Layout, flags: c_int) -> *mut u8 {
        bindings::malloc(layout.size(), self.ty, F).cast()
    }
}

#[global_allocator]
pub static WAITOK: KernelAllocator<{ M_WAITOK }> =
    KernelAllocator::new(addr_of_mut!(M_DEVBUF) as *mut malloc_type);

pub static NOWAIT: KernelAllocator<{ M_NOWAIT }> =
    KernelAllocator::new(addr_of_mut!(M_DEVBUF) as *mut malloc_type);

unsafe impl<const F: c_int> Sync for KernelAllocator<F> {}

unsafe impl GlobalAlloc for KernelAllocator<{ M_WAITOK }> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        self.malloc(layout, M_WAITOK)
    }
    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        bindings::free(ptr.cast(), self.ty);
    }
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        self.malloc(layout, M_WAITOK | M_ZERO)
    }
    unsafe fn realloc(&self, ptr: *mut u8, _layout: Layout, new_size: usize) -> *mut u8 {
        bindings::realloc(ptr.cast(), new_size, self.ty, M_WAITOK).cast()
    }
}

unsafe impl Allocator for KernelAllocator<{ M_NOWAIT }> {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = unsafe { self.malloc(layout, M_NOWAIT) };
        match NonNull::new(ptr) {
            Some(non_null_ptr) => Ok(NonNull::slice_from_raw_parts(non_null_ptr, layout.size())),
            None => Err(AllocError),
        }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        bindings::free(ptr.as_ptr().cast(), self.ty)
    }
}
