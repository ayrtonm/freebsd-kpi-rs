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

use core::cell::UnsafeCell;
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::mem::MaybeUninit;
use core::ops::{Deref, DerefMut};
use core::ptr::read;
use core::sync::atomic::{AtomicBool, Ordering};

/// Rust-style atomic reference counting with memory backed malloc(9)
pub mod arc;
/// Mutex and SpinLock backed by mtx(9)
pub mod mtx;

#[derive(Debug)]
pub struct OnceInit<T> {
    t: UnsafeCell<MaybeUninit<T>>,
    init: AtomicBool,
}

impl<T> OnceInit<T> {
    pub fn uninit() -> Self {
        Self {
            t: UnsafeCell::new(MaybeUninit::uninit()),
            init: AtomicBool::new(false),
        }
    }

    pub fn new(t: T) -> Self {
        Self {
            t: UnsafeCell::new(MaybeUninit::new(t)),
            init: AtomicBool::new(true),
        }
    }

    pub fn init(&self, t: T) -> &mut T {
        if !self
            .init
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            panic!("already init");
        }
        unsafe { self.t.get().as_mut().unwrap().write(t) }
    }

    pub fn get(&self) -> &T {
        assert!(self.init.load(Ordering::Relaxed));
        unsafe { self.t.get().as_ref().unwrap().assume_init_ref() }
    }
}

unsafe impl<T: Sync> Sync for OnceInit<T> {}

/// A value borrow-checked at runtime
///
/// This is intended for variables which are shared between multiple threads but which are expected
/// to only be accessed by one thread at a time. Attempting to access the `T` from multiple threads
/// at the same will make it panic. Note that this is essentially a mutex that doesn't sleep since
/// it uses a single atomic flag to ensure exclusive access and does not differentiate between
/// readers and writers. It has the benefit of not using the existing `mtx(9)` machinery though
/// which should make it somewhat lower cost.
#[derive(Default)]
pub struct Mutable<T> {
    t: UnsafeCell<T>,
    borrowed: AtomicBool,
}

impl<T: Debug> Debug for Mutable<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // Just read the pointer and don't overcomplicate things. There may be mutable references to
        // the pointee so this is a best effort thing and may not be fully reliable, but it avoids
        // panics (as self.get_mut() might) while being informative.
        let t = unsafe { read(self.t.get()) };
        f.debug_struct("Mutable")
            .field("t", &t)
            .field("borrowed", &self.borrowed)
            .finish()
    }
}

unsafe impl<T: Sync> Sync for Mutable<T> {}

impl<T> Mutable<T> {
    /// Creates a new `Mutable<T>`
    pub const fn new(t: T) -> Self {
        Self {
            t: UnsafeCell::new(t),
            borrowed: AtomicBool::new(false),
        }
    }

    pub fn as_ptr(&self) -> *mut T {
        self.t.get()
    }

    /// Get mutable access to the `T`.
    ///
    /// This panics if the `Mutable<T>` is already borrowed.
    pub fn get_mut(&self) -> RefMut<'_, T> {
        if !self
            .borrowed
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            panic!("already borrowed");
        }
        RefMut {
            value: self.t.get(),
            borrowed: &self.borrowed,
        }
    }
}

/// A reference to a mutably borrowed [`Mutable<T>`]
pub struct RefMut<'b, T: 'b + ?Sized> {
    value: *mut T,
    borrowed: &'b AtomicBool,
}

impl<'b, T: Debug> Debug for RefMut<'b, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

impl<T: ?Sized> Deref for RefMut<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.value.as_ref().unwrap() }
    }
}

impl<T: ?Sized> DerefMut for RefMut<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.value.as_mut().unwrap() }
    }
}

impl<T: ?Sized> Drop for RefMut<'_, T> {
    fn drop(&mut self) {
        self.borrowed.store(false, Ordering::Relaxed);
    }
}

#[cfg(test)]
mod tests {
    use crate::bindings::u_int;
    use crate::prelude::*;
    use crate::sync::arc::Arc;

    // FIXME: These are totally thread-unsafe functions which are only used for tests
    #[unsafe(no_mangle)]
    fn rust_bindings_refcount_init(count: *mut u_int, value: u_int) {
        unsafe {
            *count = value;
        }
    }

    #[unsafe(no_mangle)]
    fn rust_bindings_refcount_load(count: *mut u_int) -> u_int {
        unsafe { *count }
    }

    #[unsafe(no_mangle)]
    fn rust_bindings_refcount_acquire(count: *mut u_int) -> u_int {
        unsafe {
            let old = *count;
            *count += 1;
            old
        }
    }

    #[unsafe(no_mangle)]
    fn rust_bindings_refcount_release(count: *mut u_int) -> bool {
        unsafe {
            *count -= 1;
            *count == 0
        }
    }

    #[test]
    fn arc() {
        let x = Arc::try_new(42u32, M_DEVBUF, M_NOWAIT).unwrap();
        assert_eq!(x.snapshot_refcount(), 1);
        {
            let y = x.clone();
            assert_eq!(y.snapshot_refcount(), 2);
            assert_eq!(x.snapshot_refcount(), 2);
        }
        assert_eq!(x.snapshot_refcount(), 1);
        let z = x.clone();
        assert_eq!(z.snapshot_refcount(), 2);
        assert_eq!(x.snapshot_refcount(), 2);
        drop(x);
        assert_eq!(z.snapshot_refcount(), 1);
    }
}
