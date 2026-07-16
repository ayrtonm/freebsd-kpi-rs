/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2026 Ayrton Muñoz
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

use crate::ffi::Lease;
use core::cell::UnsafeCell;
use core::mem::MaybeUninit;
use core::ops::Deref;
use core::sync::atomic::{AtomicUsize, Ordering};

const UNINIT: usize = usize::MAX;
const REVOKED: usize = usize::MAX - 1;

/// A `Lease<T>` that can be revoked
///
/// This is narrower than a general reader-writer lock: the only value ever stored is a
/// `Lease<T>`, it is set at most once via [`init`][Self::init], read any number of times
/// concurrently via [`get`][Self::get], and released at most once via [`revoke`][Self::revoke].
/// `revoke` does not block waiting for readers to finish — it panics if called while any
/// [`LeaseGuard`] is still outstanding.
pub struct RevocableLease<T: 'static> {
    lease: UnsafeCell<MaybeUninit<Lease<T>>>,
    // UNINIT = never initialized, REVOKED = permanently emptied, otherwise the number of
    // outstanding `LeaseGuard`s (0 meaning initialized with no active readers).
    state: AtomicUsize,
}

unsafe impl<T: Sync> Sync for RevocableLease<T> {}
unsafe impl<T: Sync + Send> Send for RevocableLease<T> {}

impl<T> RevocableLease<T> {
    pub const fn uninit() -> Self {
        Self {
            lease: UnsafeCell::new(MaybeUninit::uninit()),
            state: AtomicUsize::new(UNINIT),
        }
    }

    /// Sets the leased value.
    ///
    /// Panics if called more than once.
    pub fn init(&self, lease: Lease<T>) {
        if self
            .state
            .compare_exchange(UNINIT, 0, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            panic!("RevocableLease already initialized");
        }
        unsafe { (*self.lease.get()).write(lease) };
    }

    /// Borrows the leased value.
    ///
    /// Panics if it hasn't been initialized yet or has already been revoked.
    pub fn get(&self) -> LeaseGuard<'_, T> {
        loop {
            let cur = self.state.load(Ordering::Acquire);
            if cur == UNINIT || cur == REVOKED {
                panic!("RevocableLease not initialized or already revoked");
            }
            if self
                .state
                .compare_exchange_weak(cur, cur + 1, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                let lease = unsafe { (*self.lease.get()).assume_init_ref() };
                return LeaseGuard {
                    lease,
                    state: &self.state,
                };
            }
        }
    }

    /// Drops the leased value, releasing its refcount.
    ///
    /// Panics if it isn't currently initialized with zero outstanding readers (i.e. if called
    /// before `init`, more than once, or while a [`LeaseGuard`] is still alive).
    pub fn revoke(&self) {
        if self
            .state
            .compare_exchange(0, REVOKED, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            panic!(
                "RevocableLease: cannot revoke -- not initialized, already revoked, or readers active"
            );
        }
        // `MaybeUninit` never runs `T`'s destructor on its own -- unlike a plain field, writing
        // over or dropping a `MaybeUninit<T>` does not call `T::drop`. Without this call the
        // `Lease<T>` would simply be leaked (its refcount held forever) instead of actually
        // being released, which would defeat the entire point of `revoke`.
        unsafe { (*self.lease.get()).assume_init_drop() };
    }
}

pub struct LeaseGuard<'a, T: 'static> {
    lease: &'a Lease<T>,
    state: &'a AtomicUsize,
}

impl<'a, T: 'static> Deref for LeaseGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.lease.deref()
    }
}

impl<'a, T> Drop for LeaseGuard<'a, T> {
    fn drop(&mut self) {
        self.state.fetch_sub(1, Ordering::Release);
    }
}
