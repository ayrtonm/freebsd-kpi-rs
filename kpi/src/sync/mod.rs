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

//! Synchronization primitives.

use crate::prelude::*;

pub mod arc;
pub mod mtx;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bindings::u_int;
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
        let x: Arc<u32> = Arc::try_new(42, M_DEVBUF, M_NOWAIT).unwrap();
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
