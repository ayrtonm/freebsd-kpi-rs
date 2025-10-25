/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2025 Ayrton Muñoz
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

use crate::bindings::{nvme_qpair, nvme_tracker};
use crate::prelude::*;

define_interface! {
    nvme_delayed_attach(dev: device_t) -> int;
    nvme_enable(dev: device_t);
    nvme_sq_leave(dev: device_t, qpair: *mut nvme_qpair, tr: *mut nvme_tracker);
    nvme_cq_done(dev: device_t, qpair: *mut nvme_qpair, tr: *mut nvme_tracker);
    nvme_qpair_construct(dev: device_t, qpair: *mut nvme_qpair, num_entries: u32, num_trackers: u32) -> int;
}

#[doc(hidden)]
#[macro_export]
macro_rules! nvme_sq_enter {
    ($driver_ty:ident $impl_fn_name:ident) => {
        $crate::export_function! {
            $driver_ty $impl_fn_name
            nvme_sq_enter(dev: device_t, qpair: *mut nvme_qpair, tr: *mut nvme_tracker) -> u32;
            infallible
        }
    };
}

impl<'a> AsRustType<&'a nvme_qpair> for *mut nvme_qpair {
    fn as_rust_type(self) -> &'a nvme_qpair {
        unsafe { self.as_ref().unwrap() }
    }
}

impl<'a> AsRustType<&'a mut nvme_qpair> for *mut nvme_qpair {
    fn as_rust_type(self) -> &'a mut nvme_qpair {
        unsafe { self.as_mut().unwrap() }
    }
}

impl<'a> AsRustType<&'a nvme_tracker> for *mut nvme_tracker {
    fn as_rust_type(self) -> &'a nvme_tracker {
        unsafe { self.as_ref().unwrap() }
    }
}
