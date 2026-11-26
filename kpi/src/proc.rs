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

use crate::prelude::*;
use crate::bindings::{MAXCOMLEN, proc, ucred};
use crate::ffi::{Refcounted, Ptr};
use core::ffi::c_char;
use core::mem::forget;
use core::ptr::NonNull;

define_wrapper_type!(CurThread, thread);
define_wrapper_type!(CurProc, proc);

define_wrapper_type!(Thread, thread);
define_wrapper_type!(Proc, proc);

define_wrapper_type!(Ucred, ucred);

impl CurThread {
    // b - created at fork, never changes
    define_getter!(td_tid, i32);

    // td_proc is set once by thread_link() before the thread is runnable or in any queue and
    // nothing reassigns it
    define_getter!(td_proc, *mut proc);

    // k - only accessed by curthread
    define_getter!(td_ucred, *mut ucred);

    pub fn ucred(&self) -> &Ucred {
        let ptr = self.td_ucred();
        unsafe { ptr.cast::<Ucred>().as_ref().unwrap() }
    }

    pub fn proc(&self) -> &CurProc {
        let ptr = self.td_proc();
        unsafe { ptr.cast::<CurProc>().as_ref().unwrap() }
    }

    pub fn as_thread(&self) -> &Thread {
        let ptr = self.as_ptr();
        unsafe { ptr.cast::<Thread>().as_ref().unwrap() }
    }
}

impl Thread {
    // b - created at fork, never changes
    define_getter!(td_tid, i32);

    // td_proc is set once by thread_link() before the thread is runnable or in any queue and
    // nothing reassigns it
    define_getter!(td_proc, *mut proc);

    pub fn proc(&self) -> &Proc {
        let ptr = self.td_proc();
        unsafe { ptr.cast::<Proc>().as_ref().unwrap() }
    }
}

impl CurProc {
    // x - created at fork, only changes during single threading in exec
    define_getter!(p_comm, [c_char; MAXCOMLEN as usize + 1]);

    // b - created at fork, never changes
    define_getter!(p_pid, i32);

    pub fn as_proc(&self) -> &Proc {
        let ptr = self.as_ptr();
        unsafe { ptr.cast::<Proc>().as_ref().unwrap() }
    }
}

impl Proc {
    // b - created at fork, never changes
    define_getter!(p_pid, i32);
}

impl Ucred {
    define_getter!(cr_uid, u32);
    define_getter!(cr_ruid, u32);
    define_getter!(cr_svuid, u32);
    define_getter!(cr_gid, u32);
    define_getter!(cr_rgid, u32);
    define_getter!(cr_svgid, u32);
}

unsafe impl Sync for Ucred {}
unsafe impl Send for Ucred {}

unsafe impl Refcounted for Ucred {
    fn acquire_ref(&self) {
        unsafe { bindings::crhold(self.as_ptr()) };
    }

    unsafe fn release_ref(ptr: NonNull<Ucred>) -> bool {
        unsafe { bindings::crfree(ptr.as_ptr().cast::<ucred>()) };
        false
    }
}

#[doc(hidden)]
pub mod wrappers {
    use super::*;

    pub fn crhold(cred: &Ucred) -> Ptr<Ucred> {
        cred.acquire_ref();
        Ptr::new(cred)
    }

    pub fn crfree(cred: Ptr<Ucred>) {
        drop(cred);
    }

    pub fn cr_cansee(cr1: &Ucred, cr2: &Ucred) -> bool {
        unsafe { bindings::cr_cansee(cr1.as_ptr(), cr2.as_ptr()) == 0 }
    }
}
