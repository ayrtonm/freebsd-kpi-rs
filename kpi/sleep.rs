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

use core::ptr::null_mut;
use crate::kpi_prelude::*;
use core::ffi::{c_void, CStr};

pub fn wakeup<T, P: PointsTo<T>>(chan: P) {
    let chan = chan.as_ptr().cast();
    wakeup_internal(chan)
}

fn wakeup_internal(chan: *const c_void) {
    unsafe {
        bindings::wakeup(chan)
    }
}

// convenience method since bindgen turns hz into a static mut
pub fn tsleep_in_hz<T, P: PointsTo<T>>(chan: P, priority: i32, wmesg: &CStr, timo: i32) -> Result<()> {
    tsleep(chan, priority, wmesg, timo * unsafe { bindings::hz })
}

pub fn tsleep<T, P: PointsTo<T>>(chan: P, priority: i32, wmesg: &CStr, timo: i32) -> Result<()> {
    let chan_ptr = chan.as_ptr().cast();
    tsleep_internal(chan_ptr, priority, wmesg, timo)
}

fn tsleep_internal(chan: *mut c_void, priority: i32, wmesg: &CStr, timo: i32) -> Result<()> {
    let wmesg_ptr = wmesg.as_ptr();
    let res = unsafe { bindings::_sleep(chan, null_mut(), priority, wmesg_ptr, bindings::tick_sbt * timo as i64, 0, bindings::C_HARDCLOCK) };
    if res != 0 {
        return Err(ErrCode::from(res));
    }
    Ok(())
}
