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

use crate::device::Device;
use crate::bindings;
use crate::ffi::CString;
use core::fmt;

pub struct TTY<'a>(pub Option<&'a Device>);

impl fmt::Write for TTY<'_> {
    fn write_str(&mut self, msg: &str) -> fmt::Result {
        let cmsg = CString::new(msg).unwrap();
        match &self.0 {
            Some(dev) => unsafe {
                bindings::device_printf(dev.as_ptr(), cmsg.as_c_str().as_ptr());
            },
            None => unsafe {
                bindings::printf(cmsg.as_c_str().as_ptr());
            },
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! print {
    ($($args:tt)*) => {
        {
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY(None),
                format_args!($($args)*)
            ).ok();
        }
    };
}

#[macro_export]
macro_rules! println {
    ($($args:tt)*) => {
        {
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY(None),
                format_args!($($args)*)
            ).ok();
            // llvm tries to optimize this to putchar
            //unsafe { bindings::printf(c"\n".as_ptr()); }
        }
    };
}

#[macro_export]
macro_rules! device_print {
    ($dev:expr, $($args:tt)*) => {
        {
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY(Some(&$dev)),
                format_args!($($args)*)
            ).ok();
        }
    };
}

#[macro_export]
macro_rules! device_println {
    ($dev:expr, $($args:tt)*) => {
        {
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY(Some(&$dev)),
                format_args!($($args)*)
            ).ok();
            //unsafe { bindings::device_printf($dev.0, c"\n".as_ptr()); }
        }
    };
}
