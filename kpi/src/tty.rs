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
use core::fmt;
use core::hint::black_box;

pub struct TTY;

impl fmt::Write for TTY {
    fn write_str(&mut self, msg: &str) -> fmt::Result {
        const BUF_SIZE: usize = 16;
        let mut buf = [0u8; BUF_SIZE];

        let mut copy_len = msg.as_bytes().len();
        if copy_len >= BUF_SIZE {
            copy_len = BUF_SIZE - 1;
        }
        buf[0..copy_len].copy_from_slice(&msg.as_bytes()[0..copy_len]);
        buf[copy_len] = 0;
        // Pass the message with as the %s argument to avoid printf formatting interferring with
        // the intended rust-style formatting. black_box is to avoid LLVM optimizing it to a putchar
        // FIXME: replace black_box since it's brittle and may break the build
        unsafe {
            black_box(bindings::printf(c"%s".as_ptr(), buf.as_ptr()));
        }
        // Copy the rest of the message if necessary
        if copy_len < msg.as_bytes().len() {
            self.write_str(&msg[copy_len..])?;
        }
        Ok(())
    }
}

#[cfg(not(feature = "std"))]
#[macro_export]
macro_rules! print {
    ($($args:tt)*) => {
        {
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY,
                format_args!($($args)*)
            ).ok();
        }
    };
}

#[cfg(not(feature = "std"))]
#[macro_export]
macro_rules! println {
    ($($args:tt)*) => {
        {
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY,
                format_args!($($args)*)
            ).ok();
            unsafe { core::hint::black_box(bindings::printf(c"\n".as_ptr())); }
        }
    };
}

#[macro_export]
macro_rules! device_print {
    ($dev:expr, $($args:tt)*) => {
        {
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY,
                format_args!($($args)*)
            ).ok();
        }
    };
}

#[macro_export]
macro_rules! device_println {
    ($dev:expr, $($args:tt)*) => {
        {
            #[cfg(not(feature = "std"))]
            unsafe {
                let dev: $crate::bindings::device_t = $dev;
                bindings::device_printf(dev, c"".as_ptr());
            }
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY,
                format_args!($($args)*)
            ).ok();
            unsafe { core::hint::black_box(bindings::printf(c"\n".as_ptr())); }
        }
    };
}
