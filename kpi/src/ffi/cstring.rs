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

use crate::malloc::{Malloc, MallocFlags};
use crate::prelude::*;
use crate::vec::Vec;
use core::ffi::CStr;
use core::fmt::Debug;
use core::ops::Deref;

// This could be a const parameter but I want to avoid adding too many knobs
const ARRAY_STRING_LEN: usize = 24;
#[derive(Debug)]
pub struct ArrayCString([u8; ARRAY_STRING_LEN]);

impl ArrayCString {
    pub fn new(msg: &CStr) -> Self {
        Self::try_new(msg).unwrap()
    }

    pub fn try_new(msg: &CStr) -> Result<Self> {
        let msg = msg.to_bytes_with_nul();
        if msg.len() > ARRAY_STRING_LEN {
            return Err(EINVAL);
        }

        let mut buf = [0u8; ARRAY_STRING_LEN];
        let dst = &mut buf[0..msg.len()];
        dst.copy_from_slice(msg);
        Ok(Self(buf))
    }

    pub fn as_c_str(&self) -> &CStr {
        unsafe { CStr::from_ptr(self.0.as_ptr().cast()) }
    }

    pub fn count_bytes(&self) -> usize {
        self.0.iter().position(|&x| x == 0).unwrap()
    }

    pub fn push(&mut self, b: u8) -> usize {
        if self.count_bytes() + 1 + 1 > ARRAY_STRING_LEN {
            return 0;
        }
        let cur_idx = self.count_bytes();
        self.0[cur_idx] = b;
        // pop only zeros one byte so there may be non-zero bytes after the null
        self.0[cur_idx + 1] = 0;
        1
    }

    pub fn pop(&mut self, n: usize) {
        let cur_idx = self.count_bytes();
        self.0[cur_idx - n] = 0;
    }

    pub fn push_c_str(&mut self, other: &CStr) -> usize {
        if self.count_bytes() + other.count_bytes() + 1 > ARRAY_STRING_LEN {
            return 0;
        }
        let mut added = 0;
        let start_idx = self.count_bytes();
        for &b in other.to_bytes() {
            self.0[start_idx + added] = b;
            added += 1;
        }
        added
    }
}

impl Deref for ArrayCString {
    type Target = CStr;

    fn deref(&self) -> &Self::Target {
        self.as_c_str()
    }
}

pub trait ToArrayCString {
    fn to_array_cstring(&self) -> ArrayCString;
}

macro_rules! impl_to_cstring {
    ($self:ty, $max_digits:expr) => {
        impl ToArrayCString for $self {
            fn to_array_cstring(&self) -> ArrayCString {
                let mut buf = [0; 24];
                let mut pos = 0;
                let mut tmp = *self;
                for n in 0..$max_digits {
                    let scale = (10 as $self).pow($max_digits - n - 1);
                    let digit = (tmp / scale) as u8;
                    tmp -= (digit as $self) * scale;
                    if pos == 0 && digit == 0 && n != ($max_digits - 1) {
                        continue;
                    }
                    buf[pos] = b'0' + digit;
                    pos += 1;
                }
                ArrayCString(buf)
            }
        }
    };
}

impl_to_cstring!(u8, 3);
impl_to_cstring!(u16, 5);
impl_to_cstring!(u32, 10);
impl_to_cstring!(u64, 20);
impl_to_cstring!(usize, 20);

#[derive(Debug)]
pub struct CString<M: Malloc = M_DEVBUF>(Vec<u8, M>);

impl<M: Malloc> CString<M> {
    pub fn new(msg: &CStr, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_new(msg, flags).unwrap()
    }

    pub fn try_new(msg: &CStr, flags: MallocFlags) -> Result<Self> {
        let mut buf = Vec::try_with_capacity(msg.to_bytes().len() + 1, flags).unwrap();
        for &b in msg.to_bytes() {
            buf.push(b);
        }
        buf.push(0);
        Ok(Self(buf))
    }
}

impl<M: Malloc> CString<M> {
    pub fn as_c_str(&self) -> &CStr {
        let ptr = self.0.as_ptr();
        unsafe { CStr::from_ptr(ptr.cast()) }
    }

    pub fn push(&mut self, b: u8) -> Result<()> {
        if self.0.push(b).is_none() {
            return Ok(());
        }
        Err(ENOBUFS)
    }

    pub fn pop(&mut self, n: usize) {
        for _ in 0..n {
            if self.0.pop().is_none() {
                return;
            }
        }
    }

    pub fn push_c_str(&mut self, other: &CStr) -> usize {
        let mut added = 0;
        for &b in other.to_bytes() {
            if self.push(b).is_err() {
                return added;
            }
            added += 1;
        }
        added
    }
}

impl<M: Malloc> Deref for CString<M> {
    type Target = CStr;

    fn deref(&self) -> &Self::Target {
        self.as_c_str()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::string::ToString;

    #[test]
    fn u8_cstring() {
        for x in 0..u8::MAX {
            check_to_cstring(x)
        }
    }

    #[test]
    fn u16_cstring() {
        for x in 0..u16::MAX {
            check_to_cstring(x)
        }
    }

    #[test]
    fn push_c_string() {
        let mut x = ArrayCString::new(c"hello");
        x.push(b' ');
        x.push(b'w');
        x.push(b'o');
        x.push(b'r');
        x.push(b'l');
        x.push(b'd');
        assert_eq!(x.as_c_str(), c"hello world");
    }

    #[test]
    fn pop_cstring() {
        let mut x = ArrayCString::new(c"hello world");
        x.pop(c" world".to_bytes().len());
        assert_eq!(x.as_c_str(), c"hello");
    }

    #[test]
    fn push_short_cstring() {
        let mut full_str: CString<M_DEVBUF> = CString::new(c"hello", M_WAITOK);
        let res = full_str.push(b'x');
        assert!(res.is_err());
    }

    #[test]
    fn push_c_string_words() {
        let mut x: ArrayCString = ArrayCString::new(c"the");

        let len = x.push_c_str(c" quick");
        assert_eq!(len, 6);

        let len = x.push_c_str(c" brown");
        assert_eq!(len, 6);

        let len = x.push_c_str(c" fox");
        assert_eq!(len, 4);

        // This operation won't succeed since there's not enough space in the buffer, so it won't
        // push anything
        let len = x.push_c_str(c" jumped");
        assert_eq!(len, 0);

        // This word is smaller but there's still not enough space so it'll also fail
        let len = x.push_c_str(c" over");
        assert_eq!(len, 0);

        // There is enough room for this word so this will succeed
        let len = x.push_c_str(c" the");
        assert_eq!(len, 4);

        assert_eq!(x.as_c_str(), c"the quick brown fox the");
    }

    fn check_to_cstring<T: ToArrayCString + ToString>(x: T) {
        let test_val = x.to_array_cstring();
        let expected = std::ffi::CString::new(x.to_string()).unwrap();
        assert_eq!(test_val.as_c_str(), expected.as_c_str());
    }
}
