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

use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use crate::vec::Vec;
use core::cmp::max;
use core::ffi::CStr;
use core::fmt::Debug;
use core::ops::Deref;

pub trait ToCString {
    fn to_cstring(&self) -> CString;
}

macro_rules! impl_to_cstring {
    ($self:ty, $max_digits:expr) => {
        impl ToCString for $self {
            fn to_cstring(&self) -> CString {
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
                CString::Small(buf)
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
pub enum CString<const N: usize = 24> {
    Small([u8; N]),
    Heap(Vec<u8>),
}

impl CString {
    pub fn new(msg: &CStr, ty: MallocType, flags: MallocFlags) -> Self {
        assert!(flags.contains(M_WAITOK));
        assert!(!flags.contains(M_NOWAIT));
        Self::try_new(msg, ty, flags).unwrap()
    }

    pub fn try_new(msg: &CStr, ty: MallocType, flags: MallocFlags) -> Result<Self> {
        let mut buf = Vec::try_with_capacity(msg.to_bytes().len() + 1, ty, flags).unwrap();
        for &b in msg.to_bytes() {
            buf.push(b);
        }
        buf.push(0);
        Ok(Self::Heap(buf))
    }
}

impl<const N: usize> CString<N> {
    pub fn try_new_small(msg: &CStr) -> Result<Self> {
        let mut buf = [0u8; N];
        if msg.count_bytes() + 1 > N {
            return Err(EINVAL);
        }
        let msg_bytes = msg.to_bytes();
        for i in 0..msg.count_bytes() {
            buf[i] = msg_bytes[i];
        }
        Ok(Self::Small(buf))
    }

    pub fn as_c_str(&self) -> &CStr {
        let ptr = match self {
            Self::Small(ar) => ar.as_ptr(),
            Self::Heap(v) => v.as_ptr(),
        };
        unsafe { CStr::from_ptr(ptr.cast()) }
    }

    pub fn push(&mut self, b: u8) -> Result<()> {
        match self {
            Self::Small(ar) => {
                let first_zero = ar
                    .iter()
                    .position(|&x| x == 0)
                    .expect("CString contained no null-terminator");
                if first_zero != ar.len() - 1 {
                    ar[first_zero] = b;
                    return Ok(());
                }
            }
            Self::Heap(v) => {
                if v.push(b).is_none() {
                    return Ok(());
                }
            }
        }
        Err(ENOBUFS)
    }

    pub fn pop(&mut self, n: usize) {
        match self {
            Self::Small(ar) => {
                let first_zero = ar
                    .iter()
                    .position(|&x| x == 0)
                    .expect("CString contained no null-terminator");
                let new_zero = max(first_zero - n, 0);
                ar[new_zero] = 0;
            }
            Self::Heap(v) => {
                for _ in 0..n {
                    if v.pop().is_none() {
                        return;
                    }
                }
            }
        }
    }

    pub fn push_cstr(&mut self, other: &CStr) -> usize {
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

impl<const N: usize> Deref for CString<N> {
    type Target = CStr;
    fn deref(&self) -> &Self::Target {
        self.as_c_str()
    }
}

impl<const N: usize> From<[u8; N]> for CString<N> {
    fn from(array: [u8; N]) -> Self {
        Self::Small(array)
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
    fn push_cstring() {
        let mut x = CString::<24>::try_new_small(c"hello").unwrap();
        x.push(b' ').unwrap();
        x.push(b'w').unwrap();
        x.push(b'o').unwrap();
        x.push(b'r').unwrap();
        x.push(b'l').unwrap();
        x.push(b'd').unwrap();
        assert_eq!(x.as_c_str(), c"hello world");
    }

    #[test]
    fn pop_cstring() {
        let mut x = CString::<24>::try_new_small(c"hello world").unwrap();
        x.pop(c" world".to_bytes().len());
        assert_eq!(x.as_c_str(), c"hello");
    }

    #[test]
    fn push_short_cstring() {
        let short_str = CString::<5>::try_new_small(c"hello");
        assert!(short_str.is_err());

        let mut full_str = CString::<6>::try_new_small(c"hello").unwrap();
        let res = full_str.push(b'x');
        assert!(res.is_err());
    }

    #[test]
    fn push_cstring_words() {
        let mut x = CString::<24>::try_new_small(c"the").unwrap();
        let len = x.push_cstr(c" quick");
        assert_eq!(len, 6);
        let len = x.push_cstr(c" brown");
        assert_eq!(len, 6);
        let len = x.push_cstr(c" fox");
        assert_eq!(len, 4);
        let len = x.push_cstr(c" jumped");
        // The previous operation won't completely succeed since there's not enough space in the
        // buffer, but it will push the first 4 characters
        assert_eq!(len, 4);
        let len = x.push_cstr(c" over");
        assert_eq!(len, 0);
        let len = x.push_cstr(c" the");
        assert_eq!(len, 0);
        assert_eq!(x.as_c_str(), c"the quick brown fox jum");
    }

    fn check_to_cstring<T: ToCString + ToString>(x: T) {
        let test_val = x.to_cstring();
        let expected = std::ffi::CString::new(x.to_string()).unwrap();
        assert_eq!(test_val.as_c_str(), expected.as_c_str());
    }
}
