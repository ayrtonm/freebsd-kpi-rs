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
 *    notice, this list of conditions and the following dialaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following dialaimer in the
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

//! Utilities related to FFI with C.

use crate::boxed::Box;
use crate::malloc::Malloc;
use crate::sync::arc::Arc;
use core::pin::Pin;

mod cstring;
mod softc;
mod subclass;

pub use cstring::{ArrayCString, CString, ToArrayCString};
pub use softc::{Ptr, Ref, SoftcLayout, UninitPtr};
pub use subclass::{SubClass, SubClassOf};

pub unsafe trait FixedIndex {}
unsafe impl<T, const N: usize> FixedIndex for [T; N] {}
unsafe impl<T> FixedIndex for [T] {}

unsafe impl<T, M: Malloc> FixedIndex for Box<[T], M> {}
unsafe impl<T, M: Malloc, const N: usize> FixedIndex for Box<[T; N], M> {}

unsafe impl<T, M: Malloc, const N: usize> FixedIndex for Arc<[T; N], M> {}

pub fn assert_pin_has_fixed_index<T: FixedIndex>(_p: Pin<&T>) {}

pub unsafe trait IsPinning {}
unsafe impl<T> IsPinning for Pin<T> {}
unsafe impl<'a, T> IsPinning for Ref<'a, T> {}
unsafe impl<T> IsPinning for Ptr<T> {}
pub fn assert_is_pinning<P: IsPinning>(_p: &P) {}
