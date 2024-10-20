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

#![allow(nonstandard_style, private_interfaces, unused_imports, dead_code)]

// Creating the following aliases is not advisable in general, but it simplifies the changes
// makeobjops.awk needs since this allows the code it generates to use C type names
pub type void = core::ffi::c_void;
pub type char = core::ffi::c_char;
pub type int = core::ffi::c_int;
pub type long = core::ffi::c_long;

include!(concat!(env!("OUT_DIR"), "/device_if.rs"));
include!(concat!(env!("OUT_DIR"), "/pic_if.rs"));

// The next dummy type definitions are for blocklisted types in the bindgen invocation.
#[repr(C)]
struct __va_list(());
#[repr(C)]
struct mcontext_t(());
#[repr(C)]
struct pcb(());
#[repr(C)]
struct vfpstate(());
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));