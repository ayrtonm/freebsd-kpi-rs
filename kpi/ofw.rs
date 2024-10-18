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

use crate::err_codes::*;
use crate::bindings::{ofw_compat_data, phandle_t};
use crate::device::Device;
use crate::{bindings, ErrCode, PointsTo, Result};
use core::cell::LazyCell;
use core::ffi::{c_int, CStr};
use core::marker::PhantomData;
use core::mem::{size_of, MaybeUninit};
use core::ptr::null;

#[repr(C)]
#[derive(Debug)]
pub struct CompatData<T, const N: usize, F = fn() -> [CompatEntry<T>; N]>(
    pub LazyCell<[CompatEntry<T>; N], F>,
);

impl<T, const N: usize, F: FnOnce() -> [CompatEntry<T>; N]> CompatData<T, N, F> {
    pub const fn new(f: F) -> Self {
        Self(LazyCell::new(f))
    }
}

unsafe impl<T: Sync, const N: usize, F: FnOnce() -> [CompatEntry<T>; N]> Sync
    for CompatData<T, N, F>
{
}

#[repr(C)]
#[derive(Debug)]
pub struct CompatEntry<T>(ofw_compat_data, PhantomData<T>);

impl<T> CompatEntry<T> {
    pub const fn null() -> Self {
        Self(
            ofw_compat_data {
                ocd_str: null(),
                ocd_data: 0,
            },
            PhantomData,
        )
    }

    pub fn new(compat: &'static CStr, data: &'static T) -> Self {
        Self(
            ofw_compat_data {
                ocd_str: compat.as_ptr(),
                ocd_data: data as *const T as usize,
            },
            PhantomData,
        )
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, Debug)]
pub struct Node(pub(crate) phandle_t);

#[repr(transparent)]
#[derive(Copy, Clone, Debug)]
pub struct XRef(pub(crate) phandle_t);

impl Device {
    pub fn ofw_bus_status_okay(&self) -> bool {
        let dev_ptr = self.0;
        unsafe { bindings::ofw_bus_status_okay(dev_ptr) != 0 }
    }

    pub fn ofw_bus_is_compatible(&self, compat: &CStr) -> bool {
        let dev_ptr = self.0;
        let compat_ptr = compat.as_ptr();
        let res = unsafe { bindings::ofw_bus_is_compatible(dev_ptr, compat_ptr) };
        res == 1
    }

    pub fn ofw_bus_search_compatible<T>(&self, compat: &[CompatEntry<T>]) -> Result<&'static T> {
        let dev_ptr = self.0;
        let compat_ptr = unsafe {
            bindings::ofw_bus_search_compatible(
                dev_ptr,
                compat.as_ptr() as *const CompatEntry<T> as *const ofw_compat_data,
            )
        };
        let compat_ref = unsafe {
            compat_ptr
                .as_ref()
                .expect("ofw_bus_search_compatible returned null pointer")
        };
        let found = match compat_ref.ocd_data {
            0 => None,
            res_addr => {
                // use core::ptr::from_raw_parts, can prob get provenance from copmat argument
                let res_ptr = res_addr as *const T;
                unsafe { res_ptr.as_ref() }
            }
        };
        found.ok_or(ENULLPTR)
    }

    pub fn ofw_bus_get_node(&self) -> Node {
        let dev_ptr = self.0;
        let node = unsafe { bindings::rust_bindings_ofw_bus_get_node(dev_ptr) };
        Node(node)
    }
}

impl Device {
    // TODO: this will break if OF_device_register_xref ever changes to return non-zero
    pub fn register_xref(&mut self, xref: XRef) {
        let dev_ptr = self.0;
        unsafe {
            bindings::OF_device_register_xref(xref.0, dev_ptr);
        }
    }
}

impl Node {
    pub fn xref_from_node(&self) -> XRef {
        XRef(unsafe { bindings::OF_xref_from_node(self.0) })
    }

    pub fn ofw_bus_find_string_index(&self, list_name: &CStr, name: &CStr) -> Result<c_int> {
        let list_name_ptr = list_name.as_ptr();
        let name_ptr = name.as_ptr();
        let mut idx = 0;
        let res = unsafe {
            bindings::ofw_bus_find_string_index(self.0, list_name_ptr, name_ptr, &mut idx)
        };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(idx)
        }
    }

    pub fn getencprop<T>(&self, propname: &CStr) -> Result<T> {
        let mut t = MaybeUninit::uninit();
        let propname_ptr = propname.as_ptr();
        let res = unsafe {
            bindings::OF_getencprop(
                self.0,
                propname_ptr,
                t.as_mut_ptr() as *mut u32,
                size_of::<T>(),
            )
        };
        if res != size_of::<T>() as isize {
            // TODO: this is the wrong interpretation of res
            Err(ErrCode::from(res as c_int))
        } else {
            unsafe { Ok(t.assume_init()) }
        }
    }

    pub fn get_xref_prop(&self, propname: &CStr) -> Result<XRef> {
        self.getencprop(propname)
    }
}

impl XRef {
    pub fn device_from_xref(&self) -> Result<Device> {
        let res = unsafe { bindings::OF_device_from_xref(self.0) };
        if res.is_null() {
            Err(ENULLPTR)
        } else {
            Ok(unsafe { Device::new(res) })
        }
    }
}
