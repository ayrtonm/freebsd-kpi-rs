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

use crate::ErrCode;
use crate::bindings::{device_t, ofw_compat_data, phandle_t};
use crate::collections::Pod;
use crate::kobj::AsRustType;
use crate::prelude::*;
use core::ffi::{CStr, c_char, c_int};
use core::mem::{MaybeUninit, align_of, offset_of, size_of};
use core::ptr::null;

#[repr(C)]
#[derive(Debug)]
struct OfwCompatEntry<T> {
    name: *const c_char,
    data: *const T,
}

unsafe impl<T: Sync, const N: usize> Sync for OfwCompatData<T, N> {}

impl<T> OfwCompatEntry<T> {
    const fn null() -> Self {
        Self {
            name: null(),
            data: null(),
        }
    }

    const fn new(compat: &'static CStr, data: &'static T) -> Self {
        Self {
            name: compat.as_ptr(),
            data: data as *const T,
        }
    }
}

/// An array of `ofw_compat_data` entries.
#[repr(C)]
#[derive(Debug)]
pub struct OfwCompatData<T, const N: usize>([OfwCompatEntry<T>; N], OfwCompatEntry<T>);

impl<T, const N: usize> OfwCompatData<T, N> {
    // TODO: Add a constructor that takes no data `()`
    /// Create a new OfwCompatData from an array of tuples of compatible strings and their data.
    pub const fn new(data: [(&'static CStr, &'static T); N]) -> Self {
        let _: () = {
            assert!(size_of::<OfwCompatEntry<T>>() == size_of::<ofw_compat_data>());
            assert!(align_of::<OfwCompatEntry<T>>() == align_of::<ofw_compat_data>());
            assert!(offset_of!(OfwCompatEntry<T>, name) == offset_of!(ofw_compat_data, ocd_str));
            assert!(offset_of!(OfwCompatEntry<T>, data) == offset_of!(ofw_compat_data, ocd_data));
        };
        let mut entries = [const { OfwCompatEntry::null() }; N];
        let mut i = 0;
        while i < N {
            entries[i] = OfwCompatEntry::new(data[i].0, data[i].1);
            i += 1;
        }
        Self(entries, OfwCompatEntry::null())
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct Node(pub phandle_t);

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct XRef(pub phandle_t);

unsafe impl Pod for Node {}
unsafe impl Pod for XRef {}

impl AsRustType<XRef> for phandle_t {
    fn as_rust_type(self) -> XRef {
        XRef(self)
    }
}

impl AsRustType<Node> for phandle_t {
    fn as_rust_type(self) -> Node {
        Node(self)
    }
}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
#[allow(non_snake_case)]
pub mod wrappers {
    use super::*;

    pub fn ofw_bus_status_okay(dev: device_t) -> bool {
        unsafe { bindings::ofw_bus_status_okay(dev) != 0 }
    }

    pub fn ofw_bus_is_compatible(dev: device_t, compat: &CStr) -> bool {
        let compat_ptr = compat.as_ptr();
        let res = unsafe { bindings::ofw_bus_is_compatible(dev, compat_ptr) };
        res == 1
    }

    /// Checks if a device is compatible with any entries in `compat`.
    ///
    /// Returns a reference to the first compatible entry or `Err` if none are found. Unlike the C
    /// version, `compat` does not need to be explicitly terminated by a null entry.
    pub fn ofw_bus_search_compatible<T, const N: usize>(
        dev: device_t,
        compat: &OfwCompatData<T, N>,
    ) -> Result<&'static T> {
        let compat_ptr = unsafe {
            bindings::ofw_bus_search_compatible(
                dev,
                compat as *const OfwCompatData<T, N> as *const OfwCompatEntry<T>
                    as *const ofw_compat_data,
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

    pub fn ofw_bus_get_node(dev: device_t) -> Node {
        let node = unsafe { bindings::ofw_bus_get_node(dev) };
        Node(node)
    }

    // TODO: this will break if OF_device_register_xref ever changes to return non-zero
    pub fn OF_device_register_xref(xref: XRef, dev: device_t) {
        unsafe {
            bindings::OF_device_register_xref(xref.0, dev);
        }
    }

    pub fn OF_xref_from_node(node: Node) -> XRef {
        XRef(unsafe { bindings::OF_xref_from_node(node.0) })
    }

    pub fn ofw_bus_find_string_index(node: Node, list_name: &CStr, name: &CStr) -> Result<c_int> {
        let list_name_ptr = list_name.as_ptr();
        let name_ptr = name.as_ptr();
        let mut idx = 0;
        let res = unsafe {
            bindings::ofw_bus_find_string_index(node.0, list_name_ptr, name_ptr, &mut idx)
        };
        if res != 0 {
            Err(ErrCode::from(res))
        } else {
            Ok(idx)
        }
    }

    pub unsafe fn OF_getencprop<T: Pod>(node: Node, propname: &CStr) -> Result<T> {
        let mut t = MaybeUninit::uninit();
        let propname_ptr = propname.as_ptr();
        let res = unsafe {
            bindings::OF_getencprop(
                node.0,
                propname_ptr,
                t.as_mut_ptr() as *mut u32,
                size_of::<T>(),
            )
        };
        if res != size_of::<T>() as isize {
            // TODO: this is the wrong interpretation of res
            //Err(ErrCode::from(res as c_int))
            Err(EINVAL)
        } else {
            unsafe { Ok(t.assume_init()) }
        }
    }

    pub fn OF_device_from_xref(xref: XRef) -> Result<device_t> {
        let res = unsafe { bindings::OF_device_from_xref(xref.0) };
        if res.as_ptr().is_null() {
            Err(ENULLPTR)
        } else {
            Ok(res)
        }
    }

    pub fn OF_child(node: Node) -> Option<Node> {
        let res = unsafe { bindings::OF_child(node.0) };
        if res > 0 { Some(Node(res)) } else { None }
    }

    pub fn OF_peer(node: Node) -> Option<Node> {
        let res = unsafe { bindings::OF_peer(node.0) };
        if res > 0 { Some(Node(res)) } else { None }
    }

    pub fn OF_hasprop(node: Node, propname: &CStr) -> bool {
        let res = unsafe { bindings::OF_hasprop(node.0, propname.as_ptr()) };
        res == 1
    }
}
