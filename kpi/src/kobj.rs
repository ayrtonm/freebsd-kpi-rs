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

use crate::prelude::*;

/// Expands to an expression if $condition is passed in. Otherwise expands to null_mut()
#[doc(hidden)]
#[macro_export]
macro_rules! expand_if_something_or_else_null {
    ($thing:expr, ) => {
        core::ptr::null_mut()
    };
    ($thing:expr, $($condition:tt)*) => {
        $thing
    };
}

/// Count the number of idents
#[doc(hidden)]
#[macro_export]
macro_rules! count {
    () => {
        0
    };
    ($x:ident) => {
        1
    };
    ($x:ident $($y:ident)*) => {
        1 + $crate::count!($($y)*)
    };
}

pub trait KobjClass {
    const SIZE: usize;
}

#[doc(hidden)]
#[repr(C)]
pub struct BaseClasses<const N: usize>(pub [bindings::kobj_class_t; N]);
unsafe impl<const N: usize> Sync for BaseClasses<N> {}

#[doc(hidden)]
#[repr(C)]
pub struct MethodTable<const N: usize>(pub [bindings::kobj_method_t; N]);
unsafe impl<const N: usize> Sync for MethodTable<N> {}

#[macro_export]
macro_rules! define_class {
    ($class_sym:ident, $class_name:expr, $class_ty:ident, $method_table:ident
        $(inherit from $($base_classes:ident)*,)?
    ) => {
        #[repr(C)]
        #[derive(Debug)]
        pub struct $class_ty($crate::bindings::kobj_class);

        // Rust cannot access the only instance of $driver_ty created so we can impl Sync to allow
        // creating a static
        unsafe impl Sync for $class_ty {}

        #[unsafe(no_mangle)]
        static mut $class_sym: $class_ty = $class_ty($crate::bindings::kobj_class {
            name: {
                let c: &'static core::ffi::CStr = $class_name;
                c.as_ptr()
            },
            methods: unsafe { &raw const $method_table.0[0] },
            size: <$class_ty as $crate::kobj::KobjClass>::SIZE,
            baseclasses: {
                $crate::expand_if_something_or_else_null!({
                    // expand to this
                    static mut BASE_CLASSES: BaseClasses<{$crate::count!($($base_classes)*) + 1 }> = $crate::kobj::BaseClasses([
                        $(&raw mut $crate::bindings::$base_classes,)*
                        core::ptr::null_mut(),
                    ]);
                    BASE_CLASSES.0.as_mut_ptr()
                },
                // if this is something
                $($($base_classes)*)*)
            },
            refs: 0,
            ops: core::ptr::null_mut(),
        });
    };
}

#[macro_export]
macro_rules! method_table {
    ($class_sym:ident, $class_ty:ident, $method_table:ident = {
        $($if_fn:ident $impl:ident,)*
    };) => {
        #[unsafe(no_mangle)]
        static mut $method_table: $crate::kobj::MethodTable<{ $crate::count!($($impl)*) + 1 }> =
            $crate::kobj::MethodTable([
                $(
                    {
                        let desc = {
                            use $crate::bindings::*;
                            &raw mut ${concat($if_fn, _desc)}
                        };
                        let func_as_ptr = $class_sym::$impl as *const ();
                        type MethodTableFn = Option<unsafe extern "C" fn()>;
                        let func = unsafe { core::mem::transmute::<*const (), MethodTableFn>(func_as_ptr) };
                        $crate::bindings::kobj_method_t { desc, func }
                    },
                )*
                $crate::bindings::kobj_method_t { desc: core::ptr::null_mut(), func: None }
            ]);
        mod $class_sym {
            use super::$class_ty;
            use $crate::bindings::*;
            use $crate::{AsRustType, AsCType};
            use $crate::interfaces::*;
            $(
                use $crate::$if_fn;
                $if_fn!($class_ty $impl);
            )*
        }
    };
}
