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

#[macro_export]
macro_rules! method_table {
    ($class_sym:ident, $class_ty:ident, $method_table:ident = {
        $($if_fn:ident $impl:ident $(defined in $lang:ident)?,)*
    }; $(with interfaces from { $($extra_imports:path$(,)?)* };)? ) => {

        // Define a new module to avoid polluting the namespace in which this macro was invoked.
        // Arbitrarily choose $class_sym (a unique ELF symbol) as the module name to allow using
        // this macro multiple times in one source file.
        mod $class_sym {

            use core::cell::UnsafeCell;
            use core::ptr::null_mut;
            use core::mem::transmute;
            use $crate::bindings::kobj_method_t;
            use $crate::kobj::MethodTable;
            use $crate::from_bindings_if_c;
            use $crate::expand_if_not_c;

            // Import the class type
            use super::$class_ty;

            // Make everything from bindings available to simplify the extern "C" function
            // definitions
            use $crate::bindings::*;
            #[cfg(not(test))]
            use core::ffi::{c_void, c_int};

            // Import all interfaces in the KPI crate and the AsRustType/AsCType traits since the
            // extern "C" functions use them.
            use $crate::kobj::interfaces::*;
            use $crate::kobj::{AsRustType, AsCType};

            // Import all macros from this crate
            use $crate::*;

            // Import all macros and interfaces from extra imports
            $($(use $extra_imports::*;)*)*

            const NUM_METHODS: usize = $crate::count!($($impl)*);

            type MethodTableFn = Option<unsafe extern "C" fn()>;

            #[unsafe(no_mangle)]
            pub static $method_table: MethodTable<{ NUM_METHODS + 1 }> =
                MethodTable(UnsafeCell::new([
                    // Repeat the following for each $if_fn $impl pair
                    $(
                        {
                            let desc = &raw mut $if_fn!(get_desc);
                            let func_as_ptr = from_bindings_if_c!($impl, $($lang)*) as *const ();
                            let func = unsafe { transmute::<*const (), MethodTableFn>(func_as_ptr) };
                            kobj_method_t { desc, func }
                        },
                    )*
                    // Add the method table's null-terminator
                    kobj_method_t { desc: null_mut(), func: None }
            ]));
            $(expand_if_not_c! {
                // Invoke the interface function macro with two identifiers so it expands to the
                // extern "C" functions that were inserted in the method table and call the
                // corresponding interface trait methods
                $if_fn!($class_ty $impl);,
                $($lang)*
            })*
        }
    };
}
