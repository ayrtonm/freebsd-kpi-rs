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
macro_rules! define_class {
    ($class_sym:ident, $class_name:expr, $class_ty:ident, $method_table:ident
        $(inherit from $($base_classes:ident)*,)?
    ) => {
        // UnsafeCell needed to ensure static ends up in .data
        #[repr(C)]
        #[derive(Debug)]
        pub struct $class_ty(core::cell::UnsafeCell<$crate::bindings::kobj_class>);

        impl $crate::kobj::KobjClass for $class_ty {
            fn get_class(&self) -> *mut $crate::bindings::kobj_class {
                self.0.get()
            }
        }

        // Rust cannot access the only instance of $driver_ty created so we can impl Sync to allow
        // creating a static
        unsafe impl Sync for $class_ty {}

        #[unsafe(no_mangle)]
        static $class_sym: $class_ty = $class_ty(core::cell::UnsafeCell::new($crate::bindings::kobj_class {
            name: {
                let c: &'static core::ffi::CStr = $class_name;
                c.as_ptr()
            },
            methods: $class_sym::$method_table.0.get().cast::<$crate::bindings::kobj_method_t>(),
            size: core::mem::size_of::<<$class_ty as $crate::kobj::KobjLayout>::Layout>(),
            baseclasses: {
                $crate::expand_if_something_or_else_null!({
                    // expand to this
                    static BASE_CLASSES: $crate::kobj::BaseClasses<{$crate::count!($($($base_classes)*)*) + 1 }> = $crate::kobj::BaseClasses(core::cell::UnsafeCell::new([
                        $(&raw mut $crate::bindings::$($base_classes)*,)*
                        core::ptr::null_mut(),
                    ]));
                    BASE_CLASSES.0.get().cast::<$crate::bindings::kobj_class_t>()
                },
                // if this is something
                $($($base_classes)*)*)
            },
            refs: 0,
            ops: core::ptr::null_mut(),
        }));
    };
}
