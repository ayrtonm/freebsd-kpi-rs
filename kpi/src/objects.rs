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

use crate::bindings::{kobj_class, kobj_class_t, kobj_method_t};
use core::cell::UnsafeCell;

/// Expands to a statement if $condition is not passed in.
#[doc(hidden)]
#[macro_export]
macro_rules! expand_if_not_something {
    ($thing:item; ) => {
        $thing
    };
    ($thing:item; $condition:ident) => {};
}

#[doc(hidden)]
#[macro_export]
macro_rules! path_if {
    ($module:ident, $thing:ident, ) => {
        $module::$thing
    };
    ($module:ident, $thing:ident, $condition:ident) => {
        $crate::bindings::$thing
    };
}
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

pub trait KobjLayout {
    type Layout;
}

pub trait KobjClass: KobjLayout {
    fn get_class(&self) -> *mut kobj_class;
}

// UnsafeCell needed to ensure static ends up in .data
#[doc(hidden)]
#[repr(C)]
pub struct BaseClasses<const N: usize>(pub UnsafeCell<[kobj_class_t; N]>);
unsafe impl<const N: usize> Sync for BaseClasses<N> {}

// UnsafeCell needed to ensure static ends up in .data
#[doc(hidden)]
#[repr(C)]
pub struct MethodTable<const N: usize>(pub UnsafeCell<[kobj_method_t; N]>);
unsafe impl<const N: usize> Sync for MethodTable<N> {}

#[doc(hidden)]
#[macro_export]
macro_rules! get_first {
    ($x:ident $($rest:ident)*) => {
        $x
    };
}

#[macro_export]
macro_rules! define_interface {
    (in $trait:ident
     $(fn $fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*) $(-> $ret:ty)?
     $(, with init glue { $($init_glue:tt)* })?
     $(, with drop glue { $($drop_glue:tt)* })?
     $(, $infallible:ident )? ;)*) => {
        $(
            #[doc(hidden)]
            #[macro_export]
            macro_rules! $fn_name {
                ($driver_ty:ident $impl_fn_name:ident) => {
                    $crate::define_c_function! {
                        $driver_ty impls $impl_fn_name in $trait as
                        fn $fn_name($($arg_name: $arg,)*) $(-> $ret)*;
                        with init glue { $($($init_glue)*)* }
                        with drop glue { $($($drop_glue)*)* }
                        $($infallible)*
                    }
                };
            }
        )*
    };
}
#[macro_export]
macro_rules! define_dev_interface {
    (in $trait:ident
     $(fn $fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*) $(-> $ret:ty)?
     $(, with init glue { $($init_glue:tt)* })?
     $(, with drop glue { $($drop_glue:tt)* })? ;)*) => {
        $(
            #[doc(hidden)]
            #[macro_export]
            macro_rules! $fn_name {
                ($driver_ty:ident $impl_fn_name:ident) => {
                    $crate::define_c_function! {
                        $driver_ty impls $impl_fn_name in $trait as
                        fn $fn_name($($arg_name: $arg,)*) $(-> $ret)*;
                        with init glue {
                            $($($init_glue)*)*
                            use $crate::bindings;
                            use $crate::objects::KobjLayout;
                            use $crate::sync::arc::ArcRef;

                            let _void_ptr = unsafe { bindings::device_get_softc($crate::get_first!($($arg_name)*)) };
                            let _sc_ptr = _void_ptr.cast::<<$driver_ty as KobjLayout>::Layout>();
                            let _sc = unsafe { ArcRef::from_raw(_sc_ptr) };
                        }
                        with drop glue {
                            $($($drop_glue)*)*
                        }
                        with prefix args { _sc }
                    }
                };
            }
        )*
    };
}

#[macro_export]
macro_rules! define_c_function {
    (
        $driver_ty:ident impls $impl:ident in $trait:ident as
        fn $fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*);
        $(with init glue { $($init_glue:tt)* })?
        $(with drop glue { $($drop_glue:tt)* })?
        $(with prefix args { $($prefix_args:ident)* })?
    ) => {
        #[allow(unused_mut)]
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $impl($($arg_name: $arg,)*) {
            use core::any::{Any, TypeId};
            let typedef_val = {
                use $crate::bindings;
                ${concat($fn_name, _t)}::default()
            };
            let typedef_id = typedef_val.type_id();
            let this_fn_id = TypeId::of::<Option<unsafe extern "C" fn($($arg,)*) -> $ret>>();
            assert!(typedef_id == this_fn_id);

            // Convert all arguments from C types to rust types
            $(let mut $arg_name: _ = $arg_name.as_rust_type();)*

            // Call init glue if any
            $($($init_glue)*)*;

            // Call the rust implementation
            <$driver_ty as $trait>::$fn_name($($($prefix_args,)*)* $($arg_name,)*);

            // Call drop glue if any
            $($($drop_glue)*)*;
        }
    };
    (
        $driver_ty:ident impls $impl:ident in $trait:ident as
        fn $fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*) -> $ret:ty;
        $(with init glue { $($init_glue:tt)* })?
        $(with drop glue { $($drop_glue:tt)* })?
        $(with prefix args { $($prefix_args:ident)* })?
        $(rust returns $ret_as_rust_ty:ty)?
    ) => {
        #[allow(unused_mut)]
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $impl($($arg_name: $arg,)*) -> $ret {
            use core::any::{Any, TypeId};
            let typedef_val = {
                ${concat($fn_name, _t)}::default()
            };
            let typedef_id = typedef_val.type_id();
            let this_fn_id = TypeId::of::<Option<unsafe extern "C" fn($($arg,)*) -> $ret>>();
            assert!(typedef_id == this_fn_id);

            // Convert all arguments from C types to rust types
            $(let mut $arg_name: _ = $arg_name.as_rust_type();)*

            // Call init glue if any
            $($($init_glue)*)*

            // Call the rust implementation, coercing to the result type if specified
            let res$(: $crate::Result<$ret_as_rust_ty>)* = <$driver_ty as $trait>::$fn_name($($($prefix_args,)*)* $($arg_name,)*);

            // Call drop glue if any
            $($($drop_glue)*)*

            // Convert return value from rust type to a C type
            match res {
                Ok(r) => r.as_c_type(),
                Err(e) => e.as_c_type(),
            }
        }
    };
    (
        $driver_ty:ident impls $impl:ident in $trait:ident as
        fn $fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*) -> $ret:ty;
        $(with init glue { $($init_glue:tt)* })?
        $(with drop glue { $($drop_glue:tt)* })?
        $(with prefix args { $($prefix_args:ident)* })?
        infallible
    ) => {
        #[allow(unused_mut)]
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $impl($($arg_name: $arg,)*) -> $ret {
            use core::any::{Any, TypeId};
            let typedef_val = {
                use $crate::bindings;
                ${concat($fn_name, _t)}::default()
            };
            let typedef_id = typedef_val.type_id();
            let this_fn_id = TypeId::of::<Option<unsafe extern "C" fn($($arg,)*) -> $ret>>();
            assert!(typedef_id == this_fn_id);

            // Convert all arguments from C types to rust types
            $(let mut $arg_name: _ = $arg_name.as_rust_type();)*

            // Call init glue if any
            $($($init_glue)*)*

            // Call the rust implementation, coercing to the result type if specified
            let res = <$driver_ty as $trait>::$fn_name($($($prefix_args,)*)* $($arg_name,)*);

            // Call drop glue if any
            $($($drop_glue)*)*

            res
        }
    };
}

#[macro_export]
macro_rules! define_class {
    ($class_sym:ident, $class_name:expr, $class_ty:ident, $method_table:ident
        $(inherit from $($base_classes:ident)*,)?
    ) => {
        // UnsafeCell needed to ensure static ends up in .data
        #[repr(C)]
        #[derive(Debug)]
        pub struct $class_ty(core::cell::UnsafeCell<$crate::bindings::kobj_class>);

        impl $crate::objects::KobjClass for $class_ty {
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
            methods: $method_table.0.get().cast::<$crate::bindings::kobj_method_t>(),
            size: core::mem::size_of::<<$class_ty as $crate::objects::KobjLayout>::Layout>(),
            baseclasses: {
                $crate::expand_if_something_or_else_null!({
                    // expand to this
                    static BASE_CLASSES: $crate::objects::BaseClasses<{$crate::count!($($($base_classes)*)*) + 1 }> = $crate::objects::BaseClasses(core::cell::UnsafeCell::new([
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

#[macro_export]
macro_rules! method_table {
    ($class_sym:ident, $class_ty:ident, $method_table:ident = {
        $($if_fn:ident $impl:ident $(defined in $lang:ident)?,)*
    }; $(with interfaces from { $($extra_imports:path$(,)?)* };)? ) => {
        #[unsafe(no_mangle)]
        static $method_table: $crate::objects::MethodTable<{ $crate::count!($($impl)*) + 1 }> =
            $crate::objects::MethodTable(core::cell::UnsafeCell::new([
                $(
                    {
                        let desc = {
                            use $crate::bindings::*;
                            &raw mut ${concat($if_fn, _desc)}
                        };
                        let func_as_ptr = $crate::path_if!($class_sym, $impl, $($lang)*) as *const ();
                        type MethodTableFn = Option<unsafe extern "C" fn()>;
                        let func = unsafe { core::mem::transmute::<*const (), MethodTableFn>(func_as_ptr) };
                        $crate::bindings::kobj_method_t { desc, func }
                    },
                )*
                $crate::bindings::kobj_method_t { desc: core::ptr::null_mut(), func: None }
            ]));
        mod $class_sym {
            use super::$class_ty;
            use $crate::bindings::*;
            // Import all interfaces known in the KPI crate and AsRustType/AsCType
            use $crate::interfaces::*;
            // Import all macros from this crate
            use $crate::*;
            // Import all macros from extra imports
            $($(use $extra_imports::*;)*)*
            $($crate::expand_if_not_something!(
                $if_fn!($class_ty $impl);;
                $($lang)*
            );)*
        }
    };
}
