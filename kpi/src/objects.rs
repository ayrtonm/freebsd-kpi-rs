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

/// Expands to aif $condition is not passed in.
#[doc(hidden)]
#[macro_export]
macro_rules! expand_if_not_c {
    ($function:item, ) => {
        $function
    };
    ($function:item, $condition:ident) => {};
}

#[doc(hidden)]
#[macro_export]
macro_rules! from_bindings_if_c {
    ($var_name:ident, ) => {
        $var_name
    };
    ($var_name:ident, $condition:ident) => {
        $crate::bindings::$var_name
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
     , with desc $desc:ident and typedef $typedef:ident
     $(, with init glue { $($init_glue:tt)* })?
     $(, with drop glue { $($drop_glue:tt)* })?
     $(, $infallible:ident )? ;)*) => {
        $(
            #[doc(hidden)]
            #[macro_export]
            macro_rules! $fn_name {
                (get_typedef) => { $crate::bindings::$typedef };
                (get_desc) => { $crate::bindings::$desc };
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
     , with desc $desc:ident and typedef $typedef:ident
     $(, with init glue { $($init_glue:tt)* })?
     $(, with drop glue { $($drop_glue:tt)* })? ;)*) => {
        $(
            #[doc(hidden)]
            #[macro_export]
            macro_rules! $fn_name {
                (get_typedef) => { $crate::bindings::$typedef };
                (get_desc) => { $crate::bindings::$desc };
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
            let typedef_val = <$fn_name!(get_typedef)>::default();
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
            let typedef_val = <$fn_name!(get_typedef)>::default();
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
            let typedef_val = <$fn_name!(get_typedef)>::default();
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
            methods: $class_sym::$method_table.0.get().cast::<$crate::bindings::kobj_method_t>(),
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

        // Define a new module to avoid polluting the namespace in which this macro was invoked.
        // Arbitrarily choose $class_sym (a unique ELF symbol) as the module name to allow using
        // this macro multiple times in one source file.
        mod $class_sym {

            use core::cell::UnsafeCell;
            use core::ptr::null_mut;
            use core::mem::transmute;
            use $crate::bindings::kobj_method_t;
            use $crate::objects::MethodTable;
            use $crate::from_bindings_if_c;
            use $crate::expand_if_not_c;

            // Import the class type
            use super::$class_ty;

            // Make everything from bindings available to simplify the extern "C" function
            // definitions
            use $crate::bindings::*;

            // Import all interfaces in the KPI crate and the AsRustType/AsCType traits since the
            // extern "C" functions use them.
            use $crate::interfaces::*;

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
