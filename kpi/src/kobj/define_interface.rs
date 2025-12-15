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
     $(, with prefix args { $($prefix_args:tt)* })?
     $(, is $infallible:ident )? ;)*) => {
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
                        with prefix args { $($($prefix_args)*)* }
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
                            use $crate::kobj::KobjLayout;
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
            let this_fn_id = TypeId::of::<Option<unsafe extern "C" fn($($arg,)*)>>();
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

            res.as_c_type()
        }
    };
}
