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

#[macro_export]
macro_rules! gen_enum {
    ($(#[$($meta:meta)*])* $pub:vis enum $enum:ident { $($macro_name:ident$(,)?)* }) => {
        $(#[$($meta)*])*
        $pub enum $enum {
            $($macro_name = $crate::bindings::$macro_name as _,)*
        }
    };
}
#[macro_export]
macro_rules! gen_newtype {
    ($new_ty:ident, $($macro_name:ident$(,)?)*) => {
        $(pub const $macro_name: $new_ty = $new_ty($crate::bindings::$macro_name);)*
    };
}

/// Accesses the base class in a [`SubClass`][crate::ffi::SubClass] using C-like syntax.
///
/// Use `base!(&my_subclass->some_base_field)` to get the address of `some_base_field` in
/// `my_subclass`.
///
/// Use `let base_field_value = unsafe { base!(my_subclass->some_base_field) };` to read
/// `some_base_field` in `my_subclass`.
///
/// Use `unsafe { base!(my_subclass->some_base_field) = base_field_value };` to write to
/// `some_base_field` in `my_subclass`.
///
/// # Safety
///
/// The caller must ensure that reads or writes using this macro are synchronized with other code
/// which accesses those fields. In general this means each fields may have either any number of
/// concurrent readers or one writer. Any other combination requires a lock on the field, atomic
/// accesses or any other form of implicit synchronization. These rules are needed to avoid data
/// races when modifying any fields and are language-agnostic.
#[macro_export]
macro_rules! base {
    (& $sub:ident) => {
        {
            use core::ops::Deref;
            use $crate::ffi::SubClass;
            let subclass: &SubClass<_, _> = &$sub;
            SubClass::as_base_ptr($sub.deref())
        }
    };
    (& $sub:ident -> $field:ident) => {
        {
            let base_ptr = $crate::ffi::SubClass::as_base_ptr(&$sub);
            let field_ptr = unsafe { &raw const (*base_ptr).$field };
            field_ptr.cast_mut()
        }
    };

    ($sub:ident -> $field:ident) => {
        {
            let field_ptr = $crate::base!(&$sub->$field);
            *field_ptr
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! get_first {
    ($x:ident $($rest:ident)*) => {
        $x
    };
}

macro_rules! define_interface {
    ($($fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*) $(-> $ret:ty)?
     $(, with init glue { $($init_glue:tt)* })?
     $(, with drop glue { $($drop_glue:tt)* })? ;)*) => {
        $(
            #[doc(hidden)]
            #[macro_export]
            macro_rules! $fn_name {
                ($driver_ty:ident $impl_fn_name:ident) => {
                    $crate::export_function! {
                        $driver_ty $impl_fn_name
                        $fn_name($($arg_name: $arg,)*) $(-> $ret)*;
                        with init glue {
                            $($($init_glue)*)*
                            let _sc_as_void_ptr = unsafe { bindings::device_get_softc($crate::get_first!($($arg_name)*)) };
                            let _rust_sc_ptr = _sc_as_void_ptr.cast::<RefCounted<<$driver_ty as DeviceIf>::Softc>>();
                            let _sc = unsafe { _rust_sc_ptr.as_ref().unwrap() };
                        }
                        with drop glue { $($($drop_glue)*)* }
                        with prefix args { _sc }
                    }
                };
            }
        )*
    };
}
