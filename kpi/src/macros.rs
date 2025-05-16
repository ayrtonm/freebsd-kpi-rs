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

// TODO: make pub optional
#[macro_export]
macro_rules! enum_c_macros {
    ($(#[$($meta:meta)*])*
     pub enum $enum_name:ident {
         $($macro_name:ident$(,)?)*
     }) => {
        $(#[$($meta)*])*
        #[allow(nonstandard_style)]
        pub enum $enum_name {
            $($macro_name = bindings::$macro_name,)*
        }

        impl core::convert::TryFrom<i32> for $enum_name {
            type Error = $crate::ErrCode;

            fn try_from(x: i32) -> $crate::Result<$enum_name> {
                match x {
                    $(bindings::$macro_name => Ok($enum_name::$macro_name),)*
                    _ => Err($crate::err_codes::EDOOFUS),
                }
            }
        }
    };
}

#[macro_export]
macro_rules! typesafe_c_macros {
    ($new_ty:ident, $($macro_name:ident$(,)?)*) => {
        $(pub static $macro_name: $new_ty = $new_ty(bindings::$macro_name);)*
    };
}

#[macro_export]
macro_rules! project {
    (&$owned_ref:ident . $($field_name:tt)*) => {
        {
            &$owned_ref.project(|t| &t.$($field_name)*)
        }
    };
    (&mut $owned_ref:ident . $($field_name:tt)*) => {
        {
            &mut $owned_ref.project_mut(|t| &mut t.$($field_name)*)
        }
    };
    (&$owned_ref:ident -> $($field_name:tt)*) => {
        {
            $owned_ref.project(|t| &t.$($field_name)*).erase_lifetime()
        }
    };
}

macro_rules! define_interface {
    ($($fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*) $(-> $ret:ty)?;)*) => {
        $(
            #[macro_export]
            macro_rules! $fn_name {
                ($driver_ty:ident $impl_fn_name:ident) => {
                    $crate::export_function! {
                        $driver_ty $impl_fn_name
                        $fn_name($($arg_name: $arg,)*) $(-> $ret)*;
                    }
                };
            }
        )*
    };
}
