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

use crate::bindings::driver_t;
use crate::kobj::KobjClass;

pub trait Driver: KobjClass {
    const DRIVER: *mut driver_t;
}

/// Defines a driver
///
/// `$driver_sym` should be a snake_case name for the driver. This macro creates a
/// `struct kobj_class` for it without mangling its name so it must be unique among all ELF symbols
/// linked into the kernel.
///
/// `$driver_name` should be a `&'static CStr` literal for the driver (i.e. `c"some_name"`). This
/// macro sets the `struct kobj_class`'s `name` field to it.
///
/// `$driver_ty` should be a PascalCase name for the driver's type. There are no restrictions on
/// this other than it can't match other types imported in the namespace where `driver!` is invoked.
/// That leads to a build error.
///
/// `$method_table` shuld be a snake_case name for the driver's method table. This macro creates a
/// kobj_method_t array for it without mangling its name so it must be unique among all ELF symbols
/// linked into the kernel.
///
/// If the driver is a subclass of another driver the method table name must be followed by `inherit
/// from $BASE_DRIVER1 $BASE_DRIVER2 ...,`. For example a driver that is just a subclass of the
/// simplebus driver would use
/// ```
/// driver!(my_driver, c"my_driver", MyDriver, my_driver_methods,
///         interit from simplebus_driver);
/// ```
/// If there is no subclass driver, this clause is omitted.
///
/// This is followed by interface functions for the method table surrounded by
/// `INTERFACES { $METHODS }`. Each method must have the interface function name followed by the
/// implementation's unmangled ELF symbol and a trailing comma. Like other unmangled symbols, the
/// implementation name must be unique.
///
/// This is followed by other exported C functions and the name of the `$driver_ty` method
/// implementing them
#[macro_export]
macro_rules! driver {
    ($driver_sym:ident, $driver_name:expr, $driver_ty:ident, $method_table:ident = { $($if_fn:ident $impl_name:ident $(defined in $lang:ident)?,)* }
        $(,inherit from $($base_classes:ident)*,)?
        $(with interfaces from { $($extra_imports:path$(,)?)* };)?
    ) => {
        $crate::define_class!($driver_sym, $driver_name, $driver_ty, $method_table $(inherit from $($base_classes)*,)*);
        $crate::method_table!($driver_sym, $driver_ty, $method_table = { $($if_fn $impl_name $(defined in $lang)*,)* }; $(with interfaces from { $($extra_imports)* };)*);

        impl $crate::kobj::KobjLayout for $driver_ty {
            type Layout = $crate::sync::arc::InnerArc<<$driver_ty as $crate::device::DeviceIf>::Softc>;
        }

        impl $crate::driver::Driver for $driver_ty {
            const DRIVER: *mut $crate::bindings::driver_t = $driver_sym.0.get();
        }
    };
}
