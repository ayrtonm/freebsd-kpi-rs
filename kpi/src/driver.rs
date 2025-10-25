/*-
 * spdx-license-identifier: bsd-2-clause
 *
 * copyright (c) 2025 ayrton muñoz
 * all rights reserved.
 *
 * redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * this software is provided by the author and contributors ``as is'' and
 * any express or implied warranties, including, but not limited to, the
 * implied warranties of merchantability and fitness for a particular purpose
 * are disclaimed.  in no event shall the author or contributors be liable
 * for any direct, indirect, incidental, special, exemplary, or consequential
 * damages (including, but not limited to, procurement of substitute goods
 * or services; loss of use, data, or profits; or business interruption)
 * however caused and on any theory of liability, whether in contract, strict
 * liability, or tort (including negligence or otherwise) arising in any way
 * out of the use of this software, even if advised of the possibility of
 * such damage.
 */

use crate::bindings::{driver_t, u_int};
use crate::prelude::*;

/// Count the number of idents
#[doc(hidden)]
#[macro_export]
macro_rules! count {
    ($x:ident) => {
        1
    };
    ($x:ident $($y:ident)*) => {
        1 + $crate::count!($($y)*)
    };
}

/// Expands to an expression if $condition is passed in. Otherwise expands to null_mut()
#[doc(hidden)]
#[macro_export]
macro_rules! expand_if_or_null {
    ($thing:expr, ) => {
        core::ptr::null_mut()
    };
    ($thing:expr, $($condition:tt)*) => {
        $thing
    };
}

#[doc(hidden)]
#[repr(C)]
pub struct BaseClasses<const N: usize>(pub [bindings::kobj_class_t; N]);
unsafe impl<const N: usize> Sync for BaseClasses<N> {}

#[doc(hidden)]
#[repr(C)]
pub struct MethodTable<const N: usize>(pub [bindings::kobj_method_t; N]);
unsafe impl<const N: usize> Sync for MethodTable<N> {}

#[doc(hidden)]
pub trait DriverIf {
    const DROP_FN: unsafe fn(*mut u_int);
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
    ($driver_sym:ident, $driver_name:expr, $driver_ty:ident, $method_table:ident,
        $(inherit from $($base_classes:ident)*,)?
        INTERFACES {
            $($if_fn:ident $impl:ident,)*
        }$(,)?
        $(EXPORTS {
            $($fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*) $(-> $ret:ty)?;)*
        })?
    ) => {
        // Import the prelude. The prelude includes `kpi::bindings` so we rely on this below.
        use $crate::prelude::*;

        // Define a type for the driver. This is defined in this macro rather than the KPI crate to
        // allow inherent impls in driver crates.
        #[repr(C)]
        #[derive(Debug)]
        pub struct $driver_ty(bindings::driver_t);

        // Rust cannot access the only instance of $driver_ty created so we can impl Sync to allow
        // creating a static
        unsafe impl Sync for $driver_ty {}

        impl $crate::driver::DriverIf for $driver_ty {
            const DROP_FN: unsafe fn(*mut bindings::u_int) = $driver_sym::drop_softc;
            const DRIVER: *mut bindings::driver_t = &$driver_sym.0 as *const bindings::driver_t as *mut bindings::driver_t;
            //pub const DRIVER: &'static bindings::driver_t = &$driver_sym.0;
        }

        #[unsafe(no_mangle)]
        pub static $driver_sym: $driver_ty = $driver_ty(
            bindings::driver_t {
                name: $driver_name.as_ptr(),
                methods: $method_table.0.as_ptr(),
                // TODO: ensure alignment of softc memory supports Softc
                size: $driver_sym::SOFTC_SIZE,
                // Expands to either the expression before the comma or null_mut() depending on
                // whether $base_classes was passed in or not
                baseclasses: $crate::expand_if_or_null!(
                    $driver_sym::BASE_CLASSES.0.as_ptr().cast_mut(), $($($base_classes)*)*
                ),
                refs: 0,
                ops: core::ptr::null_mut(),
            }
        );

        #[unsafe(no_mangle)]
        static $method_table: $crate::driver::MethodTable<{ $driver_sym::NUM_METHODS + 1 }> = $crate::driver::MethodTable([
            $(
                {
                    let desc = {
                        // Glob import everything in this let binding's scope to pick out the one we need
                        use $crate::bindings::*;
                        &raw mut ${concat($if_fn, _desc)}
                    };
                    let func_as_ptr = $driver_sym::exported_fns::$impl as *const ();
                    let func = unsafe { core::mem::transmute::<*const (), Option<unsafe extern "C" fn()>>(func_as_ptr) };
                    bindings::kobj_method_t { desc, func }
                },
            )*
            bindings::kobj_method_t { desc: core::ptr::null_mut(), func: None }
        ]);

        // Create a module to create some constants without requiring the user to pass in a unique
        // name for them. The module name is arbitrary but we use $driver_sym since it already needs
        // to be unique
        mod $driver_sym {
            use core::ffi::{CStr, c_void};
            use core::ptr::{null_mut, drop_in_place};
            use core::alloc::Layout;
            use $crate::bindings::u_int;
            use core::mem::align_of;
            use core::mem::offset_of;
            use $crate::bindings;
            use $crate::driver::BaseClasses;
            use $crate::device::DeviceIf;
            use $crate::ptr::RefCounted;
            use $crate::ptr::RefCountData;
            use super::$driver_ty;


            // Ensure the second argument is a &'static CStr or break the build
            const _NAME_IS_STATIC_CSTR: () = {
                let _: &'static CStr = $driver_name;
            };

            // Count the number of interface methods to figure out the size of the method table
            pub const NUM_METHODS: usize = $crate::count!($($impl)*);

            // Define a static kobj_class_t array for the baseclasses. This will not exist if
            // `inherit from $base` is not specified so it must be accessed with
            // `expand_if_or_null!`
            $(
                const NUM_BASE_CLASSES: usize = $crate::count!($($base_classes)*) + 1;
                pub static BASE_CLASSES: BaseClasses<NUM_BASE_CLASSES> = BaseClasses([
                    $(
                        &raw mut bindings::$base_classes,
                    )*
                    null_mut(),
                ]);
            )?

            pub const SOFTC_SIZE: usize = {
                let sc_layout = Layout::new::<RefCounted<<$driver_ty as DeviceIf>::Softc>>();
                // TODO: replace usize with size_t and check if this is the right value for FreeBSD
                // TODO: figure out why NVMe requires somewhere between an 8 and 16 factor
                //if sc_layout.align() > 2 * align_of::<usize>() {
                //    panic!("softc requires more alignment than can malloc provides");
                //}
                sc_layout.size()
            };

            // The driver drop fn uses a pointer to the refcount to find the softc pointer and drops
            // it
            pub unsafe fn drop_softc(count_ptr: *mut u_int) {
                // Find the offset of the refcount within the RefCounted<T> for this driver
                let metadata_offset = RefCounted::<<$driver_ty as DeviceIf>::Softc>::metadata_offset();
                let count_offset = metadata_offset + offset_of!(RefCountData, count);
                // Get a pointer to the start of the softc
                let sc_void_ptr = unsafe { count_ptr.cast::<c_void>().byte_sub(count_offset) };
                // Cast it to the right type
                let sc_ptr = sc_void_ptr.cast::<RefCounted<<$driver_ty as DeviceIf>::Softc>>();
                // Drop the RefCounted<T> (softc plus count and drop function) without freeing the softc memory.
                unsafe {
                    drop_in_place(sc_ptr)
                };
                unsafe { bindings::device_free_softc(sc_void_ptr) }
            }
            // Create another module to glob import all bindings and allow C types in function
            // declaration without polluting the namespace driver! was invoked in. The module name
            // is completely arbitrary.
            pub mod exported_fns {
                use super::*;
                use $crate::bindings::*;
                use $crate::{AsRustType, AsCType};
                #[cfg(feature = "intrng")]
                use $crate::intr::PicIf;
                $(
                    #[cfg(not(feature = "std"))]
                    use $crate::$if_fn;
                )*
                $($if_fn!($driver_ty $impl);)*

                $($($crate::export_function!($driver_ty $fn_name $fn_name($($arg_name: $arg,)*) $(-> $ret)*;);)*)*
            }
        }
    };
}
