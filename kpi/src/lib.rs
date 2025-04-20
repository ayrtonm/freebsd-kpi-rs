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

#![no_std]
#![deny(improper_ctypes, unused_must_use, unreachable_patterns)]

#[cfg(feature = "std")]
extern crate std;

#[macro_use]
mod macros;

#[cfg(target_arch = "aarch64")]
pub mod arm64;
pub mod bindings;
pub mod boxed;
pub mod bus;
pub mod cell;
pub mod device;
pub mod ffi;
#[cfg(feature = "intrng")]
pub mod intr;
pub mod malloc;
#[cfg(feature = "fdt")]
pub mod ofw;
#[cfg(not(feature = "std"))]
mod panic;
pub mod sleep;
pub mod sync;
pub mod taskq;
#[cfg(not(feature = "std"))]
pub mod tty;
pub mod vec;

pub type Result<T> = core::result::Result<T, ErrCode>;

pub trait AsCType<T> {
    fn as_c_type(self) -> T;
}

pub trait AsRustType<T> {
    fn as_rust_type(self) -> T;
}

impl<T> AsRustType<T> for T {
    fn as_rust_type(self) -> T {
        self
    }
}

pub mod prelude {
    pub use crate::bindings;
    pub use crate::boxed::Box;
    pub use crate::Result;
    #[cfg(target_arch = "aarch64")]
    pub use crate::{curthread, pcpu_get, pcpu_ptr, read_reg, write_reg};
    pub use crate::{device_get_softc, device_init_softc};
    #[cfg(not(feature = "std"))]
    pub use crate::{dprint, dprintln, print, println};
    pub use crate::{pin_field, pin_field_mut};

    // Error code macros
    pub use crate::err_codes::*;
    // SYS_RES_* macros
    pub use crate::bus::SysRes::*;
    // BUS_PROBE_* macros
    pub use crate::device::BusProbe::*;
    // INTR_ROOT_* macros
    #[cfg(feature = "intrng")]
    pub use crate::intr::IntrRoot::*;
    // FILTER_* macros
    pub use crate::bus::Filter::*;

    pub use crate::bus::wrappers::*;
    pub use crate::device::wrappers::*;
    // M_* macros
    pub use crate::malloc::wrappers::*;
    #[cfg(feature = "fdt")]
    pub use crate::ofw::wrappers::*;
    pub use crate::sleep::wrappers::*;
    pub use crate::sync::wrappers::*;
    pub use crate::taskq::wrappers::*;

    pub use crate::bus::BusIfWrappers;
    pub use crate::device::{DeviceIf, IsDriver};

    // These are implemented widely throughout the KPI crate
    pub use crate::{AsCType, AsRustType};
}

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

#[doc(hidden)]
#[macro_export]
macro_rules! if_desc {
    (device_probe) => {
        $crate::bindings::device_probe_desc
    };
    (device_attach) => {
        $crate::bindings::device_attach_desc
    };
    (device_detach) => {
        $crate::bindings::device_detach_desc
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! export_function {
    (
        $driver_ty:ident $impl:ident
        void $fn_name:ident($($arg:ident $arg_name:ident$(,)?)*)
        $(with init glue { $($init_glue:tt)* })?
        $(with drop glue { $($drop_glue:tt)* })?
    ) => {
        #[no_mangle]
        pub unsafe extern "C" fn $impl($($arg_name: $arg,)*) {
            // Checks that this extern "C" function matches the declaration in bindings.h
            const _TYPES_MATCH: unsafe extern "C" fn($($arg,)*) = $crate::bindings::$fn_name;

            // Convert all arguments from C types to rust types
            $(let mut $arg_name = $arg_name.as_rust_type();)*

            // Call init glue if any
            $($($init_glue)*)*;

            // Call the rust implementation
            $driver_ty::$fn_name($($arg_name,)*);

            // Call drop glue if any
            $($($drop_glue)*)*;
        }
    };
    (
        $driver_ty:ident $impl:ident
        $ret:ident $fn_name:ident($($arg:ident $arg_name:ident$(,)?)*)
        $(with init glue { $($init_glue:tt)* })?
        $(with drop glue { $($drop_glue:tt)* })?
        $(rust returns $ret_as_rust_ty:ty)?
    ) => {
        #[no_mangle]
        pub unsafe extern "C" fn $impl($($arg_name: $arg,)*) -> $ret {
            // Checks that this extern "C" function matches the declaration in bindings.h
            const _TYPES_MATCH: unsafe extern "C" fn($($arg,)*) -> $ret = $crate::bindings::$fn_name;

            // Convert all arguments from C types to rust types
            $(let mut $arg_name = $arg_name.as_rust_type();)*

            // Call init glue if any
            $($($init_glue)*)*;

            // Call the rust implementation, coercing to the result type if specified
            let res$(: $crate::Result<$ret_as_rust_ty>)* = $driver_ty::$fn_name($($arg_name,)*);

            // Call drop glue if any
            $($($drop_glue)*)*;

            // Convert return value from rust type to a C type
            match res {
                Ok(r) => r.as_c_type(),
                Err(e) => e.as_c_type(),
            }
        }
    };
}

#[macro_export]
macro_rules! driver {
    ($driver_sym:ident, $driver_name:expr, $driver_ty:ident, $methods:ident,
        INTERFACES {
            $($if_fn:ident $impl:ident,)*
        }$(,)?
        $(EXPORTS {
            $($ret:ident $fn_name:ident($($arg:ident $arg_name:ident$(,)?)*);)*
        })?
    ) => {
        use $crate::prelude::*;

        // These structs are not defined in the KPI crate to allow inherent impls in drivers
        #[repr(C)]
        #[derive(Debug)]
        pub struct $driver_ty(core::cell::UnsafeCell<$crate::bindings::kobj_class>);
        unsafe impl Sync for $driver_ty {}
        impl IsDriver for $driver_ty {
            fn get_driver() -> *const $crate::bindings::kobj_class {
                $driver_sym.0.get()
            }
        }

        #[no_mangle]
        pub static $driver_sym: $driver_ty = $driver_ty(
            core::cell::UnsafeCell::new($crate::bindings::kobj_class {
                // TODO: Ensure type is &'static CStr
                name: $driver_name.as_ptr(),
                methods: core::ptr::addr_of!($methods).cast(),
                // TODO: ensure alignment of softc memory supports Softc
                size: core::mem::size_of::<Option<<$driver_ty as DeviceIf>::Softc>>(),
                baseclasses: core::ptr::null_mut(),
                refs: 0,
                ops: core::ptr::null_mut(),
            })
        );

        #[no_mangle]
        static $methods: [$crate::device::DeviceMethod; $crate::count!($($impl)*) + 1] = [
            $(
                {
                    let desc = &raw mut $crate::if_desc!($if_fn);
                    $crate::device::DeviceMethod::new(desc, $driver_sym::$impl as *const ())
                },
            )*
            $crate::device::DeviceMethod::null(),
        ];

        // Create a module to glob import bindings and allow C types in function declaration without
        // polluting the namespace driver! was invoked in. The module name is arbitrary so
        // $driver_sym is used to ensure it is unique (which holds since it corresponds to an
        // unmangled symbol name).
        mod $driver_sym {
            use super::$driver_ty;
            use $crate::bindings::*;
            use $crate::{AsRustType, AsCType};
            use $crate::device::DeviceIf;
            use $crate::export_function;
            $($crate::$if_fn!($driver_ty $impl);)*
            $($(export_function!($driver_ty $fn_name $ret $fn_name($($arg $arg_name,)*));)*)*
        }
    };
}

macro_rules! err_codes {
    ($($name:ident, $desc:literal,)+) => {
        use core::ffi::c_int;
        use core::fmt;
        use core::fmt::{Debug, Formatter};
        use core::num::NonZeroI32;

        // This is effectively an extensible enum with some overlapping variants
        #[derive(Copy, Clone, PartialEq, Eq)]
        pub struct ErrCode(pub(crate) NonZeroI32);

        pub mod err_codes {
            use core::num::NonZeroI32;
            use crate::{bindings, ErrCode};

            const _: () = {
                // out of an abundance of caution justify the new_unchecked below
                $(assert!(bindings::$name != 0);)*
                $(assert!(bindings::$name != i32::MIN);)*
                $(assert!(bindings::$name != i32::MIN + 1);)*
            };
            $(pub const $name: ErrCode = ErrCode(unsafe { NonZeroI32::new_unchecked(bindings::$name) });)*
            // TODO: make overlap checking less error-prone before adding more KPI crate errors
            pub const ENULLPTR: ErrCode = ErrCode(unsafe { NonZeroI32::new_unchecked(i32::MIN) });
            pub const EBADFFI: ErrCode = ErrCode(unsafe { NonZeroI32::new_unchecked(i32::MIN + 1) });
        }

        impl AsCType<c_int> for () {
            fn as_c_type(self) -> c_int {
                0
            }
        }

        impl AsCType<c_int> for ErrCode {
            fn as_c_type(self) -> c_int {
                self.0.get()
            }
        }

        impl Debug for ErrCode {
            // EWOULDBLOCK/EAGAIN and ENOTSUP/EOPNOTSUPP have identical values on the C side so
            // this match statement must have unreachable patterns
            #[allow(unreachable_patterns)]
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                let msg = match *self {
                    $(err_codes::$name => $desc,)*
                    err_codes::ENULLPTR => "Received a NULL pointer as an error code",
                    err_codes::EBADFFI => "Error at the FFI boundary",
                    _ => "Unknown error code",
                };
                f.write_str(msg)
            }
        }

        impl From<c_int> for ErrCode {
            // EWOULDBLOCK/EAGAIN and ENOTSUP/EOPNOTSUPP have identical values on the C side so
            // this match statement must have unreachable patterns
            #[allow(unreachable_patterns)]
            fn from(val: c_int) -> Self {
                match val {
                    // Map KPI error code values to the respective enum variants
                    $(bindings::$name => err_codes::$name,)*
                    // Map ENULLPTR to itself in case an error code gets round-tripped
                    i32::MIN => err_codes::ENULLPTR,
                    // Map anything else to EBADFFI
                    _ => err_codes::EBADFFI,
                }
            }
        }
    };
}

err_codes! {
    EPERM, "Operation not permitted",
    ENOENT, "No such file or directory",
    ESRCH, "No such process",
    EINTR, "Interrupted system call",
    EIO, "Input/output error",
    ENXIO, "Device not configured",
    E2BIG, "Argument list too long",
    ENOEXEC, "Exec format error",
    EBADF, "Bad file descriptor",
    ECHILD, "No child processes",
    EDEADLK, "Resource deadlock avoided",
    ENOMEM, "Cannot allocate memory",
    EACCES, "Permission denied",
    EFAULT, "Bad address",
    ENOTBLK, "Block device required",
    EBUSY, "Device busy",
    EEXIST, "File exists",
    EXDEV, "Cross-device link",
    ENODEV, "Operation not supported by device",
    ENOTDIR, "Not a directory",
    EISDIR, "Is a directory",
    EINVAL, "Invalid argument",
    ENFILE, "Too many open files in system",
    EMFILE, "Too many open files",
    ENOTTY, "Inappropriate ioctl for device",
    ETXTBSY, "Text file busy",
    EFBIG, "File too large",
    ENOSPC, "No space left on device",
    ESPIPE, "Illegal seek",
    EROFS, "Read-only filesystem",
    EMLINK, "Too many links",
    EPIPE, "Broken pipe",
    EDOM, "Numerical argument out of domain",
    ERANGE, "Result too large",
    EWOULDBLOCK, "Operation would block or Resource temporarily unavailable",
    EAGAIN, "Resource temporarily unavailable",
    EINPROGRESS, "Operation now in progress",
    EALREADY, "Operation already in progress",
    ENOTSOCK, "Socket operation on non-socket",
    EDESTADDRREQ, "Destination address required",
    EMSGSIZE, "Message too long",
    EPROTOTYPE, "Protocol wrong type for socket",
    ENOPROTOOPT, "Protocol not available",
    EPROTONOSUPPORT, "Protocol not supported",
    ESOCKTNOSUPPORT, "Socket type not supported",
    EOPNOTSUPP, "Operation not supported",
    ENOTSUP, "Operation not supported",
    EPFNOSUPPORT, "Protocol family not supported",
    EAFNOSUPPORT, "Address family not supported by protocol family",
    EADDRINUSE, "Address already in use",
    EADDRNOTAVAIL, "Can't assign requested address",
    ENETDOWN, "Network is down",
    ENETUNREACH, "Network is unreachable",
    ENETRESET, "Network dropped connection on reset",
    ECONNABORTED, "Software caused connection abort",
    ECONNRESET, "Connection reset by peer",
    ENOBUFS, "No buffer space available",
    EISCONN, "Socket is already connected",
    ENOTCONN, "Socket is not connected",
    ESHUTDOWN, "Can't send after socket shutdown",
    ETOOMANYREFS, "Too many references: can't splice",
    ETIMEDOUT, "Operation timed out",
    ECONNREFUSED, "Connection refused",
    ELOOP, "Too many levels of symbolic links",
    ENAMETOOLONG, "File name too long",
    EHOSTDOWN, "Host is down",
    EHOSTUNREACH, "No route to host",
    ENOTEMPTY, "Directory not empty",
    EPROCLIM, "Too many processes",
    EUSERS, "Too many users",
    EDQUOT, "Disc quota exceeded",
    ESTALE, "Stale NFS file handle",
    EREMOTE, "Too many levels of remote in path",
    EBADRPC, "RPC struct is bad",
    ERPCMISMATCH, "RPC version wrong",
    EPROGUNAVAIL, "RPC prog. not avail",
    EPROGMISMATCH, "Program version wrong",
    EPROCUNAVAIL, "Bad procedure for program",
    ENOLCK, "No locks available",
    ENOSYS, "Function not implemented",
    EFTYPE, "Inappropriate file type or format",
    EAUTH, "Authentication error",
    ENEEDAUTH, "Need authenticator",
    EIDRM, "Identifier removed",
    ENOMSG, "No message of desired type",
    EOVERFLOW, "Value too large to be stored in data type",
    ECANCELED, "Operation canceled",
    EILSEQ, "Illegal byte sequence",
    ENOATTR, "Attribute not found",
    EDOOFUS, "Programming error",
    EBADMSG, "Bad message",
    EMULTIHOP, "Multihop attempted",
    ENOLINK, "Link has been severed",
    EPROTO, "Protocol error",
    ENOTCAPABLE, "Capabilities insufficient",
    ECAPMODE, "Not permitted in capability mode",
    ENOTRECOVERABLE, "State not recoverable",
    EOWNERDEAD, "Previous owner died",
    EINTEGRITY, "Integrity check failed",
}
