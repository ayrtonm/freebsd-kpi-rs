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
#![feature(macro_metavar_expr_concat)]

#[cfg(feature = "std")]
extern crate std;

#[macro_use]
mod macros;

#[cfg(target_arch = "aarch64")]
pub mod arm64;
pub mod bindings;
pub mod boxed;
pub mod cell;
pub mod bus;
pub mod device;
pub mod ffi;
mod interfaces;
#[cfg(feature = "intrng")]
pub mod intr;
pub mod malloc;
#[cfg(feature = "fdt")]
pub mod ofw;
#[cfg(not(feature = "std"))]
mod panic;
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

// TODO: Put these in the right modules
pub mod misc {
    use crate::bindings;
    use crate::bindings::{u_int, cpuset_t};
    use core::ffi::c_int;

    pub fn DELAY(usec: u32) {
        unsafe { bindings::DELAY(usec as c_int) }
    }

    pub fn mp_maxid() -> usize {
        let val = unsafe { bindings::mp_maxid };
        val as usize
    }
    pub fn atomic_readandclear_32(p: &mut u32) -> u32 {
        unsafe {
            bindings::rust_bindings_atomic_readandclear_32(p)
        }
    }
    pub fn atomic_set_32(p: &mut u32, x: u32) {
        unsafe {
            bindings::rust_bindings_atomic_set_32(p, x)
        }
    }
    pub fn CPU_SET(cpu: u_int, set: *mut cpuset_t) {
        unsafe {
            bindings::rust_bindings_CPU_SET(cpu, set)
        }
    }
    pub fn CPU_ISSET(cpu: u_int, set: &cpuset_t) -> bool {
        unsafe {
            bindings::rust_bindings_CPU_ISSET(cpu, set as *const cpuset_t as *mut cpuset_t)
        }
    }
}

pub mod prelude {
    #[doc(inline)]
    #[cfg(target_arch = "aarch64")]
    pub use crate::arm64::*;

    pub use crate::bindings;
    pub use crate::Result;
    pub use crate::project;
    #[cfg(target_arch = "aarch64")]
    pub use crate::{curthread, isb, rmb, wmb, pcpu_get, pcpu_ptr, read_specialreg, write_specialreg};
    pub use crate::{device_get_softc, device_init_softc};
    #[cfg(not(feature = "std"))]
    pub use crate::{device_print, device_println, print, println};
    #[cfg(feature = "std")]
    pub use std::{print, println};

    // Error code macros
    #[doc(inline)]
    pub use crate::err_codes::*;

    // SYS_RES_* macros
    #[doc(inline)]
    pub use crate::bus::SysRes::*;

    // BUS_PROBE_* macros
    #[doc(inline)]
    pub use crate::device::BusProbe::*;

    // INTR_ROOT_* macros
    #[doc(inline)]
    #[cfg(feature = "intrng")]
    pub use crate::intr::IntrRoot::*;

    // INTR_ISRCF_* macros
    #[doc(inline)]
    #[cfg(feature = "intrng")]
    pub use crate::intr::IntrIsrcf::*;

    // FILTER_* macros
    #[doc(inline)]
    pub use crate::bus::Filter::*;

    #[doc(inline)]
    pub use crate::bus::wrappers::*;

    #[doc(inline)]
    pub use crate::device::wrappers::*;

    #[doc(inline)]
    #[cfg(feature = "intrng")]
    pub use crate::intr::wrappers::*;

    // M_* macros
    #[doc(inline)]
    pub use crate::malloc::wrappers::*;

    #[doc(inline)]
    #[cfg(feature = "fdt")]
    pub use crate::ofw::wrappers::*;

    #[doc(inline)]
    pub use crate::cell::wrappers::*;

    #[doc(inline)]
    pub use crate::sync::wrappers::*;

    #[doc(inline)]
    pub use crate::taskq::wrappers::*;

    pub use crate::device::DeviceIf;

    #[cfg(feature = "intrng")]
    pub use crate::intr::PicIf;

    #[doc(inline)]
    pub use crate::misc::*;

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
macro_rules! export_function {
    (
        $driver_ty:ident $impl:ident
        $fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*);
        $(with init glue { $($init_glue:tt)* })?
        $(with drop glue { $($drop_glue:tt)* })?
    ) => {
        #[allow(unused_mut)]
        #[no_mangle]
        pub unsafe extern "C" fn $impl($($arg_name: $arg,)*) {
            // Checks that this extern "C" function matches the declaration in bindings.h
            //const _TYPES_MATCH: unsafe extern "C" fn($($arg,)*) = $crate::bindings::$fn_name;

            // Convert all arguments from C types to rust types
            $(let mut $arg_name: _ = $arg_name.as_rust_type();)*

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
        $fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*) -> $ret:ty;
        $(with init glue { $($init_glue:tt)* })?
        $(with drop glue { $($drop_glue:tt)* })?
        $(rust returns $ret_as_rust_ty:ty)?
    ) => {
        #[allow(unused_mut)]
        #[no_mangle]
        pub unsafe extern "C" fn $impl($($arg_name: $arg,)*) -> $ret {
            // Checks that this extern "C" function matches the declaration in bindings.h
            //const _TYPES_MATCH: unsafe extern "C" fn($($arg,)*) -> $ret = $crate::bindings::$fn_name;

            // Convert all arguments from C types to rust types
            $(let mut $arg_name: _ = $arg_name.as_rust_type();)*

            // Call init glue if any
            $($($init_glue)*)*

            // Call the rust implementation, coercing to the result type if specified
            let res$(: $crate::Result<$ret_as_rust_ty>)* = $driver_ty::$fn_name($($arg_name,)*);

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
        $driver_ty:ident $impl:ident
        $fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*) -> $ret:ty;
        $(with init glue { $($init_glue:tt)* })?
        $(with drop glue { $($drop_glue:tt)* })?
        infallible
    ) => {
        #[allow(unused_mut)]
        #[no_mangle]
        pub unsafe extern "C" fn $impl($($arg_name: $arg,)*) -> $ret {
            // Checks that this extern "C" function matches the declaration in bindings.h
            //const _TYPES_MATCH: unsafe extern "C" fn($($arg,)*) -> $ret = $crate::bindings::$fn_name;

            // Convert all arguments from C types to rust types
            $(let mut $arg_name: _ = $arg_name.as_rust_type();)*

            // Call init glue if any
            $($($init_glue)*)*

            // Call the rust implementation, coercing to the result type if specified
            let res = $driver_ty::$fn_name($($arg_name,)*);

            // Call drop glue if any
            $($($drop_glue)*)*

            res
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
            $($fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*) $(-> $ret:ty)?;)*
        })?
    ) => {
        use $crate::prelude::*;

        // These structs are not defined in the KPI crate to allow inherent impls in drivers
        #[repr(C)]
        #[derive(Debug)]
        pub struct $driver_ty(core::cell::UnsafeCell<$crate::bindings::kobj_class>);
        unsafe impl Sync for $driver_ty {}
        impl $driver_ty {
            pub fn get_drop_fn() -> unsafe extern "C" fn(*mut core::ffi::c_void) {
                $driver_sym::drop_softc
            }
        }

        #[no_mangle]
        pub static $driver_sym: $driver_ty = $driver_ty(
            core::cell::UnsafeCell::new($crate::bindings::kobj_class {
                // TODO: Ensure type is &'static CStr
                name: $driver_name.as_ptr(),
                methods: core::ptr::addr_of!($methods).cast(),
                // TODO: ensure alignment of softc memory supports Softc
                size: core::mem::size_of::<$crate::device::Softc<<$driver_ty as DeviceIf>::Softc>>(),
                baseclasses: core::ptr::null_mut(),
                refs: 0,
                ops: core::ptr::null_mut(),
            })
        );

        #[no_mangle]
        static $methods: [$crate::device::DeviceMethod; $crate::count!($($impl)*) + 1] = [
            $(
                {
                    let desc = {
                        // Glob import everything in this let binding's scope to pick out the one we need
                        use $crate::bindings::*;
                        &raw mut ${concat($if_fn, _desc)}
                    };
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
            use core::ffi::c_void;
            use core::ptr::drop_in_place;

            pub unsafe extern "C" fn drop_softc(sc_void_ptr: *mut c_void) {
                let sc_ptr = sc_void_ptr.cast::<<$driver_ty as DeviceIf>::Softc>();
                drop_in_place(sc_ptr)
            }
            use super::$driver_ty;
            use $crate::bindings::*;
            use $crate::{AsRustType, AsCType};
            use $crate::device::DeviceIf;
            #[cfg(feature = "intrng")]
            use $crate::intr::PicIf;
            $($crate::$if_fn!($driver_ty $impl);)*
            $($($crate::export_function!($driver_ty $fn_name $fn_name($($arg_name: $arg,)*) $(-> $ret)*;);)*)*
        }
    };
}

macro_rules! err_codes {
    ($($name:ident, $desc:literal,)+) => {
        use core::ffi::c_int;
        use core::fmt;
        use core::fmt::{Debug, Display, Formatter};
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

        impl Display for ErrCode {
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

macro_rules! define_stub_syms {
    ($($sym:ident)*) => {
        $(
        #[no_mangle]
        pub extern "C" fn $sym() {
            panic!("uh oh")
        }
        )*
    };
}

define_stub_syms! {
    __floatundisf
    __mulsf3
    __divsf3
    __floatundidf
    __muldf3
    __divdf3
    __nesf2
    __ltsf2
    __gesf2
    __nedf2
    __ltdf2
    __gedf2
    __udivti3
}
