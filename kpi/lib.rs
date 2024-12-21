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
#![feature(allocator_api, concat_idents, associated_type_defaults, negative_impls)]
#![deny(improper_ctypes, unused_must_use, unreachable_patterns)]

extern crate alloc;

#[macro_use]
mod macros;

#[macro_use]
pub mod bindings;

pub mod allocator;
pub mod arm64;
pub mod bus;
pub mod cell;
pub mod device;
pub mod intr;
pub mod ofw;
mod panic;
pub mod sleep;
pub mod sync;
pub mod taskq;
pub mod tty;

use crate::kpi_prelude::*;
use alloc::collections::TryReserveError;
use core::ffi::c_int;
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::mem::MaybeUninit;

#[macro_export]
macro_rules! count {
    ($x:ident) => {
        1
    };
    ($x:ident $($y:ident)*) => {
        1 + $crate::count!($($y)*)
    };
}

#[macro_export]
macro_rules! export_function {
    ($cdriver:ident device_probe $impl:ident) => {
        $crate::export_function! {
            $cdriver int $impl(device_t dev)
        }
    };
    ($cdriver:ident device_attach $impl:ident) => {
        $crate::export_function! {
            $cdriver int $impl(device_t dev)
        }
    };
    ($cdriver:ident device_detach $impl:ident) => {
        $crate::export_function! {
            $cdriver int $impl(device_t dev)
            {
                use $crate::device::DriverIf;
                let sc = $cdriver.get_softc_as_mut(dev);
                unsafe { core::ptr::drop_in_place(sc) }
            }
        }
    };
    (
    $cdriver:ident
    $ret:ident $fn_name:ident($($arg:ident $arg_name:ident$(,)?)*)
    $({ $($drop_glue:tt)* })?
    ) => {
        // TODO: type check
        //const _TYPE_CHECK: unsafe extern "C" fn($($arg,)*) -> $ret = $crate::bindings::$fn_name;
        #[no_mangle]
        pub unsafe extern "C" fn $fn_name($($arg_name: $arg,)*) -> $ret {
            $(let mut $arg_name = $arg_name.as_rust_type();)*
            let res = $cdriver.$fn_name($($arg_name,)*);
            $($($drop_glue)*)*;
            match res {
                Ok(r) => r.as_c_type(),
                Err(e) => e.as_c_type(),
            }
        }
    };
}

// this only needs to define the two statics in the consumer (driver) crate. Driver is also good to
// define in the consumer to avoid orphan rule issues around implementing the interface traits i.e.
// impl Driver for DeviceIf
#[macro_export]
macro_rules! driver {
    //(@device_detach) => {
    //    compile_error!("needs special glue")
    //};
    ($cdriver:ident, $cname:expr, $driver:ident, $methods:ident,
        $($if_fn:ident $impl:ident,)*
        $(
        {
            $($ret:ident $fn_name:ident($($arg:ident $arg_name:ident$(,)?)*);)*
        }
        )?
    ) => {
        // Using this macro pulls in KPI prelude for convenience
        use $crate::prelude::*;

        // These structs are not defined in the KPI crate to allow inherent impls in drivers
        #[repr(C)]
        #[derive(Debug)]
        pub struct $driver(core::cell::UnsafeCell<$crate::bindings::kobj_class>);
        unsafe impl Sync for $driver {}

        #[no_mangle]
        pub static $cdriver: $driver = $driver(
            core::cell::UnsafeCell::new($crate::bindings::kobj_class {
                name: $cname.as_ptr(),
                methods: core::ptr::addr_of!($methods).cast(),
                // TODO: ensure alignment of softc memory supports Softc
                size: core::mem::size_of::<<$driver as DriverIf>::Softc>(),
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
                        use $crate::bindings::*;
                        &raw mut concat_idents!($if_fn, _desc)
                    };
                    $crate::device::DeviceMethod::new(desc, $cdriver::$impl as *const ())
                },
            )*
            $crate::device::DeviceMethod::null(),
        ];

        mod $cdriver {
            use super::{$cdriver, $driver};
            use $crate::bindings::*;
            use $crate::{AsRustType, AsCType};
            use $crate::export_function;
            $(export_function!($cdriver $if_fn $impl);)*

            $(
                $(
                    #[no_mangle]
                    unsafe extern "C" fn $fn_name($($arg_name: $arg,)*) -> $ret {

                        const _TYPE_CHECK: unsafe extern "C" fn($($arg,)*) -> $ret = $crate::bindings::$fn_name;

                        $(let mut $arg_name = $arg_name.as_rust_type();)*
                        let res = $cdriver.$fn_name($($arg_name,)*);
                        match res {
                            Ok(r) => r.as_c_type(),
                            Err(e) => e.as_c_type(),
                        }
                    }
                )*
            )*
        }
    };
}

pub type Result<T> = core::result::Result<T, ErrCode>;
// Making `A = KernelAllocator` a param gives nicer errors than just aliasing to
// `Box<T, KernelAllocator>` when the global allocator is used in unexpected places
pub type Box<T, A = KernelAllocator> = alloc::boxed::Box<T, A>;

pub trait AsCType<T> {
    fn as_c_type(self) -> T;
}

pub trait AsRustType<T> {
    fn as_rust_type(self) -> T;
}

impl<T> AsRustType<T> for T {
    fn as_rust_type(self) -> T { self }
}

// Internal prelude module for commonly used imports in the KPI crate
mod kpi_prelude {
    pub use crate::allocator::KernelAllocator;
    pub use crate::bindings;
    pub use crate::cell::{FFICell, SubClass, UniqueCell};
    pub use crate::err_codes::*;
    pub use crate::println;
    pub use crate::{AsCType, AsRustType, Box, ErrCode, Result};

    pub use crate::bus::BusIfWrappers;
    pub use crate::device::{DriverIf};
    pub use crate::sync::{Mutex, SpinLock};
}

pub mod prelude {
    pub use crate::kpi_prelude::*;

    pub use crate::allocator::{NOWAIT, WAITOK};
    pub use crate::bus::wrappers::*;
    pub use crate::bus::SysRes::*;
    pub use crate::device::wrappers::*;
    pub use crate::device::AttachRes;
    pub use crate::device::ProbeRes::*;
    pub use crate::device::{Device};
    pub use crate::intr::FilterRes::*;
    pub use crate::intr::IntrRoot::*;
    pub use crate::ofw::wrappers::*;
    pub use crate::{dprint, dprintln, print, println};

    pub use crate::sleep::Sleepable;
}

macro_rules! err_codes {
    ($($name:ident, $desc:literal,)+) => {
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
                    $(bindings::$name => $name,)*
                    // Map ENULLPTR to itself in case an error code gets round-tripped
                    i32::MIN => err_codes::ENULLPTR,
                    // Map anything else to EBADFFI
                    _ => err_codes::EBADFFI,
                }
            }
        }

        impl From<TryReserveError> for ErrCode {
            // TODO: this is a lossy conversion, maybe add a new rust error code?
            fn from(_: TryReserveError) -> Self {
                err_codes::ENOMEM
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
