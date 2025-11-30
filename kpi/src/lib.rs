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
#![deny(
    improper_ctypes,
    unused_must_use,
    unreachable_patterns,
    unsafe_op_in_unsafe_fn
)]

//! This crate provides access to [FreeBSD kernel interfaces](https://man.freebsd.org/cgi/man.cgi).
//!
//! # Navigating the docs
//!
//! The suggested way to use these docs is to first check if the function you want to call is in
//! [`prelude`]. That module provides safe rust wrappers for the C KPI under similar names. The
//! wrapper argument and return types often differ to prevent mis-use. Searching unfamiliar types
//! will show if they have constructors or are returned by other functions. Arguments may also be
//! generic with [trait bounds](https://doc.rust-lang.org/rust-by-example/generics/bounds.html). The
//! trait documentation which will show types that implement them or how to implement them for
//! custom types.
//!
//! If a function is not available in [`prelude`], the C version can be accessed from the
//! [`bindings`] module. This can be helpful for prototyping rust in the kernel, but consider
//! adding a safe rust wrapper before proliferating the use of C functions across a codebase.

// This is only for userspace unit tests.
#[cfg(feature = "std")]
extern crate std;

// These modules must be first since the following modules use macros they define.
#[macro_use]
mod macros;
#[macro_use]
pub mod kobj;

/// Arch-specific functions for ARM64
#[cfg(target_arch = "aarch64")]
pub mod arm64;
/// Declarations for using C functions, types and variables directly
pub mod bindings;
/// The `Box` type for heap allocations
pub mod boxed;
/// Resource bus functions
pub mod bus;
/// sglist
pub mod collections;
/// Device driver and softc functions
pub mod device;
/// Contains a macro and trait for defining drivers
pub mod driver;
/// `CString`, `SubClass` and other FFI utilities
pub mod ffi;
/// Interrupt functions, config hook, callout and tsleep
pub mod intr;
/// Malloc flags and types
pub mod malloc;
/// Devicetree utilities
#[cfg(feature = "fdt")]
pub mod ofw;
#[cfg(not(feature = "std"))]
mod panic;
/// Synchronization primitives
pub mod sync;
/// Taskqueue functions
pub mod taskq;
// This module only exports macros
#[doc(hidden)]
pub mod tty;
/// The `Vec` type to manage a contiguous growable array on the heap
pub mod vec;

#[doc(hidden)]
#[macro_export]
macro_rules! define_stub_syms {
    ($($sym:ident)*) => {
        $(
            #[doc(hidden)]
            #[unsafe(no_mangle)]
            pub extern "C" fn $sym() {
                panic!("called panicking stub for {}", stringify!($sym))
            }
        )*
    };
}

#[cfg(test)]
mod tests;

/// A result where the `Err` is always a FreeBSD [`ErrCode`]
pub type Result<T> = core::result::Result<T, ErrCode>;

/// A catch-all module for miscellaneous functions
#[allow(non_snake_case)]
pub mod misc {
    #[cfg(target_arch = "aarch64")]
    use crate::bindings::device_t;
    #[cfg(target_arch = "aarch64")]
    use crate::prelude::*;
    #[cfg(target_arch = "aarch64")]
    use core::sync::atomic::AtomicU32;

    use crate::bindings;
    use crate::bindings::{cpuset_t, u_int};
    use core::ffi::c_int;

    pub const PAGE_SIZE: u64 = crate::bindings::PAGE_SIZE as u64;
    pub const BUS_SPACE_MAXADDR: u64 = crate::bindings::BUS_SPACE_MAXADDR as u64;

    pub fn cold() -> bool {
        // nonzero if we are doing a cold boot
        unsafe { bindings::cold != 0 }
    }

    pub fn hz() -> i32 {
        unsafe { bindings::hz }
    }

    pub fn DELAY(usec: u32) {
        unsafe { bindings::DELAY(usec as c_int) }
    }

    pub fn mp_maxid() -> usize {
        let val = unsafe { bindings::mp_maxid };
        val as usize
    }
    #[cfg(target_arch = "aarch64")]
    pub fn atomic_readandclear_32(p: &AtomicU32) -> u32 {
        unsafe { bindings::atomic_readandclear_32(p.as_ptr()) }
    }
    #[cfg(target_arch = "aarch64")]
    pub fn atomic_set_32(p: &AtomicU32, x: u32) {
        unsafe { bindings::atomic_set_32(p.as_ptr(), x) }
    }
    pub fn CPU_SET(cpu: u_int, set: *mut cpuset_t) {
        unsafe { bindings::rust_bindings_CPU_SET(cpu, set) }
    }
    pub fn CPU_ISSET(cpu: u_int, set: &cpuset_t) -> bool {
        unsafe { bindings::rust_bindings_CPU_ISSET(cpu, set as *const cpuset_t as *mut cpuset_t) }
    }
    #[cfg(target_arch = "aarch64")]
    pub fn gpiobus_add_bus(dev: device_t) -> Result<device_t> {
        let res = unsafe { bindings::gpiobus_add_bus(dev) };
        if res.0.is_null() {
            Err(ENULLPTR)
        } else {
            Ok(res)
        }
    }
}

/// Contains all rust wrappers for C KPI functions.
///
/// Also contains the KPI `Result`. Unlike the standard rust prelude this must be explicitly
/// imported with `use kpi::prelude::*`.
pub mod prelude {
    #[doc(inline)]
    #[cfg(target_arch = "aarch64")]
    pub use crate::arm64::wrappers::*;

    pub use crate::{Result, bindings};
    #[cfg(target_arch = "aarch64")]
    pub use crate::{
        curthread, isb, pcpu_get, pcpu_ptr, read_specialreg, rmb, wmb, write_specialreg,
    };
    #[cfg(not(feature = "std"))]
    pub use crate::{device_print, device_println, print, println};
    #[cfg(feature = "std")]
    pub use std::{print, println};

    #[doc(inline)]
    pub use crate::err_codes::*;

    #[doc(inline)]
    pub use crate::bus::dma::wrappers::*;
    #[doc(inline)]
    pub use crate::bus::wrappers::*;

    #[doc(inline)]
    pub use crate::collections::wrappers::*;

    #[doc(inline)]
    pub use crate::device::wrappers::*;

    #[doc(inline)]
    pub use crate::intr::wrappers::*;

    #[doc(inline)]
    pub use crate::malloc::wrappers::*;

    #[doc(inline)]
    #[cfg(feature = "fdt")]
    pub use crate::ofw::wrappers::*;

    #[doc(inline)]
    pub use crate::sync::mtx::wrappers::*;

    #[doc(inline)]
    pub use crate::taskq::wrappers::*;

    #[doc(inline)]
    pub use crate::misc::*;

    #[doc(inline)]
    pub use crate::collections::*;
}

macro_rules! err_codes {
    ($($name:ident, $desc:literal,)+) => {
        use core::ffi::c_int;
        use core::fmt;
        use core::fmt::{Debug, Display, Formatter};
        use core::num::NonZeroI32;
        use crate::kobj::AsCType;

        /// A FreeBSD error code.
        ///
        /// In addition to the standard error codes defined in C (`EPERM`, `ENXIO`, etc.) this also
        /// supports `ENULLPTR` for cases where C APIs report errors by returning null pointers and
        /// `EBADFFI` to represent cases where errors are caused by the FFI layer between C and
        /// rust. Keep in mind that rust code understands the two extra error codes, but not all C
        /// code may treat it as an error.
        #[derive(Copy, Clone, PartialEq, Eq)]
        pub struct ErrCode(pub(crate) NonZeroI32);

        #[doc(hidden)]
        pub mod err_codes {
            use core::num::NonZeroI32;
            use crate::{bindings, ErrCode};

            const _: () = {
                // out of an abundance of caution justify the new_unchecked below
                $(assert!(bindings::$name != 0);)*
                $(assert!(bindings::$name != i32::MIN);)*
                $(assert!(bindings::$name != i32::MIN + 1);)*
            };
            $(pub const $name: ErrCode = ErrCode(NonZeroI32::new(bindings::$name).unwrap());)*
            // TODO: make overlap checking less error-prone before adding more KPI crate errors
            pub const ENULLPTR: ErrCode = ErrCode(NonZeroI32::new(i32::MIN).unwrap());
            pub const EBADFFI: ErrCode = ErrCode(NonZeroI32::new(i32::MIN + 1).unwrap());
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
    __eqsf2
    __gnu_h2f_ieee
    __gnu_f2h_ieee

    // stubs for amd64 KERNCONF=MINIMAL
    sgx_encls
    __floatdisf
    __floatdidf
    __extendhfsf2
    __truncsfhf2
}
