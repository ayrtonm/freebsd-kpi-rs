/*-
 * spdx-license-identifier: bsd-2-clause
 *
 * copyright (c) 2024 ayrton muñoz
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

#![no_std]
#![deny(improper_ctypes, unused_must_use, unreachable_patterns)]
#![feature(macro_metavar_expr_concat)]

//! This crate provides access to [FreeBSD kernel interfaces](https://man.freebsd.org/cgi/man.cgi).
//!
//! It exposes similar interfaces to the C equivalents with differences mainly to provide ergonomic
//! or safety benefits. This crate only relies on the rust
//! [`core` library](https://doc.rust-lang.org/core/) and will not allocate using
//! [`malloc(9)`](https://man.freebsd.org/cgi/man.cgi?malloc(9)) unless
//! [`MallocFlags`][malloc::MallocFlags] are explicitly passed in. It does not rely on any 3rd-party
//! crates (e.g. [`crates.io`](<https://crates.io>)).
//!
//! The **main goal** is to let developers use mostly
//! [safe rust](https://doc.rust-lang.org/nomicon/what-unsafe-does.html) to define clear
//! [ownership semantics](https://doc.rust-lang.org/nomicon/ownership.html) and avoid
//! [data races](https://doc.rust-lang.org/nomicon/races.html). This should minimize the chance of
//! mis-using interfaces so developers can focus on implementing the functionality they actually
//! care about.
//!
//! # Navigating the docs
//!
//! The suggested way to use these docs to call a given function is to first check if it's in
//! [`prelude`]. That module provides safe rust wrappers for C functions under similar names. The
//! wrapper argument types often differ to improve ergonomics and prevent mis-use. Searching
//! unfamiliar argument types will show if they have constructors or are returned by other
//! functions. Arguments may also require
//! [trait bounds](https://doc.rust-lang.org/rust-by-example/generics/bounds.html) which will
//! document how to construct them.
//!
//! For example, [`device_probe`](https://man.freebsd.org/cgi/man.cgi?query=DEVICE_PROBE) may need
//! to check if a `device_t`'s devicetree `"compatible"` string matches a driver using
//! [`ofw_bus_search_compatible`](https://man.freebsd.org/cgi/man.cgi?query=ofw_bus_search_compatible).
//! The rust [`ofw_bus_search_compatible`][prelude::ofw_bus_search_compatible] wrapper is exported
//! by both [`ofw`] (where it's defined) and [`prelude`] (avoiding the need to explicitly import
//! every function used). Rather than use `struct ofw_compat_data` pointers, the rust wrapper takes
//! an [`OfwCompatData`][crate::ofw::OfwCompatData] reference and returns a `&'static T` to the
//! field the caller cares about. The developer can then create an
//! [`OfwCompatData`][crate::ofw::OfwCompatData] using
//! [`OfwCompatData::new`][crate::ofw::OfwCompatData::new], call
//! [`ofw_bus_search_compatible`][prelude::ofw_bus_search_compatible] and get back a reference to
//! the entry with a matching `"compatible"` string. Unless otherwise specified this crate will
//! ensure that lifetime and memory layouts are constrained as necessary.
//!
//! If a function is not available in [`prelude`], the C version can be accessed from the
//! [`bindings`] module. This can be useful for prototyping rust in different areas of the kernel,
//! but consider creating safe wrappers before proliferating the use of the C functions across a
//! codebase.
//!
//! # Driver development
//!
//! This crate's initial purpose was for developing kernel drivers. These docs do not intend to be
//! an exhaustive guide on writing drivers, but the most relevant points are:
//!
//! - the [`driver!`] macro for defining the driver kobj class and method table
//! - the [`DeviceIf`][device::DeviceIf] trait to define the driver softc
//! - the [`AsCType`]/[`AsRustType`] traits to make the driver's exported functions ergonomic
//!
//! [`driver!`] is used to define the driver kobj class and its method table (i.e. the equivalent of
//! the `DEFINE_CLASS` macro and `device_method_t` in C). This crate does not define an equivalent
//! of the `DRIVER_MODULE`/`EARLY_DRIVER_MODULE` C macros so they must be invoked in .c files for
//! each rust driver.
//!
//! # Other use-cases
//!
//! Nothing precludes using this crate for things other than drivers in the kernel. Defining a
//! useful boundary between rust and C is very application-specific, but in general the starting
//! point is defining an
//! [`extern "C"`](https://doc.rust-lang.org/nomicon/ffi.html#calling-rust-code-from-c) function to
//! call rust from C.

// This is only for userspace unit tests.
#[cfg(feature = "std")]
extern crate std;

#[macro_use]
mod macros;

/// Arch-specific functions for ARM64
#[cfg(target_arch = "aarch64")]
pub mod arm64;
/// Declarations for using C functions, types and variables directly.
pub mod bindings;
pub mod boxed;
/// Resource bus functions
pub mod bus;
/// Device driver and softc functions
pub mod device;
#[doc(hidden)]
pub mod driver;
pub mod ffi;
mod interfaces;
/// Interrupt functions and config hook
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
pub mod vec;

macro_rules! define_stub_syms {
    ($($sym:ident)*) => {
        $(
            #[doc(hidden)]
            #[unsafe(no_mangle)]
            pub extern "C" fn $sym() {
                panic!("uh oh")
            }
        )*
    };
}

#[cfg(test)]
mod tests;

/// A result where the `Err` is always a FreeBSD [`ErrCode`]
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

/// A catch-all module for miscellaneous functions
#[allow(non_snake_case)]
pub mod misc {
    use crate::bindings::{cpuset_t, device_t, u_int};
    use crate::prelude::*;
    use crate::sync::Mutable;
    use crate::{ErrCode, bindings};
    use core::ffi::{CStr, c_int, c_void};
    use core::ptr::null_mut;
    use core::sync::atomic::{AtomicU8, AtomicU16, AtomicU32, AtomicU64};

    pub use bindings::{BUS_SPACE_MAXADDR, PAGE_SIZE};

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
    pub fn atomic_readandclear_32(p: &AtomicU32) -> u32 {
        unsafe { bindings::atomic_readandclear_32(p.as_ptr()) }
    }
    pub fn atomic_set_32(p: &AtomicU32, x: u32) {
        unsafe { bindings::atomic_set_32(p.as_ptr(), x) }
    }
    pub fn CPU_SET(cpu: u_int, set: *mut cpuset_t) {
        unsafe { bindings::rust_bindings_CPU_SET(cpu, set) }
    }
    pub fn CPU_ISSET(cpu: u_int, set: &cpuset_t) -> bool {
        unsafe { bindings::rust_bindings_CPU_ISSET(cpu, set as *const cpuset_t as *mut cpuset_t) }
    }
    pub fn gpiobus_add_bus(dev: device_t) -> Result<device_t> {
        let res = unsafe { bindings::gpiobus_add_bus(dev) };
        if res.0.is_null() {
            Err(ENULLPTR)
        } else {
            Ok(res)
        }
    }

    pub trait Sleepable {
        fn as_ptr(&self) -> *mut c_void;
    }

    impl<T> Sleepable for Mutable<T> {
        fn as_ptr(&self) -> *mut c_void {
            self.as_ptr().cast::<c_void>()
        }
    }

    impl Sleepable for AtomicU8 {
        fn as_ptr(&self) -> *mut c_void {
            self.as_ptr().cast::<c_void>()
        }
    }

    impl Sleepable for AtomicU16 {
        fn as_ptr(&self) -> *mut c_void {
            self.as_ptr().cast::<c_void>()
        }
    }

    impl Sleepable for AtomicU32 {
        fn as_ptr(&self) -> *mut c_void {
            self.as_ptr().cast::<c_void>()
        }
    }

    impl Sleepable for AtomicU64 {
        fn as_ptr(&self) -> *mut c_void {
            self.as_ptr().cast::<c_void>()
        }
    }

    pub fn tsleep<T: Sleepable>(chan: &T, priority: i32, wmesg: &CStr, timo: i32) -> Result<()> {
        let chan_ptr = chan.as_ptr();
        let wmesg_ptr = wmesg.as_ptr();
        let res = unsafe {
            bindings::_sleep(
                chan_ptr,
                null_mut(),
                priority,
                wmesg_ptr,
                bindings::tick_sbt * timo as i64,
                0,
                bindings::C_HARDCLOCK,
            )
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn wakeup<T: Sleepable>(chan: &T) {
        let chan_ptr = chan.as_ptr();
        unsafe { bindings::wakeup(chan_ptr) }
    }
}

/// Glob imports commonly-used items.
///
/// This must be explicitly imported with `use kpi::prelude::*`.
pub mod prelude {
    #[doc(inline)]
    #[cfg(target_arch = "aarch64")]
    pub use crate::arm64::*;

    pub use crate::{Result, base, bindings, device_get_softc, project};
    #[cfg(target_arch = "aarch64")]
    pub use crate::{
        curthread, isb, pcpu_get, pcpu_ptr, read_specialreg, rmb, wmb, write_specialreg,
    };
    #[cfg(not(feature = "std"))]
    pub use crate::{device_print, device_println, print, println};
    #[cfg(feature = "std")]
    pub use std::{print, println};

    // Error code macros
    #[doc(inline)]
    pub use crate::err_codes::*;

    #[doc(inline)]
    pub use crate::bus::dma::wrappers::*;
    #[doc(inline)]
    pub use crate::bus::wrappers::*;

    #[doc(inline)]
    pub use crate::device::wrappers::*;

    #[doc(inline)]
    pub use crate::intr::wrappers::*;

    // M_* macros
    #[doc(inline)]
    pub use crate::malloc::wrappers::*;

    #[doc(inline)]
    #[cfg(feature = "fdt")]
    pub use crate::ofw::wrappers::*;

    #[doc(inline)]
    pub use crate::sync::mtx::wrappers::*;

    #[doc(inline)]
    pub use crate::taskq::wrappers::*;

    pub use crate::device::DeviceIf;
    pub use crate::ffi::{RefCounted, Uninit};

    #[cfg(feature = "intrng")]
    pub use crate::intr::PicIf;

    #[doc(inline)]
    pub use crate::misc::*;

    // These are implemented widely throughout the KPI crate
    pub use crate::{AsCType, AsRustType};
}

#[doc(hidden)]
#[macro_export]
macro_rules! export_function {
    (
        $driver_ty:ident $impl:ident
        $fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*);
        $(with init glue { $($init_glue:tt)* })?
        $(with drop glue { $($drop_glue:tt)* })?
        $(with prefix args { $($prefix_args:ident)* })?
    ) => {
        #[allow(unused_mut)]
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $impl($($arg_name: $arg,)*) {
            // Checks that this extern "C" function matches the declaration in bindings.h
            //const _TYPES_MATCH: unsafe extern "C" fn($($arg,)*) = $crate::bindings::$fn_name;

            // Convert all arguments from C types to rust types
            $(let mut $arg_name: _ = $arg_name.as_rust_type();)*

            // Call init glue if any
            $($($init_glue)*)*;

            // Call the rust implementation
            $driver_ty::$fn_name($($($prefix_args,)*)* $($arg_name,)*);

            // Call drop glue if any
            $($($drop_glue)*)*;
        }
    };
    (
        $driver_ty:ident $impl:ident
        $fn_name:ident($($arg_name:ident: $arg:ty$(,)?)*) -> $ret:ty;
        $(with init glue { $($init_glue:tt)* })?
        $(with drop glue { $($drop_glue:tt)* })?
        $(with prefix args { $($prefix_args:ident)* })?
        $(rust returns $ret_as_rust_ty:ty)?
    ) => {
        #[allow(unused_mut)]
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $impl($($arg_name: $arg,)*) -> $ret {
            // Checks that this extern "C" function matches the declaration in bindings.h
            //const _TYPES_MATCH: unsafe extern "C" fn($($arg,)*) -> $ret = $crate::bindings::$fn_name;

            // Convert all arguments from C types to rust types
            $(let mut $arg_name: _ = $arg_name.as_rust_type();)*

            // Call init glue if any
            $($($init_glue)*)*

            // Call the rust implementation, coercing to the result type if specified
            let res$(: $crate::Result<$ret_as_rust_ty>)* = $driver_ty::$fn_name($($($prefix_args,)*)* $($arg_name,)*);

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
        $(with prefix args { $($prefix_args:ident)* })?
        infallible
    ) => {
        #[allow(unused_mut)]
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $impl($($arg_name: $arg,)*) -> $ret {
            // Checks that this extern "C" function matches the declaration in bindings.h
            //const _TYPES_MATCH: unsafe extern "C" fn($($arg,)*) -> $ret = $crate::bindings::$fn_name;

            // Convert all arguments from C types to rust types
            $(let mut $arg_name: _ = $arg_name.as_rust_type();)*

            // Call init glue if any
            $($($init_glue)*)*

            // Call the rust implementation, coercing to the result type if specified
            let res = $driver_ty::$fn_name($($($prefix_args,)*)* $($arg_name,)*);

            // Call drop glue if any
            $($($drop_glue)*)*

            res
        }
    };
}

macro_rules! err_codes {
    ($($name:ident, $desc:literal,)+) => {
        use core::ffi::c_int;
        use core::fmt;
        use core::fmt::{Debug, Display, Formatter};
        use core::num::NonZeroI32;

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
}
