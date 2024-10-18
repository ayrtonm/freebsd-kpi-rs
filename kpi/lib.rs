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
#![feature(allocator_api, concat_idents)]
#![deny(improper_ctypes, unused_must_use, unreachable_patterns)]
#![allow(dead_code, unused_imports, unused_variables)]

extern crate alloc;

#[macro_use]
mod macros;

#[macro_use]
pub mod bindings;

pub mod allocator;
pub mod arm64;
pub mod bus;
pub mod device;
pub mod intr;
pub mod ofw;
mod panic;
pub mod sync;
pub mod tty;
pub mod taskq;

use alloc::boxed::Box;
use alloc::collections::TryReserveError;
use core::alloc::Allocator;
use core::cell::UnsafeCell;
use core::ffi::c_int;
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::mem::{offset_of, MaybeUninit};
use core::ops::{Deref, DerefMut};
use core::ptr::{addr_of, addr_of_mut};
use crate::device::Device;
use crate::allocator::KernelAllocator;

pub trait AsCType<T> {
    fn as_c_type(self) -> T;
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

pub trait AsRustType<T> {
    fn as_rust_type(self) -> T;
}

#[macro_export]
macro_rules! count {
    ($x:ident) => {
        1
    };
    ($x:ident $($y:ident)*) => {
        1 + $crate::count!($($y)*)
    };
}

/// Rust's view of a variable of type T that has its address shared with C.
///
/// This opts out of Rust's requirements that &T must be immutable and T always have a valid value.
/// While creating a &mut FFICell<T> is perfectly fine, it may not be used to get a &mut T.
#[derive(Debug)]
#[repr(transparent)]
pub struct FFICell<T>(UnsafeCell<MaybeUninit<T>>);

impl<T> FFICell<T> {
    const fn zeroed() -> Self {
        Self(UnsafeCell::new(MaybeUninit::zeroed()))
    }

    fn get_out_ptr(&self) -> OutPtr<T> {
        unsafe {
            OutPtr::new(self.0.get().cast())
        }
    }
}

/// A struct containing a base class B and extra fields F.
///
/// The definition assumes that the base class is shared between Rust and C but places no
/// restriction on the extra fields so it may be possible to create references to them.
#[derive(Debug)]
pub struct SubClass<B, F> {
    base: FFICell<B>,
    pub sub: F,
}

impl<B, F> SubClass<B, F> {
    pub const fn new(sub: F) -> Self {
        Self {
            base: FFICell::zeroed(),
            sub,
        }
    }

    pub fn get_base_ptr(sub: &Self) -> *mut B {
        sub.base.get_out_ptr().as_ptr()
    }

    pub unsafe fn from_base<'a>(ptr: *mut B) -> &'a mut Self {
        let super_ptr = ptr.byte_sub(offset_of!(Self, base)).cast::<Self>();
        super_ptr.as_mut().unwrap()
    }
}

impl<B, F> Deref for SubClass<B, F> {
    type Target = F;

    fn deref(&self) -> &Self::Target {
        &self.sub
    }
}

impl<B, F> DerefMut for SubClass<B, F> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.sub
    }
}

// FreeBSD Rust KPI
//
// Rust requires unsafe blocks around pointer accesses because it does not enforce some aspects of
// their validity. Since this crate receives pointers from the C KPI we can assume some of these
// aspects hold and be precise about the aspects we can't assume as true. This allows code in
// downstream crates (i.e. drivers) to depend on the type system for checking the aspects of pointer
// validity.
//
// We assume pointers received from the C KPI are always:
// - non-null unless that is explicitly used to signal an error condition
// - points to a region of memory big enough to hold `T`
// - points to a region of memory with the proper alignment for `T`


/// A pointer received from the C KPI.
///
/// The memory it points to is not assumed to be initialized and may have other pointers.
#[derive(Debug)]
#[repr(transparent)]
pub struct OutPtr<T>(*mut T);

impl<T> Clone for OutPtr<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T> Copy for OutPtr<T> {}

impl<T> PointsTo<T> for OutPtr<T> {
    fn as_ptr(&self) -> *mut T {
        self.0
    }
}

impl<T> OutPtr<T> {
    // SAFETY: The caller must ensure `ptr` came directly from the C KPI to ensure the size and
    // alignment requirements of the pointee. The caller must also ensure that the C KPI this came
    // from does not use NULL as an error condition.
    pub unsafe fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }

    // SAFETY: The caller must synchronize this call with other pointers to `self.`
    pub unsafe fn write(&mut self, t: T) {
        *self.0 = t;
    }

    // SAFETY: The caller must ensure the pointee has a valid value for `T` before calling this.
    pub unsafe fn assume_init(self) -> Ptr<T> {
        Ptr(self.0)
    }

    #[doc(hidden)]
    pub fn get_field_helper<U, F>(self, f: F) -> OutPtr<U>
    where F: FnOnce(*mut T) -> *mut U {
        OutPtr(f(self.0))
    }
}

impl<T> OutPtr<MaybeUninit<T>> {
    // Cast away the MaybeUninit wrapper without assuming the pointee is initialized.
    pub fn flatten(self) -> OutPtr<T> {
        OutPtr(self.0.cast())
    }
}

/// A pointer received from the C KPI with the extra assumption that the pointee is initialized.
///
/// The pointee may have other pointers.
#[derive(Debug)]
#[repr(transparent)]
pub struct Ptr<T>(*mut T);

impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T> Copy for Ptr<T> {}

impl<T> PointsTo<T> for Ptr<T> {
    fn as_ptr(&self) -> *mut T {
        self.0
    }
}

impl<T> Ptr<T> {
    pub unsafe fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }

    pub unsafe fn write(&mut self, t: T) {
        *self.0 = t;
    }

    pub unsafe fn read(&self) -> T {
        core::ptr::read(self.0 as *const T)
    }

    pub unsafe fn allows_ref(self) -> Ref<T> {
        Ref(self.0)
    }

    #[doc(hidden)]
    pub fn get_field_helper<U, F>(self, f: F) -> Ptr<U>
    where F: FnOnce(*mut T) -> *mut U {
        Ptr(f(self.0))
    }
}

/// A pointer received from the C KPI which may be turned into a reference.
#[derive(Debug)]
#[repr(transparent)]
pub struct Ref<T>(*mut T);

impl<T> Ref<T> {
    pub unsafe fn assume_unique(self) -> RefMut<T> {
        RefMut(self.0)
    }

    #[doc(hidden)]
    pub fn get_field_helper<U, F>(self, f: F) -> Ref<U>
    where F: FnOnce(&T) -> &U {
        let new_ptr = f(self.deref()) as *const U;
        Ref(new_ptr as *mut U)
    }

    // just for documentation
    pub fn map_ref<U, F>(self, f: F) -> Ref<U>
    where F: FnOnce(&T) -> &U {
        self.get_field_helper(f)
    }
}

impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T> Copy for Ref<T> {}

impl<T> PointsTo<T> for Ref<T> {
    fn as_ptr(&self) -> *mut T {
        self.0
    }
}

impl<T> Deref for Ref<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            self.0.as_ref().unwrap()
        }
    }
}

/// A pointer received from the C KPI which may be turned into a mutable reference.
#[derive(Debug)]
#[repr(transparent)]
pub struct RefMut<T>(*mut T);

impl<T> RefMut<T> {
    pub fn share(self) -> Ref<T> {
        Ref(self.0)
    }

    #[doc(hidden)]
    pub fn get_field_helper<U, F>(&mut self, f: F) -> RefMut<U>
    where F: FnOnce(*mut T) -> *mut U {
        RefMut(f(self.0))
    }
}

impl<T> PointsTo<T> for RefMut<T> {
    fn as_ptr(&self) -> *mut T {
        self.0
    }
}

impl<T> Deref for RefMut<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            self.0.as_ref().unwrap()
        }
    }
}

impl<T> DerefMut for RefMut<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            self.0.as_mut().unwrap()
        }
    }
}

pub trait PointsTo<T> {
    fn as_ptr(&self) -> *mut T;
}

#[doc(hidden)]
pub struct CSoftc<T> {
    // This is intentionally not an option<T> to allow checking its state independently of accessing
    // the softc.
    softc: MaybeUninit<T>,
    state: State,
    initialized: bool,
}

pub trait GetSoftc<SC> {
    // helper method to avoid specifying Device::get_softc generic paramters each time
    #[doc(hidden)]
    fn get_softc_ptr(&self, mut dev: Device) -> OutPtr<CSoftc<SC>> {
        dev.get_softc::<CSoftc<SC>>()
    }

    #[doc(hidden)]
    fn get_softc_state(&self, dev: Device) -> Ptr<State> {
        let csc = self.get_softc_ptr(dev);
        let state = get_field!(csc, state);
        // SAFETY: The C KPI initializes memory to zero so the `state` field starts off with a valid
        // value. All assignments are done in Rust to valid variants of the `State` enum.
        unsafe {
            state.assume_init()
        }
    }

    #[doc(hidden)]
    fn is_softc_init(&self, dev: Device) -> Result<()> {
        let csc = self.get_softc_ptr(dev);
        // SAFETY: The C KPI initializes memory to zero so the `initialized` field starts off with a
        // valid value. All assignments are done in Rust to valid values for `bool`.
        let init = unsafe { get_field!(csc, initialized).assume_init() };
        if unsafe { init.read() } {
            Ok(())
        } else {
            Err(EDOOFUS)
        }
    }

    fn init_softc(&self, dev: Device, init_val: SC) -> Result<()> {
        let csc = self.get_softc_ptr(dev);
        let state = self.get_softc_state(dev);

        if unsafe { state.read() } != State::Available {
            return Err(EDOOFUS);
        }
        unsafe {
            get_field!(csc, initialized).write(true);
        }

        let mut sc = get_field!(csc, softc);
        unsafe {
            sc.write(MaybeUninit::new(init_val));
        }
        Ok(())
    }

    fn claim_softc(&self, dev: Device) -> Result<RefMut<SC>> {
        let csc = self.get_softc_ptr(dev);
        let mut state = self.get_softc_state(dev);

        if unsafe { state.read() } != State::Available {
            return Err(EDOOFUS);
        }
        self.is_softc_init(dev)?;
        unsafe {
            state.write(State::Claimed);
        }
        let sc = get_field!(csc, softc);
        Ok(unsafe { sc.flatten().assume_init().allows_ref().assume_unique() })
    }

    fn release_softc(&self, dev: Device, _sc: RefMut<SC>) {
        let mut state = self.get_softc_state(dev);
        unsafe {
            state.write(State::Available)
        }
    }

    fn share_softc(&self, dev: Device) -> Result<Ref<SC>> {
        let csc = self.get_softc_ptr(dev);
        let mut state = self.get_softc_state(dev);

        if unsafe { state.read() } == State::Claimed {
            return Err(EDOOFUS);
        }
        self.is_softc_init(dev)?;
        unsafe {
            state.write(State::Shared);
        }
        let sc = get_field!(csc, softc);
        Ok(unsafe { sc.flatten().assume_init().allows_ref() })
    }
}

#[repr(u32)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum State {
    // softc is zero-initialized so this ensures it starts in the correct state
    Available = 0,
    Claimed,
    Shared,
}

// this only needs to define the two statics in the consumer (driver) crate. Driver is also good to
// define in the consumer to avoid orphan rule issues around implementing the interface traits i.e.
// impl Driver for DeviceIf
#[macro_export]
macro_rules! driver {
    ($cdriver:ident, $cname:expr, $methods:ident, $sc:ty,
        $($if_fn:ident $impl:ident $(,)?)*
    ) => {
        use $crate::prelude::*;

        #[repr(C)]
        pub struct Driver(core::cell::UnsafeCell<$crate::bindings::kobj_class>);
        unsafe impl Sync for Driver {}
        impl $crate::GetSoftc<$sc> for Driver {}

        #[no_mangle]
        pub static $cdriver: Driver = Driver(
            core::cell::UnsafeCell::new($crate::bindings::kobj_class {
                name: $cname.as_ptr(),
                methods: core::ptr::addr_of!($methods).cast(),
                size: core::mem::size_of::<$crate::CSoftc<$sc>>(),
                baseclasses: core::ptr::null_mut(),
                refs: 0,
                ops: core::ptr::null_mut(),
            })
        );

        #[repr(C)]
        #[derive(Debug)]
        pub struct DeviceMethod($crate::bindings::kobj_method_t);

        impl DeviceMethod {
            pub const fn null() -> Self {
                Self($crate::bindings::kobj_method_t {
                    desc: core::ptr::null_mut(),
                    func: None,
                })
            }
        }

        unsafe impl Sync for DeviceMethod { }

        #[no_mangle]
        static $methods: [DeviceMethod; $crate::count!($($impl)*) + 1] = [
            $(
                {
                    let desc = {
                        use $crate::bindings::*;
                        core::ptr::addr_of_mut!(concat_idents!($if_fn, _desc))
                    };
                    let func = Some(unsafe {
                        core::mem::transmute($impl as *const ())
                    });
                    DeviceMethod($crate::bindings::kobj_method_t { desc, func })
                },
            )*
            DeviceMethod::null(),
        ];

        $($crate::$if_fn!($impl, $cdriver);)*
    };
}

pub type Result<T> = core::result::Result<T, ErrCode>;

pub mod prelude {
    use core::ffi::c_void;
    use core::ptr::null_mut;

    pub use crate::allocator::{NOWAIT, WAITOK};
    pub use crate::bindings;
    pub use crate::bus::SysRes::*;
    pub use crate::device::ProbeRes::*;
    pub use crate::intr::FilterRes::*;
    pub use crate::intr::IntrRoot::*;
    pub use crate::ErrCode;
    pub use crate::err_codes::*;
    pub use crate::{dprint, dprintln, print, println, Result};

    pub use crate::GetSoftc;
}

macro_rules! err_codes {
    ($($name:ident, $desc:literal,)+) => {
        use core::num::NonZeroI32;
        use crate::err_codes::*;

        // This is effectively an extensible enum with some overlapping variants
        #[derive(Copy, Clone, PartialEq, Eq)]
        pub struct ErrCode(pub(crate) NonZeroI32);
        pub mod err_codes {
            use core::num::NonZeroI32;
            use crate::{bindings, ErrCode};
            const _: () = {
                $(assert!(bindings::$name != i32::MIN);)*
                $(assert!(bindings::$name != i32::MIN + 1);)*
            };
            $(pub const $name: ErrCode = ErrCode(unsafe { NonZeroI32::new_unchecked(bindings::$name) });)*
            pub const ENULLPTR: ErrCode = ErrCode(unsafe { NonZeroI32::new_unchecked(i32::MIN) });
            pub const EBADFFI: ErrCode = ErrCode(unsafe { NonZeroI32::new_unchecked(i32::MIN + 1) });
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
                    // Map ENULLPTR to itself in case an error code keeps getting round-tripped
                    i32::MIN => ENULLPTR,
                    // Map anything else to EBADFFI
                    _ => EBADFFI,
                }
            }
        }

        impl From<TryReserveError> for ErrCode {
            // TODO: this is a lossy conversion, maybe add a new rust error code?
            fn from(_: TryReserveError) -> Self {
                ENOMEM
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
