#![no_std]
#![feature(allocator_api, concat_idents)]
#![deny(improper_ctypes, unused_must_use, unreachable_patterns)]
#![allow(dead_code, unused_imports, unused_variables)]

extern crate alloc;

#[macro_use]
mod macros;
pub mod allocator;
pub mod arm64;
pub mod bindings;
pub mod bus;
pub mod device;
pub mod intr;
pub mod ofw;
mod panic;
pub mod taskq;
pub mod tty;

use alloc::collections::TryReserveError;
use core::cell::UnsafeCell;
use core::ffi::c_int;
use core::fmt;
use core::fmt::{Debug, Formatter};
use core::mem::{offset_of, MaybeUninit};
use core::ops::{Deref, DerefMut};
use core::ptr::{addr_of, addr_of_mut};

pub trait AsCType {
    fn as_c_int(self) -> c_int;
}

impl AsCType for () {
    fn as_c_int(self) -> c_int {
        0
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

/// Rust's view of a variable of type T defined in C.
///
/// This opts out of Rust's requirements that &T must be immutable and T always have a valid value.
/// Creating &mut T to these variables is not allowed.
#[derive(Debug)]
#[repr(transparent)]
pub struct SharedValue<T>(UnsafeCell<MaybeUninit<T>>);

impl<T> SharedValue<T> {
    const fn zeroed() -> Self {
        Self(UnsafeCell::new(MaybeUninit::zeroed()))
    }
    fn as_ptr(&self) -> *mut T {
        self.0.get().cast()
    }
}

/// A struct containing a base class B and extra fields F.
///
/// The definition assumes that the base class is shared between Rust and C but places no
/// restriction on the extra fields so it may be possible to create references to them.
#[derive(Debug)]
pub struct SuperClass<B, F> {
    base: SharedValue<B>,
    fields: F,
}

impl<B, F> SuperClass<B, F> {
    pub const fn new(fields: F) -> Self {
        Self {
            base: SharedValue::zeroed(),
            fields,
        }
    }

    pub fn base_ptr(&self) -> *mut B {
        addr_of!(self.base).cast::<*const B>() as *mut B
    }

    pub unsafe fn from_base<'a>(ptr: *mut B) -> &'a Self {
        let super_ptr = ptr.byte_sub(offset_of!(Self, base)).cast::<Self>();
        super_ptr.as_ref().unwrap()
    }

    pub fn fields(&self) -> &F {
        &self.fields
    }
}

/// A pointer received from the C KPI.
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
    pub fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }

    pub unsafe fn can_ref(self) -> Ref<T> {
        Ref(self.0)
    }
}

/// A pointer received from the C KPI which may be turned into a reference.
#[derive(Debug)]
#[repr(transparent)]
pub struct Ref<T>(*mut T);

impl<T> Ref<T> {
    pub unsafe fn is_unique(self) -> UniqRef<T> {
        UniqRef(self.0)
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
pub struct UniqRef<T>(*mut T);

impl<T> UniqRef<T> {
    pub fn share(self) -> Ref<T> {
        Ref(self.0)
    }
}

impl<T> PointsTo<T> for UniqRef<T> {
    fn as_ptr(&self) -> *mut T {
        self.0
    }
}

impl<T> Deref for UniqRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            self.0.as_ref().unwrap()
        }
    }
}

impl<T> DerefMut for UniqRef<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            self.0.as_mut().unwrap()
        }
    }
}

pub trait PointsTo<T> {
    fn as_ptr(&self) -> *mut T;
}

#[repr(u32)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum State {
    Available = 0, /* softc is zero-initialized */
    Claimed,
    Shared,
}

// this only needs to define the two statics in the consumer (driver) crate. Driver is also good to
// define in the consumer to avoid orphan rule issues around implementing the interface traits i.e.
// impl Driver for DeviceIf
#[macro_export]
macro_rules! driver {
    ($cdriver:ident, $cname:expr, $methods:ident, $sc:ty,
        $($if_name:ident $if_fn:ident $impl:ident $(,)?)*
    ) => {
        use $crate::prelude::*;

        #[repr(C)]
        pub struct CDriver {
            class: core::cell::UnsafeCell<$crate::bindings::kobj_class>, /* must be first */
            pub driver: Driver,
        }

        #[repr(C)]
        #[derive(Debug)]
        pub struct Driver(());

        impl Driver {
            pub fn softc_init(&self, mut dev: $crate::device::Device, sc_init: $sc) -> $crate::Result<()> {
                use core::mem::{MaybeUninit, offset_of};
                use $crate::State;

                let csc_ptr = dev.get_softc::<CSoftc>();

                // The softc should be in the available state
                if unsafe { (*csc_ptr).state } != State::Available {
                    return Err(EPERM);
                }
                // The softc can only be initialized once
                if unsafe { (*csc_ptr).initialized } {
                    return Err(EPERM);
                }
                // Mark the softc as initialized
                unsafe {
                    (*csc_ptr).initialized = true;
                }
                let sc_ptr = unsafe { csc_ptr.byte_add(offset_of!(CSoftc, sc)).cast::<MaybeUninit<$sc>>() };
                let mut sc_ref = unsafe {
                    $crate::Ptr::new(sc_ptr).can_ref().is_unique()
                };
                sc_ref.write(sc_init);
                Ok(())
            }

            pub fn softc_claim(&self, mut dev: $crate::device::Device) -> $crate::Result<$crate::UniqRef<$sc>> {
                use core::mem::offset_of;
                use $crate::State;

                let csc_ptr = dev.get_softc::<CSoftc>();

                // The softc must be in the available state
                if unsafe { (*csc_ptr).state } != State::Available {
                    return Err(EPERM);
                }
                // The softc must have been initialized
                if unsafe { !(*csc_ptr).initialized } {
                    return Err(EPERM);
                }
                // Mark the softc as claimed
                unsafe {
                    (*csc_ptr).state = State::Claimed;
                }
                let sc_ptr = unsafe { csc_ptr.byte_add(offset_of!(CSoftc, sc)).cast::<$sc>() };

                let sc_ref = unsafe {
                    $crate::Ptr::new(sc_ptr).can_ref().is_unique()
                };
                Ok(sc_ref)
            }

            pub fn softc_release(&self, mut dev: $crate::device::Device, _sc: $crate::UniqRef<$sc>) {
                use $crate::State;

                let csc_ptr = dev.get_softc::<CSoftc>();
                // Mark the softc as available
                unsafe {
                    (*csc_ptr).state = State::Available;
                }
            }

            pub fn softc_share(&self, mut dev: $crate::device::Device) -> $crate::Result<$crate::Ref<$sc>> {
                use core::mem::offset_of;
                use $crate::State;

                let csc_ptr = dev.get_softc::<CSoftc>();

                // The softc must either be in the available or shared states
                if unsafe { (*csc_ptr).state } == State::Claimed {
                    return Err(EPERM);
                }
                // The softc must have been initialized
                if unsafe { !(*csc_ptr).initialized } {
                    return Err(EPERM);
                }
                // Mark the softc as shared
                unsafe {
                    (*csc_ptr).state = State::Shared;
                }
                let sc_ptr = unsafe { csc_ptr.byte_add(offset_of!(CSoftc, sc)).cast::<$sc>() };

                let sc_ref = unsafe {
                    $crate::Ptr::new(sc_ptr).can_ref()
                };
                Ok(sc_ref)
            }
        }

        unsafe impl Sync for CDriver { }

        struct CSoftc {
            // this is intentionally not an Option<$sc> to allow checking claims independently of
            // accessing the softc
            sc: core::mem::MaybeUninit<$sc>,
            state: $crate::State,
            initialized: bool
        }

        #[no_mangle]
        pub static $cdriver: CDriver = CDriver {
            class: core::cell::UnsafeCell::new($crate::bindings::kobj_class {
                name: $cname.as_ptr(),
                methods: core::ptr::addr_of!($methods).cast(),
                size: core::mem::size_of::<CSoftc>(),
                baseclasses: core::ptr::null_mut(),
                refs: 0,
                ops: core::ptr::null_mut(),
            }),
            driver: Driver(()),
        };

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

        $($if_name::$if_fn!($impl, $cdriver);)*
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
    pub use crate::ErrCode::*;
    pub use crate::{dprint, dprintln, print, println, Result};
}

macro_rules! err_codes {
    ($($name:ident, $desc:literal,)+) => {
        // This enum has a variant for all KPI error code (including non-POSIX ones) and a few extra
        // to handle the impedance mismatch between the c_int and ErrCode types and a catch-all to
        // handle C KPIs that don't return descriptive error info (e.g. fns returning NULL pointer
        // on error). Since all variants are glob imported in mod prelude, the extra variants are
        // stylized like the KPI error codes to avoid unexpected surprises.
        #[repr(i32)]
        #[derive(Copy, Clone)]
        pub enum ErrCode {
            $($name = bindings::$name,)*
            ENOERR,
            EUNKNOWN,
            ENULLPTR,
        }

        impl Debug for ErrCode {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                let msg = match self {
                    $(ErrCode::$name => $desc,)*
                    ErrCode::ENOERR => "Accidentally converted non-error int into ErrCode",
                    ErrCode::EUNKNOWN => "Unknown error code",
                    ErrCode::ENULLPTR => "Catch-all for issues not described by KPI error codes",
                };
                f.write_str(msg)
            }
        }

        impl From<c_int> for ErrCode {
            fn from(val: c_int) -> Self {
                match val {
                    // Error codes are positive so map negative integers to ENOERR
                    c_int::MIN..=0 => ErrCode::ENOERR,
                    // Map KPI error code values to the respective enum variants
                    $(bindings::$name => ErrCode::$name,)*
                    // Map anything else to EUNKNOWN
                    _ => ErrCode::EUNKNOWN,
                    // ENULLPTR is only for use on the rust side while this function converts
                    // values coming from the C side so I shouldn't need to map ENULLPTR's value
                }
            }
        }

        impl From<TryReserveError> for ErrCode {
            // TODO: this is a lossy conversion, maybe add a new rust error code?
            fn from(_: TryReserveError) -> Self {
                ErrCode::ENOMEM
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
    EWOULDBLOCK, "Operation would block",
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
