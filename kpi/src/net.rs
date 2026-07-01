use crate::misc::Thread;
use crate::prelude::*;
use core::marker::PhantomData;
use core::ptr::NonNull;

pub type SockAddr = bindings::sockaddr;

pub trait ProtoSw {
    fn attach(so: SocketRef, proto: u32, td: Thread) -> Result<()> {
        unimplemented!()
    }
    fn bind(so: SocketRef, addr: &SockAddr, td: Thread) -> Result<()> {
        unimplemented!()
    }
    fn listen(so: SocketRef, backlog: u32, td: Thread) -> Result<()> {
        unimplemented!()
    }
}

#[derive(Copy, Clone)]
pub struct SocketRef<'a>(NonNull<bindings::socket>, PhantomData<&'a bindings::socket>);

impl<'a> SocketRef<'a> {
    pub fn new(ptr: &'a *mut bindings::socket) -> Self {
        Self(NonNull::new(*ptr).unwrap(), PhantomData)
    }
}

#[macro_export]
macro_rules! define_protosw {
    (
        $sw_ty:ident, $sw_name:ident,
        $($trait_fn:ident: $unmangled_name:ident,)*
    ) => {
        #[repr(C)]
        pub struct $sw_ty(core::cell::UnsafeCell<$crate::bindings::protosw>);
        unsafe impl Sync for $sw_ty {}

        static $sw_name: $sw_ty = $sw_ty(core::cell::UnsafeCell::new({
            use core::mem::MaybeUninit;
            let mut res: $crate::bindings::protosw = unsafe { MaybeUninit::zeroed().assume_init() };
            res
        }));
        $($crate::c_fn_for_protosw!($unmangled_name, $sw_ty, $trait_fn);)*
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! c_fn_for_protosw {
    ($unmangled_name:ident, $sw_ty:ident, on_attach) => {
        extern "C" fn $unmangled_name(
            so: *mut $crate::bindings::socket,
            proto: u32,
            td: *mut $crate::bindings::thread,
        ) -> core::ffi::c_int {
            use $crate::kobj::{AsRustType, AsCType};
            let so_ref = $crate::net::SocketRef::new(&so);
            let res = <$sw_ty as ProtoSw>::attach(so_ref, proto, td.as_rust_type());
            match res {
                Ok(()) => 0,
                Err(e) => e.as_c_type(),
            }
        }
    };

    ($unmangled_name:ident, $sw_ty:ident, on_listen) => {
        extern "C" fn $unmangled_name(
            so: *mut $crate::bindings::socket,
            proto: u32,
            td: *mut $crate::bindings::thread,
        ) -> core::ffi::c_int {
            use $crate::kobj::{AsRustType, AsCType};
            let so_ref = $crate::net::SocketRef::new(&so);
            let res = <$sw_ty as ProtoSw>::listen(so_ref, proto, td.as_rust_type());
            match res {
                Ok(()) => 0,
                Err(e) => e.as_c_type(),
            }
        }
    };

    ($unmangled_name:ident, $sw_ty:ident, on_bind) => {
        extern "C" fn $unmangled_name(
            so: *mut $crate::bindings::socket,
            addr: *mut $crate::bindings::sockaddr,
            td: *mut $crate::bindings::thread,
        ) -> core::ffi::c_int {
            use $crate::kobj::{AsRustType, AsCType};
            let so_ref = $crate::net::SocketRef::new(&so);
            let addr = unsafe { addr.as_ref().unwrap() };
            let res = <$sw_ty as ProtoSw>::bind(so_ref, addr, td.as_rust_type());
            match res {
                Ok(()) => 0,
                Err(e) => e.as_c_type(),
            }
        }
    };
}
