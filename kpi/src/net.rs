use crate::misc::Thread;
use crate::prelude::*;
use crate::define_interface;
use crate::kobj::AsRustType;
use core::marker::PhantomData;
use core::ptr::NonNull;

pub type SockAddr = bindings::sockaddr;

pub trait ProtoSw {
    fn pr_attach(so: SocketRef, proto: i32, td: Thread) -> Result<()> {
        unimplemented!()
    }
    fn pr_bind(so: SocketRef, addr: &SockAddr, td: Thread) -> Result<()> {
        unimplemented!()
    }
    fn pr_listen(so: SocketRef, backlog: u32, td: Thread) -> Result<()> {
        unimplemented!()
    }
}

#[derive(Copy, Clone)]
pub struct SocketRef<'a>(NonNull<bindings::socket>, PhantomData<&'a bindings::socket>);

impl<'a> AsRustType<'a, SocketRef<'a>> for *mut bindings::socket {
    fn as_rust_type(&'a self) -> SocketRef {
        SocketRef(NonNull::new(*self).unwrap(), PhantomData)
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
            $(res.$trait_fn = Some($unmangled_name);)*
            res
        }));
        $($crate::$trait_fn!($sw_ty $unmangled_name);)*
    };
}

define_interface! {
    in ProtoSw
    fn pr_attach(so: *mut bindings::socket, proto: i32, td: *mut bindings::thread) -> core::ffi::c_int;
    fn pr_listen(so: *mut bindings::socket, proto: u32, td: *mut bindings::thread) -> core::ffi::c_int;
    fn pr_bind(so: *mut bindings::socket, addr: *mut bindings::sockaddr, td: *mut bindings::thread) -> core::ffi::c_int;
}
