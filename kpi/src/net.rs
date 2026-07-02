use core::ffi::c_void;
use crate::misc::Thread;
use crate::prelude::*;
use crate::define_interface;
use crate::kobj::AsRustType;
use crate::boxed::Box;
use core::marker::PhantomData;
use core::ptr::NonNull;

pub type SockAddr = bindings::sockaddr;

#[allow(unused_variables)]
pub trait ProtoSw {
    // Protocol control block storing state of an active connection
    type Pcb;

    fn pr_attach(so: SocketRef, proto: i32, td: Thread) -> Result<()> {
        unimplemented!()
    }
    fn pr_bind(so: SocketRef, addr: &SockAddr, td: Thread) -> Result<()> {
        unimplemented!()
    }
    fn pr_listen(so: SocketRef, backlog: i32, td: Thread) -> Result<()> {
        unimplemented!()
    }

    // TODO: leaks the box on error
    fn set_pcb(so: SocketRef, pcb: Box<Self::Pcb>) -> Result<()>{//, Option<Box<Self::Pcb>>) {
        let so_ptr = so.0.as_ptr();
        let pcb_ptr = unsafe { (*so_ptr).so_pcb };
        if !pcb_ptr.is_null() {
            return Err(EISCONN);//, Some(pcb));
        }
        unsafe {
            (*so_ptr).so_pcb = Box::into_raw(pcb).cast::<c_void>();
        }
        Ok(())
    }
}

#[derive(Copy, Clone)]
pub struct SocketRef<'a>(NonNull<bindings::socket>, PhantomData<&'a bindings::socket>);

pub struct SockLockGuard<'a> {
    so_lock: *mut bindings::mtx,
    so_options: &'a mut i32,
    so_state: &'a mut i16,
}

impl<'a> Drop for SockLockGuard<'a> {
    fn drop(&mut self) {
        unsafe { bindings::__mtx_unlock_flags(self.so_lock.cast::<usize>(), 0, c"".as_ptr(), 0) };
    }
}

// sys/socketvar.h contains a comment with what locks each struct socket field. That key is used to
// define the following methods
impl<'a> SocketRef<'a> {
    pub fn get_type(&self) -> i32 {
        let so_ptr = self.0.as_ptr();
        // SAFETY: constant after allocation, no locking required.
        i32::from(unsafe { (*so_ptr).so_type })
    }

    pub fn sock_lock(&self) -> SockLockGuard {
        let so_ptr = self.0.as_ptr();
        let so_lock = unsafe { &raw mut (*so_ptr).so_lock };
        unsafe { bindings::__mtx_lock_flags(so_lock.cast::<usize>(), 0, c"".as_ptr(), 0) };
        let so_options_ptr = unsafe { &raw mut (*so_ptr).so_options };
        let so_options = unsafe { so_options_ptr.as_mut().unwrap() };
        let so_state_ptr = unsafe { &raw mut (*so_ptr).so_state };
        let so_state = unsafe { so_state_ptr.as_mut().unwrap() };
        SockLockGuard {
            so_lock,
            so_options,
            so_state,
        }
    }
}

impl<'a> AsRustType<'a, SocketRef<'a>> for *mut bindings::socket {
    fn as_rust_type(&'a self) -> SocketRef {
        SocketRef(NonNull::new(*self).unwrap(), PhantomData)
    }
}

#[macro_export]
macro_rules! define_protosw {
    (
        $sw_ty:ident, $sw_name:ident, $sock_ty:expr,
        $($trait_fn:ident: $unmangled_name:ident,)*
    ) => {
        #[repr(C)]
        pub struct $sw_ty(core::cell::UnsafeCell<$crate::bindings::protosw>);
        unsafe impl Sync for $sw_ty {}

        #[unsafe(no_mangle)]
        static $sw_name: $sw_ty = $sw_ty(core::cell::UnsafeCell::new({
            use core::mem::MaybeUninit;
            let mut res: $crate::bindings::protosw = unsafe { MaybeUninit::zeroed().assume_init() };
            res.pr_type = $sock_ty as i16;
            $(res.$trait_fn = Some($unmangled_name);)*
            res
        }));
        $($crate::$trait_fn!($sw_ty $unmangled_name);)*
    };
}

define_interface! {
    in ProtoSw
    fn pr_attach(so: *mut bindings::socket, proto: i32, td: *mut bindings::thread) -> core::ffi::c_int;
    fn pr_listen(so: *mut bindings::socket, backlog: i32, td: *mut bindings::thread) -> core::ffi::c_int;
    fn pr_bind(so: *mut bindings::socket, addr: *mut bindings::sockaddr, td: *mut bindings::thread) -> core::ffi::c_int;
}
