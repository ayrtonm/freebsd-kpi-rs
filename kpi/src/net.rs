use core::ffi::c_void;
use crate::misc::Thread;
use crate::prelude::*;
use crate::define_interface;
use crate::kobj::AsRustType;
use crate::boxed::Box;
use core::marker::PhantomData;
use core::ptr::NonNull;
use core::sync::atomic::{AtomicU16, Ordering};

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
    pub so_options: &'a mut i32,
    pub so_state: &'a mut i16,
    pub so_label: &'a mut *mut bindings::label,
    pub so_dtor: &'a mut Option<unsafe extern "C" fn(*mut bindings::socket)>,
    pub so_splice: &'a mut *mut bindings::so_splice,
    pub so_splice_back: &'a mut *mut bindings::so_splice,
}

impl<'a> Drop for SockLockGuard<'a> {
    fn drop(&mut self) {
        unsafe { bindings::fn_mtx_unlock(self.so_lock) };
    }
}

// sys/socketvar.h contains a comment with what locks each struct socket field. That key is used to
// define the following methods
impl<'a> SocketRef<'a> {
    pub fn so_type(&self) -> i32 {
        let so_ptr = self.0.as_ptr();
        // SAFETY: constant after allocation, no locking required.
        i32::from(unsafe { (*so_ptr).so_type })
    }

    // TODO: Can I return a shared reference?
    pub fn so_cred(&self) -> *mut bindings::ucred {
        let so_ptr = self.0.as_ptr();
        // SAFETY: constant after allocation, no locking required.
        unsafe { (*so_ptr).so_cred }
    }

    // TODO: Can I return a shared reference?
    pub fn so_vnet(&self) -> *mut bindings::vnet {
        let so_ptr = self.0.as_ptr();
        // SAFETY: constant after allocation, no locking required.
        unsafe { (*so_ptr).so_vnet }
    }

    // TODO: Can I return a shared reference?
    pub fn so_proto(&self) -> *mut bindings::protosw {
        let so_ptr = self.0.as_ptr();
        // SAFETY: constant after allocation, no locking required.
        unsafe { (*so_ptr).so_proto }
    }

    pub fn so_error(&self) -> u16 {
        let so_ptr = self.0.as_ptr();
        let so_error_ptr = unsafe { &raw mut (*so_ptr).so_error };
        // SAFETY: field is aligned to u16 and only used in other atomic u16 accesses
        unsafe { AtomicU16::from_ptr(so_error_ptr).load(Ordering::Relaxed) }
    }

    pub fn so_rerror(&self) -> u16 {
        let so_ptr = self.0.as_ptr();
        let so_rerror_ptr = unsafe { &raw mut (*so_ptr).so_rerror };
        // SAFETY: field is aligned to u16 and only used in other atomic u16 accesses
        unsafe { AtomicU16::from_ptr(so_rerror_ptr).load(Ordering::Relaxed) }
    }

    fn sock_lock(&self) -> SockLockGuard<'_> {
        let so_ptr = self.0.as_ptr();
        let so_lock = unsafe { &raw mut (*so_ptr).so_lock };

        unsafe { bindings::fn_mtx_lock(so_lock) };

        let so_options_ptr = unsafe { &raw mut (*so_ptr).so_options };
        let so_options = unsafe { so_options_ptr.as_mut().unwrap() };

        let so_state_ptr = unsafe { &raw mut (*so_ptr).so_state };
        let so_state = unsafe { so_state_ptr.as_mut().unwrap() };

        let so_label_ptr = unsafe { &raw mut (*so_ptr).so_label };
        let so_label = unsafe { so_label_ptr.as_mut().unwrap() };

        let so_dtor_ptr =  unsafe { &raw mut (*so_ptr).so_dtor };
        let so_dtor = unsafe { so_dtor_ptr.as_mut().unwrap() };

        let so_splice_ptr =  unsafe { &raw mut (*so_ptr).so_splice };
        let so_splice = unsafe { so_splice_ptr.as_mut().unwrap() };

        let so_splice_back_ptr =  unsafe { &raw mut (*so_ptr).so_splice_back };
        let so_splice_back = unsafe { so_splice_back_ptr.as_mut().unwrap() };

        SockLockGuard {
            so_lock,
            so_options,
            so_state,
            so_label,
            so_dtor,
            so_splice,
            so_splice_back,
        }
    }
}

impl<'a> AsRustType<'a, SocketRef<'a>> for *mut bindings::socket {
    fn as_rust_type(&'a self) -> SocketRef<'a> {
        SocketRef(NonNull::new(*self).unwrap(), PhantomData)
    }
}

#[doc(hidden)]
pub mod wrappers {
    use super::*;

    pub fn SOCK_LOCK<'a>(so: &'a SocketRef<'_>) -> SockLockGuard<'a> {
        so.sock_lock()
    }
}

#[macro_export]
macro_rules! define_protosw {
    (
        static $sw_name:ident: $sw_ty:ident = {
            pr_type: $sock_ty:expr,
            pr_protocol: $sock_protocol:expr,
            pr_flags: $sock_flags:expr,
            $($trait_fn:ident: $unmangled_name:ident,)*
        }
    ) => {
        #[repr(C)]
        pub struct $sw_ty(core::cell::UnsafeCell<$crate::bindings::protosw>);
        unsafe impl Sync for $sw_ty {}

        #[unsafe(no_mangle)]
        static $sw_name: $sw_ty = $sw_ty(core::cell::UnsafeCell::new({
            use core::mem::MaybeUninit;
            let mut res: $crate::bindings::protosw = unsafe { MaybeUninit::zeroed().assume_init() };
            res.pr_type = $sock_ty;
            res.pr_protocol = $sock_protocol;
            res.pr_flags = $sock_flags;
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
