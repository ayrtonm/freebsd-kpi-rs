use core::sync::atomic::{AtomicU32, Ordering};
use core::ops::{Deref, DerefMut};
use core::cell::UnsafeCell;
use core::mem::{offset_of, MaybeUninit};
use crate::kpi_prelude::*;

pub trait AsCType<T> {
    fn as_c_type(self) -> T;
}

pub trait AsRustType<T> {
    fn as_rust_type(self) -> T;
}

/// Rust's view of a variable of type T that has its address shared with C.
///
/// This opts out of Rust's requirements that &T must be immutable and T always have a valid value.
/// While creating a &mut FFICell<T> is perfectly fine, it may not be used to get a &mut T.
#[derive(Debug)]
#[repr(transparent)]
pub struct FFICell<T>(UnsafeCell<MaybeUninit<T>>);

impl<T> FFICell<T> {
    pub const fn zeroed() -> Self {
        Self(UnsafeCell::new(MaybeUninit::zeroed()))
    }

    pub fn get_out_ptr(&self) -> OutPtr<T> {
        unsafe { OutPtr::new(self.0.get().cast()) }
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

#[repr(transparent)]
#[derive(Debug)]
struct State(AtomicU32);

impl State {
    const AVAILABLE: u32 = 0;
    const CLAIMED: u32 = 1;
    const SHARED: u32 = 2;
}

/// A value with dynamically-checked borrow rules.
#[derive(Debug)]
pub struct Claimable<T> {
    data: T,
    state: State,
}

impl<T> Claimable<T> {
    pub fn new(data: T) -> Self {
        Self {
            data,
            state: State(AtomicU32::new(State::AVAILABLE)),
        }
    }

    pub fn claim(&self) -> Result<RefMut<T>> {
        if self
            .state
            .0
            .compare_exchange(
                State::AVAILABLE,
                State::CLAIMED,
                Ordering::Relaxed,
                Ordering::Relaxed,
            )
            .is_ok()
        {
            let ptr = &raw const self.data;
            Ok(unsafe { RefMut::new(ptr as *mut T) })
        } else {
            Err(EDOOFUS)
        }
    }

    pub fn release(&self, prev_claim: RefMut<T>) -> Result<()> {
        if prev_claim.as_ptr() == &raw const self.data as *mut T {
            self.state.0.store(State::AVAILABLE, Ordering::Relaxed);
            Ok(())
        } else {
            Err(EDOOFUS)
        }
    }

    pub fn borrow(&self) -> Result<Ref<T>> {
        if self
            .state
            .0
            .compare_exchange(
                State::AVAILABLE,
                State::SHARED,
                Ordering::Relaxed,
                Ordering::Relaxed,
            )
            .is_ok()
        {
            let ptr = &raw const self.data;
            Ok(unsafe { Ref::new(ptr as *mut T) })
        } else {
            Err(EDOOFUS)
        }
    }
}
