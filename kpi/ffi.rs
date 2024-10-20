use crate::kpi_prelude::*;
use core::cell::UnsafeCell;
use core::mem::{offset_of, MaybeUninit};
use core::ops::{Deref, DerefMut};
use core::sync::atomic::{AtomicU32, Ordering};

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

    // This could take `&self` but since `SubClass` `Deref`s to its subclass `F` that would
    // introduce behavior that's hard to analyze if `F` had another method named `get_base_ptr`.
    pub fn get_base_ptr(sub: &Self) -> OutPtr<B> {
        sub.base.get_out_ptr()
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
    const UNINIT: u32 = 0;
    const INITIALIZING: u32 = 1;
    const AVAILABLE: u32 = 2;
    const CLAIMED: u32 = 3;
    const SHARED: u32 = 4;

    pub fn new() -> Self {
        Self(AtomicU32::new(Self::AVAILABLE))
    }

    pub fn uninit() -> Self {
        Self(AtomicU32::new(Self::UNINIT))
    }

    // Atomically switch to state B if in state A
    fn try_transition<const A: u32, const B: u32>(&self) -> Result<()> {
        if !self.0
            .compare_exchange(A, B, Ordering::Relaxed, Ordering::Relaxed)
            .is_ok() {
            return Err(EDOOFUS)
        }
        Ok(())
    }

    pub fn try_init(&self) -> Result<()> {
        self.try_transition::<{ Self::UNINIT }, { Self::INITIALIZING }>()
    }
    pub fn try_claim(&self) -> Result<()> {
        self.try_transition::<{ Self::AVAILABLE }, { Self::CLAIMED }>()
    }
    pub fn try_release(&self) -> Result<()> {
        self.try_transition::<{ Self::CLAIMED }, { Self::AVAILABLE }>()
    }
    pub fn try_share(&self) -> Result<()> {
        self.try_transition::<{ Self::AVAILABLE }, { Self::SHARED }>()
    }
}

/// A value with dynamically-checked borrow rules.
#[derive(Debug)]
pub struct Claimable<T> {
    data: T,
    // Any 32-bit value is valid here, but only the ones above are expected
    state: State,
}

impl<T> OutPtr<Claimable<T>> {
    fn get_state(&self) -> &State {
        unsafe { get_field!(self, state).as_ptr().as_ref().unwrap() }
    }
}

impl<T> OutPtr<Claimable<MaybeUninit<T>>> {
    pub fn try_init(self, initial_value: T) -> Result<()> {
        // Try switching from UNINIT to INITIALIZING
        self.get_state().try_init()?;

        let mut data_ref = unsafe { get_field!(self, data).assume_init().allows_mut_ref() };
        *data_ref = MaybeUninit::new(initial_value);

        // Switch to AVAILABLE to mark end of initialization
        self.get_state().0.store(State::AVAILABLE, Ordering::Relaxed);
        Ok(())
    }

    pub fn claim(self) -> Result<RefMut<T>> {
        // Try switching from AVAILABLE to CLAIMED
        self.get_state().try_claim()?;
        // SAFETY: If the transition above was successful then data must have been initialized
        let ptr = unsafe { get_field!(self, data).assume_init() };
        Ok(unsafe { ptr.flatten().allows_mut_ref() })
    }

    pub fn release(self, prev_claim: RefMut<T>) -> Result<()> {
        if prev_claim.as_ptr().cast() != get_field!(self, data).as_ptr() {
            return Err(EDOOFUS)
        }
        self.get_state().try_release()
    }

    pub fn share(self) -> Result<Ref<T>> {
        // Try switching from AVAILABLE to SHARED
        self.get_state().try_share()?;
        let ptr = unsafe { get_field!(self, data).assume_init() };
        Ok(unsafe { ptr.flatten().allows_ref() })
    }
}

impl<T> Claimable<MaybeUninit<T>> {
    pub fn uninit() -> Self {
        Self {
            data: MaybeUninit::uninit(),
            state: State::uninit(),
        }
    }
}
impl<T> Claimable<T> {
    pub fn new(data: T) -> Self {
        Self {
            data,
            state: State::new(),
        }
    }

    pub fn claim(&self) -> Result<RefMut<T>> {
        self.state.try_claim()?;
        let ptr = &raw const self.data;
        Ok(unsafe { RefMut::new(ptr as *mut T) })
    }

    pub fn release(&self, prev_claim: RefMut<T>) -> Result<()> {
        if prev_claim.as_ptr() != &raw const self.data as *mut T {
            return Err(EDOOFUS);
        }
        self.state.try_release()?;
        Ok(())
    }

    pub fn share(&self) -> Result<Ref<T>> {
        self.state.try_share()?;
        let ptr = &raw const self.data;
        Ok(unsafe { Ref::new(ptr as *mut T) })
    }
}
