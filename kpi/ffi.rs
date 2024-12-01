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


/*
#[repr(transparent)]
#[derive(Debug)]
struct State(AtomicU32);

impl State {
    const UNINIT: u32 = 0;
    const INITIALIZING: u32 = 1;
    const AVAILABLE: u32 = 2;
    const CLAIMED: u32 = 3;
    const SHARED: u32 = 4;

    pub fn available() -> Self {
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

// TODO: doing anything but panicking is super unsafe from the point of view of rust since it means
// there may still be references to the dropped memory. I need to double check but it should be fine
// if we never call back into rust code that uses those `Ref`/`RetMut`s after this happens
// (e.g. after device_detach).
// TODO: panic! is per-thread and this needs to be taken into account to make PanicOnDrop actually
// safe
#[derive(Debug)]
pub enum DropBehavior {
    PanicOnDrop = 0,
    WarnOnDrop,
    Nothing,
}

/// A value with dynamically-checked borrow rules.
#[derive(Debug)]
pub struct BorrowCk<T> {
    data: T,
    // Any 32-bit value is valid here, but only the ones above are expected
    state: State,
    drop_behavior: DropBehavior,
}

//impl<T> OutPtr<BorrowCk<T>> {
//    fn get_state(&self) -> &State {
//        unsafe { get_field!(self, state).as_ptr().as_ref().unwrap() }
//    }
//
//    pub fn init(self, initial_value: T) -> Result<()> {
//        // Try switching from UNINIT to INITIALIZING
//        self.get_state().try_init()?;
//
//        unsafe { get_field!(self, data).write(initial_value) };
//
//        // Switch to AVAILABLE to mark end of initialization
//        self.get_state().0.store(State::AVAILABLE, Ordering::Relaxed);
//        Ok(())
//    }
//
//    pub fn get(self) -> OutPtr<T> {
//        get_field!(self, data)
//    }
//
//    pub fn claim(self) -> Result<RefMut<T>> {
//        // Try switching from AVAILABLE to CLAIMED
//        self.get_state().try_claim()?;
//        // SAFETY: If the transition above was successful then data must have been initialized
//        let ptr = unsafe { get_field!(self, data).assume_init() };
//        Ok(unsafe { ptr.allows_mut_ref() })
//    }
//
//    pub fn release(self, prev_claim: RefMut<T>) -> Result<()> {
//        if prev_claim.as_ptr().cast() != get_field!(self, data).as_ptr() {
//            return Err(EDOOFUS)
//        }
//        self.get_state().try_release()
//    }
//
//    pub fn share(self) -> Result<Ref<T>> {
//        // Try switching from AVAILABLE to SHARED
//        self.get_state().try_share()?;
//        let ptr = unsafe { get_field!(self, data).assume_init() };
//        Ok(unsafe { ptr.allows_ref() })
//    }
//
//    //pub fn panic_on_drop(self) {
//    //    self.set_drop_behavior(DropBehavior::PanicOnDrop)
//    //}
//
//    //pub unsafe fn warn_on_drop(self) {
//    //    self.set_drop_behavior(DropBehavior::WarnOnDrop)
//    //}
//
//    //pub unsafe fn ignore_on_drop(self) {
//    //    self.set_drop_behavior(DropBehavior::Nothing)
//    //}
//
//    // SAFETY: this is a racy write and anything besides PanicOnDrop is a bad idea if we're
//    // expecting to call back into rust code that uses Ref/RefMut derived from this BorrowCk.
//    // Basically that means the only safe place to call this is at the end of device_detach.
//    pub unsafe fn set_drop_behavior(self, behavior: DropBehavior) {
//        get_field!(self, drop_behavior).write(behavior);
//    }
//}

impl<T> BorrowCk<T> {
    pub fn new(data: T) -> Self {
        Self {
            data,
            state: State::available(),
            drop_behavior: DropBehavior::PanicOnDrop,
        }
    }
    pub fn uninit() -> BorrowCk<MaybeUninit<T>> {
        BorrowCk {
            data: MaybeUninit::uninit(),
            state: State::uninit(),
            drop_behavior: DropBehavior::PanicOnDrop,
        }
    }

    pub fn panic_on_drop(&mut self) {
        // SAFETY: PanicOnDrop is a safe argument
        unsafe {
            self.set_drop_behavior(DropBehavior::PanicOnDrop);
        }
    }

    pub unsafe fn warn_on_drop(&mut self) {
        self.set_drop_behavior(DropBehavior::WarnOnDrop);
    }

    pub unsafe fn ignore_on_drop(&mut self) {
        self.set_drop_behavior(DropBehavior::Nothing);
    }

    // SAFETY: PanicOnDrop is safe. TODO: clarify safety conditions for other DropBehaviors
    pub unsafe fn set_drop_behavior(&mut self, behavior: DropBehavior) {
        self.drop_behavior = behavior;
    }

    //pub fn claim(&self) -> Result<RefMut<T>> {
    //    self.state.try_claim()?;
    //    let ptr = &raw const self.data;
    //    Ok(unsafe { RefMut::new(ptr as *mut T) })
    //}

    //pub fn release(&self, prev_claim: RefMut<T>) -> Result<()> {
    //    if prev_claim.as_ptr() != &raw const self.data as *mut T {
    //        return Err(EDOOFUS);
    //    }
    //    self.state.try_release()?;
    //    Ok(())
    //}

    //pub fn share(&self) -> Result<Ref<T>> {
    //    self.state.try_share()?;
    //    let ptr = &raw const self.data;
    //    Ok(unsafe { Ref::new(ptr as *mut T) })
    //}
}

//impl<T> Drop for BorrowCk<T> {
//    fn drop(&mut self) {
//        match self.drop_behavior {
//            DropBehavior::PanicOnDrop => {
//                // There's no good way to bubble up errors so just panic. There's not much we can do to
//                // recover if a driver was not refcounting references to the softc in the first place
//                // and tried to detach with outstanding references.
//                self.claim().unwrap();
//            },
//            DropBehavior::WarnOnDrop => {
//                if self.state.try_claim().is_err() {
//                    println!("WARNING: dropping dynamically borrow-checked value at {:p} in state {:?}", self, self.state);
//                }
//            },
//            DropBehavior::Nothing => (),
//        }
//    }
//}
*/
