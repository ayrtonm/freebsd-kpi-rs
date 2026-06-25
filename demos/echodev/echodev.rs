/*-
 * Copyright (c) 2024 John Baldwin <jhb@FreeBSD.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#![no_std]

use core::cell::UnsafeCell;
use core::ffi::{c_int, c_uint, c_void};
use core::marker::PhantomData;
use kpi::boxed::Box;
use core::mem::MaybeUninit;
use kpi::module::Module;
use core::ptr::{NonNull, null_mut};
use kpi::ErrCode;
use kpi::prelude::*;
use kpi::{define_module,define_cdev};
use kpi::cdev::{UioRef, CDevSw, cdev_t};
use kpi::vec::Vec;
use kpi::sync::Checked;
use core::sync::atomic::{AtomicPtr, Ordering};
use core::cmp::min;
use kpi::sync::mtx::Mutex;

#[derive(Default)]
pub struct EchoDevSoftc {
    dev: cdev_t,
    state: Mutex<EchoDevSoftcState>,
}

#[derive(Default)]
struct EchoDevSoftcState {
    valid: usize,
    buf: Vec<u8>,
    writers: usize,
    dying: bool,
}

impl CDevSw for EchoDev {
    type Softc = EchoDevSoftc;

    fn on_read(sc: &EchoDevSoftc, dev: cdev_t, uio: UioRef, ioflag: c_int) -> Result<()> {
        if uio.resid() == 0 {
            return Ok(());
        }
        // mtx_unlock are also added for clarity, but not necessary since the mutex gets unlocked
        // when the state guard returned by mtx_lock goes out of scope.
        let mut state = mtx_lock(&sc.state);

        /* Wait for bytes to read */
        while state.valid == 0 && state.writers != 0 {
            let res = if state.dying {
                Err(ENXIO)
            } else if ioflag & bindings::O_NONBLOCK != 0 {
                Err(EWOULDBLOCK)
            } else {
                unimplemented!("mtx_sleep()")
            };
            if res.is_err() {
                // Unlock added for clarity, but not necessary since the mutex gets unlocked when
                // the state guard returned by mtx_lock goes out of scope.
                mtx_unlock(state);
                return res;
            }
        }

        let todo = min(uio.resid(), state.valid);
        let res = uiomove_read(&mut state.buf[..todo], uio);
        if res.is_ok() {
            /* Wakeup any waiting writers. */
            if state.valid == state.buf.len() {
                //wakeup(sc);
            }
            state.valid -= todo;
            // Do a memmove from state.buf[todo..todo + state.valid] to state.buf[0..state.valid]
            let end = todo + state.valid;
            state.buf.copy_within(todo..end, 0);

            // Alternatively there's also split_at_mut to create two mutable references to
            // non-overlapping parts of the original slice then copy_from_slice
            //let (first, second) = state.buf.split_at_mut(todo);
            //let src = &second[..valid];
            //let dst = &mut first[..valid];
            //dst.copy_from_slice(src);

            // selwakeup()
        }

        mtx_unlock(state);
        res
    }

    fn on_write(sc: &EchoDevSoftc, dev: cdev_t, uio: UioRef, ioflag: c_int) -> Result<()> {
        if uio.resid() == 0 {
            return Ok(());
        }
        let mut state = mtx_lock(&sc.state);
        while uio.resid() != 0 {
            /* Wait for space to write */
            while state.valid == state.buf.len() {
                if state.dying {
                    Err(ENXIO)
                } else if ioflag & bindings::O_NONBLOCK != 0 {
                    Err(EWOULDBLOCK)
                } else {
                    unimplemented!("mtx_sleep()")
                }
                if res.is_err() {
                    mtx_unlock(state);
                    return res;
                }
            }

            let todo = min(uio.resid(), state.buf.len() - state.valid);
            let res = uiomove_write(&state.buf[sc.valid..sc.valid + todo], uio);
            if res.is_ok() {
                /* Wakeup any waiting readers. */
                if state.valid == 0 {
                    //wakeup(sc);
                }
                sc.valid += todo;
                // selwakeup()
            }
        }
        mtx_unlock(state);
        res
    }
}

impl Module for EchoDev {
    fn on_load(data: *mut c_void) -> Result<()> {
        // Allocates the softc on the heap. Box is a uniquely-owned pointer to the heap.
        let sc: Box<_, M_DEVBUF> = Box::new(EchoDevSoftc::default(), M_WAITOK);
        // make_dev_args_init takes ownership of the boxed (heap-allocated) softc that's passed in
        // so we can't use it after this. This returns a MakeDevArgs which has the only pointer to
        // the softc at this point. MakeDevArgs knows the softc type, but does not provide access to
        // it yet.
        let mut args = Self::make_dev_args_init(sc);
        // Call make_dev_s then call a closure to initialize the softc. Since MakeDevArgs owned the
        // only pointer to the softc we can give the closure exclusive access to it (i.e. a
        // &mut EchoDevSoftc). The sc arg below is a mutable reference rather than a Box, because
        // Box owns its pointer (and therefore frees it when the Box goes out of scope). A mutable
        // reference (or "borrow") is a pointer that provides exclusive access for the duration of
        // the borrow, but leaves the responsibility of freeing the pointee to the owner.
        let echodev = make_dev_s(args, |sc, dev| {
            sc.dev = dev;
            //mtx_init(dev, &sc.state, c"echo", None, None);
            sc.state.get_mut().buf = Vec::fill_with_capacity(0u8, 64, M_WAITOK);
        })?;
        Ok(())
    }

    fn on_unload(data: *mut c_void) -> Result<()> {
        unsafe {
            //bindings::destroy_dev(
        }
        Ok(())
    }
}

define_cdev! {
    EchoDev, c"echo", echo_cdevsw,
    on_read: echodev_read,
}

define_module!(EchoDev, echodev_modevent);
