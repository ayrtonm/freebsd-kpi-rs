/*-
 * Copyright (c) 2024 John Baldwin <jhb@FreeBSD.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#![no_std]

use core::cmp::min;
use core::ffi::{c_int, c_void};
use core::marker::PhantomData;
use core::mem::MaybeUninit;
use core::ptr::{NonNull, null_mut};
use core::sync::atomic::{AtomicPtr, Ordering};
use kpi::ErrCode;
use kpi::boxed::Box;
use kpi::cdev::{CDevSw, MakeDevArgs, Uio};
use kpi::ffi::{Ptr, Softc};
use kpi::intr::Channel;
use kpi::proc::Thread;
use kpi::module::Module;
use kpi::sync::Checked;
use kpi::sync::sx::SxLock;
use kpi::vec::Vec;
use kpi::{define_cdev, define_module, define_projectable};
use kpi::ffi::NullablePtr;

use kpi::prelude::*;

define_projectable! {
    #[derive(Default)]
    pub struct EchoDevSoftc {
        state: SxLock<EchoDevState>,
        chan: Channel,
    }
}

#[derive(Default)]
struct EchoDevState {
    valid: usize,
    buf: Vec<u8>,
    writers: usize,
    dying: bool,
}

impl CDevSw for EchoDev {
    type Softc = EchoDevSoftc;

    fn d_open(sc: &Softc<EchoDevSoftc>, fflag: i32, devtype: i32, td: &Thread) -> Result<()> {
        if fflag & bindings::FWRITE != 0 {
            let mut state = sx_xlock(&sc.state);
            if state.writers == usize::MAX {
                sx_xunlock(state);
                return Err(EBUSY);
            }
            state.writers += 1;
        }
        Ok(())
    }
    fn d_close(sc: &Softc<EchoDevSoftc>, fflag: i32, devtype: i32, td: &Thread) -> Result<()> {
        if fflag & bindings::FWRITE != 0 {
            let mut state = sx_xlock(&sc.state);
            state.writers -= 1;
            if state.writers == 0 {
                wakeup(&sc.chan);
            }
            sx_xunlock(state);
        }
        Ok(())
    }
    fn d_read(sc: &Softc<EchoDevSoftc>, uio: &Uio, ioflag: c_int) -> Result<()> {
        if uio.uio_resid() == 0 {
            return Ok(());
        }
        // sx_xunlock are also added for clarity, but not necessary since the lock gets unlocked
        // when the state guard returned by sx_xlock goes out of scope.
        let mut state = sx_xlock(&sc.state);

        /* Wait for bytes to read */
        while state.valid == 0 && state.writers != 0 {
            let res = if state.dying {
                Err(ENXIO)
            } else if ioflag & bindings::O_NONBLOCK != 0 {
                Err(EWOULDBLOCK)
            } else {
                let (new_guard, res) = sx_sleep(&sc.chan, state, bindings::PCATCH, c"echord", 0);
                state = new_guard;
                res
            };
            if res.is_err() {
                // Unlock added for clarity, but not necessary since the lock gets unlocked when
                // the state guard returned by sx_xlock goes out of scope.
                sx_xunlock(state);
                return res;
            }
        }

        let todo = min(uio.uio_resid(), state.valid);
        let res = uiomove_read(&mut state.buf[..todo], uio);
        if res.is_ok() {
            /* Wakeup any waiting writers. */
            if state.valid == state.buf.len() {
                wakeup(&sc.chan);
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

        sx_xunlock(state);
        res
    }

    fn d_write(sc: &Softc<EchoDevSoftc>, uio: &Uio, ioflag: c_int) -> Result<()> {
        if uio.uio_resid() == 0 {
            return Ok(());
        }
        let mut state = sx_xlock(&sc.state);
        let mut res = Ok(());
        while uio.uio_resid() != 0 {
            /* Wait for space to write */
            while state.valid == state.buf.len() {
                res = if state.dying {
                    Err(ENXIO)
                } else if ioflag & bindings::O_NONBLOCK != 0 {
                    Err(EWOULDBLOCK)
                } else {
                    let (new_guard, res) = sx_sleep(&sc.chan, state, bindings::PCATCH, c"echowr", 0);
                    state = new_guard;
                    res
                };
                if res.is_err() {
                    sx_xunlock(state);
                    return res;
                }
            }

            let todo = min(uio.uio_resid(), state.buf.len() - state.valid);
            res = uiomove_write(&state.buf[state.valid..state.valid + todo], uio);
            if res.is_ok() {
                /* Wakeup any waiting readers. */
                if state.valid == 0 {
                    wakeup(&sc.chan);
                }
                state.valid += todo;
                // selwakeup()
            }
        }
        sx_xunlock(state);
        res
    }
}

static ECHODEV: NullablePtr<EchoDevSoftc> = NullablePtr::new();

impl Module for EchoDev {
    fn on_load(data: *mut c_void) -> Result<()> {
        let mut sc = EchoDevSoftc::default();

        sc.state.get_mut().buf = Vec::fill_with_capacity(0, 64, M_WAITOK);

        let sc = Softc::new_boxed::<M_DEVBUF>(sc, M_WAITOK);
        sx_init(sc.project().state, c"echo");

        let mut args = make_dev_args_init(sc, &echo_cdevsw);

        args.name = c"echo";
        args.flags = bindings::MAKEDEV_CHECKNAME | bindings::MAKEDEV_WAITOK;
        args.uid = bindings::UID_ROOT;
        args.gid = bindings::GID_WHEEL;
        args.mode = 0o600;

        let sc = make_dev_s(args)?;

        ECHODEV.swap(sc);
        Ok(())
    }

    fn on_unload(data: *mut c_void) -> Result<()> {
        let sc = ECHODEV.take().unwrap();
        let mut state = sx_xlock(&sc.state);
        state.dying = true;
        wakeup(&sc.chan);
        sx_xunlock(state);
        destroy_dev(sc);
        Ok(())
    }
}

define_cdev! {
    static echo_cdevsw: EchoDev = {
        d_name: c"echo",
        d_read: echodev_read,
        d_write: echodev_write,
        d_open: echodev_open,
        d_close: echodev_close,
    }
}

define_module!(EchoDev, echodev_modevent);
