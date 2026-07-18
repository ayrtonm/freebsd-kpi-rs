/*-
 * Copyright (c) 2024 John Baldwin <jhb@FreeBSD.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#![no_std]

use core::cell::UnsafeCell;
use core::cmp::min;
use core::ffi::{c_int, c_uint, c_void};
use core::marker::PhantomData;
use core::mem::MaybeUninit;
use core::ptr::{NonNull, null_mut};
use core::sync::atomic::{AtomicPtr, Ordering};
use kpi::ErrCode;
use kpi::boxed::Box;
use kpi::cdev::{CDevSw, MakeDevArgs, UioRef};
use kpi::ffi::{Lease, LeaseSlot, Loan, LoanLayout};
use kpi::misc::Thread;
use kpi::module::Module;
use kpi::sync::Checked;
use kpi::sync::sx::SxLock;
use kpi::vec::Vec;
use kpi::{define_cdev, define_module, proj};

// This imports all public functions defined in the `mod wrappers` modules across the KPI crate.
// These functions typically are named after their C counterparts, but may have different arg/return
// types. To call the underlying C functions use `bindings::the_c_function()`
use kpi::prelude::*;

// The rust FFI glue can only provide shared access to the softc. This means you cannot get mutable
// references to the softc fields (`&mut sc.state`) once the cdev is created and managing its softc.
//
// The restriction means that fields will be either read-only, atomic or behind some kind of lock
// (e.g. Mutex, SpinLock, SxLock). `kpi/src/sync/mod.rs` has some wrappers for other kinds of access
// patterns that are useful here. `OnceInit` is for fields that read-only, but can't be initialized
// immediately. `Checked` for fields that are known to only be modified from one place/thread at a
// time (uses runtime checks to ensure this and panics if that's wrong). Finally there's `Unchecked`
// which is like `Checked` but has no runtime cost. It's getters are unsafe so if you are wrong
// about the variable only being modified from one place/thread at a time you are essentially giving
// LLVM permission to perform invalid optimizations on the code.
#[derive(Default)]
pub struct EchoDevSoftc {
    state: SxLock<EchoDevState>,
}

// These are all behind a lock in the softc so we'll be able to mutate them
#[derive(Default)]
struct EchoDevState {
    valid: usize,
    buf: Vec<u8>,
    writers: usize,
    dying: bool,
}

// The implementation of the CDevSw trait for the EchoDev type defined by `define_cdev!`. It knows
// the softc type and uses pointers to the softc where a `cdev *` normally appears in C. All methods
// are optional and to wire them up correctly they must appear both in the trait implementation and
// the `define_cdev!` invocation below.
impl CDevSw for EchoDev {
    // This ensures all cdevsw use the same softc type
    type Softc = EchoDevSoftc;

    // A `Loan<T>` is a pointer (ABI-compatible with `void*`) to a heap-allocated memory for a T and
    // a counter. It can only be used for the duration of the function (otherwise you'll get a
    // compiler-error). If you need to use it beyond that point (e.g. the cdevsw method needs to be
    // asynchronous so it passes the softc to a callback) you can call `sc.lease()` to create a
    // `Lease<EchoDevSoftc>`. This increments the counter and when you ensure the callbacks are done
    // with the pointer the counter is decremented. When the cdev is destroyed by `destroy_dev` it
    // will assert there are no remaining leases.
    //
    // TODO: the transition to `Lease` is still underway so some callback KPIs (e.g. taskqueue)
    // don't use it yet but will eventually. Also for some callbacks (like config_intrhook in device
    // drivers) its useless since we know the callback always gets invoked before the softc is
    // destroyed. These will allow passing in a Loan<T>.
    //
    // To access the cdev pointer itself we can use `sc.cdev()` on any `Loan` or `Lease` to a cdev
    // softc. That returns a cdev that has a lifetime tied to the `Loan`/`Lease` so there's no way
    // to stash away a copy of the cdev and use it after the device is destroyed.
    fn d_open(sc: Loan<EchoDevSoftc>, fflag: i32, devtype: i32, td: Thread) -> Result<()> {
        Ok(())
    }
    fn d_close(sc: Loan<EchoDevSoftc>, fflag: i32, devtype: i32, td: Thread) -> Result<()> {
        Ok(())
    }
    fn d_read(sc: Loan<EchoDevSoftc>, uio: UioRef, ioflag: c_int) -> Result<()> {
        if uio.resid() == 0 {
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
                unimplemented!("sx_sleep()")
            };
            if res.is_err() {
                // Unlock added for clarity, but not necessary since the lock gets unlocked when
                // the state guard returned by sx_xlock goes out of scope.
                sx_xunlock(state);
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

        sx_xunlock(state);
        res
    }

    fn d_write(sc: Loan<EchoDevSoftc>, uio: UioRef, ioflag: c_int) -> Result<()> {
        if uio.resid() == 0 {
            return Ok(());
        }
        let mut state = sx_xlock(&sc.state);
        let mut res = Ok(());
        while uio.resid() != 0 {
            /* Wait for space to write */
            while state.valid == state.buf.len() {
                res = if state.dying {
                    Err(ENXIO)
                } else if ioflag & bindings::O_NONBLOCK != 0 {
                    Err(EWOULDBLOCK)
                } else {
                    unimplemented!("sx_sleep()")
                };
                if res.is_err() {
                    sx_xunlock(state);
                    return res;
                }
            }

            let todo = min(uio.resid(), state.buf.len() - state.valid);
            res = uiomove_write(&state.buf[state.valid..state.valid + todo], uio);
            if res.is_ok() {
                /* Wakeup any waiting readers. */
                if state.valid == 0 {
                    //wakeup(sc);
                }
                state.valid += todo;
                // selwakeup()
            }
        }
        sx_xunlock(state);
        res
    }
}

// A global variable where we can store a pointer to the cdev softc. This is what make_dev_s returns
// when loading the module and what we'll use to destroy the cdev when unloading the module.
static ECHODEV: LeaseSlot<EchoDevSoftc> = LeaseSlot::uninit();

impl Module for EchoDev {
    fn on_load(data: *mut c_void) -> Result<()> {
        // Allocate the softc on the heap. We use LoanLayout::new instead of just the softc type
        // because make_dev_args_init requires a LoanLayout<T> where T is the softc in the CDevSw
        // trait impl. This Box<LoanLayout<EchoDevSoftc>> is ABI-compatible with a `void *`
        let mut sc: Box<_, M_DEVBUF> = Box::new(LoanLayout::new(EchoDevSoftc::default()), M_WAITOK);

        // We haven't passed the pointer anywhere and Box<T> provides mutable access to T so we can
        // mutate it at this point. Even though rust uses `.` for field accesses via pointers, this
        // mutates the softc on the heap. So it'd be equivalent to something like
        // `sc->inner.state...` in C.
        //
        // `inner` is the only public field on LoanLayout<T> and it gives us access to the T.
        //
        // `state` is the `SxLock` in `EchoDevSoftc` and its `.get_mut()` gives us mutable access to
        // its data making us grab the lock. It's ok to skip grabbing the lock here because the Box
        // ensures its the only pointer to the thing.
        //
        // `buf = ` just sets the `Vec<u8>` field in `EchoDevState`. `Vec<T, M>` has two generic
        // parameters. The first is the type it stores (`u8`) and the second is the `malloc_type`
        // which defaults to `M_DEVBUF` if not specified.
        //
        // That makes this `Vec::fill_with_capacity(0, 64, M_WAITOK)` equivalent to
        // `malloc(64, M_DEVBUF, M_WAITOK | M_ZERO)`
        sc.inner.state.get_mut().buf = Vec::fill_with_capacity(0, 64, M_WAITOK);

        // make_dev_args_init takes ownership of the boxed (heap-allocated) softc that's passed in
        // so we can't use it after this. This returns a MakeDevArgs which has the only pointer to
        // the softc at this point. MakeDevArgs knows the softc type, but does not provide access to
        // it yet.

        // This function takes a reference to the cdevsw variable we defined with `define_cdev!` and
        // a pointer to the softc. It takes ownership of the boxed softc so we can't access it via
        // the Box after this. The type name on the LHS can be inferred but it's spelled out here to
        // make it easier to follow how the functions map the types. The `MakeDevArgs` now owns the
        // `Box` but doesn't provide us direct access to it.
        let mut args: MakeDevArgs<EchoDevSoftc, M_DEVBUF> = make_dev_args_init(sc, &echo_cdevsw);

        // Like the C version we need to set some fields in the make_dev_args struct.
        // TODO: Do I want to prefix the field names like in C? These are distinct from the fields
        // in the underlying C make_dev_args struct and just get copied over in make_dev_s, but
        // keeping the prefixes would make repo-wide grepping more uniform.
        args.name = c"echo";
        args.flags = bindings::MAKEDEV_CHECKNAME | bindings::MAKEDEV_WAITOK;
        args.uid = bindings::UID_ROOT;
        args.gid = bindings::GID_WHEEL;
        // unfortunately C and rust differ in how they represent octal numbers so 0600 gets
        // interpreted as decimal
        args.mode = 0o600;

        // Call make_dev_s then call a closure to initialize the softc. Since MakeDevArgs owned the
        // only pointer to the softc we can give the closure exclusive access to it (i.e. a
        // &mut EchoDevSoftc). The sc arg below is a mutable reference rather than a Box, because
        // Box owns its pointer (and therefore frees it when the Box goes out of scope). A mutable
        // reference (or "borrow") is a pointer that provides exclusive access for the duration of
        // the borrow, but leaves the responsibility of freeing the pointee to the owner.

        // Call make_dev_s to create the character device with the softc. This takes ownership of
        // MakeDevArgs above so it can't be used after this. TODO: consider letting users re-use a
        // MakeDevArgs for multiple devices. This would require giving MakeDevArgs a new softc Box
        // for each device.
        //
        // The `?` at the end means "if this function fails, return the error to the caller". This
        // "drops" any variables created in this function (e.g. the `MakeDevArgs` which owns the
        // `Box` which owns the `EchoDevSoftc` which owns its `Vec<u8>` field). Dropping each
        // variable in this chain releases any resources it manages (e.g. the malloc memory managed
        // by the `Vec<u8>` and `Box`) so fields of each variable are dropped before the variable
        // itself runs its drop implementation.
        //
        // Normally make_dev_s returns an out-pointer for the new cdev, but here it returns a
        // pointer to the softc. The counter in the LoanLayout<EchoDevSoftc> is initialized to 2.
        // One for the FFI glue and another for the pointer returned here. To access the cdev
        // pointer itself we can call the `.cdev()` method on any `Lease<T>` or `Loan<T>`. This
        // returns a `CDev` type which has a lifetime tied to the `Loan`/`Lease`.
        let sc: Lease<EchoDevSoftc> = make_dev_s(args)?;

        // proj! takes an argument of the form `&struct.field` and where struct is a
        // Loan<TheStructType>/Lease<TheStructType>/Pin<TheStructType> and returns a
        // Pin<TheFieldType>. It's required to initialize the SxLock
        // TODO: explain Pin/field projection
        sx_init(proj!(&sc.state), c"echo");

        // If we want to eventually destroy the character device we need to pass a
        // Lease<EchoDevSoftc> to destroy_dev. To make sure we can do that let's stash this pointer
        // in the static ECHODEV which is a slot that can hold a Lease.
        ECHODEV.init(sc);
        Ok(())
    }

    fn on_unload(data: *mut c_void) -> Result<()> {
        // Take the lease we previously stored in ECHODEV out of the slot.
        let sc = ECHODEV.take();
        // Give ownership of the softc to destroy_dev. This will wait for all cdevsw operations to
        // stop, check that there are no other outstanding leases to the softc and finally drop
        // (read: clean up) the softc memory. If any cdevsw operation created a new lease (e.g. to
        // pass as some callback arg because it needed to be asynchronous) you are responsible for
        // making sure that lease gets dropped by this point. Failure to do that causes this to
        // panic because otherwise there is a potential for UAF.
        destroy_dev(sc);
        Ok(())
    }
}

define_cdev! {
    static echo_cdevsw: EchoDev = {
        d_name: c"echo",
        // For each entry below this macro will generate a function with the platform's default C
        // ABI (AAPCS on aarch64 and SysV ABI on x86_64). Each function just calls the rust trait
        // methods above and does the argument/return type casts. The RHS below is the unmangled
        // name of the C function and they must be unique amongst all ELF symbols in the
        // kernel/loaded modules.
        d_read: echodev_read,
        d_write: echodev_write,
        d_open: echodev_open,
        d_close: echodev_close,
    }
}

// The second parameter is an unmangled function name just like in `define_cdev!`. This function is
// what gets passed to the DEV_MODULE invocation in the C source file.
define_module!(EchoDev, echodev_modevent);
