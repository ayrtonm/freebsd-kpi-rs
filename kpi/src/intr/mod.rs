/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2025 Ayrton Muñoz
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

use crate::ErrCode;
use crate::bindings::{
    C_HARDCLOCK, callout, callout_func_t, ich_func_t, intr_config_hook, sbintime_t, tick_sbt,
};
use crate::prelude::*;
use crate::sync::Mutable;
use crate::sync::arc::{Arc, ArcRef, ArcRefCount};
use crate::sync::mtx::Mutex;
use core::cell::UnsafeCell;
use core::ffi::{c_uint, c_void};
use core::mem::transmute;
use core::ptr::null_mut;
use core::sync::atomic::{AtomicU8, AtomicU16, AtomicU32, AtomicU64};

#[cfg(feature = "intrng")]
mod intrng;

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IntrType(pub c_uint);

#[doc(inline)]
#[cfg(feature = "intrng")]
pub use intrng::*;

#[derive(Debug, Default)]
pub struct ConfigHook {
    inner: UnsafeCell<intr_config_hook>,
    init_addr: Mutable<*mut intr_config_hook>,
}

unsafe impl Sync for ConfigHook {}
unsafe impl Send for ConfigHook {}

pub type ConfigHookFn<T> = extern "C" fn(Arc<T>);

impl ConfigHook {
    pub fn new() -> Self {
        let inner = UnsafeCell::new(intr_config_hook::default());
        Self {
            inner,
            init_addr: Mutable::new(null_mut()),
        }
    }

    pub fn init<T>(&self, func: ConfigHookFn<T>, arg: Arc<T>) {
        let func = unsafe { transmute::<Option<ConfigHookFn<T>>, ich_func_t>(Some(func)) };
        let arg_ptr = Arc::into_raw(arg);

        let c_hook = self.inner.get();
        unsafe { (*c_hook).ich_func = func };
        unsafe { (*c_hook).ich_arg = arg_ptr.cast::<c_void>() };

        *self.init_addr.get_mut() = c_hook;
    }
}

pub type CalloutFn<T> = extern "C" fn(ArcRef<T>);

#[derive(Debug, Default)]
pub struct Callout {
    inner: UnsafeCell<callout>,
    init_addr: *mut callout,
    // The Mutable here is really dumb since most callout functions take a &mut Callout, but
    // callout_drain cannot so we'll have to live with it
    refcount: Mutable<Option<ArcRefCount>>,
}

unsafe impl Sync for Callout {}
unsafe impl Send for Callout {}

impl Callout {
    pub fn new() -> Self {
        let inner = UnsafeCell::new(callout::default());
        Self {
            inner,
            init_addr: null_mut(),
            refcount: Mutable::new(None),
        }
    }
}

pub trait Sleepable {
    fn as_ptr(&self) -> *mut c_void;
}

impl<T> Sleepable for Mutable<T> {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}

impl Sleepable for AtomicU8 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}

impl Sleepable for AtomicU16 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}

impl Sleepable for AtomicU32 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}

impl Sleepable for AtomicU64 {
    fn as_ptr(&self) -> *mut c_void {
        self.as_ptr().cast::<c_void>()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Priority(i32);

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;
    use core::ffi::CStr;

    gen_newtype! {
        IntrType,
        INTR_TYPE_TTY,
        INTR_TYPE_BIO,
        INTR_TYPE_NET,
        INTR_TYPE_CAM,
        INTR_TYPE_MISC,
        INTR_TYPE_CLK,
        INTR_TYPE_AV,
    }

    gen_newtype! {
        Priority,
        PRI_MIN,
        PRI_MAX,
        PRI_MIN_ITHD,
        //PRI_MAX_ITHD,
        PI_REALTIME,
        PI_INTR,
        PI_AV,
        PI_NET,
        PI_DISK,
        PI_TTY,
        PI_DULL,
        PI_SOFT,
        PI_SOFTCLOCK,
        //PI_SWI,

        PRI_MIN_REALTIME,
        //PRI_MAX_REALTIME,

        PRI_MIN_KERN,
        //PRI_MAX_KERN,

        PSWP,
        PVM,
        PINOD,
        PRIBIO,
        PVFS,
        PZERO,
        PSOCK,
        PWAIT,
        PLOCK,
        PPAUSE,

        PRI_MIN_TIMESHARE,
        //PRI_MAX_TIMESHARE,

        PUSER,

        PRI_MIN_IDLE,
        PRI_MAX_IDLE,
    }

    #[doc(inline)]
    #[cfg(feature = "intrng")]
    pub use intrng::wrappers::*;

    pub fn config_intrhook_establish(hook: &ConfigHook) -> Result<()> {
        let c_hook = hook.inner.get();
        if *hook.init_addr.get_mut() != c_hook {
            return Err(EDOOFUS);
        }
        let res = unsafe { bindings::config_intrhook_establish(c_hook) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn config_intrhook_disestablish(hook: &ConfigHook) {
        unsafe { bindings::config_intrhook_disestablish(hook.inner.get()) };
    }

    pub fn callout_init(c: &mut Callout) {
        let c_callout = c.inner.get();
        c.init_addr = c_callout;
        unsafe {
            bindings::callout_init(c_callout, 1 /* always mpsafe */)
        }
    }

    pub fn callout_reset<T>(
        c: &mut Callout,
        ticks: sbintime_t,
        func: CalloutFn<T>,
        arg: Arc<T>,
    ) -> Result<()> {
        let time = ticks * unsafe { tick_sbt };
        let func = unsafe { transmute::<Option<CalloutFn<T>>, callout_func_t>(Some(func)) };
        let (arg_ptr, refcount) = Arc::take_refcount(arg);
        *c.refcount.get_mut() = Some(refcount);
        let res = unsafe {
            bindings::callout_reset_sbt_on(
                c.inner.get(),
                time,
                0,
                func,
                arg_ptr.cast::<c_void>(),
                -1,
                C_HARDCLOCK,
            )
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn callout_schedule(c: &mut Callout, ticks: sbintime_t) -> Result<()> {
        if c.refcount.get_mut().is_none() {
            return Err(EDOOFUS);
        }
        let res = unsafe { bindings::callout_schedule(c.inner.get(), ticks.try_into().unwrap()) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn callout_drain(c: &Mutex<Callout>) {
        // TODO: call the C callout_drain, for now this just releases the refcount
        let callout_ptr = c.data_ptr();
        let refcount_ptr = unsafe { &raw mut (*callout_ptr).refcount };
        let refcount_ref = unsafe { refcount_ptr.as_ref().unwrap() };
        let _drop_refcount: Option<ArcRefCount> = refcount_ref.get_mut().take();
    }

    pub fn tsleep<T: Sleepable>(
        chan: &T,
        new_priority: Option<Priority>,
        wmesg: &CStr,
        timo: i32,
    ) -> Result<()> {
        let chan_ptr = chan.as_ptr();
        let wmesg_ptr = wmesg.as_ptr();
        let priority = match new_priority {
            Some(Priority(p)) => p,
            None => 0,
        };
        let res = unsafe {
            bindings::_sleep(
                chan_ptr,
                null_mut(),
                priority,
                wmesg_ptr,
                bindings::tick_sbt * timo as i64,
                0,
                bindings::C_HARDCLOCK,
            )
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn wakeup<T: Sleepable>(chan: &T) {
        let chan_ptr = chan.as_ptr();
        unsafe { bindings::wakeup(chan_ptr) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bindings::device_t;
    use crate::device::{BusProbe, DeviceIf};
    use crate::driver;
    use crate::sync::arc::{Arc, UninitArc};
    use crate::tests::{DriverManager, LoudDrop};

    #[repr(C)]
    pub struct HookSoftc {
        dev: device_t,
        hook: ConfigHook,
        loud: LoudDrop,
    }
    impl DeviceIf for HookDriver {
        type Softc = HookSoftc;
        fn device_probe(dev: device_t) -> Result<BusProbe> {
            if !ofw_bus_is_compatible(dev, c"intr,hook_driver") {
                return Err(ENXIO);
            }
            Ok(BUS_PROBE_DEFAULT)
        }
        fn device_attach(uninit_sc: UninitArc<Self::Softc>, dev: device_t) -> Result<()> {
            let hook = ConfigHook::new();
            let loud = LoudDrop;
            let sc = uninit_sc.init(HookSoftc { dev, hook, loud }).into_arc();
            sc.hook.init(HookDriver::deferred_attach, sc.clone());
            config_intrhook_establish(&sc.hook).unwrap();
            Ok(())
        }
        fn device_detach(_sc: Arc<Self::Softc>, _dev: device_t) -> Result<()> {
            Ok(())
        }
    }

    impl HookDriver {
        extern "C" fn deferred_attach(sc: Arc<HookSoftc>) {
            println!("called config hook rust function/deferred_attach");
            config_intrhook_disestablish(&sc.hook);
        }
    }

    #[test]
    fn run_hook() {
        let mut m = DriverManager::new();
        m.add_test_device(c"intr,hook_driver");
        m.add_test_driver::<HookDriver>();
        m.probe_all();
        m.attach_all();
        m.trigger_config_hooks();
        m.detach_all();
    }

    driver!(hook_driver, c"hook_driver", HookDriver,
            hook_driver_methods = {
                device_probe hook_driver_probe,
                device_attach hook_driver_attach,
                device_detach hook_driver_detach,
            }
    );
}
