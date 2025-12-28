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
    C_HARDCLOCK, callout, callout_func_t, device_t, ich_func_t, intr_config_hook, sbintime_t,
    tick_sbt,
};
use crate::ffi::{ExtRef, MutExtRef};
use crate::prelude::*;
use crate::sync::Mutable;
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
}

unsafe impl Sync for ConfigHook {}
unsafe impl Send for ConfigHook {}

pub type ConfigHookFn<T> = extern "C" fn(MutExtRef<T>);

impl ConfigHook {
    pub fn new() -> Self {
        let inner = UnsafeCell::new(intr_config_hook::default());
        Self { inner }
    }
}

pub type CalloutFn<T> = extern "C" fn(ExtRef<T>);

#[derive(Debug, Default)]
pub struct Callout {
    inner: UnsafeCell<callout>,
}

unsafe impl Sync for Callout {}
unsafe impl Send for Callout {}

impl Callout {
    pub fn new() -> Self {
        let inner = UnsafeCell::new(callout::default());
        Self { inner }
    }
}

impl Drop for Callout {
    fn drop(&mut self) {
        let c_callout = self.inner.get();
        let _res = unsafe { bindings::_callout_stop_safe(c_callout, bindings::CS_DRAIN) };
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
    use crate::ffi::CallbackArg;
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

    pub fn config_intrhook_establish<T>(
        hook: &ConfigHook,
        func: ConfigHookFn<T>,
        arg: ExtRef<T>,
    ) -> Result<()> {
        let c_hook = hook.inner.get();
        unsafe {
            (*c_hook).ich_func = transmute::<Option<ConfigHookFn<T>>, ich_func_t>(Some(func));
            (*c_hook).ich_arg = (&*arg as *const T).cast_mut().cast::<c_void>();
        }
        // TODO: Handle case where func will be invoked immediately since it easily triggers rust UB
        let res = unsafe { bindings::config_intrhook_establish(c_hook) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn config_intrhook_disestablish(hook: &ConfigHook) {
        unsafe { bindings::config_intrhook_disestablish(hook.inner.get()) };
    }

    pub fn callout_init(dev: device_t, c: &mut Callout) -> Result<()> {
        unsafe {
            bindings::callout_init(c.inner.get(), 1 /* always mpsafe */)
        }
        Ok(())
    }

    pub fn callout_reset<T: CallbackArg>(
        c: &mut Callout,
        ticks: sbintime_t,
        func: CalloutFn<T>,
        arg: ExtRef<T>,
    ) -> Result<()> {
        if arg.get_callout() != c as *mut Callout {
            return Err(EINVAL);
        }
        let c_callout = c.inner.get();
        let time = ticks * unsafe { tick_sbt };
        let func = unsafe { transmute::<Option<CalloutFn<T>>, callout_func_t>(Some(func)) };
        let arg_ptr = ExtRef::into_raw(arg).cast::<c_void>();
        let res = unsafe {
            bindings::callout_reset_sbt_on(c_callout, time, 0, func, arg_ptr, -1, C_HARDCLOCK)
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn callout_schedule(c: &mut Callout, ticks: sbintime_t) -> Result<()> {
        let res = unsafe { bindings::callout_schedule(c.inner.get(), ticks.try_into().unwrap()) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
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
    use crate::ffi::{ExtRef, UninitExtRef};
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
        fn device_attach(uninit_sc: UninitExtRef<Self::Softc>, dev: device_t) -> Result<()> {
            let hook = ConfigHook::new();
            let loud = LoudDrop;
            let sc = uninit_sc.init(HookSoftc { dev, hook, loud }).into_ref();
            config_intrhook_establish(&sc.hook, HookDriver::deferred_attach, sc).unwrap();
            Ok(())
        }
        fn device_detach(_sc: ExtRef<Self::Softc>, _dev: device_t) -> Result<()> {
            Ok(())
        }
    }

    impl HookDriver {
        extern "C" fn deferred_attach(sc: MutExtRef<HookSoftc>) {
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
