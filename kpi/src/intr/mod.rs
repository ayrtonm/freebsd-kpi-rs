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
    callout, callout_func_t, ich_func_t, intr_config_hook, u_int,
};
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::{c_int, c_void};
use crate::ffi::{Loan, Lease};
use core::mem::transmute;
use core::ptr;
use core::ptr::null_mut;
use core::pin::Pin;
use core::ops::Deref;

#[cfg(feature = "intrng")]
mod intrng;

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IntrType(pub c_int);

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IntrFlags(pub c_int);

#[doc(inline)]
#[cfg(feature = "intrng")]
pub use intrng::*;

#[derive(Debug, Default)]
pub struct ConfigHook {
    inner: UnsafeCell<intr_config_hook>,
}

unsafe impl Sync for ConfigHook {}
unsafe impl Send for ConfigHook {}

pub type ConfigHookFn<T> = extern "C" fn(Pin<&T>);

impl ConfigHook {
    pub fn new() -> Self {
        let c_hook = intr_config_hook::default();
        Self {
            inner: UnsafeCell::new(c_hook),
        }
    }

    pub fn init<T>(self: Pin<&Self>, func: ConfigHookFn<T>, arg: Loan<T>) {
        let c_hook = self.inner.get();
        let arg_ptr = arg.deref() as *const T;
        unsafe {
            (*c_hook).ich_arg = arg_ptr.cast::<c_void>().cast_mut();
            (*c_hook).ich_func = transmute::<Option<ConfigHookFn<T>>, ich_func_t>(Some(func));
        }
    }
}

impl Drop for ConfigHook {
    fn drop(&mut self) {
        config_intrhook_disestablish(self)
    }
}

pub type CalloutFn<T> = extern "C" fn(Loan<T>);

#[derive(Debug, Default)]
pub struct Callout {
    inner: UnsafeCell<callout>,
    count_ptr: *mut u_int,
}

unsafe impl Sync for Callout {}
unsafe impl Send for Callout {}

impl Callout {
    pub fn new() -> Self {
        let inner = UnsafeCell::new(callout::default());
        Self { inner, count_ptr: null_mut() }
    }
}

impl Drop for Callout {
    fn drop(&mut self) {
        let c_callout = self.inner.get();
        let _res = unsafe { bindings::_callout_stop_safe(c_callout, bindings::CS_DRAIN) };
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Priority(pub i32);

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;
    use core::ffi::CStr;

    gen_newtype! {
        IntrType as i32,
        INTR_TYPE_TTY,
        INTR_TYPE_BIO,
        INTR_TYPE_NET,
        INTR_TYPE_CAM,
        INTR_TYPE_MISC,
        INTR_TYPE_CLK,
        INTR_TYPE_AV,
    }
    gen_newtype! {
        IntrFlags as i32,
        INTR_EXCL,
        INTR_MPSAFE,
        INTR_ENTROPY,
        INTR_SLEEPABLE,
        INTR_MD1,
        INTR_MD2,
        INTR_MD3,
        INTR_MD4,
    }

    gen_newtype! {
        Priority as i32,
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

    pub fn config_intrhook_establish(hook: Pin<&ConfigHook>) -> Result<()> {
        if !cold() {
            return Err(EPERM);
        }
        let c_hook = hook.inner.get();
        let res = unsafe { bindings::config_intrhook_establish(c_hook) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn config_intrhook_disestablish(hook: &ConfigHook) {
        unsafe { bindings::config_intrhook_disestablish(hook.inner.get()) };
    }

    pub fn callout_init(c: &mut Callout) -> Result<()> {
        unsafe {
            bindings::callout_init(c.inner.get(), 1 /* always mpsafe */)
        }
        Ok(())
    }

    pub fn callout_drain(c: *mut Callout) {
        let inner_ptr = unsafe { &raw mut (*c).inner };
        let c_callout = UnsafeCell::raw_get(inner_ptr);
        let _res = unsafe { bindings::fn_callout_drain(c_callout) };
    }

    pub fn callout_reset<T>(
        c: &mut Callout,
        ticks: u32,
        func: CalloutFn<T>,
        arg: Lease<T>,
    ) -> Result<()> {
        if !c.count_ptr.is_null() {
            let last = unsafe { bindings::refcount_release(c.count_ptr) };
            assert!(!last);
        }
        let (arg_ptr, count_ptr) = Lease::into_raw(arg);
        c.count_ptr = count_ptr;

        let c_callout = c.inner.get();
        let ticks = ticks.try_into().unwrap();
        let func = unsafe { transmute::<Option<CalloutFn<T>>, callout_func_t>(Some(func)) };

        let res = unsafe {
            bindings::fn_callout_reset(c_callout, ticks, func, arg_ptr.cast::<c_void>())
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn callout_schedule(c: &mut Callout, ticks: u32) -> Result<()> {
        let res = unsafe { bindings::callout_schedule(c.inner.get(), ticks.try_into().unwrap()) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn tsleep<T>(
        chan: &T,
        new_priority: Option<Priority>,
        wmesg: &CStr,
        timo: i32,
    ) -> Result<()> {
        let chan_ptr = ptr::from_ref(chan);
        let wmesg_ptr = wmesg.as_ptr();
        let priority = match new_priority {
            Some(Priority(p)) => p,
            None => 0,
        };
        let res = unsafe {
            bindings::fn_tsleep(chan_ptr.cast::<c_void>(),
                priority,
                wmesg_ptr,
                timo)
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn wakeup<T>(chan: &T) {
        let chan_ptr = ptr::from_ref(chan);
        unsafe { bindings::wakeup(chan_ptr.cast::<c_void>()) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::{BusProbe, Device, DeviceIf};
    use crate::define_driver;
    use crate::ffi::{Uninit, Loan};
    use crate::tests::{DriverManager, LoudDrop};

    #[repr(C)]
    pub struct HookSoftc {
        hook: ConfigHook,
        loud: LoudDrop,
    }
    impl DeviceIf for HookDriver {
        type Softc = HookSoftc;
        fn device_probe(dev: Device) -> Result<BusProbe> {
            if !ofw_bus_is_compatible(dev, c"intr,hook_driver") {
                return Err(ENXIO);
            }
            Ok(BUS_PROBE_DEFAULT)
        }
        fn device_attach(uninit_sc: Uninit<Self::Softc>) -> Result<()> {
            let hook = ConfigHook::new();
            let loud = LoudDrop;
            let sc = uninit_sc.init(HookSoftc { hook, loud });
            proj!(&sc.hook).init(hook_driver_deferred_attach, sc);
            config_intrhook_establish(proj!(&sc.hook)).unwrap();
            Ok(())
        }
        fn device_detach(_sc: Loan<Self::Softc>) -> Result<()> {
            Ok(())
        }
    }

    extern "C" fn hook_driver_deferred_attach(sc: Pin<&HookSoftc>) {
        println!("called config hook rust function/deferred_attach");
        config_intrhook_disestablish(&sc.hook);
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

    define_driver!(
        static hook_driver: HookDriver = {
            name: c"hook_driver",
        }
        static hook_driver_methods = {
            device_probe: hook_driver_probe,
            device_attach: hook_driver_attach,
            device_detach: hook_driver_detach,
        }
    );
}
