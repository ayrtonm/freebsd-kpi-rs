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
use crate::sync::arc::{Arc, ArcRef};
use core::cell::UnsafeCell;
use core::ffi::{c_uint, c_void};
use core::mem::transmute;
use core::ptr::null_mut;
use core::sync::atomic::{AtomicPtr, Ordering};

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
    init_addr: AtomicPtr<intr_config_hook>,
    //init_addr: Option<*mut intr_config_hook>,
    //metadata_ptr: SyncPtr<RefCountData>,
}

unsafe impl Sync for ConfigHook {}

pub type ConfigHookFn<T> = extern "C" fn(ArcRef<T>);

impl ConfigHook {
    pub fn new() -> Self {
        let inner = UnsafeCell::new(intr_config_hook::default());
        Self {
            inner,
            init_addr: AtomicPtr::new(null_mut()),
            //metadata_ptr: SyncPtr::null(),
        }
    }

    pub fn init<T>(&self, func: ConfigHookFn<T>, arg: Arc<T>) {
        let func = unsafe { transmute::<Option<ConfigHookFn<T>>, ich_func_t>(Some(func)) };
        ////let (arg_ptr, metadata_ptr) = SharedPtr::into_raw_parts(arg);
        let arg_ptr = Arc::into_raw(arg);
        let c_hook = self.inner.get();
        unsafe { (*c_hook).ich_func = func };
        unsafe { (*c_hook).ich_arg = arg_ptr.cast::<c_void>() };
        //self.metadata_ptr = SyncPtr::new(metadata_ptr);
        self.init_addr.store(c_hook, Ordering::Relaxed);
    }
}

#[derive(Debug, Default)]
pub struct Callout {
    inner: UnsafeCell<callout>,
    init_addr: Option<*mut callout>,
}

unsafe impl Sync for Callout {}

impl Callout {
    pub fn new() -> Self {
        let inner = UnsafeCell::new(callout::default());
        Self {
            inner,
            init_addr: None,
        }
    }

    pub fn init(&mut self) {
        let c_callout = self.inner.get();
        self.init_addr = Some(c_callout);
        unsafe {
            bindings::callout_init(c_callout, 1 /* always mpsafe */)
        }
    }
}

#[doc(inline)]
pub use wrappers::*;

#[doc(hidden)]
pub mod wrappers {
    use super::*;

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

    #[doc(inline)]
    #[cfg(feature = "intrng")]
    pub use intrng::wrappers::*;

    pub fn config_intrhook_establish(hook: &ConfigHook) -> Result<()> {
        let c_hook = hook.inner.get();
        assert!(hook.init_addr.load(Ordering::Relaxed) == c_hook);
        let res = unsafe { bindings::config_intrhook_establish(c_hook) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn config_intrhook_disestablish(hook: &ConfigHook) {
        unsafe { bindings::config_intrhook_disestablish(hook.inner.get()) };

        //unsafe { RefCountData::release_ref(hook.metadata_ptr.as_ptr()) }
    }

    pub fn callout_init(c: &mut Callout) {
        c.init()
    }

    pub fn callout_reset(
        c: &Callout,
        ticks: sbintime_t,
        func: callout_func_t,
        arg: *mut c_void,
    ) -> Result<()> {
        let time = ticks * unsafe { tick_sbt };
        let res = unsafe {
            bindings::callout_reset_sbt_on(c.inner.get(), time, 0, func, arg, -1, C_HARDCLOCK)
        };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn callout_schedule(c: &Callout, ticks: sbintime_t) -> Result<()> {
        let res = unsafe { bindings::callout_schedule(c.inner.get(), ticks.try_into().unwrap()) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
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
        fn device_detach(_sc: &Arc<Self::Softc>, _dev: device_t) -> Result<()> {
            Ok(())
        }
    }

    impl HookDriver {
        extern "C" fn deferred_attach(sc: ArcRef<HookSoftc>) {
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
