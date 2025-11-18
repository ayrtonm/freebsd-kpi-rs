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
use crate::bindings::{ich_func_t, intr_config_hook};
use crate::boxed::Box;
use crate::ffi::{RefCountData, RefCounted, SharedPtr, SyncPtr};
use crate::malloc::{MallocFlags, MallocType};
use crate::prelude::*;
use core::cell::UnsafeCell;
use core::ffi::{c_uint, c_void};
use core::mem::transmute;

#[cfg(feature = "intrng")]
mod intrng;

#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct IntrType(pub c_uint);

#[doc(inline)]
#[cfg(feature = "intrng")]
pub use intrng::*;

#[derive(Debug)]
pub struct ConfigHook {
    c_hook: Box<UnsafeCell<intr_config_hook>>,
    metadata_ptr: SyncPtr<RefCountData>,
}

unsafe impl Sync for ConfigHook {}

pub type ConfigHookFn<T> = extern "C" fn(&RefCounted<T>);

impl ConfigHook {
    pub fn try_new(ty: MallocType, flags: MallocFlags) -> Result<Self> {
        let c_hook = Box::try_new(UnsafeCell::new(intr_config_hook::default()), ty, flags)?;
        Ok(Self {
            c_hook,
            metadata_ptr: SyncPtr::null(),
        })
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

    pub fn config_intrhook_establish<T, P: SharedPtr<T>>(
        hook: &mut ConfigHook,
        func: ConfigHookFn<T>,
        arg: P,
    ) -> Result<()> {
        let func = unsafe { transmute::<Option<ConfigHookFn<T>>, ich_func_t>(Some(func)) };
        let (arg_ptr, metadata_ptr) = SharedPtr::into_raw_parts(arg);
        let c_hook = hook.c_hook.get_mut();
        c_hook.ich_func = func;
        c_hook.ich_arg = arg_ptr.cast::<c_void>();
        hook.metadata_ptr = SyncPtr::new(metadata_ptr);
        let res = unsafe { bindings::config_intrhook_establish(c_hook) };
        if res != 0 {
            return Err(ErrCode::from(res));
        }
        Ok(())
    }

    pub fn config_intrhook_disestablish(hook: &ConfigHook) {
        unsafe { bindings::config_intrhook_disestablish(hook.c_hook.get()) };

        unsafe { RefCountData::release_ref(hook.metadata_ptr.as_ptr()) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bindings::device_t;
    use crate::device::{BusProbe, DeviceIf};
    use crate::driver;
    use crate::ffi::UninitPtr;
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
        fn device_attach(uninit_sc: UninitPtr<Self::Softc>, dev: device_t) -> Result<()> {
            let hook = ConfigHook::try_new(M_DEVBUF, M_WAITOK).unwrap();
            let loud = LoudDrop;
            let mut sc = uninit_sc.init(HookSoftc { dev, hook, loud });
            let hook_ctx = sc.weak_ref();
            config_intrhook_establish(&mut sc.hook, HookDriver::deferred_attach, hook_ctx).unwrap();
            Ok(())
        }
        fn device_detach(_sc: &RefCounted<Self::Softc>, _dev: device_t) -> Result<()> {
            Ok(())
        }
    }

    impl HookDriver {
        extern "C" fn deferred_attach(sc: &RefCounted<HookSoftc>) {
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
