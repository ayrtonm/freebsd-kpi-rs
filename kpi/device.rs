use crate::bindings::_device;
use crate::{bindings, AsCType, AsRustType, ErrCode, PointsTo, Ptr, Result, UniqRef};
use core::ffi::{c_int, CStr};
use core::ptr::NonNull;

enum_c_macros! {
    #[repr(i32)]
    #[derive(Copy, Clone, Debug)]
    #[allow(nonstandard_style)]
    pub enum ProbeRes {
        BUS_PROBE_SPECIFIC,
        BUS_PROBE_VENDOR,
        BUS_PROBE_DEFAULT,
        BUS_PROBE_LOW_PRIORITY,
        BUS_PROBE_GENERIC,
        BUS_PROBE_HOOVER,
        BUS_PROBE_NOWILDCARD,
    }
}

impl AsCType for ProbeRes {
    fn as_c_int(self) -> c_int {
        self as c_int
    }
}

impl AsRustType<Device> for *mut _device {
    fn as_rust_type(self) -> Device {
        Device::new(self)
    }
}

pub trait DeviceIf {
    fn device_probe(&self, dev: Device) -> Result<ProbeRes>;
    fn device_attach(&self, dev: Device) -> Result<()>;
    fn device_detach(&self, dev: Device) -> Result<()>;
}

pub type Device = Ptr<_device>;

impl Device {
    pub fn get_parent(&self) -> Result<Device> {
        let dev_ptr = self.0;
        let res = unsafe { bindings::device_get_parent(dev_ptr) };
        if res.is_null() {
            Err(ErrCode::ENULLPTR)
        } else {
            Ok(Device::new(res))
        }
    }

    pub fn set_desc(&self, desc: &'static CStr) {
        let dev_ptr = self.0;
        let desc_ptr = desc.as_ptr();
        unsafe { bindings::device_set_desc(dev_ptr, desc_ptr) }
    }

    pub fn get_nameunit(&self) -> &CStr {
        let dev_ptr = self.0;
        let name = unsafe { bindings::device_get_nameunit(dev_ptr) };
        unsafe { CStr::from_ptr(name) }
    }

    pub fn get_softc<SC>(&mut self) -> *mut SC {
        let dev_ptr = self.0;
        let sc_void_ptr = unsafe { bindings::device_get_softc(dev_ptr) };
        sc_void_ptr.cast::<SC>()
    }
}
