use crate::device::Device;
use crate::{bindings, PointsTo};
use alloc::ffi::CString;
use core::fmt;

pub struct TTY<'a>(pub Option<&'a Device>);

impl fmt::Write for TTY<'_> {
    fn write_str(&mut self, msg: &str) -> fmt::Result {
        let cmsg = CString::new(msg).unwrap();
        match &self.0 {
            Some(dev_ptr) => unsafe {
                bindings::device_printf(dev_ptr.as_ptr(), cmsg.as_c_str().as_ptr());
            },
            None => unsafe {
                bindings::printf(cmsg.as_c_str().as_ptr());
            },
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! print {
    ($($args:tt)*) => {
        {
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY(None),
                format_args!($($args)*)
            ).ok();
        }
    };
}

#[macro_export]
macro_rules! println {
    ($($args:tt)*) => {
        {
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY(None),
                format_args!($($args)*)
            ).ok();
            // llvm tries to optimize this to putchar
            //unsafe { bindings::printf(c"\n".as_ptr()); }
        }
    };
}

#[macro_export]
macro_rules! dprint {
    ($dev:expr, $($args:tt)*) => {
        {
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY(Some(&$dev)),
                format_args!($($args)*)
            ).ok();
        }
    };
}

#[macro_export]
macro_rules! dprintln {
    ($dev:expr, $($args:tt)*) => {
        {
            <$crate::tty::TTY as core::fmt::Write>::write_fmt(
                &mut $crate::tty::TTY(Some(&$dev)),
                format_args!($($args)*)
            ).ok();
            //unsafe { bindings::device_printf($dev.0, c"\n".as_ptr()); }
        }
    };
}
