use crate::bindings;

pub fn in_vhe() -> bool {
    unsafe { bindings::in_vhe() }
}

#[macro_export]
macro_rules! pcpu_get {
    ($member:ident) => {
        {
            let pcpup: *mut $crate::bindings::pcpu;
            unsafe {
                core::arch::asm!("mov {}, x18", out(reg) pcpup, options(nomem, nostack));
                (*pcpup).$member
            }
        }
    };
}

#[macro_export]
macro_rules! pcpu_ptr {
    ($member:ident) => {
        core::ptr::addr_of_mut!($crate::pcpu_get!($member))
    };
}

#[macro_export]
macro_rules! curthread {
    () => {
        {
            let td: *mut $crate::bindings::thread;
            unsafe {
                core::arch::asm!("ldr {}, [x18]", out(reg) td, options(nomem, nostack));
            }
            td
        }
    };
    ($field:ident) => {
        unsafe {
            (*curthread!()).$field
        }
    };
}

#[macro_export]
macro_rules! read_reg {
    ($reg:expr) => {{
        let res: u64;
        unsafe {
            core::arch::asm! {
                concat!("mrs {}, ", $reg),
                out(reg) res, options(nomem, nostack)
            }
        };
        res
    }};
}
#[macro_export]
macro_rules! write_reg {
    ($reg:expr, $val:expr) => {{
        let _tyck: u64 = $val;
        unsafe {
            core::arch::asm! {
                concat!("msr ", $reg, ", {}"),
                in(reg) $val, options(nomem, nostack)
            }
        }
    }};
}

// rust inverts the asm "memory" option with nomem
#[macro_export]
macro_rules! isb {
    () => {{
        unsafe { core::arch::asm!("isb") }
    }};
}
