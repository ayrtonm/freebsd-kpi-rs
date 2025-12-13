/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2024 Ayrton Muñoz
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

#[doc(inline)]
pub use wrappers::*;

#[allow(non_snake_case)]
#[doc(hidden)]
pub mod wrappers {
    use crate::bindings;
    use crate::bindings::u_int;

    /// Checks whether the kernel is running in VHE mode.
    pub fn in_vhe() -> bool {
        unsafe { bindings::in_vhe() }
    }

    pub fn CPU_AFFINITY(cpu: u_int) -> u64 {
        unsafe { bindings::rust_bindings_CPU_AFFINITY(cpu) }
    }

    pub fn CPU_AFF0(mpidr: u64) -> u64 {
        unsafe { bindings::rust_bindings_CPU_AFF0(mpidr) }
    }
    pub fn CPU_AFF1(mpidr: u64) -> u64 {
        unsafe { bindings::rust_bindings_CPU_AFF1(mpidr) }
    }
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
macro_rules! read_specialreg {
    ($reg:ident) => {{
        let res: u64;
        unsafe {
            core::arch::asm! {
                concat!("mrs {}, ", stringify!($reg)),
                out(reg) res, options(nomem, nostack)
            }
        };
        res
    }};
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
macro_rules! write_specialreg {
    ($reg:ident, $val:expr) => {{
        let _tyck: u64 = $val;
        unsafe {
            core::arch::asm! {
                concat!("msr ", stringify!($reg), ", {}"),
                in(reg) $val, options(nomem, nostack)
            }
        }
    }};
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
    () => {{ unsafe { core::arch::asm!("isb") } }};
}

// rust inverts the asm "memory" option with nomem
#[macro_export]
macro_rules! rmb {
    () => {{ unsafe { core::arch::asm!("dmb ld") } }};
}

// rust inverts the asm "memory" option with nomem
#[macro_export]
macro_rules! wmb {
    () => {{ unsafe { core::arch::asm!("dmb st") } }};
}
