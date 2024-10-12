#![allow(nonstandard_style, private_interfaces, unused_imports, dead_code)]

// Creating the following aliases is not advisable in general, but it simplifies the changes
// makeobjops.awk needs since this allows the code it generates to use C type names
pub type void = core::ffi::c_void;
pub type char = core::ffi::c_char;
pub type int = core::ffi::c_int;
pub type long = core::ffi::c_long;
// The next dummy type definitions are for blocklisted types in the bindgen invocation.
#[repr(C)]
struct __va_list(());
#[repr(C)]
struct mcontext_t(());
#[repr(C)]
struct pcb(());
#[repr(C)]
struct vfpstate(());
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
