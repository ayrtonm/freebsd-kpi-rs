#![no_std]

use kpi::prelude::*;
use kpi::define_module;
use kpi::module::Module;
use core::ffi::c_void;

struct HelloWorldMod;

impl Module for HelloWorldMod {
    fn on_load(data: *mut c_void) -> Result<()> {
        println!("hello world: loaded the module");
        Ok(())
    }
    fn on_unload(data: *mut c_void) -> Result<()> {
        println!("hello world: unloaded the module");
        Ok(())
    }
}

define_module!(HelloWorldMod, hello_world_modevent);
