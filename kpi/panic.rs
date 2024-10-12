use crate::{bindings, println};
use core::panic::PanicInfo;

#[allow(unreachable_code)]
#[panic_handler]
fn rust_panic(info: &PanicInfo) -> ! {
    // TODO: need to disable interrupts here
    match info.location() {
        Some(location) => {
            println!(
                "Panicked at {}:{}:{}",
                location.file(),
                location.line(),
                location.column()
            )
        }
        None => {
            println!("Panicked at unknown location in source")
        }
    }
    println!("{}", info.message());
    unsafe {
        bindings::panic(c"Rust panicked! Now transferring control to KPI panic...".as_ptr());
    }
    // This should not be reached but in case KPI panic returns, print a warning and stop the thread
    println!("KPI panic should not return(?)");
    loop {}
}
