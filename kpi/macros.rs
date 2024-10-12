include!(concat!(env!("OUT_DIR"), "/device_if.rs"));
include!(concat!(env!("OUT_DIR"), "/pic_if.rs"));

#[macro_export]
macro_rules! enum_c_macros {
    ($(#[$($meta:meta)*])*
     pub enum $enum_name:ident {
         $($macro_name:ident$(,)?)*
     }) => {
        $(#[$($meta)*])*
        #[allow(nonstandard_style)]
        pub enum $enum_name {
            $($macro_name = bindings::$macro_name,)*
        }
    };
}
