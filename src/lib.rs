#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unsafe_code)]

extern crate libc;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
