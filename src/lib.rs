#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unsafe_code)]

extern crate libc;
extern crate serde;
#[macro_use]
extern crate serde_derive;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub mod kv_storage;

pub struct World(pub *mut librdf_world);

impl World {
    pub fn new() -> Self {
        World(unsafe {
            librdf_new_world()
        })
    }

    pub fn as_mut_ptr(&self) -> *mut librdf_world {
        self.0
    }
}

impl Drop for World {
    fn drop(&mut self) {
        unsafe {
            librdf_free_world(self.0);
        }
    }
}

pub struct Model(pub *mut librdf_model);

impl Drop for Model {
    fn drop(&mut self) {
        unsafe {
            librdf_free_model(self.0);
        }
    }
}

pub struct Serializer(pub *mut librdf_serializer);

impl Drop for Serializer {
    fn drop(&mut self) {
        unsafe {
            librdf_free_serializer(self.0);
        }
    }
}

