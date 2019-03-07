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

use libc::c_char;
use std::{
    ffi::{CStr, CString},
    ptr,
};

pub struct World(*mut librdf_world);

impl World {
    pub fn new() -> Self {
        World(unsafe { librdf_new_world() })
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

impl Model {
    pub fn as_mut_ptr(&self) -> *mut librdf_model {
        self.0
    }
}

impl Drop for Model {
    fn drop(&mut self) {
        unsafe {
            librdf_free_model(self.0);
        }
    }
}

pub struct Serializer(pub *mut librdf_serializer);

impl Serializer {
    pub fn set_namespace<S: Into<Vec<u8>>>(
        &self,
        uri: *mut librdf_uri,
        prefix: S,
    ) -> Result<(), i32> {
        let c_prefix = CString::new(prefix).map_err(|_| -1)?;

        let res = unsafe { librdf_serializer_set_namespace(self.0, uri, c_prefix.as_ptr()) };
        if res != 0 {
            return Err(res);
        }
        Ok(())
    }

    pub fn serialize_model_to_string(&self, model: &Model) -> Result<String, i32> {
        unsafe {
            let result = librdf_serializer_serialize_model_to_string(
                self.0,
                ptr::null_mut(),
                model.as_mut_ptr(),
            );
            if result.is_null() {
                return Err(-1);
            }

            let res = if let Ok(string) = CStr::from_ptr(result as *const c_char).to_str() {
                Some(string.to_owned())
            } else {
                None
            };
            librdf_free_memory(result as *mut _);

            return res.ok_or(-1);
        }
    }
}

impl Drop for Serializer {
    fn drop(&mut self) {
        unsafe {
            librdf_free_serializer(self.0);
        }
    }
}
