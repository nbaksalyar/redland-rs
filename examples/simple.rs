extern crate libc;
extern crate redland_rs;

use redland_rs::*;

use libc::{c_char, c_uchar};
use std::ffi::CStr;
use std::ptr;

struct World(*mut librdf_world);

impl Drop for World {
    fn drop(&mut self) {
        unsafe {
            librdf_free_world(self.0);
        }
    }
}

struct Model(*mut librdf_model);

impl Drop for Model {
    fn drop(&mut self) {
        unsafe {
            librdf_free_model(self.0);
        }
    }
}

fn main() {
    unsafe {
        let world = World(librdf_new_world());
        let storage = librdf_new_storage(
            world.0,
            b"memory\0" as *const _ as *const c_char,
            ptr::null(),
            ptr::null(),
        );

        let serializer = librdf_new_serializer(
            world.0,
            b"turtle\0" as *const _ as *const c_char,
            ptr::null(),
            ptr::null_mut(),
        );
        let ms_schema = librdf_new_uri(
            world.0,
            b"http://maidsafe.net/\0" as *const _ as *const c_uchar,
        );
        librdf_serializer_set_namespace(
            serializer,
            ms_schema,
            b"ms\0" as *const _ as *const c_char,
        );

        let subject = librdf_new_node_from_uri_local_name(
            world.0,
            ms_schema,
            b"MaidSafe\0" as *const _ as *const c_uchar,
        );
        let predicate = librdf_new_node_from_uri_local_name(
            world.0,
            ms_schema,
            b"location\0" as *const _ as *const c_uchar,
        );

        let model = Model(librdf_new_model(world.0, storage, ptr::null()));
        librdf_model_add_string_literal_statement(
            model.0,
            subject,
            predicate,
            b"Ayr\0" as *const _ as *const c_uchar,
            ptr::null(),
            0,
        );

        let result =
            librdf_serializer_serialize_model_to_string(serializer, ptr::null_mut(), model.0);
        println!(
            "{}",
            CStr::from_ptr(result as *const c_char).to_str().unwrap()
        );

        librdf_free_memory(result as *mut _);

        librdf_free_storage(storage);
        librdf_free_serializer(serializer);
    }
}
