extern crate libc;
extern crate redland_rs;
#[macro_use]
extern crate unwrap;

use redland_rs::*;

use libc::c_char;
use std::ffi::CStr;
use std::ptr;

fn main() {
    let world = World::new();

    let storage = unsafe {
        librdf_new_storage(
            world.as_mut_ptr(),
            b"memory\0" as *const _ as *const c_char,
            ptr::null(),
            ptr::null(),
        )
    };

    let serializer = Serializer(unsafe {
        librdf_new_serializer(
            world.as_mut_ptr(),
            b"turtle\0" as *const _ as *const c_char,
            ptr::null(),
            ptr::null_mut(),
        )
    });

    let ms_schema = unwrap!(Uri::new(&world, "http://maidsafe.net/"));
    serializer.set_namespace(&ms_schema, "ms");

    let subject = unwrap!(Node::new_from_uri_local_name(
        &world, &ms_schema, "MaidSafe"
    ));
    let predicate = unwrap!(Node::new_from_uri_local_name(
        &world, &ms_schema, "location"
    ));

    let model = unwrap!(unsafe { Model::from_raw_storage(&world, storage) });
    unwrap!(model.add_string_literal_statement(&subject, &predicate, "Ayr", None, false));

    let result = serializer.serialize_model_to_string(&model);
    println!("{}", unwrap!(result));
}
