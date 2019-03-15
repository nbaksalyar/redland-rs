extern crate foreign_types;
extern crate libc;
extern crate redland_rs;
extern crate redland_sys;
#[macro_use]
extern crate unwrap;

use foreign_types::ForeignType;
use redland_rs::*;
use redland_sys::librdf_new_storage;

use libc::c_char;
use std::ptr;

fn main() {
    let world = World::new();

    let storage = unsafe {
        librdf_new_storage(
            world.as_ptr(),
            b"memory\0" as *const _ as *const c_char,
            ptr::null(),
            ptr::null(),
        )
    };

    let serializer = unwrap!(Serializer::new("turtle", None, None));

    let ms_schema = unwrap!(Uri::new("http://maidsafe.net/"));
    unwrap!(serializer.set_namespace(&ms_schema, "ms"));

    let subject = unwrap!(Node::new_from_uri_local_name(&ms_schema, "MaidSafe"));
    let predicate = unwrap!(Node::new_from_uri_local_name(&ms_schema, "location"));

    let model = unwrap!(unsafe { Model::from_raw_storage(storage) });
    unwrap!(model.add_string_literal_statement(&subject, &predicate, "Ayr", None, false));
    unwrap!(model.add_string_literal_statement(&subject, &predicate, "Scotland", None, false));

    let result = serializer.serialize_model_to_string(&model);
    println!("{}", unwrap!(result));
}
