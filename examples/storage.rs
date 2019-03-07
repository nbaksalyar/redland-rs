#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_mut
)]

extern crate libc;
extern crate bincode;
extern crate redland_rs;

use libc::{c_char, c_uchar};
use redland_rs::*;
use redland_rs::kv_storage::*;
use std::ffi::CStr;
use std::fs::File;
use std::io::prelude::*;
use std::ptr;

use bincode::{deserialize, serialize};

unsafe fn create_mock_model(world: &World, storage: &KvStorage) -> Model {
    let ms_schema = librdf_new_uri(
        world.as_mut_ptr(),
        b"http://maidsafe.net/\0" as *const _ as *const c_uchar,
    );
    let subject = librdf_new_node_from_uri_local_name(
        world.as_mut_ptr(),
        ms_schema,
        b"MaidSafe\0" as *const _ as *const c_uchar,
    );
    let predicate = librdf_new_node_from_uri_local_name(
        world.as_mut_ptr(),
        ms_schema,
        b"location\0" as *const _ as *const c_uchar,
    );

    let model = Model(librdf_new_model(world.0, storage.as_mut_ptr(), ptr::null()));
    librdf_model_add_string_literal_statement(
        model.0,
        subject,
        predicate,
        b"Ayr\0" as *const _ as *const c_uchar,
        ptr::null(),
        0,
    );

    model
}

fn main() {
    unsafe {
        let world = World::new();
        let mut storage = KvStorage::new(&world).unwrap();

        // Convert entries into a hash
        {
            // Create mock entries and write to a file
            let _model = create_mock_model(&world, &storage);
            let entry_actions = storage.entry_actions();
            println!("{:?}", entry_actions);

            let ser = serialize(entry_actions).unwrap();

            {
                let mut file = File::create("md-storage").unwrap();
                file.write_all(&ser).unwrap();
            }
        }

        // Load entries from a file
        let mut entry_actions: Vec<EntryAction> = {
            let mut file = File::open("md-storage").unwrap();
            let mut contents = Vec::new();
            file.read_to_end(&mut contents).unwrap();

            deserialize(&contents).unwrap()
        };
        println!("{:?}", entry_actions);

        storage.copy_entries(&mut entry_actions);

        let model = Model(librdf_new_model(world.0, storage.as_mut_ptr(), ptr::null()));

        // Serialise to string - Turtle
        let serializer = Serializer(librdf_new_serializer(
            world.0,
            b"turtle\0" as *const _ as *const c_char,
            ptr::null(),
            ptr::null_mut(),
        ));
        let ms_schema = librdf_new_uri(
            world.0,
            b"http://maidsafe.net/\0" as *const _ as *const c_uchar,
        );
        librdf_serializer_set_namespace(
            serializer.0,
            ms_schema,
            b"ms\0" as *const _ as *const c_char,
        );

        let result =
            librdf_serializer_serialize_model_to_string(serializer.0, ptr::null_mut(), model.0);
        println!(
            "{}",
            CStr::from_ptr(result as *const c_char).to_str().unwrap()
        );
        librdf_free_memory(result as *mut _);
    }
}
