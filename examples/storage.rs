extern crate bincode;
extern crate libc;
extern crate redland_rs;

use libc::c_char;
use redland_rs::librdf_new_serializer;
use redland_rs::{EntryAction, KvStorage, Model, Node, Serializer, Uri, World};
use std::fs::File;
use std::io::prelude::*;
use std::ptr;

use bincode::{deserialize, serialize};

fn create_mock_model(world: &World, storage: &KvStorage) -> Result<Model, i32> {
    let ms_schema = Uri::new(world, "http://maidsafe.net/")?;
    let subject = Node::new_from_uri_local_name(world, &ms_schema, "MaidSafe")?;
    let predicate = Node::new_from_uri_local_name(world, &ms_schema, "location")?;
    let model = Model::new(world, storage)?;
    model.add_string_literal_statement(&subject, &predicate, "Ayr", None, false)?;
    Ok(model)
}

fn main() {
    let world = World::new();
    let mut storage = KvStorage::new(&world).unwrap();

    // Convert entries into a hash
    {
        // Create mock entries and write to a file
        let _model = create_mock_model(&world, &storage).unwrap();
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

    let model = Model::new(&world, &storage).unwrap();

    // Serialise to string - Turtle
    let serializer = Serializer(unsafe {
        librdf_new_serializer(
            world.as_mut_ptr(),
            b"turtle\0" as *const _ as *const c_char,
            ptr::null(),
            ptr::null_mut(),
        )
    });
    let ms_schema = Uri::new(&world, "http://maidsafe.net/").unwrap();
    serializer.set_namespace(&ms_schema, "ms").unwrap();

    println!("{}", serializer.serialize_model_to_string(&model).unwrap());
}
