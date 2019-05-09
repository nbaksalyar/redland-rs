extern crate bincode;
extern crate libc;
extern crate redland_rs;
#[macro_use]
extern crate unwrap;

use redland_rs::{EntryAction, KvStorage, Model, Node, Serializer, Uri};
use std::fs::File;
use std::io::prelude::*;

use bincode::{deserialize, serialize};

fn create_mock_model(storage: &KvStorage) -> Result<Model, i32> {
    let ms_schema = Uri::new("http://maidsafe.net/")?;
    let subject = Node::new_from_uri_local_name(&ms_schema, "MaidSafe")?;
    let predicate = Node::new_from_uri_local_name(&ms_schema, "location")?;
    let model = Model::new(storage)?;
    model.add_string_literal_statement(&subject, &predicate, "Ayr", None, false)?;
    Ok(model)
}

fn main() {
    let mut storage = unwrap!(KvStorage::new());

    // Convert entries into a hash
    {
        // Create mock entries and write to a file
        let _model = unwrap!(create_mock_model(&storage));
        let entry_actions = storage.entry_actions();
        println!("{:?}", entry_actions);

        let ser = unwrap!(serialize(entry_actions));

        {
            let mut file = unwrap!(File::create("md-storage"));
            file.write_all(&ser).unwrap();
        }
    }

    // Load entries from a file
    let mut entry_actions: Vec<EntryAction> = {
        let mut file = unwrap!(File::open("md-storage"));
        let mut contents = Vec::new();
        unwrap!(file.read_to_end(&mut contents));

        unwrap!(deserialize(&contents))
    };
    println!("{:?}", entry_actions);

    unwrap!(storage.copy_entries(&mut entry_actions));

    let model = unwrap!(Model::new(&storage));

    // Serialise to string - Turtle
    let serializer = unwrap!(Serializer::new("turtle", None, None));
    let ms_schema = unwrap!(Uri::new("http://maidsafe.net/"));
    unwrap!(serializer.set_namespace(&ms_schema, "ms"));

    println!("{}", unwrap!(serializer.serialize_model_to_string(&model)));
}
