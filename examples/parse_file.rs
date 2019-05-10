#[macro_use]
extern crate unwrap;

use redland_rs::{KvStorage, Model, Parser, Serializer, Uri};
use std::fs::File;

fn main() {
    let file_path = "sample_code/turtle_code.ttl";
    let file = unwrap!(File::open(file_path));
    //Initialize Key-Value storage
    let storage = unwrap!(KvStorage::new());
    //Initialize Model
    let model = unwrap!(Model::new(&storage));
    //Variables for setting up parser
    let mime_type = "text/turtle";
    let base_uri = unwrap!(Uri::new("http://www.w3.org/2006/vcard/ns#"));
    let sn_schema = unwrap!(Uri::new("http://www.snee.com/hr/"));
    let vcard_schema = unwrap!(Uri::new("http://www.w3.org/2006/vcard/ns#"));
    //Initialize Parser
    let parser = unwrap!(Parser::new(mime_type));
    //Parsing from file
    let _res = Parser::parse_from_file(parser, &file, &base_uri, &model);
    //Serialize parsed content in model
    let serializer = unwrap!(Serializer::new("turtle", None, None));
    unwrap!(serializer.set_namespace(&sn_schema, "sn"));
    unwrap!(serializer.set_namespace(&vcard_schema, "vcard"));

    let result = serializer.serialize_model_to_string(&model);
    println!("{}", unwrap!(result));
}
