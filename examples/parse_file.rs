extern crate bincode;
extern crate libc;
extern crate redland_rs;
#[macro_use]
extern crate unwrap;

use libc::{c_char, fclose, fdopen, fgets};
use redland_rs::{KvStorage, Model, Parser, Serializer, Uri};
use std::ffi::CStr;
use std::fs::File;
use std::io::prelude::*;
use std::os::unix::io::IntoRawFd;

fn main() {
    let file_path = "sample_code/turtle_code.ttl";
    let file = unwrap!(File::open(file_path));

    unsafe {
        let storage = unwrap!(KvStorage::new());

        let model = unwrap!(Model::new(&storage));

        let mime_type = "text/turtle";
        let base_uri = unwrap!(Uri::new("http://www.w3.org/2006/vcard/ns#"));
        let sn_schema = unwrap!(Uri::new("http://www.snee.com/hr/"));
        let vcard_schema = unwrap!(Uri::new("http://www.w3.org/2006/vcard/ns#"));

        let parser = unwrap!(Parser::new(mime_type));

        let res = Parser::parse_from_file(parser, &file, base_uri, &model);

        let serializer = unwrap!(Serializer::new("turtle", None, None));
        unwrap!(serializer.set_namespace(&sn_schema, "sn"));
        unwrap!(serializer.set_namespace(&vcard_schema, "vcard"));

        let result = serializer.serialize_model_to_string(&model);
        println!("{}", unwrap!(result));
    }
}
