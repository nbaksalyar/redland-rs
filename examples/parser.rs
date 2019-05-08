extern crate libc;
extern crate redland_rs;
#[macro_use]
extern crate unwrap;

use redland_rs::{KvStorage, Model, Parser, Serializer, Uri};

fn main() {
    let turtle_code = r#"
        @prefix vcard: <http://www.w3.org/2006/vcard/ns#> .
        @prefix sn: <http://www.snee.com/hr/> .

        sn:emp1   vcard:given-name   "Heidi" .
        sn:emp1   vcard:family-name   "Smith" .
        sn:emp1   vcard:title   "CEO" .
        sn:emp1   sn:hireDate   "2015-01-13" .
        sn:emp1   sn:completedOrientation   "2015-01-30" .

        sn:emp2   vcard:given-name   "John" .
        sn:emp2   vcard:family-name   "Smith" .
        sn:emp2   sn:hireDate   "2015-01-28" .
        sn:emp2   vcard:title   "Engineer" .
        sn:emp2   sn:completedOrientation   "2015-01-30" .
        sn:emp2   sn:completedOrientation   "2015-03-15" .

        sn:emp3   vcard:given-name   "Francis" .
        sn:emp3   vcard:family-name   "Jones" .
        sn:emp3   sn:hireDate   "2015-02-13" .
        sn:emp3   vcard:title   "Vice President" .

        sn:emp4   vcard:given-name   "Jane" .
        sn:emp4   vcard:family-name   "Berger" .
        sn:emp4   sn:hireDate   "2015-03-10" .
        sn:emp4   vcard:title   "Sales" ."#;

    let mime_type = "text/turtle";
    let storage = unwrap!(KvStorage::new());

    let model = unwrap!(Model::new(&storage));

    let base_uri = unwrap!(Uri::new("http://www.w3.org/2006/vcard/ns#"));
    let sn_schema = unwrap!(Uri::new("http://www.snee.com/hr/"));
    let vcard_schema = unwrap!(Uri::new("http://www.w3.org/2006/vcard/ns#"));

    //Serializes to Turtle format
    let serializer = unwrap!(Serializer::new("turtle", None, None));
    unwrap!(serializer.set_namespace(&sn_schema, "sn"));
    unwrap!(serializer.set_namespace(&vcard_schema, "vcard"));

    //Serializes to RDFXML format
    let serializer2 = unwrap!(Serializer::new("rdfxml", None, None));
    unwrap!(serializer2.set_namespace(&sn_schema, "sn"));
    unwrap!(serializer2.set_namespace(&vcard_schema, "vcard"));

    let parser = unwrap!(Parser::new(mime_type));
    let _res = Parser::parse_string(parser, turtle_code, base_uri, &model);

    let result = serializer.serialize_model_to_string(&model);
    let result2 = serializer2.serialize_model_to_string(&model);

    println!("{}", unwrap!(result));
    println!("{}", unwrap!(result2));
}
