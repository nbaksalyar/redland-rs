#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unsafe_code)]
#![allow(unused_assignments, dead_code)]

#[macro_use]
extern crate foreign_types;
#[macro_use]
extern crate lazy_static;
extern crate libc;
extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate unwrap;
extern crate redland_sys;

mod kv_storage;
use foreign_types::{ForeignType, ForeignTypeRef};
pub use kv_storage::{EntryAction, KvStorage};
use libc::{c_char, fdopen};
use redland_sys::*;
use std::os::unix::io::AsRawFd;
use std::sync::Mutex;
use std::{
    ffi::{CStr, CString},
    fmt,
    marker::PhantomData,
    mem, ptr,
};

foreign_type! {
    pub type World: Sync + Send {
        type CType = librdf_world;
        fn drop = librdf_free_world;
    }

    pub type Model {
        type CType = librdf_model;
        fn drop = librdf_free_model;
        fn clone = librdf_new_model_from_model;
    }

    pub type Statement {
        type CType = librdf_statement;
        fn drop = librdf_free_statement;
        fn clone = librdf_new_statement_from_statement;
    }

    pub type Node {
        type CType = librdf_node;
        fn drop = librdf_free_node;
        fn clone = librdf_new_node_from_node;
    }

    pub type Query {
        type CType = librdf_query;
        fn drop = librdf_free_query;
        fn clone = librdf_new_query_from_query;
    }

    pub type QueryResult {
        type CType = librdf_query_results;
        fn drop = librdf_free_query_results;
    }

    pub type Uri {
        type CType = librdf_uri;
        fn drop = librdf_free_uri;
        fn clone = librdf_new_uri_from_uri;
    }

    pub type Serializer {
        type CType = librdf_serializer;
        fn drop = librdf_free_serializer;
    }

    pub type Parser {
        type CType = librdf_parser;
        fn drop = librdf_free_parser;
    }
}

lazy_static! {
    pub(crate) static ref WORLD: Mutex<World> = {
        let w = World::new();
        Mutex::new(w)
    };
}

impl World {
    pub fn new() -> Self {
        unsafe {
            let world_ptr = librdf_new_world();
            librdf_world_open(world_ptr);
            World::from_ptr(world_ptr)
        }
    }
}

impl Default for World {
    fn default() -> Self {
        Self::new()
    }
}

impl Model {
    pub fn new(storage: &KvStorage) -> Result<Self, i32> {
        unsafe { Self::from_raw_storage(storage.as_ptr()) }
    }

    pub unsafe fn from_raw_storage(storage: *mut librdf_storage) -> Result<Self, i32> {
        let res = librdf_new_model(unwrap!(WORLD.lock()).as_ptr(), storage, ptr::null());
        if res.is_null() {
            return Err(-1);
        }
        Ok(Model::from_ptr(res))
    }

    pub fn add(&self, subject: &Node, predicate: &Node, object: &Node) -> Result<(), i32> {
        let res = unsafe {
            librdf_model_add(
                self.as_ptr(),
                subject.as_ptr(),
                predicate.as_ptr(),
                object.as_ptr(),
            )
        };
        if res != 0 {
            return Err(res);
        }
        Ok(())
    }

    pub fn add_statement(&self, statement: &Statement) -> Result<(), i32> {
        let res = unsafe { librdf_model_add_statement(self.as_ptr(), statement.as_ptr()) };
        if res != 0 {
            return Err(res);
        }
        Ok(())
    }

    pub fn add_string_literal_statement<S: Into<Vec<u8>>>(
        &self,
        subject: &Node,
        predicate: &Node,
        literal: S,
        xml_language: Option<S>,
        is_xml: bool,
    ) -> Result<(), i32> {
        let literal_cstr = CString::new(literal).map_err(|_| -1)?;
        let xml_lang_cstr = xml_language.map(|s| CString::new(s).map_err(|_| -1));
        let res = unsafe {
            librdf_model_add_string_literal_statement(
                self.as_ptr(),
                subject.as_ptr(),
                predicate.as_ptr(),
                literal_cstr.as_ptr() as *const _,
                if let Some(xml_lang) = xml_lang_cstr {
                    xml_lang?.as_ptr() as *const _
                } else {
                    ptr::null()
                },
                is_xml as i32,
            )
        };
        if res != 0 {
            return Err(res);
        }
        Ok(())
    }
}

impl ModelRef {
    pub fn iter(&self) -> ModelIter {
        let stream = unsafe { librdf_model_as_stream(self.as_ptr()) };
        ModelIter {
            ptr: stream,
            first: true,
            _marker: PhantomData,
        }
    }

    pub fn len(&self) -> i32 {
        unsafe { librdf_model_size(self.as_ptr()) }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

pub struct ModelIter<'a> {
    ptr: *mut librdf_stream,
    first: bool,
    _marker: PhantomData<&'a ()>,
}

impl<'a> Iterator for ModelIter<'a> {
    type Item = &'a StatementRef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.first {
            self.first = false;
        } else {
            let res = unsafe { librdf_stream_next(self.ptr) };
            if res != 0 {
                // Stream has ended
                return None;
            }
        }
        let stmt = unsafe { StatementRef::from_ptr(librdf_stream_get_object(self.ptr)) };
        Some(stmt)
    }
}

impl<'a> Drop for ModelIter<'a> {
    fn drop(&mut self) {
        unsafe { librdf_free_stream(self.ptr) };
    }
}

impl Query {
    /// Creates a new query object, using a provided query language.
    pub fn new<S: Into<Vec<u8>>>(
        query_language: S,
        query_string: S,
        base_uri: Option<&Uri>,
    ) -> Result<Self, i32> {
        let c_query_string = CString::new(query_string).map_err(|_| -1)?;
        let c_query_lang = CString::new(query_language).map_err(|_| -1)?;

        let res = unsafe {
            librdf_new_query(
                unwrap!(WORLD.lock()).as_ptr(),
                c_query_lang.as_ptr(),
                ptr::null_mut(),
                c_query_string.as_ptr() as *const _,
                base_uri
                    .as_ref()
                    .map_or_else(ptr::null_mut, ForeignType::as_ptr),
            )
        };
        if res.is_null() {
            return Err(-1);
        }
        Ok(unsafe { Query::from_ptr(res) })
    }

    pub fn execute(&self, model: &Model) -> Result<QueryResult, i32> {
        unsafe {
            let query_res = librdf_query_execute(self.as_ptr() as *mut _, model.as_ptr() as *mut _);
            if query_res == ptr::null_mut() {
                return Err(-1);
            }
            Ok(QueryResult::from_ptr(query_res))
        }
    }
}

impl QueryResult {
    pub fn to_string(
        &self,
        name: &str,
        mime_type: &str,
        base_uri: Option<&Uri>,
    ) -> Result<String, i32> {
        unsafe {
            let converted_name = CString::new(name).map_err(|_| -1)?;
            let converted_mime = CString::new(mime_type).map_err(|_| -1)?;
            let result = librdf_query_results_to_string2(
                self.as_ptr() as *mut _,
                converted_name.as_ptr() as *mut _,
                converted_mime.as_ptr() as *mut _,
                ptr::null_mut(),
                base_uri.as_ref().map_or_else(ptr::null_mut, |u| u.as_ptr()),
            );
            if result == ptr::null_mut() {
                return Err(-1);
            } else {
                let res = if let Ok(string) = CStr::from_ptr(result as *const c_char).to_str() {
                    Some(string.to_owned())
                } else {
                    None
                };
                return res.ok_or(-1);
            }
        }
    }
}

impl Statement {
    pub fn new() -> Result<Self, i32> {
        let res = unsafe { librdf_new_statement(unwrap!(WORLD.lock()).as_ptr()) };
        if res.is_null() {
            return Err(-1);
        }
        Ok(unsafe { Statement::from_ptr(res) })
    }
}

impl StatementRef {
    pub fn subject(&self) -> &NodeRef {
        // Shared ownership for Node
        unsafe { NodeRef::from_ptr(librdf_statement_get_subject(self.as_ptr())) }
    }

    pub fn set_subject(&mut self, node: Node) {
        unsafe { librdf_statement_set_subject(self.as_ptr(), node.as_ptr()) };

        // Do not drop Node - it's owned by the statement now
        mem::forget(node);
    }

    pub fn predicate(&self) -> &NodeRef {
        unsafe { NodeRef::from_ptr(librdf_statement_get_predicate(self.as_ptr())) }
    }

    pub fn set_predicate(&mut self, node: Node) {
        unsafe { librdf_statement_set_predicate(self.as_ptr(), node.as_ptr()) };
        mem::forget(node);
    }

    pub fn object(&self) -> &NodeRef {
        unsafe { NodeRef::from_ptr(librdf_statement_get_object(self.as_ptr())) }
    }

    pub fn set_object(&mut self, node: Node) {
        unsafe { librdf_statement_set_object(self.as_ptr(), node.as_ptr()) };
        mem::forget(node);
    }
}

impl PartialEq for StatementRef {
    fn eq(&self, other: &StatementRef) -> bool {
        unsafe { librdf_statement_equals(self.as_ptr(), other.as_ptr()) != 0 }
    }
}

impl fmt::Debug for StatementRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let cstr = unsafe { CStr::from_ptr(librdf_statement_to_string(self.as_ptr()) as *const _) };
        write!(f, "{}", cstr.to_string_lossy())
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        StatementRef::fmt(&*self, f)
    }
}

impl Uri {
    pub fn new<S: Into<Vec<u8>>>(uri: S) -> Result<Self, i32> {
        let cstr_uri = CString::new(uri).map_err(|_| -1)?;
        let res = unsafe {
            librdf_new_uri(
                unwrap!(WORLD.lock()).as_ptr(),
                cstr_uri.as_ptr() as *const _,
            )
        };
        if res.is_null() {
            return Err(-1);
        }
        Ok(unsafe { Uri::from_ptr(res) })
    }
}

impl fmt::Debug for NodeRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let cstr = unsafe { CStr::from_ptr(librdf_node_to_string(self.as_ptr()) as *const _) };
        write!(f, "{}", cstr.to_string_lossy())
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        NodeRef::fmt(&*self, f)
    }
}

impl PartialEq for NodeRef {
    fn eq(&self, other: &NodeRef) -> bool {
        unsafe { librdf_node_equals(self.as_ptr(), other.as_ptr()) != 0 }
    }
}

impl Node {
    pub fn new() -> Result<Self, i32> {
        let res = unsafe { librdf_new_node(unwrap!(WORLD.lock()).as_ptr()) };
        if res.is_null() {
            return Err(-1);
        }
        Ok(unsafe { Node::from_ptr(res) })
    }

    /// Creates a new Node from a literal/string
    pub fn new_from_literal<S: Into<Vec<u8>>>(
        string: S,
        xml_language: Option<S>,
        is_xml: bool,
    ) -> Result<Self, i32> {
        let cstr = CString::new(string).map_err(|_| -1)?;
        let xml_lang_cstr = xml_language.map(|s| CString::new(s).map_err(|_| -1));
        let res = unsafe {
            librdf_new_node_from_literal(
                unwrap!(WORLD.lock()).as_ptr(),
                cstr.as_ptr() as *const _,
                if let Some(xml_lang) = xml_lang_cstr {
                    xml_lang?.as_ptr() as *const _
                } else {
                    ptr::null()
                },
                is_xml as i32,
            )
        };
        if res.is_null() {
            return Err(-1);
        }
        Ok(unsafe { Node::from_ptr(res) })
    }

    /// Creates a new Node from a URI with an appended name
    pub fn new_from_uri_local_name<S: Into<Vec<u8>>>(
        uri: &Uri,
        local_name: S,
    ) -> Result<Self, i32> {
        let cstr_local_name = CString::new(local_name).map_err(|_| -1)?;
        let res = unsafe {
            librdf_new_node_from_uri_local_name(
                unwrap!(WORLD.lock()).as_ptr(),
                uri.as_ptr(),
                cstr_local_name.as_ptr() as *const _,
            )
        };
        if res.is_null() {
            return Err(-1);
        }
        Ok(unsafe { Node::from_ptr(res) })
    }

    /// Creates a new Node from a URI
    pub fn new_from_uri(uri: &Uri) -> Result<Self, i32> {
        let res = unsafe { librdf_new_node_from_uri(unwrap!(WORLD.lock()).as_ptr(), uri.as_ptr()) };
        if res.is_null() {
            return Err(-1);
        }
        Ok(unsafe { Node::from_ptr(res) })
    }
}

impl Parser {
    pub fn new(mime_type: &str) -> Result<Self, i32> {
        let mime = CString::new(mime_type).map_err(|_| -1)?;
        unsafe {
            let parser = librdf_new_parser(
                unwrap!(WORLD.lock()).as_ptr(),
                ptr::null(),
                mime.as_ptr() as *mut _,
                ptr::null_mut(),
            );
            if parser.is_null() {
                return Err(-1);
            }
            Ok(Parser::from_ptr(parser))
        }
    }

    pub fn parse_string(
        parser: Parser,
        syntax: &str,
        base_uri: Uri,
        model: &Model,
    ) -> Result<(), i32> {
        let converted_syntax = CString::new(syntax).map_err(|_| -1)?;
        unsafe {
            let res = librdf_parser_parse_string_into_model(
                parser.as_ptr(),
                converted_syntax.as_ptr() as *mut _,
                base_uri.as_ptr(),
                model.as_ptr(),
            );
            if res > 0 {
                return Err(-1);
            }
            Ok(())
        }
    }

    pub fn parse_from_file(
        parser: Parser,
        file_handle: &std::fs::File,
        base_uri: &Uri,
        model: &Model,
    ) -> Result<(), i32> {
        unsafe {
            #[cfg(unix)]
            let c_file = fdopen(
                file_handle.as_raw_fd(),
                CStr::from_bytes_with_nul_unchecked(b"r\0").as_ptr() as *const i8,
            );
            #[cfg(windows)]
            let c_file = fdopen(
                file_handle.as_raw_handle(),
                CStr::from_bytes_with_nul_unchecked(b"r\0").as_ptr() as *const i8,
            );

            let res = librdf_parser_parse_file_handle_into_model(
                parser.as_ptr(),
                c_file as *mut _,
                0,
                base_uri.as_ptr(),
                model.as_ptr(),
            );

            if res != 0 {
                return Err(-1);
            }

            Ok(())
        }
    }
}

impl Serializer {
    pub fn new<S: Into<Vec<u8>>>(
        name: S,
        mime_type: Option<S>,
        type_uri: Option<&Uri>,
    ) -> Result<Self, i32> {
        let c_name = CString::new(name).map_err(|_| -1)?;
        let c_mime = mime_type.map(|s| CString::new(s).map_err(|_| -1));
        let ser = unsafe {
            librdf_new_serializer(
                unwrap!(WORLD.lock()).as_ptr(),
                c_name.as_ptr(),
                if let Some(mime) = c_mime {
                    mime?.as_ptr() as *const _
                } else {
                    ptr::null()
                },
                type_uri.as_ref().map_or_else(ptr::null_mut, |u| u.as_ptr()),
            )
        };
        if ser.is_null() {
            return Err(-1);
        }
        Ok(unsafe { Serializer::from_ptr(ser) })
    }

    pub fn set_namespace<S: Into<Vec<u8>>>(&self, uri: &Uri, prefix: S) -> Result<(), i32> {
        let c_prefix = CString::new(prefix).map_err(|_| -1)?;
        let res = unsafe {
            librdf_serializer_set_namespace(self.as_ptr(), uri.as_ptr(), c_prefix.as_ptr())
        };
        if res != 0 {
            return Err(res);
        }
        Ok(())
    }

    pub fn serialize_model_to_string(&self, model: &Model) -> Result<String, i32> {
        unsafe {
            let result = librdf_serializer_serialize_model_to_string(
                self.as_ptr(),
                ptr::null_mut(),
                model.as_ptr(),
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

            res.ok_or(-1)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{KvStorage, Model, Node, Statement, Uri};

    #[test]
    fn statement_constructor() {
        let uri1 = unwrap!(Uri::new("https://localhost/#dolly"));

        let s = unwrap!(Node::new_from_uri(&uri1));

        let mut triple = unwrap!(Statement::new());
        triple.set_subject(s); // `s` moved to `triple`

        let s = triple.subject();
        println!("S: {:?}", s);
    }

    #[test]
    fn model_iterator() {
        let storage = unwrap!(KvStorage::new());
        let model = unwrap!(Model::new(&storage));

        let uri1 = unwrap!(Uri::new("https://localhost/#dolly"));
        let uri2 = unwrap!(Uri::new("https://localhost/#hears"));

        let mut triple1 = unwrap!(Statement::new());
        triple1.set_subject(unwrap!(Node::new_from_uri(&uri1)));
        triple1.set_predicate(unwrap!(Node::new_from_uri(&uri2)));
        triple1.set_object(unwrap!(Node::new_from_literal("hello", None, false)));

        let mut triple2 = unwrap!(Statement::new());
        triple2.set_subject(unwrap!(Node::new_from_uri(&uri1)));
        triple2.set_predicate(unwrap!(Node::new_from_uri(&uri2)));
        triple2.set_object(unwrap!(Node::new_from_literal("goodbye", None, false)));

        unwrap!(model.add_statement(&triple1));
        unwrap!(model.add_statement(&triple2));

        // println!("{:?}, {:?}", triple1, triple2);

        assert_eq!(model.len(), 2);

        // Test the iterator
        let mut iter = model.iter();
        assert_eq!(unwrap!(iter.next()), &*triple2);
        assert_eq!(unwrap!(iter.next()), &*triple1);
        assert!(iter.next().is_none());
    }
}
