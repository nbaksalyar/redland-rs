#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unsafe_code)]
#![allow(unused_assignments)]

#[macro_use]
extern crate foreign_types;
extern crate libc;
extern crate serde;
#[macro_use]
extern crate serde_derive;
#[cfg(test)]
#[macro_use]
extern crate unwrap;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

mod kv_storage;

use foreign_types::{ForeignType, ForeignTypeRef};
pub use kv_storage::{EntryAction, KvStorage};
use libc::c_char;
use std::{
    ffi::{CStr, CString},
    fmt,
    marker::PhantomData,
    mem, ptr,
};

pub struct World(*mut librdf_world);

impl World {
    pub fn new() -> Self {
        World(unsafe { librdf_new_world() })
    }

    pub fn as_mut_ptr(&self) -> *mut librdf_world {
        self.0
    }
}

impl Drop for World {
    fn drop(&mut self) {
        unsafe {
            librdf_free_world(self.0);
        }
    }
}

pub struct Model(*mut librdf_model);

impl Model {
    pub fn new(world: &World, storage: &KvStorage) -> Result<Self, i32> {
        unsafe { Self::from_raw_storage(world, storage.as_mut_ptr()) }
    }

    pub fn len(&self) -> i32 {
        unsafe { librdf_model_size(self.0) }
    }

    pub unsafe fn from_raw_storage(
        world: &World,
        storage: *mut librdf_storage,
    ) -> Result<Self, i32> {
        let res = librdf_new_model(world.as_mut_ptr(), storage, ptr::null());
        if res.is_null() {
            return Err(-1);
        }
        Ok(Model(res))
    }

    pub fn add(&self, subject: &Node, predicate: &Node, object: &Node) -> Result<(), i32> {
        let res = unsafe {
            librdf_model_add(
                self.0,
                subject.as_ptr(),
                predicate.as_ptr(),
                object.as_ptr(),
            )
        };
        if res != 0 {
            return Err(res);
        }
        return Ok(());
    }

    pub fn add_statement(&self, statement: &Statement) -> Result<(), i32> {
        let res = unsafe { librdf_model_add_statement(self.0, statement.as_ptr()) };
        if res != 0 {
            return Err(res);
        }
        return Ok(());
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
                self.0,
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
        return Ok(());
    }

    pub fn as_mut_ptr(&self) -> *mut librdf_model {
        self.0
    }

    pub fn iter(&self) -> ModelIter {
        let stream = unsafe { librdf_model_as_stream(self.as_mut_ptr()) };
        ModelIter {
            ptr: stream,
            first: true,
            _marker: PhantomData,
        }
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

impl Drop for Model {
    fn drop(&mut self) {
        unsafe {
            librdf_free_model(self.0);
        }
    }
}

foreign_type! {
    pub type Statement {
        type CType = librdf_statement;
        fn drop = librdf_free_statement;
    }

    pub type Node {
        type CType = librdf_node;
        fn drop = librdf_free_node;
    }
}

impl Statement {
    pub fn new(world: &World) -> Result<Self, i32> {
        let res = unsafe { librdf_new_statement(world.as_mut_ptr()) };
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

pub struct Uri(*mut librdf_uri);

impl Uri {
    pub fn new<S: Into<Vec<u8>>>(world: &World, uri: S) -> Result<Self, i32> {
        let cstr_uri = CString::new(uri).map_err(|_| -1)?;
        let res = unsafe { librdf_new_uri(world.as_mut_ptr(), cstr_uri.as_ptr() as *const _) };
        if res.is_null() {
            return Err(-1);
        }
        Ok(Uri(res))
    }

    pub fn as_mut_ptr(&self) -> *mut librdf_uri {
        self.0
    }
}

impl Drop for Uri {
    fn drop(&mut self) {
        unsafe {
            librdf_free_uri(self.0);
        }
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
    pub fn new(world: &World) -> Result<Self, i32> {
        let res = unsafe { librdf_new_node(world.as_mut_ptr()) };
        if res.is_null() {
            return Err(-1);
        }
        Ok(unsafe { Node::from_ptr(res) })
    }

    /// Creates a new Node from a literal/string
    pub fn new_from_literal<S: Into<Vec<u8>>>(
        world: &World,
        string: S,
        xml_language: Option<S>,
        is_xml: bool,
    ) -> Result<Self, i32> {
        let cstr = CString::new(string).map_err(|_| -1)?;
        let xml_lang_cstr = xml_language.map(|s| CString::new(s).map_err(|_| -1));
        let res = unsafe {
            librdf_new_node_from_literal(
                world.as_mut_ptr(),
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
        world: &World,
        uri: &Uri,
        local_name: S,
    ) -> Result<Self, i32> {
        let cstr_local_name = CString::new(local_name).map_err(|_| -1)?;
        let res = unsafe {
            librdf_new_node_from_uri_local_name(
                world.as_mut_ptr(),
                uri.as_mut_ptr(),
                cstr_local_name.as_ptr() as *const _,
            )
        };
        if res.is_null() {
            return Err(-1);
        }
        Ok(unsafe { Node::from_ptr(res) })
    }

    /// Creates a new Node from a URI
    pub fn new_from_uri(world: &World, uri: &Uri) -> Result<Self, i32> {
        let res = unsafe { librdf_new_node_from_uri(world.as_mut_ptr(), uri.as_mut_ptr()) };
        if res.is_null() {
            return Err(-1);
        }
        Ok(unsafe { Node::from_ptr(res) })
    }
}

pub struct Serializer(pub *mut librdf_serializer);

impl Serializer {
    pub fn set_namespace<S: Into<Vec<u8>>>(&self, uri: &Uri, prefix: S) -> Result<(), i32> {
        let c_prefix = CString::new(prefix).map_err(|_| -1)?;
        let res =
            unsafe { librdf_serializer_set_namespace(self.0, uri.as_mut_ptr(), c_prefix.as_ptr()) };
        if res != 0 {
            return Err(res);
        }
        Ok(())
    }

    pub fn serialize_model_to_string(&self, model: &Model) -> Result<String, i32> {
        unsafe {
            let result = librdf_serializer_serialize_model_to_string(
                self.0,
                ptr::null_mut(),
                model.as_mut_ptr(),
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

            return res.ok_or(-1);
        }
    }
}

impl Drop for Serializer {
    fn drop(&mut self) {
        unsafe {
            librdf_free_serializer(self.0);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{KvStorage, Model, Node, Statement, Uri, World};

    #[test]
    fn statement_constructor() {
        let w = World::new();

        let uri1 = unwrap!(Uri::new(&w, "https://localhost/#dolly"));
        let uri2 = unwrap!(Uri::new(&w, "https://localhost/#hears"));

        let s = unwrap!(Node::new_from_uri(&w, &uri1));
        let p = unwrap!(Node::new_from_uri(&w, &uri2));
        let o = unwrap!(Node::new_from_literal(&w, "hello", None, false));

        let mut triple = unwrap!(Statement::new(&w));
        triple.set_subject(s); // `s` moved to `triple`

        let s = triple.subject();
        println!("S: {:?}", s);
    }

    #[test]
    fn model_iterator() {
        let w = World::new();
        let storage = unwrap!(KvStorage::new(&w));
        let model = unwrap!(Model::new(&w, &storage));

        let uri1 = unwrap!(Uri::new(&w, "https://localhost/#dolly"));
        let uri2 = unwrap!(Uri::new(&w, "https://localhost/#hears"));

        let mut triple1 = unwrap!(Statement::new(&w));
        triple1.set_subject(unwrap!(Node::new_from_uri(&w, &uri1)));
        triple1.set_predicate(unwrap!(Node::new_from_uri(&w, &uri2)));
        triple1.set_object(unwrap!(Node::new_from_literal(&w, "hello", None, false)));

        let mut triple2 = unwrap!(Statement::new(&w));
        triple2.set_subject(unwrap!(Node::new_from_uri(&w, &uri1)));
        triple2.set_predicate(unwrap!(Node::new_from_uri(&w, &uri2)));
        triple2.set_object(unwrap!(Node::new_from_literal(&w, "goodbye", None, false)));

        unwrap!(model.add_statement(&triple1));
        unwrap!(model.add_statement(&triple2));

        assert_eq!(model.len(), 2);

        // Test the iterator
        let mut iter = model.iter();
        assert_eq!(unwrap!(iter.next()), &*triple2);
        assert_eq!(unwrap!(iter.next()), &*triple1);
        assert!(iter.next().is_none());
    }
}
