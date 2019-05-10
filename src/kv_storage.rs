//! Redland storage implementation based on key/value representation.
//! Converted from C code into Rust using C2Rust.

// We ignore some clippy lints because most of this code was automatically generated.
#![allow(
    clippy::zero_ptr,
    clippy::toplevel_ref_arg,
    clippy::assign_op_pattern,
    clippy::single_match,
    clippy::nonminimal_bool,
    clippy::if_same_then_else,
    clippy::cast_lossless,
    clippy::transmute_ptr_to_ptr,
    clippy::needless_return,
    clippy::unreadable_literal,
    clippy::cast_ptr_alignment,
    dead_code
)]

use crate::*;
use libc::c_char;
use std::ptr;
use std::slice;

pub struct KvStorage(*mut self::librdf_storage);

impl KvStorage {
    pub fn new() -> Result<Self, i32> {
        let world = { unwrap!(WORLD.lock()).as_ptr() };

        unsafe {
            librdf_init_storage_hashes(world);
        }

        let storage = unsafe {
            librdf_new_storage(
                world,
                b"mdata\0" as *const _ as *const c_char,
                b"mdata\0" as *const _ as *const c_char,
                b"hash-type='memory'\0" as *const _ as *const c_char,
            )
        };

        if storage.is_null() {
            return Err(-1);
        }

        Ok(KvStorage(storage as *mut _))
    }

    pub fn entry_actions<'a>(&self) -> &'a [EntryAction] {
        unsafe {
            let context: *mut librdf_storage_hashes_instance =
                (*self.0).instance as *mut librdf_storage_hashes_instance;

            &(*(*context).mdata_context).entry_actions
        }
    }

    pub fn copy_entries(&mut self, eas: &mut [EntryAction]) -> Result<(), i32> {
        let context: *mut librdf_storage_hashes_instance =
            unsafe { (*self.0).instance as *mut librdf_storage_hashes_instance };

        let mut hd_key: librdf_hash_datum = Default::default();
        let mut hd_value: librdf_hash_datum = Default::default();

        for action in eas {
            match action {
                EntryAction::Insert(ref i, ref mut key, ref mut value) => {
                    hd_key.data = key.as_mut_ptr() as *mut _;
                    hd_key.size = key.len() as u64;

                    hd_value.data = value.as_mut_ptr() as *mut _;
                    hd_value.size = value.len() as u64;

                    let status = unsafe {
                        librdf_hash_put(
                            *(*context).hashes.offset(*i as isize),
                            &mut hd_key,
                            &mut hd_value,
                        )
                    };
                    if status != 0 {
                        return Err(-1);
                    }
                }
                EntryAction::Delete(_i, _key) => {}
            }
        }

        Ok(())
    }

    pub fn as_ptr(&self) -> *mut super::librdf_storage {
        self.0 as *mut _
    }
}

impl Drop for KvStorage {
    fn drop(&mut self) {
        unsafe { librdf_free_storage(self.0 as *mut _) }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EntryAction {
    Insert(i32, Vec<u8>, Vec<u8>),
    Delete(i32, Vec<u8>),
}

pub struct MDataContext {
    entry_actions: Vec<EntryAction>,
}

extern "C" {
    /* Required for va_list in raptor_vsnprintf */
    /* *
     * RAPTOR_V2_AVAILABLE
     *
     * Flag for marking raptor2 API availability.
     */
    /* *
     * RAPTOR_VERSION:
     *
     * Raptor library version number
     *
     * Format: major * 10000 + minor * 100 + release
     */
    /* *
     * RAPTOR_VERSION_STRING:
     *
     * Raptor library version string
     */
    /* *
     * RAPTOR_VERSION_MAJOR:
     *
     * Raptor library major version
     */
    /* *
     * RAPTOR_VERSION_MINOR:
     *
     * Raptor library minor version
     */
    /* *
     * RAPTOR_VERSION_RELEASE:
     *
     * Raptor library release
     */
    /* *
     * RAPTOR_API:
     *
     * Macro for wrapping API function call declarations.
     *
     */
    #[no_mangle]
    static mut stderr: *mut _IO_FILE;
    #[no_mangle]
    fn fprintf(_: *mut FILE, _: *const libc::c_char, ...) -> libc::c_int;
    #[no_mangle]
    fn sprintf(_: *mut libc::c_char, _: *const libc::c_char, ...) -> libc::c_int;
    #[no_mangle]
    fn memcpy(_: *mut libc::c_void, _: *const libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn strcpy(_: *mut libc::c_char, _: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn strcmp(_: *const libc::c_char, _: *const libc::c_char) -> libc::c_int;
    #[no_mangle]
    fn strdup(_: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn strlen(_: *const libc::c_char) -> libc::c_ulong;
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn free(__ptr: *mut libc::c_void);
    #[no_mangle]
    fn librdf_new_iterator(
        world: *mut librdf_world,
        context: *mut libc::c_void,
        is_end_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
        next_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
        get_method: Option<
            unsafe extern "C" fn(_: *mut libc::c_void, _: libc::c_int) -> *mut libc::c_void,
        >,
        finished_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> ()>,
    ) -> *mut librdf_iterator;
    #[no_mangle]
    fn librdf_free_iterator(iterator: *mut librdf_iterator);
    #[no_mangle]
    fn librdf_iterator_end(iterator: *mut librdf_iterator) -> libc::c_int;
    #[no_mangle]
    fn librdf_iterator_next(iterator: *mut librdf_iterator) -> libc::c_int;
    #[no_mangle]
    fn librdf_iterator_get_object(iterator: *mut librdf_iterator) -> *mut libc::c_void;
    #[no_mangle]
    fn librdf_iterator_get_context(iterator: *mut librdf_iterator) -> *mut libc::c_void;
    #[no_mangle]
    fn librdf_iterator_get_key(iterator: *mut librdf_iterator) -> *mut libc::c_void;
    #[no_mangle]
    fn librdf_iterator_get_value(iterator: *mut librdf_iterator) -> *mut libc::c_void;
    #[no_mangle]
    fn librdf_new_empty_iterator(world: *mut librdf_world) -> *mut librdf_iterator;
    /* -*- Mode: c; c-basic-offset: 2 -*-
     *
     * rdf_heuristics.h - Heuristic routines to guess things about RDF prototypes
     *
     * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
     * Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
     *
     * This package is Free Software and part of Redland http://librdf.org/
     *
     * It is licensed under the following three licenses as alternatives:
     *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
     *   2. GNU General Public License (GPL) V2 or any newer version
     *   3. Apache License, V2.0 or any newer version
     *
     * You may not use this file except in compliance with at least one of
     * the above three licenses.
     *
     * See LICENSE.html or LICENSE.txt at the top of this package for the
     * complete terms and further detail along with the license texts for
     * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
     *
     *
     */
    #[no_mangle]
    fn librdf_heuristic_gen_name(name: *const libc::c_char) -> *mut libc::c_char;
    #[no_mangle]
    fn librdf_log(
        world: *mut librdf_world,
        code: libc::c_int,
        level: librdf_log_level,
        facility: librdf_log_facility,
        locator: *mut libc::c_void,
        message: *const libc::c_char,
        ...
    );
    /* constructor / destructor for above */
    #[no_mangle]
    fn librdf_new_hash_datum(
        world: *mut librdf_world,
        data: *mut libc::c_void,
        size: size_t,
    ) -> *mut librdf_hash_datum;
    #[no_mangle]
    fn librdf_free_hash_datum(ptr: *mut librdf_hash_datum);
    /* methods */
    /* open/create hash with identifier and options  */
    #[no_mangle]
    fn librdf_hash_open(
        hash: *mut librdf_hash,
        identifier: *const libc::c_char,
        mode: libc::c_int,
        is_writable: libc::c_int,
        is_new: libc::c_int,
        options: *mut librdf_hash,
    ) -> libc::c_int;
    /* end hash association */
    #[no_mangle]
    fn librdf_hash_close(hash: *mut librdf_hash) -> libc::c_int;
    /* how many values */
    #[no_mangle]
    fn librdf_hash_values_count(hash: *mut librdf_hash) -> libc::c_int;
    /* retrieve all values for a given hash key according to flags */
    #[no_mangle]
    fn librdf_hash_get_all(
        hash: *mut librdf_hash,
        key: *mut librdf_hash_datum,
        value: *mut librdf_hash_datum,
    ) -> *mut librdf_iterator;
    /* insert a key/value pair */
    #[no_mangle]
    fn librdf_hash_put(
        hash: *mut librdf_hash,
        key: *mut librdf_hash_datum,
        value: *mut librdf_hash_datum,
    ) -> libc::c_int;
    /* returns true if key exists in hash, without returning value */
    #[no_mangle]
    fn librdf_hash_exists(
        hash: *mut librdf_hash,
        key: *mut librdf_hash_datum,
        value: *mut librdf_hash_datum,
    ) -> libc::c_int;
    #[no_mangle]
    fn librdf_hash_delete(
        hash: *mut librdf_hash,
        key: *mut librdf_hash_datum,
        value: *mut librdf_hash_datum,
    ) -> libc::c_int;
    #[no_mangle]
    fn librdf_hash_keys(
        hash: *mut librdf_hash,
        key: *mut librdf_hash_datum,
    ) -> *mut librdf_iterator;
    /* flush any cached information to disk */
    #[no_mangle]
    fn librdf_hash_sync(hash: *mut librdf_hash) -> libc::c_int;
    /* -*- Mode: c; c-basic-offset: 2 -*-
     *
     * rdf_hash.h - RDF Hash Factory and Hash interfaces and definitions
     *
     * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
     * Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
     *
     * This package is Free Software and part of Redland http://librdf.org/
     *
     * It is licensed under the following three licenses as alternatives:
     *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
     *   2. GNU General Public License (GPL) V2 or any newer version
     *   3. Apache License, V2.0 or any newer version
     *
     * You may not use this file except in compliance with at least one of
     * the above three licenses.
     *
     * See LICENSE.html or LICENSE.txt at the top of this package for the
     * complete terms and further detail along with the license texts for
     * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
     *
     *
     */
    /* public constructors */
    #[no_mangle]
    fn librdf_new_hash(world: *mut librdf_world, name: *const libc::c_char) -> *mut librdf_hash;
    /* public copy constructor */
    #[no_mangle]
    fn librdf_new_hash_from_hash(old_hash: *mut librdf_hash) -> *mut librdf_hash;
    /* public destructor */
    #[no_mangle]
    fn librdf_free_hash(hash: *mut librdf_hash);
    /* lookup a hash key and decode value as a boolean */
    #[no_mangle]
    fn librdf_hash_get_as_boolean(hash: *mut librdf_hash, key: *const libc::c_char) -> libc::c_int;
    /* lookup a hash key and decode value as a long */
    #[no_mangle]
    fn librdf_hash_get_as_long(hash: *mut librdf_hash, key: *const libc::c_char) -> libc::c_long;
    /* retrieve one value for key and delete from hash all other values */
    #[no_mangle]
    fn librdf_hash_get_del(hash: *mut librdf_hash, key: *const libc::c_char) -> *mut libc::c_char;
    /* methods */
    #[no_mangle]
    fn librdf_uri_as_string(uri: *mut librdf_uri) -> *mut libc::c_uchar;
    /* Create a new Node from a typed literal string / language. */
    #[no_mangle]
    fn librdf_new_node_from_typed_literal(
        world: *mut librdf_world,
        value: *const libc::c_uchar,
        xml_language: *const libc::c_char,
        datatype_uri: *mut librdf_uri,
    ) -> *mut librdf_node;
    /* Create a new Node from an existing Node - CLONE */
    #[no_mangle]
    fn librdf_new_node_from_node(node: *mut librdf_node) -> *mut librdf_node;
    /* destructor */
    #[no_mangle]
    fn librdf_free_node(node: *mut librdf_node);
    /* serialise / deserialise */
    #[no_mangle]
    fn librdf_node_encode(
        node: *mut librdf_node,
        buffer: *mut libc::c_uchar,
        length: size_t,
    ) -> size_t;
    #[no_mangle]
    fn librdf_node_decode(
        world: *mut librdf_world,
        size_p: *mut size_t,
        buffer: *mut libc::c_uchar,
        length: size_t,
    ) -> *mut librdf_node;
    /* Create a new Statement from an existing Statement - DEEP CLONE */
    #[no_mangle]
    fn librdf_new_statement_from_statement(
        statement: *mut librdf_statement,
    ) -> *mut librdf_statement;
    /* Init a statically allocated statement */
    #[no_mangle]
    fn librdf_statement_init(world: *mut librdf_world, statement: *mut librdf_statement);
    /* Clear a statically allocated statement */
    #[no_mangle]
    fn librdf_statement_clear(statement: *mut librdf_statement);
    /* destructor */
    #[no_mangle]
    fn librdf_free_statement(statement: *mut librdf_statement);
    /* functions / methods */
    #[no_mangle]
    fn librdf_statement_get_subject(statement: *mut librdf_statement) -> *mut librdf_node;
    #[no_mangle]
    fn librdf_statement_set_subject(statement: *mut librdf_statement, node: *mut librdf_node);
    #[no_mangle]
    fn librdf_statement_get_predicate(statement: *mut librdf_statement) -> *mut librdf_node;
    #[no_mangle]
    fn librdf_statement_set_predicate(statement: *mut librdf_statement, node: *mut librdf_node);
    #[no_mangle]
    fn librdf_statement_get_object(statement: *mut librdf_statement) -> *mut librdf_node;
    #[no_mangle]
    fn librdf_statement_set_object(statement: *mut librdf_statement, node: *mut librdf_node);
    #[no_mangle]
    fn librdf_statement_encode2(
        world: *mut librdf_world,
        statement: *mut librdf_statement,
        buffer: *mut libc::c_uchar,
        length: size_t,
    ) -> size_t;
    #[no_mangle]
    fn librdf_statement_encode_parts2(
        world: *mut librdf_world,
        statement: *mut librdf_statement,
        context_node: *mut librdf_node,
        buffer: *mut libc::c_uchar,
        length: size_t,
        fields: librdf_statement_part,
    ) -> size_t;
    #[no_mangle]
    fn librdf_statement_decode2(
        world: *mut librdf_world,
        statement: *mut librdf_statement,
        context_node: *mut *mut librdf_node,
        buffer: *mut libc::c_uchar,
        length: size_t,
    ) -> size_t;
    #[no_mangle]
    fn librdf_storage_remove_reference(storage: *mut librdf_storage);
    /* methods */
    #[no_mangle]
    fn librdf_storage_add_reference(storage: *mut librdf_storage);
    /* constructor */
    #[no_mangle]
    fn librdf_new_stream(
        world: *mut librdf_world,
        context: *mut libc::c_void,
        is_end_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
        next_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
        get_method: Option<
            unsafe extern "C" fn(_: *mut libc::c_void, _: libc::c_int) -> *mut libc::c_void,
        >,
        finished_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> ()>,
    ) -> *mut librdf_stream;
    #[no_mangle]
    fn librdf_new_empty_stream(world: *mut librdf_world) -> *mut librdf_stream;
    #[no_mangle]
    fn librdf_stream_statement_find_map(
        stream: *mut librdf_stream,
        context: *mut libc::c_void,
        statement: *mut librdf_statement,
    ) -> *mut librdf_statement;
    #[no_mangle]
    fn librdf_stream_add_map(
        stream: *mut librdf_stream,
        map_function: librdf_stream_map_handler,
        free_context: librdf_stream_map_free_context_handler,
        map_context: *mut libc::c_void,
    ) -> libc::c_int;
    /* destructor */
    #[no_mangle]
    fn librdf_free_stream(stream: *mut librdf_stream);
    /* methods */
    #[no_mangle]
    fn librdf_stream_end(stream: *mut librdf_stream) -> libc::c_int;
    #[no_mangle]
    fn librdf_stream_next(stream: *mut librdf_stream) -> libc::c_int;
    #[no_mangle]
    fn librdf_stream_get_object(stream: *mut librdf_stream) -> *mut librdf_statement;
    #[no_mangle]
    fn librdf_storage_set_instance(storage: *mut librdf_storage, instance: librdf_storage_instance);
    /* -*- Mode: c; c-basic-offset: 2 -*-
     *
     * rdf_storage.h - RDF Storage Factory and Storage interfaces and definitions
     *
     * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
     * Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
     *
     * This package is Free Software and part of Redland http://librdf.org/
     *
     * It is licensed under the following three licenses as alternatives:
     *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
     *   2. GNU General Public License (GPL) V2 or any newer version
     *   3. Apache License, V2.0 or any newer version
     *
     * You may not use this file except in compliance with at least one of
     * the above three licenses.
     *
     * See LICENSE.html or LICENSE.txt at the top of this package for the
     * complete terms and further detail along with the license texts for
     * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
     *
     *
     */
    /* class methods */
    #[no_mangle]
    fn librdf_storage_register_factory(
        world: *mut librdf_world,
        name: *const libc::c_char,
        label: *const libc::c_char,
        factory: Option<unsafe extern "C" fn(_: *mut librdf_storage_factory) -> ()>,
    ) -> libc::c_int;
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct __va_list_tag {
    pub gp_offset: libc::c_uint,
    pub fp_offset: libc::c_uint,
    pub overflow_arg_area: *mut libc::c_void,
    pub reg_save_area: *mut libc::c_void,
}
pub type __off_t = libc::c_long;
pub type __off64_t = libc::c_long;
pub type size_t = libc::c_ulong;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct _IO_FILE {
    pub _flags: libc::c_int,
    pub _IO_read_ptr: *mut libc::c_char,
    pub _IO_read_end: *mut libc::c_char,
    pub _IO_read_base: *mut libc::c_char,
    pub _IO_write_base: *mut libc::c_char,
    pub _IO_write_ptr: *mut libc::c_char,
    pub _IO_write_end: *mut libc::c_char,
    pub _IO_buf_base: *mut libc::c_char,
    pub _IO_buf_end: *mut libc::c_char,
    pub _IO_save_base: *mut libc::c_char,
    pub _IO_backup_base: *mut libc::c_char,
    pub _IO_save_end: *mut libc::c_char,
    pub _markers: *mut _IO_marker,
    pub _chain: *mut _IO_FILE,
    pub _fileno: libc::c_int,
    pub _flags2: libc::c_int,
    pub _old_offset: __off_t,
    pub _cur_column: libc::c_ushort,
    pub _vtable_offset: libc::c_schar,
    pub _shortbuf: [libc::c_char; 1],
    pub _lock: *mut libc::c_void,
    pub _offset: __off64_t,
    pub __pad1: *mut libc::c_void,
    pub __pad2: *mut libc::c_void,
    pub __pad3: *mut libc::c_void,
    pub __pad4: *mut libc::c_void,
    pub __pad5: size_t,
    pub _mode: libc::c_int,
    pub _unused2: [libc::c_char; 20],
}
pub type _IO_lock_t = ();
#[derive(Copy, Clone)]
#[repr(C)]
pub struct _IO_marker {
    pub _next: *mut _IO_marker,
    pub _sbuf: *mut _IO_FILE,
    pub _pos: libc::c_int,
}
pub type FILE = _IO_FILE;
pub type raptor_uri = raptor_uri_s;
pub type raptor_world = raptor_world_s;
pub type raptor_iostream = raptor_iostream_s;
/* *
 * raptor_term_type:
 * @RAPTOR_TERM_TYPE_URI: RDF URI
 * @RAPTOR_TERM_TYPE_LITERAL: RDF literal
 * @RAPTOR_TERM_TYPE_BLANK: RDF blank node
 * @RAPTOR_TERM_TYPE_UNKNOWN: Internal
 *
 * Type of term in a #raptor_statement
 *
 * Node type 3 is unused but exists to preserve numeric compatibility
 * with librdf_node_type values.
 */
pub type raptor_term_type = libc::c_uint;
/* unused type 3 */
pub const RAPTOR_TERM_TYPE_BLANK: raptor_term_type = 4;
pub const RAPTOR_TERM_TYPE_LITERAL: raptor_term_type = 2;
pub const RAPTOR_TERM_TYPE_URI: raptor_term_type = 1;
pub const RAPTOR_TERM_TYPE_UNKNOWN: raptor_term_type = 0;
/* *
 * raptor_locator:
 * @uri: URI of location (or NULL)
 * @file: Filename of location (or NULL)
 * @line: Line number of location (or <0 for no line)
 * @column: Column number of location (or <0 for no column)
 * @byte: Byte number of location (or <0 for no byte)
 *
 * Location information for an error, warning or information message.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct raptor_locator {
    pub uri: *mut raptor_uri,
    pub file: *const libc::c_char,
    pub line: libc::c_int,
    pub column: libc::c_int,
    pub byte: libc::c_int,
}
/* *
 * raptor_term_literal_value:
 * @string: literal string
 * @string_len: length of string
 * @datatype: datatype URI (or NULL)
 * @language: literal language (or NULL)
 * @language_len: length of language
 *
 * Literal term value - this typedef exists solely for use in #raptor_term
 *
 * Either @datatype or @language may be non-NULL but not both.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct raptor_term_literal_value {
    pub string: *mut libc::c_uchar,
    pub string_len: libc::c_uint,
    pub datatype: *mut raptor_uri,
    pub language: *mut libc::c_uchar,
    pub language_len: libc::c_uchar,
}
/* *
 * raptor_term_blank_value:
 * @string: literal string
 * @string_len: length of string
 *
 * Blank term value - this typedef exists solely for use in #raptor_term
 *
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct raptor_term_blank_value {
    pub string: *mut libc::c_uchar,
    pub string_len: libc::c_uint,
}
/* *
 * raptor_term_value:
 * @uri: uri value when term type is #RAPTOR_TERM_TYPE_URI
 * @literal: literal value when term type is #RAPTOR_TERM_TYPE_LITERAL
 * @blank: blank value when term type is #RAPTOR_TERM_TYPE_BLANK
 *
 * Term value - this typedef exists solely for use in #raptor_term
 *
 **/
#[derive(Copy, Clone)]
#[repr(C)]
pub union raptor_term_value {
    pub uri: *mut raptor_uri,
    pub literal: raptor_term_literal_value,
    pub blank: raptor_term_blank_value,
}
/* *
 * raptor_term:
 * @world: world
 * @usage: usage reference count (if >0)
 * @type: term type
 * @value: term values per type
 *
 * An RDF statement term
 *
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct raptor_term {
    pub world: *mut raptor_world,
    pub usage: libc::c_int,
    pub type_0: raptor_term_type,
    pub value: raptor_term_value,
}
/* *
 * raptor_statement:
 * @world: world pointer
 * @usage: usage count
 * @subject: statement subject
 * @predicate: statement predicate
 * @object: statement object
 * @graph: statement graph name (or NULL if not present)
 *
 * An RDF triple with optional graph name (quad)
 *
 * See #raptor_term for a description of how the fields may be used.
 * As returned by a parser statement_handler.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct raptor_statement {
    pub world: *mut raptor_world,
    pub usage: libc::c_int,
    pub subject: *mut raptor_term,
    pub predicate: *mut raptor_term,
    pub object: *mut raptor_term,
    pub graph: *mut raptor_term,
}
pub type raptor_sequence = raptor_sequence_s;
pub type rasqal_world = rasqal_world_s;
pub type rasqal_query_results_formatter = rasqal_query_results_formatter_s;
/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * redland.h - Redland RDF Application Framework public API
 *
 * Copyright (C) 2000-2011, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
 *
 * This package is Free Software and part of Redland http://librdf.org/
 *
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 *
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 *
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 *
 *
 */
/* raptor */
/* rasqal: uses raptor */
/* librdf: uses rasqal and raptor */
/* Use gcc 3.1+ feature to allow marking of deprecated API calls.
 * This gives a warning during compiling.
 */
/* Public defines */
/* *
 * LIBRDF_VERSION:
 *
 * Redland librdf library version number
 *
 * Format: major * 10000 + minor * 100 + release
 */
/* *
 * LIBRDF_VERSION_STRING:
 *
 * Redland librdf library version string
 */
/* *
 * LIBRDF_VERSION_MAJOR:
 *
 * Redland librdf library major version
 */
/* *
 * LIBRDF_VERSION_MINOR:
 *
 * Redland librdf library minor version
 */
/* *
 * LIBRDF_VERSION_RELEASE:
 *
 * Redland librdf library release
 */
/* Public typedefs (references to private structures) */
/* *
 * librdf_uri:
 *
 * Redland URI class.
 */
pub type librdf_uri = raptor_uri_s;
pub type librdf_rasqal_init_handler =
    Option<unsafe extern "C" fn(_: *mut libc::c_void, _: *mut rasqal_world) -> ()>;
/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_init.h - Overall library initialisation / termination and memory
 *              management prototypes
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
 *
 * This package is Free Software and part of Redland http://librdf.org/
 *
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 *
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 *
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 *
 *
 */
pub type librdf_raptor_init_handler =
    Option<unsafe extern "C" fn(_: *mut libc::c_void, _: *mut raptor_world) -> ()>;
pub type librdf_hash = librdf_hash_s;
/* *
 * librdf_hash:
 *
 * Redland hash class.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_hash_s {
    pub world: *mut librdf_world,
    pub identifier: *mut libc::c_char,
    pub context: *mut libc::c_void,
    pub is_open: libc::c_int,
    pub factory: *mut librdf_hash_factory_s,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_hash_factory_s {
    pub next: *mut librdf_hash_factory_s,
    pub name: *mut libc::c_char,
    pub context_length: size_t,
    pub cursor_context_length: size_t,
    pub clone: Option<
        unsafe extern "C" fn(
            _: *mut librdf_hash,
            _: *mut libc::c_void,
            _: *mut libc::c_char,
            _: *mut libc::c_void,
        ) -> libc::c_int,
    >,
    pub create:
        Option<unsafe extern "C" fn(_: *mut librdf_hash, _: *mut libc::c_void) -> libc::c_int>,
    pub destroy: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
    pub open: Option<
        unsafe extern "C" fn(
            _: *mut libc::c_void,
            _: *const libc::c_char,
            _: libc::c_int,
            _: libc::c_int,
            _: libc::c_int,
            _: *mut librdf_hash,
        ) -> libc::c_int,
    >,
    pub close: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
    pub values_count: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
    pub put: Option<
        unsafe extern "C" fn(
            _: *mut libc::c_void,
            _: *mut librdf_hash_datum,
            _: *mut librdf_hash_datum,
        ) -> libc::c_int,
    >,
    pub exists: Option<
        unsafe extern "C" fn(
            _: *mut libc::c_void,
            _: *mut librdf_hash_datum,
            _: *mut librdf_hash_datum,
        ) -> libc::c_int,
    >,
    pub delete_key: Option<
        unsafe extern "C" fn(_: *mut libc::c_void, _: *mut librdf_hash_datum) -> libc::c_int,
    >,
    pub delete_key_value: Option<
        unsafe extern "C" fn(
            _: *mut libc::c_void,
            _: *mut librdf_hash_datum,
            _: *mut librdf_hash_datum,
        ) -> libc::c_int,
    >,
    pub sync: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
    pub get_fd: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
    pub cursor_init:
        Option<unsafe extern "C" fn(_: *mut libc::c_void, _: *mut libc::c_void) -> libc::c_int>,
    pub cursor_get: Option<
        unsafe extern "C" fn(
            _: *mut libc::c_void,
            _: *mut librdf_hash_datum,
            _: *mut librdf_hash_datum,
            _: libc::c_uint,
        ) -> libc::c_int,
    >,
    pub cursor_finish: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> ()>,
}
pub type librdf_hash_datum = librdf_hash_datum_s;
/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_hash_internal.h - Internal RDF Hash Factory and Hash definitions
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
 *
 * This package is Free Software and part of Redland http://librdf.org/
 *
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 *
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 *
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 *
 *
 */
/* * data type used to describe hash key and data */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_hash_datum_s {
    pub world: *mut librdf_world,
    pub data: *mut libc::c_void,
    pub size: size_t,
    pub next: *mut librdf_hash_datum_s,
}

impl Default for librdf_hash_datum_s {
    fn default() -> Self {
        librdf_hash_datum_s {
            world: ptr::null_mut(),
            data: ptr::null_mut(),
            size: 0,
            next: ptr::null_mut(),
        }
    }
}

pub type librdf_world = librdf_world_s;
/* *
 * librdf_node:
 *
 * Redland node class.
 */
pub type librdf_node = raptor_term;
pub type librdf_hash_factory = librdf_hash_factory_s;
pub type librdf_digest_factory = librdf_digest_factory_s;
/* *
 * librdf_digest_factory:
 *
 * Redland digest factory class.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_digest_factory_s {
    pub next: *mut librdf_digest_factory_s,
    pub name: *mut libc::c_char,
    pub context_length: size_t,
    pub digest_length: size_t,
    pub init: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> ()>,
    pub update: Option<
        unsafe extern "C" fn(_: *mut libc::c_void, _: *const libc::c_uchar, _: size_t) -> (),
    >,
    pub final_0: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> ()>,
    pub get_digest: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> *mut libc::c_uchar>,
}
pub type librdf_query_factory = librdf_query_factory_s;
/* *
 * librdf_query_factory:
 *
 * Redland query factory class.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_query_factory_s {
    pub world: *mut librdf_world,
    pub next: *mut librdf_query_factory_s,
    pub name: *mut libc::c_char,
    pub uri: *mut librdf_uri,
    pub context_length: size_t,
    pub init: Option<
        unsafe extern "C" fn(
            _: *mut librdf_query,
            _: *const libc::c_char,
            _: *mut librdf_uri,
            _: *const libc::c_uchar,
            _: *mut librdf_uri,
        ) -> libc::c_int,
    >,
    pub clone:
        Option<unsafe extern "C" fn(_: *mut librdf_query, _: *mut librdf_query) -> libc::c_int>,
    pub terminate: Option<unsafe extern "C" fn(_: *mut librdf_query) -> ()>,
    pub execute: Option<
        unsafe extern "C" fn(
            _: *mut librdf_query,
            _: *mut librdf_model,
        ) -> *mut librdf_query_results,
    >,
    pub get_limit: Option<unsafe extern "C" fn(_: *mut librdf_query) -> libc::c_int>,
    pub set_limit:
        Option<unsafe extern "C" fn(_: *mut librdf_query, _: libc::c_int) -> libc::c_int>,
    pub get_offset: Option<unsafe extern "C" fn(_: *mut librdf_query) -> libc::c_int>,
    pub set_offset:
        Option<unsafe extern "C" fn(_: *mut librdf_query, _: libc::c_int) -> libc::c_int>,
    pub results_as_stream:
        Option<unsafe extern "C" fn(_: *mut librdf_query_results) -> *mut librdf_stream>,
    pub results_get_count:
        Option<unsafe extern "C" fn(_: *mut librdf_query_results) -> libc::c_int>,
    pub results_next: Option<unsafe extern "C" fn(_: *mut librdf_query_results) -> libc::c_int>,
    pub results_finished: Option<unsafe extern "C" fn(_: *mut librdf_query_results) -> libc::c_int>,
    pub results_get_bindings: Option<
        unsafe extern "C" fn(
            _: *mut librdf_query_results,
            _: *mut *mut *const libc::c_char,
            _: *mut *mut librdf_node,
        ) -> libc::c_int,
    >,
    pub results_get_binding_value: Option<
        unsafe extern "C" fn(_: *mut librdf_query_results, _: libc::c_int) -> *mut librdf_node,
    >,
    pub results_get_binding_name: Option<
        unsafe extern "C" fn(_: *mut librdf_query_results, _: libc::c_int) -> *const libc::c_char,
    >,
    pub results_get_binding_value_by_name: Option<
        unsafe extern "C" fn(
            _: *mut librdf_query_results,
            _: *const libc::c_char,
        ) -> *mut librdf_node,
    >,
    pub results_get_bindings_count:
        Option<unsafe extern "C" fn(_: *mut librdf_query_results) -> libc::c_int>,
    pub free_results: Option<unsafe extern "C" fn(_: *mut librdf_query_results) -> ()>,
    pub results_is_bindings:
        Option<unsafe extern "C" fn(_: *mut librdf_query_results) -> libc::c_int>,
    pub results_is_boolean:
        Option<unsafe extern "C" fn(_: *mut librdf_query_results) -> libc::c_int>,
    pub results_is_graph: Option<unsafe extern "C" fn(_: *mut librdf_query_results) -> libc::c_int>,
    pub results_is_syntax:
        Option<unsafe extern "C" fn(_: *mut librdf_query_results) -> libc::c_int>,
    pub results_get_boolean:
        Option<unsafe extern "C" fn(_: *mut librdf_query_results) -> libc::c_int>,
    pub new_results_formatter: Option<
        unsafe extern "C" fn(
            _: *mut librdf_query_results,
            _: *const libc::c_char,
            _: *const libc::c_char,
            _: *mut librdf_uri,
        ) -> *mut librdf_query_results_formatter,
    >,
    pub free_results_formatter:
        Option<unsafe extern "C" fn(_: *mut librdf_query_results_formatter) -> ()>,
    pub results_formatter_write: Option<
        unsafe extern "C" fn(
            _: *mut raptor_iostream,
            _: *mut librdf_query_results_formatter,
            _: *mut librdf_query_results,
            _: *mut librdf_uri,
        ) -> libc::c_int,
    >,
}
pub type librdf_query_results = librdf_query_results_s;
/* *
 * librdf_query_results:
 *
 * Redland query results class.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_query_results_s {
    pub query: *mut librdf_query,
    pub next: *mut librdf_query_results,
}
pub type librdf_query = librdf_query_s;
/* *
 * librdf_query:
 *
 * Redland query class.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_query_s {
    pub world: *mut librdf_world,
    pub usage: libc::c_int,
    pub context: *mut libc::c_void,
    pub factory: *mut librdf_query_factory_s,
    pub results: *mut librdf_query_results,
}
pub type librdf_query_results_formatter = librdf_query_results_formatter_s;
/* *
 * librdf_query_results_formatter:
 *
 * Redland query results formatter class.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_query_results_formatter_s {
    pub query_results: *mut librdf_query_results,
    pub formatter: *mut rasqal_query_results_formatter,
}
pub type librdf_stream = librdf_stream_s;
/* *
 * librdf_stream:
 *
 * Redland stream class.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_stream_s {
    pub world: *mut librdf_world,
    pub context: *mut libc::c_void,
    pub is_finished: libc::c_int,
    pub is_updated: libc::c_int,
    pub is_updating: libc::c_int,
    pub current: *mut librdf_statement,
    pub map_list: *mut librdf_list,
    pub is_end_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
    pub next_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
    pub get_method:
        Option<unsafe extern "C" fn(_: *mut libc::c_void, _: libc::c_int) -> *mut libc::c_void>,
    pub finished_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> ()>,
}
pub type librdf_list = librdf_list_s;
/* *
 * librdf_statement:
 *
 * Redland statement class.
 */
pub type librdf_statement = raptor_statement;
pub type librdf_model = librdf_model_s;
/* *
 * librdf_model:
 *
 * Redland model class.
 */
/* *
 * librdf_model_factory:
 *
 * Redland model factory class.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_model_factory_s {
    pub name: *mut libc::c_char,
    pub label: *mut libc::c_char,
    pub context_length: size_t,
    pub init: Option<unsafe extern "C" fn() -> ()>,
    pub terminate: Option<unsafe extern "C" fn() -> ()>,
    pub create: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_storage,
            _: *mut librdf_hash,
        ) -> libc::c_int,
    >,
    pub clone: Option<unsafe extern "C" fn(_: *mut librdf_model) -> *mut librdf_model>,
    pub destroy: Option<unsafe extern "C" fn(_: *mut librdf_model) -> ()>,
    pub size: Option<unsafe extern "C" fn(_: *mut librdf_model) -> libc::c_int>,
    pub add_statement:
        Option<unsafe extern "C" fn(_: *mut librdf_model, _: *mut librdf_statement) -> libc::c_int>,
    pub add_statements:
        Option<unsafe extern "C" fn(_: *mut librdf_model, _: *mut librdf_stream) -> libc::c_int>,
    pub remove_statement:
        Option<unsafe extern "C" fn(_: *mut librdf_model, _: *mut librdf_statement) -> libc::c_int>,
    pub contains_statement:
        Option<unsafe extern "C" fn(_: *mut librdf_model, _: *mut librdf_statement) -> libc::c_int>,
    pub has_arc_in: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_node,
            _: *mut librdf_node,
        ) -> libc::c_int,
    >,
    pub has_arc_out: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_node,
            _: *mut librdf_node,
        ) -> libc::c_int,
    >,
    pub serialise: Option<unsafe extern "C" fn(_: *mut librdf_model) -> *mut librdf_stream>,
    pub find_statements: Option<
        unsafe extern "C" fn(_: *mut librdf_model, _: *mut librdf_statement) -> *mut librdf_stream,
    >,
    pub find_statements_with_options: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_statement,
            _: *mut librdf_node,
            _: *mut librdf_hash,
        ) -> *mut librdf_stream,
    >,
    pub get_sources: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_node,
            _: *mut librdf_node,
        ) -> *mut librdf_iterator,
    >,
    pub get_arcs: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_node,
            _: *mut librdf_node,
        ) -> *mut librdf_iterator,
    >,
    pub get_targets: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_node,
            _: *mut librdf_node,
        ) -> *mut librdf_iterator,
    >,
    pub get_arcs_in: Option<
        unsafe extern "C" fn(_: *mut librdf_model, _: *mut librdf_node) -> *mut librdf_iterator,
    >,
    pub get_arcs_out: Option<
        unsafe extern "C" fn(_: *mut librdf_model, _: *mut librdf_node) -> *mut librdf_iterator,
    >,
    pub context_add_statement: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_node,
            _: *mut librdf_statement,
        ) -> libc::c_int,
    >,
    pub context_remove_statement: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_node,
            _: *mut librdf_statement,
        ) -> libc::c_int,
    >,
    pub context_serialize: Option<
        unsafe extern "C" fn(_: *mut librdf_model, _: *mut librdf_node) -> *mut librdf_stream,
    >,
    pub query_execute: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_query,
        ) -> *mut librdf_query_results,
    >,
    pub sync: Option<unsafe extern "C" fn(_: *mut librdf_model) -> libc::c_int>,
    pub context_add_statements: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_node,
            _: *mut librdf_stream,
        ) -> libc::c_int,
    >,
    pub context_remove_statements:
        Option<unsafe extern "C" fn(_: *mut librdf_model, _: *mut librdf_node) -> libc::c_int>,
    pub get_storage: Option<unsafe extern "C" fn(_: *mut librdf_model) -> *mut librdf_storage>,
    pub find_statements_in_context: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_statement,
            _: *mut librdf_node,
        ) -> *mut librdf_stream,
    >,
    pub get_contexts: Option<unsafe extern "C" fn(_: *mut librdf_model) -> *mut librdf_iterator>,
    pub get_feature:
        Option<unsafe extern "C" fn(_: *mut librdf_model, _: *mut librdf_uri) -> *mut librdf_node>,
    pub set_feature: Option<
        unsafe extern "C" fn(
            _: *mut librdf_model,
            _: *mut librdf_uri,
            _: *mut librdf_node,
        ) -> libc::c_int,
    >,
    pub transaction_start: Option<unsafe extern "C" fn(_: *mut librdf_model) -> libc::c_int>,
    pub transaction_start_with_handle:
        Option<unsafe extern "C" fn(_: *mut librdf_model, _: *mut libc::c_void) -> libc::c_int>,
    pub transaction_commit: Option<unsafe extern "C" fn(_: *mut librdf_model) -> libc::c_int>,
    pub transaction_rollback: Option<unsafe extern "C" fn(_: *mut librdf_model) -> libc::c_int>,
    pub transaction_get_handle:
        Option<unsafe extern "C" fn(_: *mut librdf_model) -> *mut libc::c_void>,
}
pub type librdf_iterator = librdf_iterator_s;
/* *
 * librdf_iterator:
 *
 * Redland iterator class.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_iterator_s {
    pub world: *mut librdf_world,
    pub context: *mut libc::c_void,
    pub is_finished: libc::c_int,
    pub is_updated: libc::c_int,
    pub is_updating: libc::c_int,
    pub current: *mut libc::c_void,
    pub map_list: *mut librdf_list,
    pub is_end_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
    pub next_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> libc::c_int>,
    pub get_method:
        Option<unsafe extern "C" fn(_: *mut libc::c_void, _: libc::c_int) -> *mut libc::c_void>,
    pub finished_method: Option<unsafe extern "C" fn(_: *mut libc::c_void) -> ()>,
}
pub type librdf_storage = librdf_storage_s;
/* *
 * librdf_storage:
 *
 * Redland storage class.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_storage_s {
    pub world: *mut librdf_world,
    pub usage: libc::c_int,
    pub model: *mut librdf_model,
    pub instance: *mut libc::c_void,
    pub index_contexts: libc::c_int,
    pub factory: *mut librdf_storage_factory_s,
}

/* *
 * librdf_storage_factory:
 *
 * Redland storage factory class.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_storage_factory_s {
    pub version: libc::c_int,
    pub name: *mut libc::c_char,
    pub label: *mut libc::c_char,
    pub init: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *const libc::c_char,
            _: *mut librdf_hash,
        ) -> libc::c_int,
    >,
    pub clone:
        Option<unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_storage) -> libc::c_int>,
    pub terminate: Option<unsafe extern "C" fn(_: *mut librdf_storage) -> ()>,
    pub open:
        Option<unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_model) -> libc::c_int>,
    pub close: Option<unsafe extern "C" fn(_: *mut librdf_storage) -> libc::c_int>,
    pub size: Option<unsafe extern "C" fn(_: *mut librdf_storage) -> libc::c_int>,
    pub add_statement: Option<
        unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_statement) -> libc::c_int,
    >,
    pub add_statements:
        Option<unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_stream) -> libc::c_int>,
    pub remove_statement: Option<
        unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_statement) -> libc::c_int,
    >,
    pub contains_statement: Option<
        unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_statement) -> libc::c_int,
    >,
    pub has_arc_in: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_node,
            _: *mut librdf_node,
        ) -> libc::c_int,
    >,
    pub has_arc_out: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_node,
            _: *mut librdf_node,
        ) -> libc::c_int,
    >,
    pub serialise: Option<unsafe extern "C" fn(_: *mut librdf_storage) -> *mut librdf_stream>,
    pub find_statements: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_statement,
        ) -> *mut librdf_stream,
    >,
    pub find_statements_with_options: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_statement,
            _: *mut librdf_node,
            _: *mut librdf_hash,
        ) -> *mut librdf_stream,
    >,
    pub find_sources: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_node,
            _: *mut librdf_node,
        ) -> *mut librdf_iterator,
    >,
    pub find_arcs: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_node,
            _: *mut librdf_node,
        ) -> *mut librdf_iterator,
    >,
    pub find_targets: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_node,
            _: *mut librdf_node,
        ) -> *mut librdf_iterator,
    >,
    pub get_arcs_in: Option<
        unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_node) -> *mut librdf_iterator,
    >,
    pub get_arcs_out: Option<
        unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_node) -> *mut librdf_iterator,
    >,
    pub context_add_statement: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_node,
            _: *mut librdf_statement,
        ) -> libc::c_int,
    >,
    pub context_remove_statement: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_node,
            _: *mut librdf_statement,
        ) -> libc::c_int,
    >,
    pub context_serialise: Option<
        unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_node) -> *mut librdf_stream,
    >,
    pub sync: Option<unsafe extern "C" fn(_: *mut librdf_storage) -> libc::c_int>,
    pub context_add_statements: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_node,
            _: *mut librdf_stream,
        ) -> libc::c_int,
    >,
    pub context_remove_statements:
        Option<unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_node) -> libc::c_int>,
    pub find_statements_in_context: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_statement,
            _: *mut librdf_node,
        ) -> *mut librdf_stream,
    >,
    pub get_contexts: Option<unsafe extern "C" fn(_: *mut librdf_storage) -> *mut librdf_iterator>,
    pub get_feature: Option<
        unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_uri) -> *mut librdf_node,
    >,
    pub set_feature: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_uri,
            _: *mut librdf_node,
        ) -> libc::c_int,
    >,
    pub transaction_start: Option<unsafe extern "C" fn(_: *mut librdf_storage) -> libc::c_int>,
    pub transaction_start_with_handle:
        Option<unsafe extern "C" fn(_: *mut librdf_storage, _: *mut libc::c_void) -> libc::c_int>,
    pub transaction_commit: Option<unsafe extern "C" fn(_: *mut librdf_storage) -> libc::c_int>,
    pub transaction_rollback: Option<unsafe extern "C" fn(_: *mut librdf_storage) -> libc::c_int>,
    pub transaction_get_handle:
        Option<unsafe extern "C" fn(_: *mut librdf_storage) -> *mut libc::c_void>,
    pub supports_query:
        Option<unsafe extern "C" fn(_: *mut librdf_storage, _: *mut librdf_query) -> libc::c_int>,
    pub query_execute: Option<
        unsafe extern "C" fn(
            _: *mut librdf_storage,
            _: *mut librdf_query,
        ) -> *mut librdf_query_results,
    >,
}
/* *
 * librdf_log_message:
 *
 * Structure for storing parts of a log message generated by Redland.
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_log_message {
    pub code: libc::c_int,
    pub level: librdf_log_level,
    pub facility: librdf_log_facility,
    pub message: *const libc::c_char,
    pub locator: *mut raptor_locator,
}
/* *
 * librdf_log_facility:
 * @LIBRDF_FROM_CONCEPTS: Concepts
 * @LIBRDF_FROM_DIGEST: Digest
 * @LIBRDF_FROM_FILES: Files
 * @LIBRDF_FROM_HASH: Hash
 * @LIBRDF_FROM_INIT: Init
 * @LIBRDF_FROM_ITERATOR: Iterator
 * @LIBRDF_FROM_LIST: List
 * @LIBRDF_FROM_MODEL: Model
 * @LIBRDF_FROM_NODE: Node
 * @LIBRDF_FROM_PARSER: Parser
 * @LIBRDF_FROM_QUERY: Query
 * @LIBRDF_FROM_SERIALIZER: Serializer
 * @LIBRDF_FROM_STATEMENT: Statement
 * @LIBRDF_FROM_STORAGE: Storage
 * @LIBRDF_FROM_STREAM: Stream
 * @LIBRDF_FROM_URI: URI
 * @LIBRDF_FROM_UTF8: UTF8
 * @LIBRDF_FROM_MEMORY: Memory
 * @LIBRDF_FROM_NONE: Associated with no part.
 * @LIBRDF_FROM_RAPTOR: Raptor library (parser or serializer; Raptor 2.0.0+).
 * @LIBRDF_FROM_LAST: Internal, never returned.
 *
 * Indicates the part of the system that generated the log message.
 */
pub type librdf_log_facility = libc::c_uint;
pub const LIBRDF_FROM_LAST: librdf_log_facility = 19;
pub const LIBRDF_FROM_RAPTOR: librdf_log_facility = 19;
pub const LIBRDF_FROM_MEMORY: librdf_log_facility = 18;
pub const LIBRDF_FROM_UTF8: librdf_log_facility = 17;
pub const LIBRDF_FROM_URI: librdf_log_facility = 16;
pub const LIBRDF_FROM_STREAM: librdf_log_facility = 15;
pub const LIBRDF_FROM_STORAGE: librdf_log_facility = 14;
pub const LIBRDF_FROM_STATEMENT: librdf_log_facility = 13;
pub const LIBRDF_FROM_SERIALIZER: librdf_log_facility = 12;
pub const LIBRDF_FROM_QUERY: librdf_log_facility = 11;
pub const LIBRDF_FROM_PARSER: librdf_log_facility = 10;
pub const LIBRDF_FROM_NODE: librdf_log_facility = 9;
pub const LIBRDF_FROM_MODEL: librdf_log_facility = 8;
pub const LIBRDF_FROM_LIST: librdf_log_facility = 7;
pub const LIBRDF_FROM_ITERATOR: librdf_log_facility = 6;
pub const LIBRDF_FROM_INIT: librdf_log_facility = 5;
pub const LIBRDF_FROM_HASH: librdf_log_facility = 4;
pub const LIBRDF_FROM_FILES: librdf_log_facility = 3;
pub const LIBRDF_FROM_DIGEST: librdf_log_facility = 2;
pub const LIBRDF_FROM_CONCEPTS: librdf_log_facility = 1;
pub const LIBRDF_FROM_NONE: librdf_log_facility = 0;
/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_log.h - RDF logging interfaces
 *
 * Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2004-2005, University of Bristol, UK http://www.bristol.ac.uk/
 *
 * This package is Free Software and part of Redland http://librdf.org/
 *
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 *
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 *
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 *
 *
 */
/* *
 * librdf_log_level:
 * @LIBRDF_LOG_NONE: No level
 * @LIBRDF_LOG_DEBUG: Debug.
 * @LIBRDF_LOG_INFO: Information.
 * @LIBRDF_LOG_WARN: Warning.
 * @LIBRDF_LOG_ERROR: Recoverable error.  Program can continue.
 * @LIBRDF_LOG_FATAL: Fatal error.  Program will abort if this is not caught.
 * @LIBRDF_LOG_LAST: Internal, never returned.
 *
 * Indicates the level of the log message.
 */
pub type librdf_log_level = libc::c_uint;
pub const LIBRDF_LOG_LAST: librdf_log_level = 5;
pub const LIBRDF_LOG_FATAL: librdf_log_level = 5;
pub const LIBRDF_LOG_ERROR: librdf_log_level = 4;
pub const LIBRDF_LOG_WARN: librdf_log_level = 3;
pub const LIBRDF_LOG_INFO: librdf_log_level = 2;
pub const LIBRDF_LOG_DEBUG: librdf_log_level = 1;
pub const LIBRDF_LOG_NONE: librdf_log_level = 0;
/* *
 * librdf_log_func:
 * @user_data: User data pointer
 * @message: Log message structure pointer.
 *
 * Handler for all log levels.
 *
 * Return value: non-zero to indicate log message has been handled
 */
pub type librdf_log_func =
    Option<unsafe extern "C" fn(_: *mut libc::c_void, _: *mut librdf_log_message) -> libc::c_int>;
/* *
 * librdf_log_level_func:
 * @user_data: User data pointer
 * @message: Log message.
 * @arguments: Message arguments.
 *
 * Handler for one log level, for the warning and error levels ONLY.
 * Used by #librdf_world_set_warning and #librdf_world_set_error.
 *
 * Return value: non-zero to indicate log message has been handled
 */
pub type librdf_log_level_func = Option<
    unsafe extern "C" fn(
        _: *mut libc::c_void,
        _: *const libc::c_char,
        _: *mut __va_list_tag,
    ) -> libc::c_int,
>;
pub type librdf_storage_factory = librdf_storage_factory_s;
/* *
 * librdf_iterator_get_method_flags:
 * @LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT: get context from iterator - implementing librdf_iterator_get_object()
 * @LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT: get object from iterator - implementing librdf_iterator_get_context()
 * @LIBRDF_ITERATOR_GET_METHOD_GET_KEY: get iterator key object from iterator - implementing librdf_iterator_get_key()
 * @LIBRDF_ITERATOR_GET_METHOD_GET_VALUE: get iterator value from iterator - implementing librdf_iterator_get_value()
 *
 * Flags for librdf_new_iterator() get_method function pointer.
*/
/* iterator get_method flags */
pub type unnamed = libc::c_uint;
pub const LIBRDF_ITERATOR_GET_METHOD_GET_VALUE: unnamed = 3;
pub const LIBRDF_ITERATOR_GET_METHOD_GET_KEY: unnamed = 2;
pub const LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT: unnamed = 1;
pub const LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT: unnamed = 0;
/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_statement.h - RDF Statement definition
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
 *
 * This package is Free Software and part of Redland http://librdf.org/
 *
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 *
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 *
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 *
 *
 */
/* *
 * librdf_statement_part:
 * @LIBRDF_STATEMENT_SUBJECT: Subject of a statement.
 * @LIBRDF_STATEMENT_PREDICATE: Predicate of a statement.
 * @LIBRDF_STATEMENT_OBJECT: Object of a statement.
 * @LIBRDF_STATEMENT_ALL: All parts of a statement.
 *
 * Flags that are or-ed to indicate statement parts.
 *
 * Used in fields arguments to methods such as the public
 * librdf_statement_encode_parts() librdf_statement_decode_parts()
 * librdf_new_stream_from_node_iterator().
 */
pub type librdf_statement_part = libc::c_uint;
/* must be a combination of all of the above */
pub const LIBRDF_STATEMENT_ALL: librdf_statement_part = 7;
pub const LIBRDF_STATEMENT_OBJECT: librdf_statement_part = 4;
pub const LIBRDF_STATEMENT_PREDICATE: librdf_statement_part = 2;
pub const LIBRDF_STATEMENT_SUBJECT: librdf_statement_part = 1;
/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * librdf_storage_module.h - Interface for a Redland storage module
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
 *
 * This package is Free Software and part of Redland http://librdf.org/
 *
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 *
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 *
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 */
/* *
 * librdf_storage_instance:
 *
 * Opaque storage module instance handle.
 *
 * For use with a storage module and the librdf_storage_get_instance()
 * and librdf_storage_set_instance() functions.  The instance handle
 * should be set in the #librdf_storage_factory init factory method.
 */
pub type librdf_storage_instance = *mut libc::c_void;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_storage_hashes_instance {
    pub name: *mut libc::c_char,
    pub hash_type: *mut libc::c_char,
    pub db_dir: *mut libc::c_char,
    pub indexes: *mut libc::c_char,
    pub mode: libc::c_int,
    pub is_writable: libc::c_int,
    pub is_new: libc::c_int,
    pub options: *mut librdf_hash,
    pub hash_count: libc::c_int,
    pub hashes: *mut *mut librdf_hash,
    pub hash_descriptions: *mut *mut librdf_hash_descriptor,
    pub names: *mut *mut libc::c_char,
    pub sources_index: libc::c_int,
    pub arcs_index: libc::c_int,
    pub targets_index: libc::c_int,
    pub p2so_index: libc::c_int,
    pub index_contexts: libc::c_int,
    pub contexts_index: libc::c_int,
    pub all_statements_hash_index: libc::c_int,
    pub key_buffer: *mut libc::c_uchar,
    pub key_buffer_len: size_t,
    pub value_buffer: *mut libc::c_uchar,
    pub value_buffer_len: size_t,
    pub mdata_context: *mut MDataContext,
}
/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_storage_hashes.c - RDF Storage as Hashes Implementation
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
 *
 * This package is Free Software and part of Redland http://librdf.org/
 *
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 *
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 *
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 *
 *
 */
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_hash_descriptor {
    pub name: *const libc::c_char,
    pub key_fields: libc::c_int,
    pub value_fields: libc::c_int,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_storage_hashes_get_contexts_iterator_context {
    pub storage: *mut librdf_storage,
    pub iterator: *mut librdf_iterator,
    pub key: *mut librdf_hash_datum,
    pub current: *mut librdf_node,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_storage_hashes_context_serialise_stream_context {
    pub storage: *mut librdf_storage,
    pub iterator: *mut librdf_iterator,
    pub key: *mut librdf_hash_datum,
    pub value: *mut librdf_hash_datum,
    pub current: librdf_statement,
    pub index_contexts: libc::c_int,
    pub context_node: *mut librdf_node,
    pub context_node_data: *mut libc::c_char,
    pub current_is_ok: libc::c_int,
}
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_storage_hashes_node_iterator_context {
    pub storage: *mut librdf_storage,
    pub hash_index: libc::c_int,
    pub iterator: *mut librdf_iterator,
    pub want: libc::c_int,
    pub statement: librdf_statement,
    pub statement2: librdf_statement,
    pub key: librdf_hash_datum,
    pub value: librdf_hash_datum,
    pub search_node: *mut librdf_node,
    pub index_contexts: libc::c_int,
    pub context_node: *mut librdf_node,
}
/* *
 * librdf_stream_map_free_context_handler:
 * @map_context: Map data context pointer.
 *
 * Free handler function for a #librdf_stream map operation.
 *
 * See librdf_stream_add_map().
 */
pub type librdf_stream_map_free_context_handler =
    Option<unsafe extern "C" fn(_: *mut libc::c_void) -> ()>;
/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_stream.h - RDF Stream interface and definitions
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
 *
 * This package is Free Software and part of Redland http://librdf.org/
 *
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 *
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 *
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 *
 *
 */
/* *
 * librdf_stream_map_handler:
 * @stream: Stream that this map is operating over.
 * @map_context: Map data context pointer.
 * @item: Pointer to the current item in the iteration.
 *
 * Map function for a #librdf_stream map operation.
 *
 * See librdf_stream_add_map().
 *
 * Returns: item in keep the iteration or NULL to remove it
 */
pub type librdf_stream_map_handler = Option<
    unsafe extern "C" fn(
        _: *mut librdf_stream,
        _: *mut libc::c_void,
        _: *mut librdf_statement,
    ) -> *mut librdf_statement,
>;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct librdf_storage_hashes_serialise_stream_context {
    pub storage: *mut librdf_storage,
    pub hash_context: *mut librdf_storage_hashes_instance,
    pub index: libc::c_int,
    pub iterator: *mut librdf_iterator,
    pub key: *mut librdf_hash_datum,
    pub value: *mut librdf_hash_datum,
    pub search_node: *mut librdf_node,
    pub current: librdf_statement,
    pub index_contexts: libc::c_int,
    pub context_node: *mut librdf_node,
    pub current_is_ok: libc::c_int,
}
#[no_mangle]
pub unsafe extern "C" fn librdf_init_storage_hashes(world: *mut librdf_world) {
    librdf_storage_register_factory(
        world,
        b"mdata\x00" as *const u8 as *const libc::c_char,
        b"Mutable Data\x00" as *const u8 as *const libc::c_char,
        Some(librdf_storage_hashes_register_factory),
    );
}
unsafe extern "C" fn librdf_storage_hashes_register_factory(factory: *mut librdf_storage_factory) {
    if 0 != strcmp(
        (*factory).name,
        b"mdata\x00" as *const u8 as *const libc::c_char,
    ) {
        fprintf(stderr,
                b"%s:%d: (%s) assertion failed: assertion !strcmp(factory->name, \"mdata\") failed.\n\x00"
                    as *const u8 as *const libc::c_char,
                b"rdf_storage_hashes.c\x00" as *const u8 as
                    *const libc::c_char, 1937i32,
                (*::std::mem::transmute::<&[u8; 39],
                                          &[libc::c_char; 39]>(b"librdf_storage_hashes_register_factory\x00")).as_ptr());
        return;
    } else {
        (*factory).version = 1i32;
        (*factory).init = Some(librdf_storage_hashes_init);
        (*factory).clone = Some(librdf_storage_hashes_clone);
        (*factory).terminate = Some(librdf_storage_hashes_terminate);
        (*factory).open = Some(librdf_storage_hashes_open);
        (*factory).close = Some(librdf_storage_hashes_close);
        (*factory).size = Some(librdf_storage_hashes_size);
        (*factory).add_statement = Some(librdf_storage_hashes_add_statement);
        (*factory).add_statements = Some(librdf_storage_hashes_add_statements);
        (*factory).remove_statement = Some(librdf_storage_hashes_remove_statement);
        (*factory).contains_statement = Some(librdf_storage_hashes_contains_statement);
        (*factory).serialise = Some(librdf_storage_hashes_serialise);
        (*factory).find_statements = Some(librdf_storage_hashes_find_statements);
        (*factory).find_sources = Some(librdf_storage_hashes_find_sources);
        (*factory).find_arcs = Some(librdf_storage_hashes_find_arcs);
        (*factory).find_targets = Some(librdf_storage_hashes_find_targets);
        (*factory).context_add_statement = Some(librdf_storage_hashes_context_add_statement);
        (*factory).context_remove_statement = Some(librdf_storage_hashes_context_remove_statement);
        (*factory).context_serialise = Some(librdf_storage_hashes_context_serialise);
        (*factory).sync = Some(librdf_storage_hashes_sync);
        (*factory).get_contexts = Some(librdf_storage_hashes_get_contexts);
        (*factory).get_feature = Some(librdf_storage_hashes_get_feature);
        return;
    };
}
/* *
 * librdf_storage_hashes_get_feature:
 * @storage: #librdf_storage object
 * @feature: #librdf_uri feature property
 *
 * Get the value of a storage feature.
 *
 * Return value: new #librdf_node feature value or NULL if no such feature
 * exists or the value is empty.
 **/
unsafe extern "C" fn librdf_storage_hashes_get_feature(
    storage: *mut librdf_storage,
    feature: *mut librdf_uri,
) -> *mut librdf_node {
    let scontext: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut uri_string: *mut libc::c_uchar = 0 as *mut libc::c_uchar;
    if feature.is_null() {
        return 0 as *mut librdf_node;
    } else {
        uri_string = librdf_uri_as_string(feature);
        if uri_string.is_null() {
            return 0 as *mut librdf_node;
        } else if 0
            == strcmp(
                uri_string as *const libc::c_char,
                b"http://feature.librdf.org/model-contexts\x00" as *const u8 as *const libc::c_char,
            )
        {
            let mut value: [libc::c_uchar; 2] = [0; 2];
            sprintf(
                value.as_mut_ptr() as *mut libc::c_char,
                b"%d\x00" as *const u8 as *const libc::c_char,
                ((*scontext).index_contexts != 0i32) as libc::c_int,
            );
            return librdf_new_node_from_typed_literal(
                (*storage).world,
                value.as_mut_ptr(),
                0 as *const libc::c_char,
                0 as *mut librdf_uri,
            );
        } else {
            return 0 as *mut librdf_node;
        }
    };
}
/* *
 * librdf_storage_hashes_context_get_contexts:
 * @storage: #librdf_storage object
 *
 * List all context nodes in a storage.
 *
 * Return value: #librdf_iterator of context_nodes or NULL on failure or no contexts
 **/
unsafe extern "C" fn librdf_storage_hashes_get_contexts(
    storage: *mut librdf_storage,
) -> *mut librdf_iterator {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut icontext: *mut librdf_storage_hashes_get_contexts_iterator_context =
        0 as *mut librdf_storage_hashes_get_contexts_iterator_context;
    let mut iterator: *mut librdf_iterator = 0 as *mut librdf_iterator;
    if (*context).index_contexts < 0i32 {
        librdf_log(
            (*storage).world,
            0i32,
            LIBRDF_LOG_WARN,
            LIBRDF_FROM_STORAGE,
            0 as *mut libc::c_void,
            b"Storage was created without context support\x00" as *const u8 as *const libc::c_char,
        );
        return 0 as *mut librdf_iterator;
    } else {
        icontext = calloc(
            1i32 as libc::c_ulong,
            ::std::mem::size_of::<librdf_storage_hashes_get_contexts_iterator_context>()
                as libc::c_ulong,
        ) as *mut librdf_storage_hashes_get_contexts_iterator_context;
        if icontext.is_null() {
            return 0 as *mut librdf_iterator;
        } else {
            (*icontext).key =
                librdf_new_hash_datum((*storage).world, 0 as *mut libc::c_void, 0i32 as size_t);
            if (*icontext).key.is_null() {
                free(icontext as *mut libc::c_void);
                return 0 as *mut librdf_iterator;
            } else {
                (*icontext).iterator = librdf_hash_keys(
                    *(*context).hashes.offset((*context).contexts_index as isize),
                    (*icontext).key,
                );
                if (*icontext).iterator.is_null() {
                    librdf_storage_hashes_get_contexts_finished(icontext as *mut libc::c_void);
                    return 0 as *mut librdf_iterator;
                } else {
                    (*icontext).storage = storage;
                    librdf_storage_add_reference((*icontext).storage);
                    iterator = librdf_new_iterator(
                        (*storage).world,
                        icontext as *mut libc::c_void,
                        Some(librdf_storage_hashes_get_contexts_is_end),
                        Some(librdf_storage_hashes_get_contexts_next_method),
                        Some(librdf_storage_hashes_get_contexts_get_method),
                        Some(librdf_storage_hashes_get_contexts_finished),
                    );
                    if iterator.is_null() {
                        librdf_storage_hashes_get_contexts_finished(icontext as *mut libc::c_void);
                    }
                    return iterator;
                }
            }
        }
    };
}
unsafe extern "C" fn librdf_storage_hashes_get_contexts_finished(iterator: *mut libc::c_void) {
    let icontext: *mut librdf_storage_hashes_get_contexts_iterator_context =
        iterator as *mut librdf_storage_hashes_get_contexts_iterator_context;
    if !(*icontext).iterator.is_null() {
        librdf_free_iterator((*icontext).iterator);
    }
    librdf_free_hash_datum((*icontext).key);
    if !(*icontext).current.is_null() {
        librdf_free_node((*icontext).current);
    }
    if !(*icontext).storage.is_null() {
        librdf_storage_remove_reference((*icontext).storage);
    }
    free(icontext as *mut libc::c_void);
}
unsafe extern "C" fn librdf_storage_hashes_get_contexts_get_method(
    iterator: *mut libc::c_void,
    flags: libc::c_int,
) -> *mut libc::c_void {
    let icontext: *mut librdf_storage_hashes_get_contexts_iterator_context =
        iterator as *mut librdf_storage_hashes_get_contexts_iterator_context;
    let mut result: *mut libc::c_void = 0 as *mut libc::c_void;
    let mut k: *mut librdf_hash_datum = 0 as *mut librdf_hash_datum;
    match flags {
        0 => {
            k = librdf_iterator_get_key((*icontext).iterator) as *mut librdf_hash_datum;
            if k.is_null() {
                return 0 as *mut libc::c_void;
            } else {
                if !(*icontext).current.is_null() {
                    librdf_free_node((*icontext).current);
                }
                /* decode value content */
                (*icontext).current = librdf_node_decode(
                    (*(*icontext).storage).world,
                    0 as *mut size_t,
                    (*k).data as *mut libc::c_uchar,
                    (*k).size,
                );
                result = (*icontext).current as *mut libc::c_void
            }
        }
        2 | 3 => result = 0 as *mut libc::c_void,
        _ => {
            librdf_log(
                (*(*icontext).iterator).world,
                0i32,
                LIBRDF_LOG_ERROR,
                LIBRDF_FROM_STORAGE,
                0 as *mut libc::c_void,
                b"Unknown iterator method flag %d\x00" as *const u8 as *const libc::c_char,
                flags,
            );
            result = 0 as *mut libc::c_void
        }
    }
    return result;
}
unsafe extern "C" fn librdf_storage_hashes_get_contexts_next_method(
    iterator: *mut libc::c_void,
) -> libc::c_int {
    let icontext: *mut librdf_storage_hashes_get_contexts_iterator_context =
        iterator as *mut librdf_storage_hashes_get_contexts_iterator_context;
    return librdf_iterator_next((*icontext).iterator);
}
unsafe extern "C" fn librdf_storage_hashes_get_contexts_is_end(
    iterator: *mut libc::c_void,
) -> libc::c_int {
    let icontext: *mut librdf_storage_hashes_get_contexts_iterator_context =
        iterator as *mut librdf_storage_hashes_get_contexts_iterator_context;
    return librdf_iterator_end((*icontext).iterator);
}
unsafe extern "C" fn librdf_storage_hashes_sync(storage: *mut librdf_storage) -> libc::c_int {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut i: libc::c_int = 0;
    i = 0i32;
    while i < (*context).hash_count {
        librdf_hash_sync(*(*context).hashes.offset(i as isize));
        i += 1
    }
    return 0i32;
}
unsafe extern "C" fn librdf_storage_hashes_context_serialise(
    storage: *mut librdf_storage,
    context_node: *mut librdf_node,
) -> *mut librdf_stream {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut scontext: *mut librdf_storage_hashes_context_serialise_stream_context =
        0 as *mut librdf_storage_hashes_context_serialise_stream_context;
    let mut stream: *mut librdf_stream = 0 as *mut librdf_stream;
    let mut size: size_t = 0;
    if (*context).contexts_index < 0i32 {
        librdf_log(
            (*storage).world,
            0i32,
            LIBRDF_LOG_WARN,
            LIBRDF_FROM_STORAGE,
            0 as *mut libc::c_void,
            b"Storage was created without context support\x00" as *const u8 as *const libc::c_char,
        );
        return 0 as *mut librdf_stream;
    } else {
        scontext = calloc(
            1i32 as libc::c_ulong,
            ::std::mem::size_of::<librdf_storage_hashes_context_serialise_stream_context>()
                as libc::c_ulong,
        ) as *mut librdf_storage_hashes_context_serialise_stream_context;
        if scontext.is_null() {
            return 0 as *mut librdf_stream;
        } else {
            librdf_statement_init((*storage).world, &mut (*scontext).current);
            (*scontext).key =
                librdf_new_hash_datum((*storage).world, 0 as *mut libc::c_void, 0i32 as size_t);
            if (*scontext).key.is_null() {
                return 0 as *mut librdf_stream;
            } else {
                (*scontext).value =
                    librdf_new_hash_datum((*storage).world, 0 as *mut libc::c_void, 0i32 as size_t);
                if (*scontext).value.is_null() {
                    librdf_free_hash_datum((*scontext).key);
                    return 0 as *mut librdf_stream;
                } else {
                    /* scurrent->current_is_ok=0; */
                    (*scontext).index_contexts = (*context).index_contexts;
                    (*scontext).context_node = librdf_new_node_from_node(context_node);
                    size =
                        librdf_node_encode(context_node, 0 as *mut libc::c_uchar, 0i32 as size_t);
                    (*scontext).context_node_data = malloc(size) as *mut libc::c_char;
                    (*(*scontext).key).data = (*scontext).context_node_data as *mut libc::c_void;
                    (*(*scontext).key).size = librdf_node_encode(
                        context_node,
                        (*(*scontext).key).data as *mut libc::c_uchar,
                        size,
                    );
                    (*scontext).iterator = librdf_hash_get_all(
                        *(*context).hashes.offset((*context).contexts_index as isize),
                        (*scontext).key,
                        (*scontext).value,
                    );
                    if (*scontext).iterator.is_null() {
                        return librdf_new_empty_stream((*storage).world);
                    } else {
                        (*scontext).storage = storage;
                        librdf_storage_add_reference((*scontext).storage);
                        stream = librdf_new_stream(
                            (*storage).world,
                            scontext as *mut libc::c_void,
                            Some(librdf_storage_hashes_context_serialise_end_of_stream),
                            Some(librdf_storage_hashes_context_serialise_next_statement),
                            Some(librdf_storage_hashes_context_serialise_get_statement),
                            Some(librdf_storage_hashes_context_serialise_finished),
                        );
                        if stream.is_null() {
                            librdf_storage_hashes_context_serialise_finished(
                                scontext as *mut libc::c_void,
                            );
                            return 0 as *mut librdf_stream;
                        } else {
                            return stream;
                        }
                    }
                }
            }
        }
    };
}
unsafe extern "C" fn librdf_storage_hashes_context_serialise_finished(context: *mut libc::c_void) {
    let scontext: *mut librdf_storage_hashes_context_serialise_stream_context =
        context as *mut librdf_storage_hashes_context_serialise_stream_context;
    if !(*scontext).context_node.is_null() {
        librdf_free_node((*scontext).context_node);
    }
    if !(*scontext).iterator.is_null() {
        librdf_free_iterator((*scontext).iterator);
    }
    if !(*scontext).key.is_null() {
        librdf_free_hash_datum((*scontext).key);
    }
    if !(*scontext).value.is_null() {
        (*(*scontext).value).data = 0 as *mut libc::c_void;
        librdf_free_hash_datum((*scontext).value);
    }
    librdf_statement_clear(&mut (*scontext).current);
    if !(*scontext).context_node_data.is_null() {
        free((*scontext).context_node_data as *mut libc::c_void);
    }
    if !(*scontext).storage.is_null() {
        librdf_storage_remove_reference((*scontext).storage);
    }
    free(scontext as *mut libc::c_void);
}
unsafe extern "C" fn librdf_storage_hashes_context_serialise_get_statement(
    context: *mut libc::c_void,
    flags: libc::c_int,
) -> *mut libc::c_void {
    let mut scontext: *mut librdf_storage_hashes_context_serialise_stream_context =
        0 as *mut librdf_storage_hashes_context_serialise_stream_context;
    let mut v: *mut librdf_hash_datum = 0 as *mut librdf_hash_datum;
    let mut world: *mut librdf_world = 0 as *mut librdf_world;
    scontext = context as *mut librdf_storage_hashes_context_serialise_stream_context;
    world = (*(*scontext).storage).world;
    match flags {
        0 | 1 => {
            if 0 != (*scontext).current_is_ok {
                if flags == LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT as libc::c_int {
                    return &mut (*scontext).current as *mut librdf_statement as *mut libc::c_void;
                } else {
                    return (*scontext).context_node as *mut libc::c_void;
                }
            } else {
                librdf_statement_clear(&mut (*scontext).current);
                v = librdf_iterator_get_value((*scontext).iterator) as *mut librdf_hash_datum;
                /* decode value content and optional context */
                if 0 == librdf_statement_decode2(
                    world,
                    &mut (*scontext).current,
                    0 as *mut *mut librdf_node,
                    (*v).data as *mut libc::c_uchar,
                    (*v).size,
                ) {
                    return 0 as *mut libc::c_void;
                } else {
                    (*scontext).current_is_ok = 1i32;
                    if flags == LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT as libc::c_int {
                        return &mut (*scontext).current as *mut librdf_statement
                            as *mut libc::c_void;
                    } else {
                        return (*scontext).context_node as *mut libc::c_void;
                    }
                }
            }
        }
        _ => {
            librdf_log(
                (*(*scontext).iterator).world,
                0i32,
                LIBRDF_LOG_ERROR,
                LIBRDF_FROM_STORAGE,
                0 as *mut libc::c_void,
                b"Unimplemented flags %d seen\x00" as *const u8 as *const libc::c_char,
                flags,
            );
            return 0 as *mut libc::c_void;
        }
    };
}
unsafe extern "C" fn librdf_storage_hashes_context_serialise_next_statement(
    context: *mut libc::c_void,
) -> libc::c_int {
    let scontext: *mut librdf_storage_hashes_context_serialise_stream_context =
        context as *mut librdf_storage_hashes_context_serialise_stream_context;
    (*scontext).current_is_ok = 0i32;
    return librdf_iterator_next((*scontext).iterator);
}
/* context list statement stream methods */
unsafe extern "C" fn librdf_storage_hashes_context_serialise_end_of_stream(
    context: *mut libc::c_void,
) -> libc::c_int {
    let scontext: *mut librdf_storage_hashes_context_serialise_stream_context =
        context as *mut librdf_storage_hashes_context_serialise_stream_context;
    return librdf_iterator_end((*scontext).iterator);
}
unsafe extern "C" fn librdf_storage_hashes_context_remove_statement(
    storage: *mut librdf_storage,
    context_node: *mut librdf_node,
    statement: *mut librdf_statement,
) -> libc::c_int {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    /* on stack - not allocated */
    let mut key: librdf_hash_datum = librdf_hash_datum_s {
        world: 0 as *mut librdf_world,
        data: 0 as *mut libc::c_void,
        size: 0,
        next: 0 as *mut librdf_hash_datum_s,
    };
    let mut value: librdf_hash_datum = librdf_hash_datum_s {
        world: 0 as *mut librdf_world,
        data: 0 as *mut libc::c_void,
        size: 0,
        next: 0 as *mut librdf_hash_datum_s,
    };
    let mut size: size_t = 0;
    let mut status: libc::c_int = 0;
    let world: *mut librdf_world = (*storage).world;
    if !context_node.is_null() && (*context).contexts_index < 0i32 {
        librdf_log(
            (*storage).world,
            0i32,
            LIBRDF_LOG_WARN,
            LIBRDF_FROM_STORAGE,
            0 as *mut libc::c_void,
            b"Storage was created without context support\x00" as *const u8 as *const libc::c_char,
        );
    }
    if 0 != librdf_storage_hashes_add_remove_statement(storage, statement, context_node, 0i32) {
        return 1i32;
    } else {
        size = librdf_node_encode(context_node, 0 as *mut libc::c_uchar, 0i32 as size_t);
        key.data = malloc(size) as *mut libc::c_char as *mut libc::c_void;
        key.size = librdf_node_encode(context_node, key.data as *mut libc::c_uchar, size);
        size = librdf_statement_encode2(world, statement, 0 as *mut libc::c_uchar, 0i32 as size_t);
        value.data = malloc(size) as *mut libc::c_char as *mut libc::c_void;
        value.size =
            librdf_statement_encode2(world, statement, value.data as *mut libc::c_uchar, size);
        status = librdf_hash_delete(
            *(*context).hashes.offset((*context).contexts_index as isize),
            &mut key,
            &mut value,
        );
        free(key.data);
        free(value.data);
        return status;
    };
}

unsafe extern "C" fn librdf_storage_hashes_add_remove_statement(
    storage: *mut librdf_storage,
    statement: *mut librdf_statement,
    context_node: *mut librdf_node,
    is_addition: libc::c_int,
) -> libc::c_int {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut i: libc::c_int = 0;
    let mut status: libc::c_int = 0i32;
    let world: *mut librdf_world = (*storage).world;
    i = 0i32;
    while i < (*context).hash_count {
        /* on stack */
        let mut hd_key: librdf_hash_datum = librdf_hash_datum_s {
            world: 0 as *mut librdf_world,
            data: 0 as *mut libc::c_void,
            size: 0,
            next: 0 as *mut librdf_hash_datum_s,
        };
        let mut hd_value: librdf_hash_datum = librdf_hash_datum_s {
            world: 0 as *mut librdf_world,
            data: 0 as *mut libc::c_void,
            size: 0,
            next: 0 as *mut librdf_hash_datum_s,
        };
        let mut key_len: size_t = 0;
        let mut value_len: size_t = 0;
        /* ENCODE KEY */
        let mut fields: librdf_statement_part =
            (**(*context).hash_descriptions.offset(i as isize)).key_fields as librdf_statement_part;
        if !(0 == fields as u64) {
            key_len = librdf_statement_encode_parts2(
                world,
                statement,
                0 as *mut librdf_node,
                0 as *mut libc::c_uchar,
                0i32 as size_t,
                fields,
            );
            if 0 == key_len {
                return 1i32;
            } else if 0
                != librdf_storage_hashes_grow_buffer(
                    &mut (*context).key_buffer,
                    &mut (*context).key_buffer_len,
                    key_len,
                )
                || 0 == librdf_statement_encode_parts2(
                    world,
                    statement,
                    0 as *mut librdf_node,
                    (*context).key_buffer,
                    (*context).key_buffer_len,
                    fields,
                )
            {
                status = 1i32;
                break;
            } else {
                /* ENCODE VALUE */
                fields = (**(*context).hash_descriptions.offset(i as isize)).value_fields
                    as librdf_statement_part;
                if !(0 == fields as u64) {
                    value_len = librdf_statement_encode_parts2(
                        world,
                        statement,
                        context_node,
                        0 as *mut libc::c_uchar,
                        0i32 as size_t,
                        fields,
                    );
                    if 0 == value_len
                        || 0 != librdf_storage_hashes_grow_buffer(
                            &mut (*context).value_buffer,
                            &mut (*context).value_buffer_len,
                            value_len,
                        )
                        || 0 == librdf_statement_encode_parts2(
                            world,
                            statement,
                            context_node,
                            (*context).value_buffer,
                            (*context).value_buffer_len,
                            fields,
                        )
                    {
                        status = 1i32;
                        break;
                    } else {
                        /* Finally, store / remove the sucker */
                        hd_key.data = (*context).key_buffer as *mut libc::c_void;
                        hd_key.size = key_len;
                        hd_value.data = (*context).value_buffer as *mut libc::c_void;
                        hd_value.size = value_len;

                        let ea: EntryAction;

                        if 0 != is_addition {
                            ea = EntryAction::Insert(
                                i,
                                slice::from_raw_parts((*context).key_buffer, key_len as usize)
                                    .to_vec(),
                                slice::from_raw_parts((*context).value_buffer, value_len as usize)
                                    .to_vec(),
                            );

                            status = librdf_hash_put(
                                *(*context).hashes.offset(i as isize),
                                &mut hd_key,
                                &mut hd_value,
                            )
                        } else {
                            ea = EntryAction::Delete(
                                i,
                                slice::from_raw_parts((*context).key_buffer, key_len as usize)
                                    .to_vec(),
                            );

                            status = librdf_hash_delete(
                                *(*context).hashes.offset(i as isize),
                                &mut hd_key,
                                &mut hd_value,
                            )
                        }

                        (*(*context).mdata_context).entry_actions.push(ea);

                        if 0 != status {
                            break;
                        }
                    }
                }
            }
        }
        i += 1
    }
    return status;
}
unsafe extern "C" fn librdf_storage_hashes_grow_buffer(
    buffer: *mut *mut libc::c_uchar,
    len: *mut size_t,
    required_len: size_t,
) -> libc::c_int {
    if required_len <= *len {
        return 0i32;
    } else {
        if !(*buffer).is_null() {
            free(*buffer as *mut libc::c_void);
        }
        *len = required_len.wrapping_add(8i32 as libc::c_ulong);
        *buffer = malloc(*len) as *mut libc::c_uchar;
        if (*buffer).is_null() {
            *len = 0i32 as size_t
        }
        return (*len < required_len) as libc::c_int;
    };
}
/* context functions */
unsafe extern "C" fn librdf_storage_hashes_context_add_statement(
    storage: *mut librdf_storage,
    context_node: *mut librdf_node,
    statement: *mut librdf_statement,
) -> libc::c_int {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    /* on stack - not allocated */
    let mut key: librdf_hash_datum = librdf_hash_datum_s {
        world: ptr::null_mut(),
        data: ptr::null_mut(),
        size: 0,
        next: ptr::null_mut(),
    };
    let mut value: librdf_hash_datum = librdf_hash_datum_s {
        world: ptr::null_mut(),
        data: ptr::null_mut(),
        size: 0,
        next: ptr::null_mut(),
    };
    let mut size: size_t = 0;
    let mut status: libc::c_int = 0;
    let world: *mut librdf_world = (*storage).world;
    if (*context).contexts_index < 0i32 {
        librdf_log(
            (*storage).world,
            0i32,
            LIBRDF_LOG_WARN,
            LIBRDF_FROM_STORAGE,
            0 as *mut libc::c_void,
            b"Storage was created without context support\x00" as *const u8 as *const libc::c_char,
        );
        return 1i32;
    }
    if 0 != librdf_storage_hashes_add_remove_statement(storage, statement, context_node, 1i32) {
        return 1i32;
    }
    size = librdf_node_encode(context_node, 0 as *mut libc::c_uchar, 0i32 as size_t);
    key.data = malloc(size) as *mut libc::c_char as *mut libc::c_void;
    key.size = librdf_node_encode(context_node, key.data as *mut libc::c_uchar, size);
    size = librdf_statement_encode2(world, statement, 0 as *mut libc::c_uchar, 0i32 as size_t);
    value.data = malloc(size) as *mut libc::c_char as *mut libc::c_void;
    value.size = librdf_statement_encode2(world, statement, value.data as *mut libc::c_uchar, size);
    status = librdf_hash_put(
        *(*context).hashes.offset((*context).contexts_index as isize),
        &mut key,
        &mut value,
    );
    free(key.data);
    free(value.data);
    return status;
}
unsafe extern "C" fn librdf_storage_hashes_find_targets(
    storage: *mut librdf_storage,
    source: *mut librdf_node,
    arc: *mut librdf_node,
) -> *mut librdf_iterator {
    let scontext: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    return librdf_storage_hashes_node_iterator_create(
        storage,
        source,
        arc,
        (*scontext).targets_index,
        LIBRDF_STATEMENT_OBJECT as libc::c_int,
    );
}
/* common initialisation code for creating get sources, targets, arcs iterators */
unsafe extern "C" fn librdf_storage_hashes_node_iterator_create(
    storage: *mut librdf_storage,
    mut node1: *mut librdf_node,
    mut node2: *mut librdf_node,
    hash_index: libc::c_int,
    want: libc::c_int,
) -> *mut librdf_iterator {
    let scontext: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut icontext: *mut librdf_storage_hashes_node_iterator_context =
        0 as *mut librdf_storage_hashes_node_iterator_context;
    let mut hash: *mut librdf_hash = 0 as *mut librdf_hash;
    let mut fields: librdf_statement_part = 0 as librdf_statement_part;
    let mut key_buffer: *mut libc::c_uchar = 0 as *mut libc::c_uchar;
    let mut iterator: *mut librdf_iterator = 0 as *mut librdf_iterator;
    let world: *mut librdf_world = (*storage).world;
    icontext = calloc(
        1i32 as libc::c_ulong,
        ::std::mem::size_of::<librdf_storage_hashes_node_iterator_context>() as libc::c_ulong,
    ) as *mut librdf_storage_hashes_node_iterator_context;
    if icontext.is_null() {
        return 0 as *mut librdf_iterator;
    } else {
        (*icontext).storage = storage;
        (*icontext).hash_index = hash_index;
        (*icontext).want = want;
        (*icontext).index_contexts = (*scontext).index_contexts;
        node1 = librdf_new_node_from_node(node1);
        if node1.is_null() {
            free(icontext as *mut libc::c_void);
            return 0 as *mut librdf_iterator;
        } else {
            if !node2.is_null() {
                node2 = librdf_new_node_from_node(node2);
                if node2.is_null() {
                    librdf_free_node(node2);
                    free(icontext as *mut libc::c_void);
                    return 0 as *mut librdf_iterator;
                }
            }
            librdf_statement_init((*storage).world, &mut (*icontext).statement);
            librdf_statement_init((*storage).world, &mut (*icontext).statement2);
            hash = *(*scontext).hashes.offset((*icontext).hash_index as isize);
            /* set the fields in the static statement contained in the context */
            match (*icontext).want {
                1 => {
                    librdf_statement_set_predicate(&mut (*icontext).statement, node1);
                    librdf_statement_set_object(&mut (*icontext).statement, node2);
                }
                2 => {
                    librdf_statement_set_subject(&mut (*icontext).statement, node1);
                    librdf_statement_set_object(&mut (*icontext).statement, node2);
                }
                4 => {
                    librdf_statement_set_subject(&mut (*icontext).statement, node1);
                    librdf_statement_set_predicate(&mut (*icontext).statement, node2);
                }
                5 => {
                    (*icontext).search_node = librdf_new_node_from_node(node1);
                    librdf_statement_set_predicate(&mut (*icontext).statement, node1);
                }
                _ => {
                    free(icontext as *mut libc::c_void);
                    librdf_log(
                        (*storage).world,
                        0i32,
                        LIBRDF_LOG_ERROR,
                        LIBRDF_FROM_STORAGE,
                        0 as *mut libc::c_void,
                        b"Illegal statement part %d seen\x00" as *const u8 as *const libc::c_char,
                        want,
                    );
                    return 0 as *mut librdf_iterator;
                }
            }
            /* ENCODE KEY */
            fields = (**(*scontext).hash_descriptions.offset(hash_index as isize)).key_fields
                as librdf_statement_part;
            (*icontext).key.size = librdf_statement_encode_parts2(
                world,
                &mut (*icontext).statement,
                0 as *mut librdf_node,
                0 as *mut libc::c_uchar,
                0i32 as size_t,
                fields,
            );
            if 0 == (*icontext).key.size {
                free(icontext as *mut libc::c_void);
                return 0 as *mut librdf_iterator;
            } else {
                key_buffer = malloc((*icontext).key.size) as *mut libc::c_uchar;
                if key_buffer.is_null() {
                    free(icontext as *mut libc::c_void);
                    return 0 as *mut librdf_iterator;
                } else {
                    /* after this point the finished method is called on errors
                     * so must bump the reference count
                     */
                    librdf_storage_add_reference((*icontext).storage);
                    if 0 == librdf_statement_encode_parts2(
                        world,
                        &mut (*icontext).statement,
                        0 as *mut librdf_node,
                        key_buffer,
                        (*icontext).key.size,
                        fields,
                    ) {
                        free(key_buffer as *mut libc::c_void);
                        librdf_storage_hashes_node_iterator_finished(icontext as *mut libc::c_void);
                        return 0 as *mut librdf_iterator;
                    } else {
                        (*icontext).key.data = key_buffer as *mut libc::c_void;
                        (*icontext).iterator =
                            librdf_hash_get_all(hash, &mut (*icontext).key, &mut (*icontext).value);
                        if (*icontext).iterator.is_null() {
                            free(key_buffer as *mut libc::c_void);
                            librdf_storage_hashes_node_iterator_finished(
                                icontext as *mut libc::c_void,
                            );
                            return librdf_new_empty_iterator((*storage).world);
                        } else {
                            free(key_buffer as *mut libc::c_void);
                            iterator = librdf_new_iterator(
                                (*storage).world,
                                icontext as *mut libc::c_void,
                                Some(librdf_storage_hashes_node_iterator_is_end),
                                Some(librdf_storage_hashes_node_iterator_next_method),
                                Some(librdf_storage_hashes_node_iterator_get_method),
                                Some(librdf_storage_hashes_node_iterator_finished),
                            );
                            if iterator.is_null() {
                                librdf_storage_hashes_node_iterator_finished(
                                    icontext as *mut libc::c_void,
                                );
                            }
                            return iterator;
                        }
                    }
                }
            }
        }
    };
}
unsafe extern "C" fn librdf_storage_hashes_node_iterator_finished(iterator: *mut libc::c_void) {
    let icontext: *mut librdf_storage_hashes_node_iterator_context =
        iterator as *mut librdf_storage_hashes_node_iterator_context;
    let mut node: *mut librdf_node = 0 as *mut librdf_node;
    if !(*icontext).search_node.is_null() {
        librdf_free_node((*icontext).search_node);
    }
    if !(*icontext).context_node.is_null() {
        librdf_free_node((*icontext).context_node);
    }
    if !(*icontext).iterator.is_null() {
        librdf_free_iterator((*icontext).iterator);
    }
    librdf_statement_clear(&mut (*icontext).statement);
    node = librdf_statement_get_predicate(&mut (*icontext).statement2);
    if !node.is_null() {
        librdf_free_node(node);
    }
    if !(*icontext).storage.is_null() {
        librdf_storage_remove_reference((*icontext).storage);
    }
    free(icontext as *mut libc::c_void);
}
unsafe extern "C" fn librdf_storage_hashes_node_iterator_get_method(
    iterator: *mut libc::c_void,
    flags: libc::c_int,
) -> *mut libc::c_void {
    let context: *mut librdf_storage_hashes_node_iterator_context =
        iterator as *mut librdf_storage_hashes_node_iterator_context;
    let mut node: *mut librdf_node = 0 as *mut librdf_node;
    let mut value: *mut librdf_hash_datum = 0 as *mut librdf_hash_datum;
    let mut world: *mut librdf_world = 0 as *mut librdf_world;
    world = (*(*context).storage).world;
    if 0 != librdf_iterator_end((*context).iterator) {
        return 0 as *mut libc::c_void;
    } else if flags == LIBRDF_ITERATOR_GET_METHOD_GET_CONTEXT as libc::c_int {
        /* current stuff is out of date - get new cached answers */
        if 0 == (*context).index_contexts {
            return 0 as *mut libc::c_void;
        } else {
            value = librdf_iterator_get_value((*context).iterator) as *mut librdf_hash_datum;
            if !(*context).context_node.is_null() {
                librdf_free_node((*context).context_node);
            }
            (*context).context_node = 0 as *mut librdf_node;
            /* decode value content and optional context */
            if 0 == librdf_statement_decode2(
                world,
                &mut (*context).statement,
                &mut (*context).context_node,
                (*value).data as *mut libc::c_uchar,
                (*value).size,
            ) {
                return 0 as *mut libc::c_void;
            } else {
                librdf_statement_clear(&mut (*context).statement);
                return (*context).context_node as *mut libc::c_void;
            }
        }
    } else if flags != LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT as libc::c_int {
        librdf_log(
            (*(*context).iterator).world,
            0i32,
            LIBRDF_LOG_ERROR,
            LIBRDF_FROM_STORAGE,
            0 as *mut libc::c_void,
            b"Unimplemented iterator method %d\x00" as *const u8 as *const libc::c_char,
            flags,
        );
        return 0 as *mut libc::c_void;
    } else {
        /* get object */
        match (*context).want {
            1 => {
                node = librdf_statement_get_subject(&mut (*context).statement);
                if !node.is_null() {
                    librdf_free_node(node);
                }
            }
            2 => {
                node = librdf_statement_get_predicate(&mut (*context).statement);
                if !node.is_null() {
                    librdf_free_node(node);
                }
            }
            4 => {
                node = librdf_statement_get_object(&mut (*context).statement);
                if !node.is_null() {
                    librdf_free_node(node);
                }
            }
            5 => {
                node = librdf_statement_get_subject(&mut (*context).statement);
                if !node.is_null() {
                    librdf_free_node(node);
                }
                node = librdf_statement_get_object(&mut (*context).statement);
                if !node.is_null() {
                    librdf_free_node(node);
                }
            }
            _ => {
                librdf_log(
                    (*(*context).iterator).world,
                    0i32,
                    LIBRDF_LOG_ERROR,
                    LIBRDF_FROM_STORAGE,
                    0 as *mut libc::c_void,
                    b"Illegal statement part %d seen\x00" as *const u8 as *const libc::c_char,
                    (*context).want,
                );
                return 0 as *mut libc::c_void;
            }
        }
        value = librdf_iterator_get_value((*context).iterator) as *mut librdf_hash_datum;
        if value.is_null()
            || 0 == librdf_statement_decode2(
                world,
                &mut (*context).statement,
                0 as *mut *mut librdf_node,
                (*value).data as *mut libc::c_uchar,
                (*value).size,
            )
        {
            return 0 as *mut libc::c_void;
        } else {
            match (*context).want {
                1 => node = librdf_statement_get_subject(&mut (*context).statement),
                2 => node = librdf_statement_get_predicate(&mut (*context).statement),
                4 => node = librdf_statement_get_object(&mut (*context).statement),
                5 => {
                    librdf_statement_set_subject(
                        &mut (*context).statement2,
                        librdf_statement_get_subject(&mut (*context).statement),
                    );
                    /* fill in the only blank from the node stored in our context */
                    node = librdf_new_node_from_node((*context).search_node);
                    if node.is_null() {
                        return 0 as *mut libc::c_void;
                    } else {
                        librdf_statement_set_predicate(&mut (*context).statement2, node);
                        librdf_statement_set_object(
                            &mut (*context).statement2,
                            librdf_statement_get_object(&mut (*context).statement),
                        );
                        return &mut (*context).statement2 as *mut librdf_statement
                            as *mut libc::c_void;
                    }
                }
                _ => {
                    librdf_log(
                        (*(*context).iterator).world,
                        0i32,
                        LIBRDF_LOG_ERROR,
                        LIBRDF_FROM_STORAGE,
                        0 as *mut libc::c_void,
                        b"Illegal statement part %d seen\x00" as *const u8 as *const libc::c_char,
                        (*context).want,
                    );
                    return 0 as *mut libc::c_void;
                }
            }
            return node as *mut libc::c_void;
        }
    };
}
unsafe extern "C" fn librdf_storage_hashes_node_iterator_next_method(
    iterator: *mut libc::c_void,
) -> libc::c_int {
    let context: *mut librdf_storage_hashes_node_iterator_context =
        iterator as *mut librdf_storage_hashes_node_iterator_context;
    if 0 != librdf_iterator_end((*context).iterator) {
        return 1i32;
    } else {
        return librdf_iterator_next((*context).iterator);
    };
}
/* node iterator implementing functions for get sources, targets, arcs methods */
unsafe extern "C" fn librdf_storage_hashes_node_iterator_is_end(
    iterator: *mut libc::c_void,
) -> libc::c_int {
    let context: *mut librdf_storage_hashes_node_iterator_context =
        iterator as *mut librdf_storage_hashes_node_iterator_context;
    return librdf_iterator_end((*context).iterator);
}
unsafe extern "C" fn librdf_storage_hashes_find_arcs(
    storage: *mut librdf_storage,
    source: *mut librdf_node,
    target: *mut librdf_node,
) -> *mut librdf_iterator {
    let scontext: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    return librdf_storage_hashes_node_iterator_create(
        storage,
        source,
        target,
        (*scontext).arcs_index,
        LIBRDF_STATEMENT_PREDICATE as libc::c_int,
    );
}
unsafe extern "C" fn librdf_storage_hashes_find_sources(
    storage: *mut librdf_storage,
    arc: *mut librdf_node,
    target: *mut librdf_node,
) -> *mut librdf_iterator {
    let scontext: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    return librdf_storage_hashes_node_iterator_create(
        storage,
        arc,
        target,
        (*scontext).sources_index,
        LIBRDF_STATEMENT_SUBJECT as libc::c_int,
    );
}
unsafe extern "C" fn librdf_storage_hashes_find_statements(
    storage: *mut librdf_storage,
    mut statement: *mut librdf_statement,
) -> *mut librdf_stream {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut stream: *mut librdf_stream = 0 as *mut librdf_stream;
    if librdf_statement_get_subject(statement).is_null()
        && !librdf_statement_get_predicate(statement).is_null()
        && librdf_statement_get_object(statement).is_null()
        && (*context).p2so_index >= 0i32
    {
        /* (? p ?) -> (s p o) wanted */
        stream = librdf_storage_hashes_serialise_common(
            storage,
            (*context).p2so_index,
            librdf_statement_get_predicate(statement),
            LIBRDF_STATEMENT_SUBJECT as libc::c_int | LIBRDF_STATEMENT_OBJECT as libc::c_int,
        )
    } else {
        statement = librdf_new_statement_from_statement(statement);
        if statement.is_null() {
            return 0 as *mut librdf_stream;
        } else {
            stream = librdf_storage_hashes_serialise(storage);
            if !stream.is_null() {
                librdf_stream_add_map(
                    stream,
                    Some(librdf_stream_statement_find_map),
                    ::std::mem::transmute::<
                        Option<unsafe extern "C" fn(_: *mut librdf_statement) -> ()>,
                        librdf_stream_map_free_context_handler,
                    >(Some(librdf_free_statement)),
                    statement as *mut libc::c_void,
                );
            }
        }
    }
    return stream;
}
unsafe extern "C" fn librdf_storage_hashes_serialise(
    storage: *mut librdf_storage,
) -> *mut librdf_stream {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    return librdf_storage_hashes_serialise_common(
        storage,
        (*context).all_statements_hash_index,
        0 as *mut librdf_node,
        0i32,
    );
}
unsafe extern "C" fn librdf_storage_hashes_serialise_common(
    storage: *mut librdf_storage,
    hash_index: libc::c_int,
    search_node: *mut librdf_node,
    want: libc::c_int,
) -> *mut librdf_stream {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut scontext: *mut librdf_storage_hashes_serialise_stream_context =
        0 as *mut librdf_storage_hashes_serialise_stream_context;
    let mut hash: *mut librdf_hash = 0 as *mut librdf_hash;
    let mut stream: *mut librdf_stream = 0 as *mut librdf_stream;
    scontext = calloc(
        1i32 as libc::c_ulong,
        ::std::mem::size_of::<librdf_storage_hashes_serialise_stream_context>() as libc::c_ulong,
    ) as *mut librdf_storage_hashes_serialise_stream_context;
    if scontext.is_null() {
        return 0 as *mut librdf_stream;
    } else {
        (*scontext).hash_context = context;
        librdf_statement_init((*storage).world, &mut (*scontext).current);
        hash = *(*context).hashes.offset((*scontext).index as isize);
        (*scontext).key =
            librdf_new_hash_datum((*storage).world, 0 as *mut libc::c_void, 0i32 as size_t);
        if (*scontext).key.is_null() {
            return 0 as *mut librdf_stream;
        } else {
            (*scontext).value =
                librdf_new_hash_datum((*storage).world, 0 as *mut libc::c_void, 0i32 as size_t);
            if (*scontext).value.is_null() {
                librdf_free_hash_datum((*scontext).key);
                return 0 as *mut librdf_stream;
            } else {
                /* scurrent->current_is_ok=0; */
                (*scontext).index_contexts = (*context).index_contexts;
                if !search_node.is_null() {
                    (*scontext).search_node = search_node;
                    (*scontext).iterator = librdf_storage_hashes_node_iterator_create(
                        storage,
                        search_node,
                        0 as *mut librdf_node,
                        hash_index,
                        want,
                    )
                } else {
                    (*scontext).iterator =
                        librdf_hash_get_all(hash, (*scontext).key, (*scontext).value)
                }
                if (*scontext).iterator.is_null() {
                    librdf_storage_hashes_serialise_finished(scontext as *mut libc::c_void);
                    return librdf_new_empty_stream((*storage).world);
                } else {
                    (*scontext).storage = storage;
                    librdf_storage_add_reference((*scontext).storage);
                    stream = librdf_new_stream(
                        (*storage).world,
                        scontext as *mut libc::c_void,
                        Some(librdf_storage_hashes_serialise_end_of_stream),
                        Some(librdf_storage_hashes_serialise_next_statement),
                        Some(librdf_storage_hashes_serialise_get_statement),
                        Some(librdf_storage_hashes_serialise_finished),
                    );
                    if stream.is_null() {
                        librdf_storage_hashes_serialise_finished(scontext as *mut libc::c_void);
                        return 0 as *mut librdf_stream;
                    } else {
                        return stream;
                    }
                }
            }
        }
    };
}
unsafe extern "C" fn librdf_storage_hashes_serialise_finished(context: *mut libc::c_void) {
    let scontext: *mut librdf_storage_hashes_serialise_stream_context =
        context as *mut librdf_storage_hashes_serialise_stream_context;
    if !(*scontext).iterator.is_null() {
        librdf_free_iterator((*scontext).iterator);
    }
    if !(*scontext).context_node.is_null() {
        librdf_free_node((*scontext).context_node);
    }
    if !(*scontext).key.is_null() {
        (*(*scontext).key).data = 0 as *mut libc::c_void;
        librdf_free_hash_datum((*scontext).key);
    }
    if !(*scontext).value.is_null() {
        (*(*scontext).value).data = 0 as *mut libc::c_void;
        librdf_free_hash_datum((*scontext).value);
    }
    librdf_statement_clear(&mut (*scontext).current);
    if !(*scontext).storage.is_null() {
        librdf_storage_remove_reference((*scontext).storage);
    }
    free(scontext as *mut libc::c_void);
}
unsafe extern "C" fn librdf_storage_hashes_serialise_get_statement(
    context: *mut libc::c_void,
    flags: libc::c_int,
) -> *mut libc::c_void {
    let scontext: *mut librdf_storage_hashes_serialise_stream_context =
        context as *mut librdf_storage_hashes_serialise_stream_context;
    let mut hd: *mut librdf_hash_datum = 0 as *mut librdf_hash_datum;
    let mut cnp: *mut *mut librdf_node = 0 as *mut *mut librdf_node;
    let mut world: *mut librdf_world = 0 as *mut librdf_world;
    world = (*(*scontext).storage).world;
    if !(*scontext).search_node.is_null() {
        match flags {
            0 => return librdf_iterator_get_object((*scontext).iterator),
            1 => return librdf_iterator_get_context((*scontext).iterator),
            _ => {
                librdf_log(
                    (*(*scontext).iterator).world,
                    0i32,
                    LIBRDF_LOG_ERROR,
                    LIBRDF_FROM_STORAGE,
                    0 as *mut libc::c_void,
                    b"Unimplemented flags %d seen\x00" as *const u8 as *const libc::c_char,
                    flags,
                );
                return 0 as *mut libc::c_void;
            }
        }
    } else {
        match flags {
            0 | 1 => {
                if 0 != (*scontext).current_is_ok {
                    if flags == LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT as libc::c_int {
                        return &mut (*scontext).current as *mut librdf_statement
                            as *mut libc::c_void;
                    } else {
                        return (*scontext).context_node as *mut libc::c_void;
                    }
                } else {
                    /* current stuff is out of date - get new cached answers */
                    if 0 != (*scontext).index_contexts {
                        if !(*scontext).context_node.is_null() {
                            librdf_free_node((*scontext).context_node);
                        }
                        (*scontext).context_node = 0 as *mut librdf_node;
                        cnp = &mut (*scontext).context_node
                    }
                    librdf_statement_clear(&mut (*scontext).current);
                    hd = librdf_iterator_get_key((*scontext).iterator) as *mut librdf_hash_datum;
                    /* decode key content */
                    if 0 == librdf_statement_decode2(
                        world,
                        &mut (*scontext).current,
                        0 as *mut *mut librdf_node,
                        (*hd).data as *mut libc::c_uchar,
                        (*hd).size,
                    ) {
                        return 0 as *mut libc::c_void;
                    } else {
                        hd = librdf_iterator_get_value((*scontext).iterator)
                            as *mut librdf_hash_datum;
                        /* decode value content and optional context */
                        if 0 == librdf_statement_decode2(
                            world,
                            &mut (*scontext).current,
                            cnp,
                            (*hd).data as *mut libc::c_uchar,
                            (*hd).size,
                        ) {
                            return 0 as *mut libc::c_void;
                        } else {
                            (*scontext).current_is_ok = 1i32;
                            if flags == LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT as libc::c_int {
                                return &mut (*scontext).current as *mut librdf_statement
                                    as *mut libc::c_void;
                            } else {
                                return (*scontext).context_node as *mut libc::c_void;
                            }
                        }
                    }
                }
            }
            _ => {
                librdf_log(
                    (*(*scontext).iterator).world,
                    0i32,
                    LIBRDF_LOG_ERROR,
                    LIBRDF_FROM_STORAGE,
                    0 as *mut libc::c_void,
                    b"Unimplemented flags %d seen\x00" as *const u8 as *const libc::c_char,
                    flags,
                );
                return 0 as *mut libc::c_void;
            }
        }
    };
}
unsafe extern "C" fn librdf_storage_hashes_serialise_next_statement(
    context: *mut libc::c_void,
) -> libc::c_int {
    let scontext: *mut librdf_storage_hashes_serialise_stream_context =
        context as *mut librdf_storage_hashes_serialise_stream_context;
    (*scontext).current_is_ok = 0i32;
    return librdf_iterator_next((*scontext).iterator);
}
/* serialising implementing functions */
unsafe extern "C" fn librdf_storage_hashes_serialise_end_of_stream(
    context: *mut libc::c_void,
) -> libc::c_int {
    let scontext: *mut librdf_storage_hashes_serialise_stream_context =
        context as *mut librdf_storage_hashes_serialise_stream_context;
    return librdf_iterator_end((*scontext).iterator);
}
unsafe extern "C" fn librdf_storage_hashes_contains_statement(
    storage: *mut librdf_storage,
    statement: *mut librdf_statement,
) -> libc::c_int {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    /* on stack */
    let mut hd_key: librdf_hash_datum = librdf_hash_datum_s {
        world: 0 as *mut librdf_world,
        data: 0 as *mut libc::c_void,
        size: 0,
        next: 0 as *mut librdf_hash_datum_s,
    };
    let mut hd_value: librdf_hash_datum = librdf_hash_datum_s {
        world: 0 as *mut librdf_world,
        data: 0 as *mut libc::c_void,
        size: 0,
        next: 0 as *mut librdf_hash_datum_s,
    };
    let mut key_buffer: *mut libc::c_uchar = 0 as *mut libc::c_uchar;
    let mut value_buffer: *mut libc::c_uchar = 0 as *mut libc::c_uchar;
    let mut key_len: size_t = 0;
    let mut value_len: size_t = 0;
    let hash_index: libc::c_int = (*context).all_statements_hash_index;
    let mut fields: librdf_statement_part = 0 as librdf_statement_part;
    let mut status: libc::c_int = 0;
    let world: *mut librdf_world = (*storage).world;
    if 0 != (*context).index_contexts {
        /* When we have contexts, we have to use find_statements for contains
         * since a statement is encoded in KEY/VALUE and the VALUE may
         * contain some context node.
         */
        let stream: *mut librdf_stream = librdf_storage_hashes_find_statements(storage, statement);
        if stream.is_null() {
            return 0i32;
        } else {
            /* librdf_stream_end returns 0 if have more, non-0 at end */
            status = (0 == librdf_stream_end(stream)) as libc::c_int;
            /* convert to 0 if at end (not found) and non-zero otherwise (found) */
            librdf_free_stream(stream);
            return status;
        }
    } else {
        /* ENCODE KEY */
        fields = (**(*context).hash_descriptions.offset(hash_index as isize)).key_fields
            as librdf_statement_part;
        key_len = librdf_statement_encode_parts2(
            world,
            statement,
            0 as *mut librdf_node,
            0 as *mut libc::c_uchar,
            0i32 as size_t,
            fields,
        );
        if 0 == key_len {
            return 1i32;
        } else {
            key_buffer = malloc(key_len) as *mut libc::c_uchar;
            if key_buffer.is_null() {
                return 1i32;
            } else if 0
                == librdf_statement_encode_parts2(
                    world,
                    statement,
                    0 as *mut librdf_node,
                    key_buffer,
                    key_len,
                    fields,
                )
            {
                free(key_buffer as *mut libc::c_void);
                return 1i32;
            } else {
                /* ENCODE VALUE */
                fields = (**(*context).hash_descriptions.offset(hash_index as isize)).value_fields
                    as librdf_statement_part;
                value_len = librdf_statement_encode_parts2(
                    world,
                    statement,
                    0 as *mut librdf_node,
                    0 as *mut libc::c_uchar,
                    0i32 as size_t,
                    fields,
                );
                if 0 == value_len {
                    free(key_buffer as *mut libc::c_void);
                    return 1i32;
                } else {
                    value_buffer = malloc(value_len) as *mut libc::c_uchar;
                    if value_buffer.is_null() {
                        free(key_buffer as *mut libc::c_void);
                        return 1i32;
                    } else if 0
                        == librdf_statement_encode_parts2(
                            world,
                            statement,
                            0 as *mut librdf_node,
                            value_buffer,
                            value_len,
                            fields,
                        )
                    {
                        free(key_buffer as *mut libc::c_void);
                        free(value_buffer as *mut libc::c_void);
                        return 1i32;
                    } else {
                        hd_key.data = key_buffer as *mut libc::c_void;
                        hd_key.size = key_len;
                        hd_value.data = value_buffer as *mut libc::c_void;
                        hd_value.size = value_len;
                        status = librdf_hash_exists(
                            *(*context).hashes.offset(hash_index as isize),
                            &mut hd_key,
                            &mut hd_value,
                        );
                        free(key_buffer as *mut libc::c_void);
                        free(value_buffer as *mut libc::c_void);
                        /* DO NOT free statement, ownership was not passed in */
                        return status;
                    }
                }
            }
        }
    };
}
unsafe extern "C" fn librdf_storage_hashes_remove_statement(
    storage: *mut librdf_storage,
    statement: *mut librdf_statement,
) -> libc::c_int {
    return librdf_storage_hashes_add_remove_statement(
        storage,
        statement,
        0 as *mut librdf_node,
        0i32,
    );
}
unsafe extern "C" fn librdf_storage_hashes_add_statements(
    storage: *mut librdf_storage,
    statement_stream: *mut librdf_stream,
) -> libc::c_int {
    let mut status: libc::c_int = 0i32;
    while 0 == librdf_stream_end(statement_stream) {
        let statement: *mut librdf_statement = librdf_stream_get_object(statement_stream);
        if !statement.is_null() {
            status = librdf_storage_hashes_add_statement(storage, statement)
        } else {
            status = 1i32
        }
        if 0 != status {
            break;
        }
        librdf_stream_next(statement_stream);
    }
    return status;
}
unsafe extern "C" fn librdf_storage_hashes_add_statement(
    storage: *mut librdf_storage,
    statement: *mut librdf_statement,
) -> libc::c_int {
    /* Do not add duplicate statements */
    if 0 != librdf_storage_hashes_contains_statement(storage, statement) {
        return 0i32;
    } else {
        return librdf_storage_hashes_add_remove_statement(
            storage,
            statement,
            0 as *mut librdf_node,
            1i32,
        );
    };
}
unsafe extern "C" fn librdf_storage_hashes_size(storage: *mut librdf_storage) -> libc::c_int {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let any_hash: *mut librdf_hash = *(*context)
        .hashes
        .offset((*context).all_statements_hash_index as isize);
    if any_hash.is_null() {
        return -1i32;
    } else {
        return librdf_hash_values_count(any_hash);
    };
}
unsafe extern "C" fn librdf_storage_hashes_close(storage: *mut librdf_storage) -> libc::c_int {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut i: libc::c_int = 0;
    i = 0i32;
    while i < (*context).hash_count {
        if !(*(*context).hashes.offset(i as isize)).is_null() {
            librdf_hash_close(*(*context).hashes.offset(i as isize));
        }
        i += 1
    }
    return 0i32;
}

unsafe extern "C" fn librdf_storage_hashes_open(
    storage: *mut librdf_storage,
    _model: *mut librdf_model,
) -> libc::c_int {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut i: libc::c_int = 0;
    let mut result: libc::c_int = 0i32;
    i = 0i32;
    while i < (*context).hash_count {
        let hash: *mut librdf_hash = *(*context).hashes.offset(i as isize);
        if hash.is_null()
            || 0 != librdf_hash_open(
                hash,
                *(*context).names.offset(i as isize),
                (*context).mode,
                (*context).is_writable,
                (*context).is_new,
                (*context).options,
            )
        {
            /* I still have my "Structured Fortran" book */
            let mut j: libc::c_int = 0;
            j = 0i32;
            while j < i {
                librdf_hash_close(*(*context).hashes.offset(j as isize));
                let ref mut fresh0 = *(*context).hashes.offset(j as isize);
                *fresh0 = 0 as *mut librdf_hash;
                j += 1
            }
            result = 1i32
        }
        if 0 != result {
            break;
        }
        i += 1
    }
    return result;
}
unsafe extern "C" fn librdf_storage_hashes_terminate(storage: *mut librdf_storage) {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut i: libc::c_int = 0;
    if context.is_null() {
        return;
    } else {
        i = 0i32;
        while i < (*context).hash_count {
            if !(*context).hash_descriptions.is_null()
                && !(*(*context).hash_descriptions.offset(i as isize)).is_null()
            {
                free(*(*context).hash_descriptions.offset(i as isize) as *mut libc::c_void);
            }
            if !(*context).hashes.is_null() && !(*(*context).hashes.offset(i as isize)).is_null() {
                librdf_free_hash(*(*context).hashes.offset(i as isize));
            }
            if !(*context).names.is_null() && !(*(*context).names.offset(i as isize)).is_null() {
                free(*(*context).names.offset(i as isize) as *mut libc::c_void);
            }
            i += 1
        }
        if !(*context).hash_descriptions.is_null() {
            free((*context).hash_descriptions as *mut libc::c_void);
        }
        if !(*context).hashes.is_null() {
            free((*context).hashes as *mut libc::c_void);
        }
        if !(*context).names.is_null() {
            free((*context).names as *mut libc::c_void);
        }
        if !(*context).options.is_null() {
            librdf_free_hash((*context).options);
        }
        if !(*context).hash_type.is_null() {
            free((*context).hash_type as *mut libc::c_void);
        }
        if !(*context).db_dir.is_null() {
            free((*context).db_dir as *mut libc::c_void);
        }
        if !(*context).indexes.is_null() {
            free((*context).indexes as *mut libc::c_void);
        }
        if !(*context).key_buffer.is_null() {
            free((*context).key_buffer as *mut libc::c_void);
        }
        if !(*context).value_buffer.is_null() {
            free((*context).value_buffer as *mut libc::c_void);
        }
        if !(*context).name.is_null() {
            free((*context).name as *mut libc::c_void);
        }
        // Drop MData context
        let _ = Box::from_raw((*context).mdata_context);
        free(context as *mut libc::c_void);
        return;
    };
}
unsafe extern "C" fn librdf_storage_hashes_clone(
    new_storage: *mut librdf_storage,
    old_storage: *mut librdf_storage,
) -> libc::c_int {
    let mut current_block: u64;
    let mut old_context: *mut librdf_storage_hashes_instance =
        0 as *mut librdf_storage_hashes_instance;
    let mut new_options: *mut librdf_hash = 0 as *mut librdf_hash;
    let mut new_name: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut new_hash_type: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut new_db_dir: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut new_indexes: *mut libc::c_char = 0 as *mut libc::c_char;
    old_context = (*old_storage).instance as *mut librdf_storage_hashes_instance;
    /* Bump up old context name if any */
    if !(*old_context).name.is_null() {
        new_name = librdf_heuristic_gen_name((*old_context).name);
        if new_name.is_null() {
            current_block = 17690084694202169520;
        } else {
            current_block = 6239978542346980191;
        }
    } else {
        current_block = 6239978542346980191;
    }
    match current_block {
        6239978542346980191 => {
            /* This is always a copy of an in-memory hash */
            new_options = librdf_new_hash_from_hash((*old_context).options);
            if !new_options.is_null() {
                if !(*old_context).hash_type.is_null() {
                    new_hash_type = strdup((*old_context).hash_type);
                    if new_hash_type.is_null() {
                        current_block = 17690084694202169520;
                    } else {
                        current_block = 715039052867723359;
                    }
                } else {
                    current_block = 715039052867723359;
                }
                match current_block {
                    17690084694202169520 => {}
                    _ => {
                        if !(*old_context).db_dir.is_null() {
                            new_db_dir = strdup((*old_context).db_dir);
                            if new_db_dir.is_null() {
                                current_block = 17690084694202169520;
                            } else {
                                current_block = 15619007995458559411;
                            }
                        } else {
                            current_block = 15619007995458559411;
                        }
                        match current_block {
                            17690084694202169520 => {}
                            _ => {
                                if !(*old_context).indexes.is_null() {
                                    new_indexes = strdup((*old_context).indexes);
                                    if new_indexes.is_null() {
                                        current_block = 17690084694202169520;
                                    } else {
                                        current_block = 10886091980245723256;
                                    }
                                } else {
                                    current_block = 10886091980245723256;
                                }
                                match current_block {
                                    17690084694202169520 => {}
                                    _ => {
                                        if !(0
                                            != librdf_storage_hashes_init_common(
                                                new_storage,
                                                new_name,
                                                new_hash_type,
                                                new_db_dir,
                                                new_indexes,
                                                (*old_context).mode,
                                                (*old_context).is_writable,
                                                (*old_context).is_new,
                                                new_options,
                                            ))
                                        {
                                            return 0i32;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        _ => {}
    }
    if !new_name.is_null() {
        free(new_name as *mut libc::c_void);
    }
    if !new_hash_type.is_null() {
        free(new_hash_type as *mut libc::c_void);
    }
    if !new_db_dir.is_null() {
        free(new_db_dir as *mut libc::c_void);
    }
    if !new_indexes.is_null() {
        free(new_indexes as *mut libc::c_void);
    }
    if !new_options.is_null() {
        librdf_free_hash(new_options);
    }
    return 1i32;
}
unsafe extern "C" fn librdf_storage_hashes_init_common(
    storage: *mut librdf_storage,
    name: *const libc::c_char,
    hash_type: *mut libc::c_char,
    db_dir: *mut libc::c_char,
    indexes: *mut libc::c_char,
    mode: libc::c_int,
    is_writable: libc::c_int,
    is_new: libc::c_int,
    options: *mut librdf_hash,
) -> libc::c_int {
    let mut context: *mut librdf_storage_hashes_instance = 0 as *mut librdf_storage_hashes_instance;
    let mut i: libc::c_int = 0;
    let mut status: libc::c_int = 0i32;
    let mut index_predicates: libc::c_int = 0i32;
    let mut index_contexts: libc::c_int = 0i32;
    let mut hash_count: libc::c_int = 0i32;
    context = calloc(
        1i32 as libc::c_ulong,
        ::std::mem::size_of::<librdf_storage_hashes_instance>() as libc::c_ulong,
    ) as *mut librdf_storage_hashes_instance;
    if context.is_null() {
        return 1i32;
    } else {
        let mdata_context = Box::new(MDataContext {
            entry_actions: Vec::new(),
        });

        librdf_storage_set_instance(storage, context as librdf_storage_instance);
        (*context).name = name as *mut libc::c_char;
        (*context).hash_type = hash_type;
        (*context).db_dir = db_dir;
        (*context).indexes = indexes;
        (*context).mode = mode;
        (*context).is_writable = is_writable;
        (*context).is_new = is_new;
        (*context).options = options;
        (*context).mdata_context = Box::into_raw(mdata_context);
        /* Work out the number of hashes for allocating stuff below */
        hash_count = 3i32;
        index_contexts = librdf_hash_get_as_boolean(
            options,
            b"contexts\x00" as *const u8 as *const libc::c_char,
        );
        if index_contexts < 0i32 {
            /* default is no contexts */
            index_contexts = 0i32
        }
        (*context).index_contexts = index_contexts;
        if 0 != index_contexts {
            hash_count += 1
        }
        index_predicates = librdf_hash_get_as_boolean(
            options,
            b"index-predicates\x00" as *const u8 as *const libc::c_char,
        );
        if index_predicates < 0i32 {
            /* default is NO index on properties */
            index_predicates = 0i32
        }
        if 0 != index_predicates {
            hash_count += 1
        }
        /* Start allocating the arrays */
        (*context).hashes = calloc(
            hash_count as size_t,
            ::std::mem::size_of::<*mut librdf_hash>() as libc::c_ulong,
        ) as *mut *mut librdf_hash;
        if (*context).hashes.is_null() {
            if !(*context).name.is_null() {
                free((*context).name as *mut libc::c_void);
            }
            return 1i32;
        } else {
            (*context).hash_descriptions = calloc(
                hash_count as size_t,
                ::std::mem::size_of::<*mut librdf_hash_descriptor>() as libc::c_ulong,
            ) as *mut *mut librdf_hash_descriptor;
            if (*context).hash_descriptions.is_null() {
                free((*context).hashes as *mut libc::c_void);
                if !(*context).name.is_null() {
                    free((*context).name as *mut libc::c_void);
                }
                return 1i32;
            } else {
                (*context).names = calloc(
                    hash_count as size_t,
                    ::std::mem::size_of::<*mut libc::c_char>() as libc::c_ulong,
                ) as *mut *mut libc::c_char;
                if (*context).names.is_null() {
                    free((*context).hashes as *mut libc::c_void);
                    free((*context).hash_descriptions as *mut libc::c_void);
                    if !(*context).name.is_null() {
                        free((*context).name as *mut libc::c_void);
                    }
                    return 1i32;
                } else {
                    i = 0i32;
                    while i < 3i32 {
                        status = librdf_storage_hashes_register(
                            storage,
                            name,
                            &librdf_storage_hashes_descriptions[i as usize],
                        );
                        if 0 != status {
                            break;
                        }
                        i += 1
                    }
                    if 0 != index_predicates && 0 == status {
                        status = librdf_storage_hashes_register(
                            storage,
                            name,
                            librdf_storage_get_hash_description_by_name(
                                b"p2so\x00" as *const u8 as *const libc::c_char,
                            ),
                        )
                    }
                    if 0 != index_contexts && 0 == status {
                        librdf_storage_hashes_register(
                            storage,
                            name,
                            librdf_storage_get_hash_description_by_name(
                                b"contexts\x00" as *const u8 as *const libc::c_char,
                            ),
                        );
                    }
                    /* find indexes for get targets, sources and arcs */
                    (*context).sources_index = -1i32;
                    (*context).arcs_index = -1i32;
                    (*context).targets_index = -1i32;
                    (*context).p2so_index = -1i32;
                    /* and index for contexts (no key or value fields) */
                    (*context).contexts_index = -1i32;
                    (*context).all_statements_hash_index = -1i32;
                    i = 0i32;
                    while i < (*context).hash_count {
                        let mut key_fields: libc::c_int = 0;
                        let mut value_fields: libc::c_int = 0;
                        if !(*(*context).hash_descriptions.offset(i as isize)).is_null() {
                            key_fields =
                                (**(*context).hash_descriptions.offset(i as isize)).key_fields;
                            value_fields =
                                (**(*context).hash_descriptions.offset(i as isize)).value_fields;
                            if (*context).all_statements_hash_index < 0i32
                                && key_fields | value_fields
                                    == LIBRDF_STATEMENT_SUBJECT as libc::c_int
                                        | LIBRDF_STATEMENT_PREDICATE as libc::c_int
                                        | LIBRDF_STATEMENT_OBJECT as libc::c_int
                            {
                                (*context).all_statements_hash_index = i
                            }
                            if key_fields
                                == LIBRDF_STATEMENT_SUBJECT as libc::c_int
                                    | LIBRDF_STATEMENT_PREDICATE as libc::c_int
                                && value_fields == LIBRDF_STATEMENT_OBJECT as libc::c_int
                            {
                                (*context).targets_index = i
                            } else if key_fields
                                == LIBRDF_STATEMENT_PREDICATE as libc::c_int
                                    | LIBRDF_STATEMENT_OBJECT as libc::c_int
                                && value_fields == LIBRDF_STATEMENT_SUBJECT as libc::c_int
                            {
                                (*context).sources_index = i
                            } else if key_fields
                                == LIBRDF_STATEMENT_SUBJECT as libc::c_int
                                    | LIBRDF_STATEMENT_OBJECT as libc::c_int
                                && value_fields == LIBRDF_STATEMENT_PREDICATE as libc::c_int
                            {
                                (*context).arcs_index = i
                            } else if key_fields == LIBRDF_STATEMENT_PREDICATE as libc::c_int
                                && value_fields
                                    == LIBRDF_STATEMENT_SUBJECT as libc::c_int
                                        | LIBRDF_STATEMENT_OBJECT as libc::c_int
                            {
                                (*context).p2so_index = i
                            } else if 0 == key_fields || 0 == value_fields {
                                (*context).contexts_index = i
                            }
                        }
                        i += 1
                    }
                    return status;
                }
            }
        }
    };
}
/* For 'get targets' */
/* For 'get sources' */
/* For 'get arcs' */
/* For '(?, p, ?)' */
/* for contexts - do not touch when storing statements! */
unsafe extern "C" fn librdf_storage_get_hash_description_by_name(
    name: *const libc::c_char,
) -> *const librdf_hash_descriptor {
    let mut i: libc::c_int = 0;
    let mut d: *const librdf_hash_descriptor = 0 as *const librdf_hash_descriptor;
    i = 0i32;
    loop {
        d = &librdf_storage_hashes_descriptions[i as usize] as *const librdf_hash_descriptor;
        if d.is_null() {
            break;
        }
        if (*d).name.is_null() {
            return 0 as *const librdf_hash_descriptor;
        } else if 0 == strcmp((*d).name, name) {
            return d;
        } else {
            i += 1
        }
    }
    return 0 as *const librdf_hash_descriptor;
}
static mut librdf_storage_hashes_descriptions: [librdf_hash_descriptor; 6] = [
    librdf_hash_descriptor {
        name: b"sp2o\x00" as *const u8 as *const libc::c_char,
        key_fields: LIBRDF_STATEMENT_SUBJECT as libc::c_int
            | LIBRDF_STATEMENT_PREDICATE as libc::c_int,
        value_fields: LIBRDF_STATEMENT_OBJECT as libc::c_int,
    },
    librdf_hash_descriptor {
        name: b"po2s\x00" as *const u8 as *const libc::c_char,
        key_fields: LIBRDF_STATEMENT_PREDICATE as libc::c_int
            | LIBRDF_STATEMENT_OBJECT as libc::c_int,
        value_fields: LIBRDF_STATEMENT_SUBJECT as libc::c_int,
    },
    librdf_hash_descriptor {
        name: b"so2p\x00" as *const u8 as *const libc::c_char,
        key_fields: LIBRDF_STATEMENT_SUBJECT as libc::c_int
            | LIBRDF_STATEMENT_OBJECT as libc::c_int,
        value_fields: LIBRDF_STATEMENT_PREDICATE as libc::c_int,
    },
    librdf_hash_descriptor {
        name: b"p2so\x00" as *const u8 as *const libc::c_char,
        key_fields: LIBRDF_STATEMENT_PREDICATE as libc::c_int,
        value_fields: LIBRDF_STATEMENT_SUBJECT as libc::c_int
            | LIBRDF_STATEMENT_OBJECT as libc::c_int,
    },
    librdf_hash_descriptor {
        name: b"contexts\x00" as *const u8 as *const libc::c_char,
        key_fields: 0i64 as libc::c_int,
        value_fields: 0i64 as libc::c_int,
    },
    librdf_hash_descriptor {
        name: 0 as *const libc::c_char,
        key_fields: 0i64 as libc::c_int,
        value_fields: 0i64 as libc::c_int,
    },
];
/* helper function for implementing init and clone methods */
unsafe extern "C" fn librdf_storage_hashes_register(
    storage: *mut librdf_storage,
    name: *const libc::c_char,
    source_desc: *const librdf_hash_descriptor,
) -> libc::c_int {
    let context: *mut librdf_storage_hashes_instance =
        (*storage).instance as *mut librdf_storage_hashes_instance;
    let mut len: size_t = 0;
    let mut full_name: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut hash_index: libc::c_int = 0;
    let mut desc: *mut librdf_hash_descriptor = 0 as *mut librdf_hash_descriptor;
    if source_desc.is_null() {
        return 1i32;
    } else {
        desc = malloc(::std::mem::size_of::<librdf_hash_descriptor>() as libc::c_ulong)
            as *mut librdf_hash_descriptor;
        if desc.is_null() {
            return 1i32;
        } else {
            memcpy(
                desc as *mut libc::c_void,
                source_desc as *const libc::c_void,
                ::std::mem::size_of::<librdf_hash_descriptor>() as libc::c_ulong,
            );
            let fresh1 = (*context).hash_count;
            (*context).hash_count = (*context).hash_count + 1;
            hash_index = fresh1;
            let ref mut fresh2 = *(*context).hash_descriptions.offset(hash_index as isize);
            *fresh2 = desc;
            if !name.is_null() {
                /* "%s-%s\0" */
                len = strlen((*desc).name)
                    .wrapping_add(1i32 as libc::c_ulong)
                    .wrapping_add(strlen(name))
                    .wrapping_add(1i32 as libc::c_ulong);
                if !(*context).db_dir.is_null() {
                    len = (len as libc::c_ulong)
                        .wrapping_add(strlen((*context).db_dir).wrapping_add(1i32 as libc::c_ulong))
                        as size_t as size_t
                }
                full_name = malloc(len) as *mut libc::c_char;
                if full_name.is_null() {
                    return 1i32;
                } else if !(*context).db_dir.is_null() {
                    sprintf(
                        full_name,
                        b"%s/%s-%s\x00" as *const u8 as *const libc::c_char,
                        (*context).db_dir,
                        name,
                        (*desc).name,
                    );
                } else {
                    sprintf(
                        full_name,
                        b"%s-%s\x00" as *const u8 as *const libc::c_char,
                        name,
                        (*desc).name,
                    );
                }
            }
            let ref mut fresh3 = *(*context).hashes.offset(hash_index as isize);
            *fresh3 = librdf_new_hash((*storage).world, (*context).hash_type);
            let ref mut fresh4 = *(*context).names.offset(hash_index as isize);
            *fresh4 = full_name;
            return (*(*context).hashes.offset(hash_index as isize)
                == 0 as *mut libc::c_void as *mut librdf_hash) as libc::c_int;
        }
    };
}
/* prototypes for local functions */
unsafe extern "C" fn librdf_storage_hashes_init(
    storage: *mut librdf_storage,
    name: *const libc::c_char,
    options: *mut librdf_hash,
) -> libc::c_int {
    let mut hash_type: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut db_dir: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut indexes: *mut libc::c_char = 0 as *mut libc::c_char;
    let mut mode: libc::c_int;
    let mut is_writable: libc::c_int;
    let mut is_new: libc::c_int;
    let mut name_copy: *mut libc::c_char = 0 as *mut libc::c_char;
    if options.is_null() {
        return 1i32;
    } else {
        hash_type = librdf_hash_get_del(
            options,
            b"hash-type\x00" as *const u8 as *const libc::c_char,
        );
        if hash_type.is_null() {
            return 1i32;
        } else {
            db_dir = librdf_hash_get_del(options, b"dir\x00" as *const u8 as *const libc::c_char);
            indexes =
                librdf_hash_get_del(options, b"indexes\x00" as *const u8 as *const libc::c_char);
            /* POSIX open(2) modes are int so this cast is OKish */
            mode = librdf_hash_get_as_long(options, b"mode\x00" as *const u8 as *const libc::c_char)
                as libc::c_int;
            if mode < 0i32 {
                /* default mode */
                mode = 0o644i32
            }
            is_writable = librdf_hash_get_as_boolean(
                options,
                b"write\x00" as *const u8 as *const libc::c_char,
            );
            if is_writable < 0i32 {
                /* default is WRITABLE */
                is_writable = 1i32
            }
            is_new =
                librdf_hash_get_as_boolean(options, b"new\x00" as *const u8 as *const libc::c_char);
            if is_new < 0i32 {
                /* default is NOT NEW */
                is_new = 0i32
            }
            if !name.is_null() {
                name_copy =
                    malloc(strlen(name).wrapping_add(1i32 as libc::c_ulong)) as *mut libc::c_char;
                if name_copy.is_null() {
                    return 1i32;
                } else {
                    strcpy(name_copy, name);
                }
            }
            return librdf_storage_hashes_init_common(
                storage,
                name_copy,
                hash_type,
                db_dir,
                indexes,
                mode,
                is_writable,
                is_new,
                options,
            );
        }
    };
}
