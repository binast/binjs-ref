//! A multipart format, in which each part can be compressed independently.

use ast::grammar::{ Field, FieldName, Interface, NodeName, Syntax };
use bytes;
use bytes::compress::*;
use bytes::serialize::*;
use bytes::varnum::*;
use token::io::*;
use util::{ Pos, ReadConst };

use std;
use std::collections::HashMap;
use std::cell::RefCell;
use std::hash::Hash;
use std::io::{ Cursor, Read, Seek, Write };
use std::rc::Rc;
use std::string::FromUtf8Error;

use vec_map::VecMap;

const HEADER_STRINGS_TABLE : &str = "[STRINGS]";
const HEADER_GRAMMAR_TABLE: &str = "[GRAMMAR]";
const HEADER_TREE: &str = "[TREE]";


#[derive(Debug)]
pub enum GrammarError {
    NoSuchKind(String),
    NoSuchField {
        kind: NodeName,
        field: FieldName
    }
}

#[derive(Debug)]
pub enum TokenWriterError {
    GrammarError(GrammarError),
    WriteError(std::io::Error),
}

#[derive(Debug)]
pub enum TokenReaderError {
    ReadError(std::io::Error),
    Encoding(FromUtf8Error),
    EmptyValue,
    BadLength { expected: usize, got: usize },
    BadHeader,
    BadCompression(std::io::Error),
    InvalidNodeName(Option<String>),
    InvalidFieldName(Option<String>),
    NotInList,
    EndOffsetError {
        start: u64,
        expected: u64,
        found: u64,
        description: String,
    },
    BadStringIndex(u32),
    InvalidValue,
    BadKindIndex(u32),
    GrammarError(GrammarError),
}

impl Into<std::io::Error> for TokenReaderError {
    fn into(self) -> std::io::Error {
        std::io::Error::new(std::io::ErrorKind::InvalidData, format!("{:?}", self))
    }
}

/// A value that may be serialized to bytes, optionally compressed.
trait Serializable {
    /// Write the data, without compression.
    fn write<W: Write>(&self, &mut W) -> Result<(), std::io::Error>;

    /// Write the data, with compression.
    fn write_with_compression<W: Write>(&self, out: &mut W, compression: &Compression) -> Result<(), std::io::Error> {
        let mut uncompressed = Vec::with_capacity(2048);
        self.write(&mut uncompressed)?;
        compression.compress(&uncompressed, out)?;
        Ok(())
    }
}

trait FormatInTable {
    const HAS_LENGTH_INDEX : bool;
}

struct BufDeserializer;
impl Deserializer for BufDeserializer {
    type Target = Vec<u8>;
    fn read<R: Read + Seek>(&self, reader: &mut R) -> Result<Self::Target, std::io::Error> {
        let size = reader.size();
        let mut buf = Vec::with_capacity(size);
        unsafe { buf.set_len(size); }
        reader.read_exact(&mut buf)?;
        Ok(buf)
    }
}

/// A `String` is serialized as:
/// - number of UTF-8 bytes (varnum);
/// - sequence of UTF-8 bytes.
impl Serializable for String {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        out.write_varnum(self.len() as u32)?;
        out.write_all(self.as_bytes())?;
        Ok(())
    }
}

/// A `String | null` is serialized as:
/// - number of UTF-8 bytes (varnum);
/// - sequence of UTF-8 bytes.
///
/// With the following special case used to represent the null string:
/// - number of UTF-8 bytes (2 as varnum);
/// - sequence [255, 0] (which is invalid UTF-8).
impl Serializable for Option<String> {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        const EMPTY_STRING: [u8; 2] = [255, 0];
        match *self {
            None => {
                out.write_varnum(EMPTY_STRING.len() as u32)?;
                out.write_all(&EMPTY_STRING)?;
            },
            Some(ref data) => {
                data.write(out)?;
            }
        }
        Ok(())
    }
}
impl FormatInTable for Option<String> {
    const HAS_LENGTH_INDEX : bool = false;
}

impl Deserializer for Option<String> {
    type Target = Self;
    fn read<R: Read>(&self, inp: &mut R) -> Result<Self, std::io::Error> {
        let mut byte_len = 0;
        inp.read_varnum(&mut byte_len)?;
        let mut bytes = Vec::with_capacity(byte_len as usize);
        unsafe { bytes.set_len(byte_len as usize); }
        inp.read_exact(&mut bytes)?;
        if &bytes == &[255, 0] {
            Ok(None)
        } else {
            String::from_utf8(bytes)
                .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidData, err))
                .map(Some)
        }
    }
}

/// An entry in an WriterTable.
///
/// This entry tracks the number of instances of the entry used in the table.
struct TableEntry<T> where T: Clone { // We shouldn't need the `Clone`, sigh.
    /// Number of instances of this entry around.
    instances: RefCell<u32>,

    /// The actual data.
    data: T,

    /// The index, actually computed in `write()`.
    index: TableIndex<T>
}
impl<T> TableEntry<T> where T: Clone {
    fn new(data: T) -> Self {
        TableEntry {
            instances: RefCell::new(1),
            data,
            index: TableIndex::new()
        }
    }
}

/// A table, used to define a varnum-indexed header
struct WriterTable<Key, Value> where Key: Eq + Hash + Clone, Value: Clone + Serializable + FormatInTable {
    map: HashMap<Key, TableEntry<Value>>
}

impl<Key, Value> WriterTable<Key, Value> where Key: Eq + Hash + Clone, Value: Clone + Serializable + FormatInTable {
    pub fn new() -> Self {
        WriterTable {
            map: HashMap::new()
        }
    }
}


impl<Key, Value> WriterTable<Key, Value> where Key: Eq + Hash + Clone, Value: Clone + Serializable + FormatInTable {
    /// Get an entry from the header.
    ///
    /// The number of entries is incremented by 1.
    fn get(&self, kind: &Key) -> Option<&TableEntry<Value>> {
        self.map.get(kind)
            .map(|entry| {
                // Increment by 1
                let mut borrow = entry.instances.borrow_mut();
                *borrow += 1;
                entry
            })
    }

    /// Insert a new entry, with a number of instances of 1.
    fn insert(&mut self, key: Key, value: Value) -> TableIndex<Value> {
        let entry = TableEntry::new(value);
        let index = entry.index.clone();
        if let Some(_) = self.map.insert(key, entry) {
            panic!("The table already contains an entry for this key");
        }
        index
    }
}

/// An WriterTable is serialized as
///
/// - number of entries (varnum);
/// - if the type of Values does not contain its own length index
///    - for each entry,
///       -   byte length of entry (varnum);
/// - for each entry,
/// -   serialization of entry.
impl<Key, Value> Serializable for WriterTable<Key, Value> where Key: Eq + Hash + Clone, Value: Clone + FormatInTable + Serializable {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        // Sort entries by number of uses.
        let mut contents : Vec<_> = self.map.values().collect();
        contents.sort_unstable_by(|a, b| u32::cmp(&*a.instances.borrow(), &*b.instances.borrow()));

        // Assign TableIndex
        for i in 0..contents.len() {
            let mut borrow = contents[i].index.index.borrow_mut();
            *borrow = Some(i as u32);
        }

        // Serialize each entry
        let mut serialized = Vec::with_capacity(contents.len());
        for entry in contents.drain(..) {
            let mut bytes = Vec::with_capacity(256);
            entry.data.write(&mut bytes)?;
            serialized.push(bytes);
        }

        // Write number of entries
        out.write_varnum(serialized.len() as u32)?;

        if Value::HAS_LENGTH_INDEX {
            // Write length of each entry
            for entry in &serialized {
                out.write_varnum(entry.len() as u32)?;
            }
        }

        // Write actual content of each entry
        for entry in &serialized {
            out.write_all(&entry)?;
        }

        // Sanity check
        for entry in self.map.values() {
            debug_assert!(entry.index.index.borrow().is_some())
        }

        Ok(())
    }
}


struct ReaderTable<Value> {
    map: VecMap<Value>,
}
impl<Value> ReaderTable<Value> {
    fn get(&self, key: u32) -> Option<&Value> {
        self.map.get(key as usize)
    }
}

struct NodeDescriptionDeserializer<'a> {
    syntax: &'a Syntax
}

impl<'a> Deserializer for NodeDescriptionDeserializer<'a> {
    type Target = ReaderNodeDescription<'a>;
    fn read<R: Read + Seek>(&self, inp: &mut R) -> Result<Self::Target, std::io::Error> {
        // Extract kind
        let strings_deserializer : Option<String> = None;
        let name = match strings_deserializer.read(inp)? {
            None => return Err(TokenReaderError::InvalidNodeName(None).into()),
            Some(x) => x
        };

        let kind = match self.syntax.get_node_name(&name) {
            None => return Err(TokenReaderError::InvalidNodeName(Some(name)).into()),
            Some(x) => x.clone()
        };

        // Extract fields
        let mut number_of_entries = 0;
        inp.read_varnum(&mut number_of_entries)?;

        let mut fields = Vec::with_capacity(number_of_entries as usize);
        for _ in 0..number_of_entries {
            let name = match strings_deserializer.read(inp)? {
                None => return Err(TokenReaderError::InvalidFieldName(None).into()),
                Some(x) => x
            };
            let field = match self.syntax.get_field_name(&name) {
                None => return Err(TokenReaderError::InvalidFieldName(Some(name)).into()),
                Some(x) => x
            };
            fields.push(field.clone())
        }

        ReaderNodeDescription::new(self.syntax, kind, fields)
            .map_err(|err| TokenReaderError::GrammarError(err).into())
    }
}

struct ReaderTableDeserializer<D> where D: Deserializer {
    deserializer: D,
}

impl<'a, D> Deserializer for ReaderTableDeserializer<D> where D: Deserializer, D::Target: FormatInTable {
    type Target = ReaderTable<D::Target>;
    fn read<R: Read + Seek>(&self, inp: &mut R) -> Result<Self::Target, std::io::Error> {
        // Get number of entries.
        let mut number_of_entries = 0;
        inp.read_varnum(&mut number_of_entries)?;

        let mut map = VecMap::with_capacity(number_of_entries as usize);

        if D::Target::HAS_LENGTH_INDEX {
            // Read table of lengths.
            let mut byte_lengths = Vec::with_capacity(number_of_entries as usize);
            for _ in 0..number_of_entries {
                let mut byte_len = 0;
                inp.read_varnum(&mut byte_len)?;
                byte_lengths.push(byte_len);
            }

            // Now read each entry.
            for i in 0..number_of_entries as usize {
                let expected_byte_length = byte_lengths[i] as usize;
                let start = inp.pos();
                let value = self.deserializer.read(inp)?;
                let stop = inp.pos();
                if stop - start != expected_byte_length {
                    return Err(TokenReaderError::BadLength {
                        expected: expected_byte_length,
                        got: stop - start
                    }.into());
                }
                map.insert(i, value);
            }
        } else {
            for i in 0..number_of_entries as usize {
                let value = self.deserializer.read(inp)?;
                map.insert(i, value);
            }
        }

        Ok(ReaderTable { map })
    }
}



#[derive(PartialEq, Eq, Clone)] // FIXME: Clone shouldn't be necessary. Sigh.
pub struct NodeDescription {
    kind: NodeName,
    fields: Vec<FieldName>,
}

pub struct ReaderNodeDescription<'a> {
    fields: Rc<Box<[Field]>>,
    kind: &'a Interface,
}

impl<'a> FormatInTable for ReaderNodeDescription<'a> {
    const HAS_LENGTH_INDEX : bool = true;
}

impl<'a> ReaderNodeDescription<'a> {
    pub fn new(syntax: &'a Syntax, kind_name: NodeName, field_names: Vec<FieldName>) -> Result<Self, GrammarError> {
        // Get the kind
        let kind = syntax.get_kind(kind_name.to_str())
            .ok_or_else(|| GrammarError::NoSuchKind(kind_name.to_string().clone()))?;
        let interface = syntax.get_interface_by_kind(&kind)
            .ok_or_else(|| GrammarError::NoSuchKind(kind_name.to_string().clone()))?;

        // Build the list of fields
        let mut fields = Vec::with_capacity(field_names.len());
        for field_name in &field_names {
            let field = interface.get_field_by_name(field_name)
                .ok_or_else(|| GrammarError::NoSuchField {
                    kind: kind_name.clone(),
                    field: field_name.clone()
                })?;
            fields.push(field.clone())
        }

        Ok(ReaderNodeDescription {
            kind: interface,
            fields: Rc::new(fields.into_boxed_slice())
        })
    }
}
/// Format:
/// - kind name (see Option<String>);
/// - number of fields (varnum);
/// - for each field
///    - field name (see Option<String>)
impl Serializable for NodeDescription {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        self.kind.to_string().write(out)?;
        out.write_varnum(self.fields.len() as u32)?;
        for field in &self.fields {
            field.to_string().write(out)?;
        }
        Ok(())
    }
}

impl FormatInTable for NodeDescription {
    const HAS_LENGTH_INDEX : bool = true;
}

enum Item {
    String(TableIndex<Option<String>>),
    NodeDescription(TableIndex<NodeDescription>),
    Encoded(Vec<u8>),
    List {
        items: Vec<Tree>,
        byte_len: bool
    }
}

#[derive(Clone)]
pub struct Tree(Rc<Item>);

impl Serializable for Tree {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        match *self.0 {
            Item::String(ref index) => {
                index.write(out)?;
            },
            Item::NodeDescription(ref index) => {
                index.write(out)?;
            },
            Item::Encoded(ref vec) => {
                out.write_all(&vec)?;
            },
            Item::List { ref items, ref byte_len } => {
                if *byte_len {
                    // Compute byte length
                    let mut buf = Vec::with_capacity(1024);
                    for item in items {
                        item.write(&mut buf)?;
                    }
                    // Write byte length
                    out.write_varnum(buf.len() as u32)?;
                    // Write data
                    out.write_all(&buf)?;
                } else {
                    for item in items {
                        item.write(out)?;
                    }
                }
            }
        }
        Ok(())
    }
}

struct TableIndex<T> {
    phantom: std::marker::PhantomData<T>,
    index: Rc<RefCell<Option<u32>>>,
}

impl<T> Clone for TableIndex<T> {
    fn clone(&self) -> Self {
        TableIndex {
            phantom: std::marker::PhantomData,
            index: self.index.clone()
        }
    }
}
impl<T> TableIndex<T> {
    fn new() -> Self {
        TableIndex {
            phantom: std::marker::PhantomData,
            index: Rc::new(RefCell::new(None))
        }
    }
}
impl<T> Serializable for TableIndex<T> {
    fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
        if let Some(ref i) = *self.index.borrow() {
            out.write_varnum(*i)?;
        } else {
            panic!("Attempting to serialize a TableIndex whose index is None");
        }
        Ok(())
    }
}

struct TreeTokenReaderImpl<'a> {
    reader: Cursor<Vec<u8>>,
    strings_table: ReaderTable<Option<String>>,
    grammar_table: ReaderTable<ReaderNodeDescription<'a>>,
    poisoned: bool,
}
impl<'a> TreeTokenReaderImpl<'a> {
    fn position(&mut self) -> u64 {
        self.reader.position()
    }

    fn try<T, E, F>(&mut self, f: F) -> Result<T, E> where F: FnOnce(&mut Self) -> Result<T, E> {
        assert!(!self.poisoned, "TreeTokenReader is poisoned");
        f(self)
            .map_err(|err| {
                self.poisoned = true;
                err
            })
    }

    /// Read data to a buffer.
    ///
    /// # Panics
    ///
    /// If `self` is poisoned.
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, TokenReaderError> {
        self.try(move |me| {
            me.reader.read_exact(buf)
                .map_err(TokenReaderError::ReadError)?;
            Ok(buf.len())
        })
    }

    fn read_varnum(&mut self) -> Result<u32, TokenReaderError> {
        self.try(|me| {
            let mut result = 0;
            me.reader.read_varnum(&mut result)
                .map_err(TokenReaderError::ReadError)?;
            Ok(result)
        })
    }
}

pub struct TreeTokenReader<'a> {
    // Shared with all children.
    owner: Rc<RefCell<TreeTokenReaderImpl<'a>>>,
}


impl<'a> TreeTokenReader<'a> {
    pub fn new<R: Read + Seek>(mut reader: R, syntax: &'a Syntax) -> Result<Self, TokenReaderError> {
        // Check magic headers.
        println!("TreeTokenReader::new()");

        const MAGIC_HEADER: &'static [u8; 5] = b"BINJS";
        const FORMAT_VERSION: u32 = 0;

        println!("TreeTokenReader::new() checking magic header");
        reader.read_const(MAGIC_HEADER)
            .map_err(TokenReaderError::ReadError)?;

        println!("TreeTokenReader::new() checking version");
        let mut version = 0;
        reader.read_varnum(&mut version)
            .map_err(TokenReaderError::ReadError)?;

        if version != FORMAT_VERSION {
            return Err(TokenReaderError::BadHeader)
        }

        // At this stage, we could start parallelizing reads between grammar table and strings table, possibly even the tree.

        println!("TreeTokenReader::new() reading grammar table");
        reader.read_const(HEADER_GRAMMAR_TABLE.as_bytes())
            .map_err(TokenReaderError::ReadError)?;

        // Read grammar table
        let grammar_deserializer = ReaderTableDeserializer {
            deserializer: NodeDescriptionDeserializer {
                syntax: syntax
            }
        };
        let grammar_table = Compression::decompress(&mut reader, &grammar_deserializer)
            .map_err(TokenReaderError::BadCompression)?;
        println!("TreeTokenReader::new() grammar table has {} entries", grammar_table.map.len());


        // Read strings table
        reader.read_const(HEADER_STRINGS_TABLE.as_bytes())
            .map_err(TokenReaderError::ReadError)?;
        println!("TreeTokenReader::new() reading strings table");
        let strings_deserializer = ReaderTableDeserializer {
            deserializer: None /* Option<String> */
        };
        let strings_table = Compression::decompress(&mut reader, &strings_deserializer)
            .map_err(TokenReaderError::BadCompression)?;
        println!("TreeTokenReader::new() strings table has {} entries", strings_table.map.len());

        // Decompress tree section to memory (we could as well stream it)
        println!("TreeTokenReader::new() decompressing tree to memory");
        reader.read_const(HEADER_TREE.as_bytes())
            .map_err(TokenReaderError::ReadError)?;
        let decompressed_tree = Compression::decompress(&mut reader, &BufDeserializer)
            .map_err(TokenReaderError::BadCompression)?;
        let implem = TreeTokenReaderImpl {
            grammar_table,
            strings_table,
            reader: Cursor::new(decompressed_tree),
            poisoned: false,
        };

        Ok(TreeTokenReader {
            owner: Rc::new(RefCell::new(implem))
        })
    }
}

pub struct SimpleGuard<'a> {
    parent: TrivialGuard<TokenReaderError>,
    owner: Rc<RefCell<TreeTokenReaderImpl<'a>>>,
}
impl<'a> SimpleGuard<'a> {
    fn new(owner: Rc<RefCell<TreeTokenReaderImpl<'a>>>) -> Self {
        SimpleGuard {
            parent: TrivialGuard::new(),
            owner
        }
    }
}
impl<'a> Guard for SimpleGuard<'a> {
    type Error = TokenReaderError;
    fn done(mut self) -> Result<(), Self::Error> {
        self.parent.finalized = true;
        Ok(())
    }
}
impl<'a> Drop for SimpleGuard<'a> {
    fn drop(&mut self) {
        if self.owner.borrow().poisoned {
            // Don't trigger an assertion failure if we had to bailout because of an exception.
            self.parent.finalized = true;
        }
        // Now `self.parent.drop()` will be called.
    }
}

pub struct ListGuard<'a> {
    expected_end: u64,
    start: u64,
    parent: SimpleGuard<'a>
}

impl<'a> ListGuard<'a> {
    fn new(owner: Rc<RefCell<TreeTokenReaderImpl<'a>>>, start: u64, byte_len: u64) -> Self {
        ListGuard {
            parent: SimpleGuard::new(owner),
            start,
            expected_end: start + byte_len,
        }
    }
}
impl<'a> Guard for ListGuard<'a> {
    type Error = TokenReaderError;
    fn done(mut self) -> Result<(), Self::Error> {
        self.parent.parent.finalized = true;

        let mut owner = self.parent.owner.borrow_mut();
        if owner.poisoned {
            return Ok(())
        }

        let found = owner.position();
        if found != self.expected_end {
            owner.poisoned = true;
            return Err(TokenReaderError::EndOffsetError {
                start: self.start,
                expected: self.expected_end,
                found,
                description: "list".to_string()
            })
        }

        Ok(())
    }
}
impl<'a> Drop for ListGuard<'a> {
    fn drop(&mut self) {
        // Now `self.parent.drop()` will be called.
    }
}

impl<'a> TokenReader for TreeTokenReader<'a> {
    type Error = TokenReaderError;
    type TaggedGuard = SimpleGuard<'a>;
    type UntaggedGuard = SimpleGuard<'a>;
    type ListGuard = ListGuard<'a>;

    fn poison(&mut self) {
        self.owner.borrow_mut().poisoned = true;
    }

    fn string(&mut self) -> Result<Option<String>, Self::Error> {
        self.owner.borrow_mut().try(|owner| {
            let index = owner.read_varnum()?;
            match owner.strings_table.get(index) {
                Some(result) => Ok(result.clone()),
                None => Err(TokenReaderError::BadStringIndex(index))
            }
        })
    }


    /// Read a single `f64`. Note that all numbers are `f64`.
    fn float(&mut self) -> Result<Option<f64>, Self::Error> {
        self.owner.borrow_mut().try(|owner| {
            let mut buf : [u8; 8] = unsafe { std::mem::uninitialized() };
            owner.read(&mut buf)?;
            Ok(bytes::float::float_of_bytes(&buf))
        })
    }

    /// Read a single `bool`.
    fn bool(&mut self) -> Result<Option<bool>, Self::Error> {
        self.owner.borrow_mut().try(|owner| {
            let mut buf : [u8; 1] = unsafe { std::mem::uninitialized() };
            owner.read(&mut buf)?;
            bytes::bool::bool_of_bytes(&buf)
                .map_err(|_| TokenReaderError::InvalidValue)
        })
    }

    /// Start reading a list.
    ///
    /// Returns an extractor for that list and the number of elements
    /// in the list. Before dropping the sub-extractor, callers MUST
    /// either reach the end of the list or call `skip()`.
    fn list(&mut self) -> Result<(u32, Self::ListGuard), Self::Error> {
        let clone = self.owner.clone();
        self.owner.borrow_mut().try(move |owner| {
            let byte_len = owner.read_varnum()?;
            let guard = ListGuard::new(clone, owner.position(), byte_len as u64);
            let list_len = owner.read_varnum()?;
            Ok((list_len, guard))
        })
    }

    /// Start reading a tagged tuple. If the stream was encoded
    /// properly, the tag is attached to an **ordered** tuple of
    /// fields that may be extracted **in order**.
    ///
    /// Returns the tag name, the ordered array of fields in which
    /// the contents must be read, and a sub-extractor dedicated
    /// to that tuple. The sub-extractor MUST be consumed entirely.
    fn tagged_tuple(&mut self) -> Result<(String, Rc<Box<[Field]>>, Self::TaggedGuard), Self::Error> {
        let clone = self.owner.clone();
        self.owner.borrow_mut().try(|owner| {
            let index = owner.read_varnum()?;
            let description = owner.grammar_table.get(index)
                .ok_or(TokenReaderError::BadKindIndex(index))?;

            let tag = description.kind.name().to_string().clone();
            let fields = description.fields.clone();
            let guard = SimpleGuard::new(clone);
            Ok((tag, fields, guard))
        })
    }

    /// Start reading an untagged tuple. The sub-extractor MUST
    /// be consumed entirely.
    fn untagged_tuple(&mut self) -> Result<Self::UntaggedGuard, Self::Error> {
        let clone = self.owner.clone();
        Ok(SimpleGuard::new(clone))
    }
}

pub struct TreeTokenWriter<'a> {
    /// The table defining the accepted TaggedTuple
    /// and how they are laid out in the binary.
    grammar_table: WriterTable<NodeName, NodeDescription>,

    /// The strings used in the binary.
    strings_table: WriterTable<Option<String>, Option<String>>,

    root: Option<Tree>,

    syntax: &'a Syntax,

    data: Vec<u8>,

    options: WriteOptions,
}


#[derive(Clone, Debug)]
pub struct WriteOptions {
    pub grammar_table: Compression,
    pub strings_table: Compression,
    pub tree: Compression,
}

impl<'a> TreeTokenWriter<'a> {
    pub fn new(options: WriteOptions, syntax: &'a Syntax) -> Self {
        TreeTokenWriter {
            grammar_table: WriterTable::new(),
            strings_table: WriterTable::new(),
            root: None,
            data: Vec::with_capacity(1024),
            syntax,
            options,
        }
    }

    fn register(&mut self, data: Item) -> Tree {
        let result = Rc::new(data);
        self.root = Some(Tree(result.clone()));
        Tree(result)
    }

    pub fn done(mut self) -> Result<Box<[u8]>, TokenWriterError> {
        // Write header to byte stream
        self.data.write_all(b"BINJS")
            .map_err(TokenWriterError::WriteError)?;

        const FORMAT_VERSION : u32 = 0;
        self.data.write_varnum(FORMAT_VERSION)
            .map_err(TokenWriterError::WriteError)?;

        // Write grammar table to byte stream.
        self.data.write_all(HEADER_GRAMMAR_TABLE.as_bytes())
            .map_err(TokenWriterError::WriteError)?;
        self.grammar_table.write_with_compression(&mut self.data, &self.options.grammar_table)
            .map_err(TokenWriterError::WriteError)?;

        // Write strings table to byte stream.
        self.data.write_all(HEADER_STRINGS_TABLE.as_bytes())
            .map_err(TokenWriterError::WriteError)?;
        self.strings_table.write_with_compression(&mut self.data, &self.options.strings_table)
            .map_err(TokenWriterError::WriteError)?;

        // Write tree itself to byte stream.
        self.data.write_all(HEADER_TREE.as_bytes())
            .map_err(TokenWriterError::WriteError)?;
        if let Some(ref root) = self.root {
            root.write_with_compression(&mut self.data, &self.options.tree)
                .map_err(TokenWriterError::WriteError)?;
        }

        Ok(self.data.clone().into_boxed_slice())
    }
}

/// The entire file is formatted as:
///
/// - the characters "BINJS";
/// - a container version number (varnum, currently 0);
/// - the compressed grammar table (see above);
/// - the compressed strings table (see above);
/// - the compressed tree (see above).
impl<'a> TokenWriter for TreeTokenWriter<'a> {
    type Tree = Tree;
    type Error = TokenWriterError;
    type Data = Box<[u8]>;

    fn done(self) -> Result<Self::Data, Self::Error> {
        (self as TreeTokenWriter<'a>).done()
    }

    fn float(&mut self, value: Option<f64>) -> Result<Self::Tree, Self::Error> {
        let bytes : Vec<_> = bytes::float::bytes_of_float(value).iter().cloned().collect();
        Ok(self.register(Item::Encoded(bytes)))
    }

    fn bool(&mut self, data: Option<bool>)  -> Result<Self::Tree, Self::Error> {
        let bytes = bytes::bool::bytes_of_bool(data).iter().cloned().collect();
        Ok(self.register(Item::Encoded(bytes)))
    }
    fn string(&mut self, data: Option<&str>) -> Result<Self::Tree, Self::Error> {
        let key = data.map(str::to_string);
        let index = self.strings_table
            .get(&key)
            .map(|entry| entry.index.clone());
        if let Some(index) = index {
            return Ok(self.register(Item::String(index)))
        }
        let value = key.clone(); // FIXME: That's pretty wasteful.
        let index = self.strings_table.insert(key, value);
        Ok(self.register(Item::String(index)))
    }
    fn list(&mut self, children: Vec<Self::Tree>) -> Result<Self::Tree, Self::Error> {
        let mut items = Vec::with_capacity(children.len() + 1);
        // First child is the number of children.
        let mut encoded_number_of_items = Vec::with_capacity(8);
        encoded_number_of_items.write_varnum(children.len() as u32)
            .map_err(TokenWriterError::WriteError)?;
        items.push(Tree(Rc::new(Item::Encoded(encoded_number_of_items))));
        // Next, we have `children`.
        items.extend(children);
        let result = Item::List {
            items,
            byte_len: true
        };
        Ok(self.register(result))
    }
    fn untagged_tuple(&mut self, children: &[Self::Tree]) -> Result<Self::Tree, Self::Error> {
        let result = Item::List {
            items: children.iter().cloned().collect(),
            byte_len: false
        };
        Ok(self.register(result))
    }

    // Tagged tuple:
    //
    // All tagged tuples with the same `tag` are written with the fields in the same order.
    //
    // - index in the grammar table (varnum);
    // - for each item, in the order specified
    //    - the item (see item)
    fn tagged_tuple(&mut self, tag: &str, children: &[(&Field, Self::Tree)]) -> Result<Self::Tree, Self::Error> {
        let mut data : Vec<Tree> = vec![];
        {
            let tag = match self.syntax.get_node_name(tag) {
                None => return Err(TokenWriterError::GrammarError(GrammarError::NoSuchKind(tag.to_string()))),
                Some(node) => node
            };

            // Establish the order in which children need to be written.
            let mut order = vec![];
            let index = if let Some(entry) = self.grammar_table.get(tag) {
                for name in &entry.data.fields {
                    order.push(name.clone())
                }
                Some(entry.index.clone())
            } else {
                for child in children {
                    order.push(child.0.name().clone())
                }
                None
            };
            let index = match index {
                None => {
                    let description = NodeDescription {
                        kind: tag.clone(),
                        fields: order.clone()
                    };
                    self.grammar_table.insert(tag.clone(), description)
                }
                Some(index) => index
            };

            // Now write data.
            data.push(Tree(Rc::new(Item::NodeDescription(index))));

            for field in order.drain(..) {
                for child in children {
                    if child.0.name() == &field {
                        data.push(child.1.clone());
                        break
                    }
                }
            }
        }
        Ok(self.register(Item::List {
            items: data,
            byte_len: false
        }))
    }
}

trait ExtendFromUTF8 {
    fn extend_from_str(&mut self, &str);
}

impl ExtendFromUTF8 for Vec<u8> {
    fn extend_from_str(&mut self, data: &str) {
        self.extend_from_slice(data.as_bytes());
    }
}

#[test]
fn test_multipart_io() {
    println!("Multipart (de)tokenizer test starting");
    use ast::annotation::*;
    use ast::grammar::*;

    use std::fs::*;

    use serde_json;
    use serde_json::Value as JSON;

    use std::io::{ Cursor, Write };

    type Object = serde_json::Map<String, JSON>;

    // All combinations of options for compression.
    let all_options = {
        use self::Compression::*;
        let mut vec = vec![];
        let compressions = [Identity, Gzip, Deflate, /*Lzw, Brotli don't work yet*/];
        for grammar_table in &compressions {
            for strings_table in &compressions {
                for tree in &compressions {
                    vec.push(WriteOptions {
                        grammar_table: grammar_table.clone(),
                        strings_table: strings_table.clone(),
                        tree: tree.clone(),
                    });
                }
            }
        }
        vec
    };

    debug!("Setting up syntax");
    let mut builder = SyntaxBuilder::new();
    let null = builder.node_name("Null");
    let null_kind = builder.kind_name("Null");
    builder.add_kinded_interface(&null).unwrap();

    let kinded = builder.node_name("Pattern");
    let field_string = Field::new(builder.field_name("id"), Type::string());
    let field_number = Field::new(builder.field_name("value"), Type::number());

    builder.add_kinded_interface(&kinded).unwrap()
        .with_own_field(field_string.clone())
        .with_own_field(field_number.clone());

    struct FakeAnnotator;
    impl Annotator for FakeAnnotator {
        fn name(&self) -> String {
            unimplemented!()
        }
        fn process_references(&self, _: &Annotator, _: &mut Context<RefContents>, _: &mut Object) -> Result<(), ASTError> {
            unimplemented!()
        }
        fn process_declarations(&self, _: &Annotator, _: &mut Context<DeclContents>, _: &mut Object) -> Result<(), ASTError> {
            unimplemented!()
        }
    }

    let syntax = builder.into_syntax(SyntaxOptions {
        root: &kinded,
        null: &null_kind,
        annotator: Box::new(FakeAnnotator)
    });

    for options in all_options {
        println!("Options {:?}", options);
        let suffix = format!("{:?}-{:?}-{:?}", options.grammar_table, options.strings_table, options.tree);

        debug!("Testing string I/O");
        {

            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            writer.string(Some("simple string"))
                .expect("Writing simple string");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-simple-string-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let simple_string = reader.string()
                .expect("Reading simple string")
                .expect("Non-null string");
            assert_eq!(&simple_string, "simple string");
        }

        {
            let string = "string with escapes \u{0}\u{1}\u{0}";
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            writer.string(Some(string))
                .expect("Writing string with escapes");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-string-with-escapes-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let escapes_string = reader.string()
                .expect("Reading string with escapes")
                .expect("Non-null string");
            assert_eq!(escapes_string, string);
        }

        debug!("Testing untagged tuple I/O");

        {
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            writer.untagged_tuple(&[])
                .expect("Writing empty untagged tuple");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-empty-untagged-tuple-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let guard = reader.untagged_tuple()
                .expect("Reading empty untagged tuple");

            guard.done()
                .expect("Empty untagged tuple read properly");

        }

        {
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.string(Some("bar")).unwrap();
            writer.untagged_tuple(&[item_0, item_1])
                .expect("Writing trivial untagged tuple");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-trivial-untagged-tuple-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let guard = reader.untagged_tuple()
                .expect("Reading trivial untagged tuple");
            let simple_string = reader.string()
                .expect("Reading trivial tuple[0]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "foo");
            let simple_string = reader.string()
                .expect("Reading trivial tuple[1]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "bar");

            guard.done()
                .expect("Untagged tuple read properly");
        }

        debug!("Testing tagged tuple I/O");

        {
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.float(Some(3.1415)).unwrap();
            writer.tagged_tuple(kinded.to_str(), &[(&field_string, item_0), (&field_number, item_1)])
                .expect("Writing trivial tagged tuple");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-simple-tagged-tuple-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let (name, fields, guard) = reader.tagged_tuple()
                .expect("Reading trivial tagged tuple");
            assert_eq!(name, "Pattern".to_string());

            // Order of fields is not deterministic
            if fields[0].name().to_string() == &"id".to_string() {
                assert_eq!(fields[0].name().to_string(), &"id".to_string());
                assert_eq!(*fields[0].type_(), Type::string());
                assert_eq!(fields[1].name().to_string(), &"value".to_string());
                assert_eq!(*fields[1].type_(), Type::number());
                let simple_string = reader.string()
                    .expect("Reading trivial tagged tuple[0]")
                    .expect("Reading a non-null string");
                let simple_float = reader.float()
                    .expect("Reading trivial tagged tuple[1]")
                    .expect("Reading a non-null float");
                assert_eq!(&simple_string, "foo");
                assert_eq!(simple_float, 3.1415);
            } else {
                assert_eq!(fields[1].name().to_string(), &"id".to_string());
                assert_eq!(*fields[1].type_(), Type::string());
                assert_eq!(fields[0].name().to_string(), &"value".to_string());
                assert_eq!(*fields[0].type_(), Type::number());
                let simple_float = reader.float()
                    .expect("Reading trivial tagged tuple[1]")
                    .expect("Reading a non-null float");
                let simple_string = reader.string()
                    .expect("Reading trivial tagged tuple[0]")
                    .expect("Reading a non-null string");
                assert_eq!(&simple_string, "foo");
                assert_eq!(simple_float, 3.1415);
            }

            guard.done()
                .expect("Trivial tagged tuple read properly");
        }

        debug!("Testing list I/O");

        {
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            writer.list(vec![])
                .expect("Writing empty list");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-empty-list-{}.binjs", suffix)).unwrap()
                    .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let (len, guard) = reader.list()
                .expect("Reading empty list");
            assert_eq!(len, 0);

            guard.done()
                .expect("Empty list read properly");
        }

        {
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.string(Some("bar")).unwrap();
            writer.list(vec![item_0, item_1])
                .expect("Writing trivial list");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-trivial-list-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let (len, guard) = reader.list()
                .expect("Reading trivial list");
            assert_eq!(len, 2);

            let simple_string = reader.string()
                .expect("Reading trivial list[0]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "foo");
            let simple_string = reader.string()
                .expect("Reading trivial list[1]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "bar");

            guard.done()
                .expect("Trivial list read properly");
        }

        {
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.string(Some("bar")).unwrap();
            let list = writer.list(vec![item_0, item_1])
                .expect("Writing inner list");
            writer.list(vec![list])
                .expect("Writing outer list");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-nested-lists-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let (len, guard) = reader.list()
                .expect("Reading outer list");
            assert_eq!(len, 1);

            let (len, inner_guard) = reader.list()
                .expect("Reading inner list");
            assert_eq!(len, 2);

            let simple_string = reader.string()
                .expect("Reading trivial list[0]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "foo");
            let simple_string = reader.string()
                .expect("Reading trivial list[1]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "bar");

            inner_guard.done()
                .expect("Inner list read properly");
            guard.done()
                .expect("Inner list read properly");
        }
    }
}
