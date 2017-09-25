use ast::grammar::{ Field, FieldName, NodeName, Syntax };
use bytes;
use bytes::compress::*;
use bytes::varnum::*;
use token::*;
use token::io::*;
use token::multipart::*;
use token::GrammarError;

use std;
use std::collections::HashMap;
use std::cell::RefCell;
use std::hash::Hash;
use std::io::Write;
use std::rc::Rc;

use vec_map;
use vec_map::*;

#[derive(Clone, Debug)]
pub struct WriteOptions {
    pub grammar_table: Compression,
    pub strings_table: Compression,
    pub tree: Compression,
}



/// A value that may be serialized to bytes, optionally compressed.
trait Serializable {
    /// Write the data, without compression.
    fn write<W: Write>(&self, &mut W) -> Result<usize, std::io::Error>;

    /// Write the data, with compression.
    fn write_with_compression<W: Write>(&self, out: &mut W, compression: &Compression) -> Result<CompressionResult, std::io::Error> {
        let mut uncompressed = Vec::with_capacity(2048);
        self.write(&mut uncompressed)?;
        compression.compress(&uncompressed, out)
    }
}

/*
trait SerializableWithStatistics: Serializable {
    /// Write the data, without compression.
    fn write<W: Write>(&self, out: &mut W, stats: &mut ItemStatistics) -> Result<usize, std::io::Error> {
        let result = Serializable::write(self, out)?;
        stats.entries += 1;
        stats.uncompressed_bytes += result;
        Ok((result))
    }

    /// Write the data, with compression.
    fn write_with_compression<W: Write>(&self, out: &mut W, compression: &Compression, stats: Option<&mut ItemStatistics>) -> Result<CompressionResult, std::io::Error> {
        let result = Serializable::write_with_compression(self, out, compression)?;
        if let Some(stats) = stats {
            stats.entries += 1;
            stats.uncompressed_bytes += result.before;
            stats.compressed_bytes += result.after;
        }
        Ok(result)
    }
}
*/

impl Serializable for Vec<u8> {
    fn write<W: Write>(&self, out: &mut W) -> Result<usize, std::io::Error> {
        out.write_all(&self)?;
        Ok(self.len())
    }    
}

/// A `String` is serialized as:
/// - number of UTF-8 bytes (varnum);
/// - sequence of UTF-8 bytes.
impl Serializable for String {
    fn write<W: Write>(&self, out: &mut W) -> Result<usize, std::io::Error> {
        let mut total = 0;
        total += out.write_varnum(self.len() as u32)?;
        out.write_all(self.as_bytes())?;
        total += self.len();
        Ok(total)
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
    fn write<W: Write>(&self, out: &mut W) -> Result<usize, std::io::Error> {
        const EMPTY_STRING: [u8; 2] = [255, 0];
        let total = match *self {
            None => {
                let mut total = 0;
                total += out.write_varnum(EMPTY_STRING.len() as u32)?;
                out.write_all(&EMPTY_STRING)?;
                total += EMPTY_STRING.len();
                total
            },
            Some(ref data) => {
                data.write(out)?
            }
        };
        Ok(total)
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
    fn write<W: Write>(&self, out: &mut W) -> Result<usize, std::io::Error> {
        let mut total = 0;

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
        total += out.write_varnum(serialized.len() as u32)?;

        if Value::HAS_LENGTH_INDEX {
            // Write length of each entry
            for entry in &serialized {
                total += out.write_varnum(entry.len() as u32)?;
            }
        }

        // Write actual content of each entry
        for entry in &serialized {
            out.write_all(&entry)?;
            total += entry.len()
        }

        // Sanity check
        for entry in self.map.values() {
            debug_assert!(entry.index.index.borrow().is_some())
        }

        Ok(total)
    }
}


#[derive(PartialEq, Eq, Clone)] // FIXME: Clone shouldn't be necessary. Sigh.
pub struct NodeDescription {
    kind: NodeName,
    fields: Vec<FieldName>,
}

/// Format:
/// - kind name (see Option<String>);
/// - number of fields (varnum);
/// - for each field
///    - field name (see Option<String>)
impl Serializable for NodeDescription {
    fn write<W: Write>(&self, out: &mut W) -> Result<usize, std::io::Error> {
        let mut total = 0;

        total += self.kind.to_string().write(out)?;
        total += out.write_varnum(self.fields.len() as u32)?;
        for field in &self.fields {
            total += field.to_string().write(out)?;
        }
        Ok(total)
    }
}

impl FormatInTable for NodeDescription {
    const HAS_LENGTH_INDEX : bool = true;
}

enum Nature {
    ListHeader,
    ListWhole,
    TaggedTupleHeader,
    TaggedTupleWhole,
    UntaggedTuple,
    Float,
    Bool,
    String,
}

enum Item {
    String(TableIndex<Option<String>>),
    NodeDescription(TableIndex<NodeDescription>),
    Encoded(Vec<u8>),
    List(Vec<Rc<LabelledItem>>)
}

struct LabelledItem {
    item: Item,
    nature: Nature,
}

#[derive(Clone)]
pub struct Tree(Rc<LabelledItem>);

impl LabelledItem {
    fn write<W: Write>(&self, out: &mut W, stats: &mut Statistics) -> Result<usize, std::io::Error> {
        match self.item {
            Item::String(ref index) => {
                let result = index.write(out)?;
                stats.string.entries += 1;
                stats.string.uncompressed_bytes += result;
                Ok(result)
            }
            Item::NodeDescription(ref index) => {
                let result = index.write(out)?;
                stats.tagged_tuple.entries += 1;
                stats.tagged_tuple.uncompressed_bytes += result;
                Ok(result)
            },
            Item::Encoded(ref vec) => {
                out.write_all(&vec)?;
                let result = vec.len();
                let stats = match self.nature {
                    Nature::Bool => Some(&mut stats.bool),
                    Nature::Float => Some(&mut stats.float),
                    Nature::ListHeader => Some(&mut stats.list),
                    _ => None
                };
                if let Some(stats) = stats {
                    stats.entries += 1;
                    stats.uncompressed_bytes += result;
                }
                Ok(result)
            },
            Item::List(ref items) => {
                let mut total = 0;
                match self.nature {
                    Nature::ListWhole => {
                        // Compute byte length
                        let mut buf = Vec::with_capacity(1024);
                        for item in items {
                            item.write(&mut buf, stats)?;
                        }
                        // Write byte length
                        let bytelen_len = out.write_varnum(buf.len() as u32)?;
                        total += bytelen_len;
                        // Write data
                        out.write_all(&buf)?;
                        total += buf.len();
                        // Do not increase  `entries`, lists are accounted for in ListHeader.
                        stats.list.compressed_bytes += bytelen_len;
                    }
                    Nature::TaggedTupleWhole => {
                        for item in items {
                            total += item.write(out, stats)?;
                        }

                        assert!(items.len() > 0);
                        if let LabelledItem {
                            nature: Nature::TaggedTupleHeader,
                            item: Item::NodeDescription(ref index)
                        } = *items[0] {
                            let key = index.index.borrow()
                                .expect("TableIndex hasn't been resolved");
                            match stats.per_kind_index.entry(key as usize) {
                                vec_map::Entry::Occupied(mut entry) => {
                                    let borrow = entry.get_mut();
                                    borrow.entries += 1;
                                }
                                vec_map::Entry::Vacant(entry) => {
                                    entry.insert(ItemStatistics {
                                        entries: 1,
                                        ..ItemStatistics::default()
                                    });
                                }
                            }
                        } else {
                            panic!("Internal error: Tagged tuple doesn't have the expected structure")
                        }
                    }
                    _ => {
                        for item in items {
                            total += item.write(out, stats)?;
                        }
                    }
                }
                Ok(total)
            }
        }
    }
}

impl Tree {
    fn write<W: Write>(&self, out: &mut W, stats: &mut Statistics) -> Result<usize, std::io::Error> {
        self.0.write(out, stats)
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
    fn write<W: Write>(&self, out: &mut W) -> Result<usize, std::io::Error> {
        if let Some(ref i) = *self.index.borrow() {
            out.write_varnum(*i)
        } else {
            panic!("Attempting to serialize a TableIndex whose index is None");
        }
    }
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
            statistics: Statistics::default()
        }
    }

    fn register(&mut self, data: LabelledItem) -> Tree {
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
        let compression = self.grammar_table.write_with_compression(&mut self.data, &self.options.grammar_table)
            .map_err(TokenWriterError::WriteError)?;
        self.statistics.grammar_table.entries = self.grammar_table.map.len();
        self.statistics.grammar_table.uncompressed_bytes = compression.before;
        self.statistics.grammar_table.compressed_bytes = compression.after;

        // Write strings table to byte stream.
        self.data.write_all(HEADER_STRINGS_TABLE.as_bytes())
            .map_err(TokenWriterError::WriteError)?;
        let compression = self.strings_table.write_with_compression(&mut self.data, &self.options.strings_table)
            .map_err(TokenWriterError::WriteError)?;
        self.statistics.strings_table.entries = self.strings_table.map.len();
        self.statistics.strings_table.uncompressed_bytes = compression.before;
        self.statistics.strings_table.compressed_bytes = compression.after;

        // Write tree itself to byte stream.
        self.data.write_all(HEADER_TREE.as_bytes())
            .map_err(TokenWriterError::WriteError)?;
        if let Some(ref root) = self.root {
            let mut buf = Vec::with_capacity(2048);
            root.write(&mut buf, &mut self.statistics)
                .map_err(TokenWriterError::WriteError)?;
            let compression = buf.write_with_compression(&mut self.data, &self.options.tree)
                .map_err(TokenWriterError::WriteError)?;
            self.statistics.tree.entries = 1;
            self.statistics.tree.uncompressed_bytes = compression.before;
            self.statistics.tree.compressed_bytes = compression.after;
        }

        // Compute more statistics on nodes.
        for (key, value) in self.grammar_table.map {
            let index = value.index.index.borrow()
                .expect("Table index hasn't been resolved yet");
            let stats = self.statistics.per_kind_index.get(index as usize)
                .expect("Could not find entry per index");
            self.statistics.per_kind_name.insert(key.clone(), stats.clone());
        }

        println!("Statistics (WIP): {:?}", self.statistics);
        Ok(self.data.clone().into_boxed_slice())
    }
}

impl<'a> TokenWriter for TreeTokenWriter<'a> {
    type Tree = Tree;
    type Error = TokenWriterError;
    type Data = Box<[u8]>;

    fn done(self) -> Result<Self::Data, Self::Error> {
        (self as TreeTokenWriter<'a>).done()
    }

    fn float(&mut self, value: Option<f64>) -> Result<Self::Tree, Self::Error> {
        let bytes : Vec<_> = bytes::float::bytes_of_float(value).iter().cloned().collect();
        Ok(self.register(LabelledItem {
            item: Item::Encoded(bytes),
            nature: Nature::Float,
        }))
    }

    fn bool(&mut self, data: Option<bool>)  -> Result<Self::Tree, Self::Error> {
        let bytes = bytes::bool::bytes_of_bool(data).iter().cloned().collect();
        Ok(self.register(LabelledItem {
            item: Item::Encoded(bytes),
            nature: Nature::Bool
        }))
    }

    fn string(&mut self, data: Option<&str>) -> Result<Self::Tree, Self::Error> {
        let key = data.map(str::to_string);
        let index = self.strings_table
            .get(&key)
            .map(|entry| entry.index.clone());
        if let Some(index) = index {
            return Ok(self.register(LabelledItem {
                item: Item::String(index),
                nature: Nature::String
            }));
        }
        let value = key.clone(); // FIXME: That's pretty wasteful.
        let index = self.strings_table.insert(key, value);
        Ok(self.register(LabelledItem {
            item: Item::String(index),
            nature: Nature::String
        }))
    }
    fn list(&mut self, mut children: Vec<Self::Tree>) -> Result<Self::Tree, Self::Error> {
        let mut items : Vec<Rc<LabelledItem>> = Vec::with_capacity(children.len() + 1);
        // First child is the number of children.
        let mut encoded_number_of_items = Vec::with_capacity(8);
        encoded_number_of_items.write_varnum(children.len() as u32)
            .map_err(TokenWriterError::WriteError)?;
        items.push(Rc::new(LabelledItem {
            item: Item::Encoded(encoded_number_of_items),
            nature: Nature::ListHeader,
        }));
        // Next, we have `children`.
        let children : Vec<_> = children.drain(..)
            .map(|tree| tree.0.clone())
            .collect();
        items.extend(children);
        Ok(self.register(LabelledItem {
            item: Item::List(items),
            nature: Nature::ListWhole,
        }))
    }
    fn untagged_tuple(&mut self, children: &[Self::Tree]) -> Result<Self::Tree, Self::Error> {
        let result = LabelledItem {
            item: Item::List(children.iter()
                .map(|tree| tree.0.clone())
                .collect()
            ),
            nature: Nature::UntaggedTuple,
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
        let mut data : Vec<Rc<LabelledItem>> = vec![];
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
            data.push(Rc::new(LabelledItem {
                item: Item::NodeDescription(index),
                nature: Nature::TaggedTupleHeader,
            }));

            for field in order.drain(..) {
                for child in children {
                    if child.0.name() == &field {
                        let tree = &child.1;
                        let item = &tree.0;
                        data.push(item.clone());
                        break
                    }
                }
            }
        }
        Ok(self.register(LabelledItem {
            item: Item::List(data),
            nature: Nature::TaggedTupleWhole,
        }))
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

    statistics: Statistics,
}


#[derive(Clone, Debug, Default)]
pub struct ItemStatistics {
    entries: usize,
    uncompressed_bytes: usize,
    compressed_bytes: usize,
}

#[derive(Debug, Default)]
pub struct Statistics {
    grammar_table: ItemStatistics,
    strings_table: ItemStatistics,
    tree: ItemStatistics,
    per_kind_index: VecMap<ItemStatistics>, // FIXME: We need to add the kind names
    per_kind_name: HashMap<NodeName, ItemStatistics>,

    bool: ItemStatistics,
    float: ItemStatistics,
    string: ItemStatistics,
    list: ItemStatistics,
    tagged_tuple: ItemStatistics,
    untagged_tuple: ItemStatistics,
}
