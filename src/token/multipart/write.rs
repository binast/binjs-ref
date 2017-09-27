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
use std::fmt::{ Display, Formatter };
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
        contents.sort_unstable_by(|a, b| u32::cmp(&*b.instances.borrow(), &*a.instances.borrow()));

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
                stats.string.own_bytes += result;
                stats.string.total_bytes += result;
                Ok(result)
            }
            Item::NodeDescription(ref index) => {
                let result = index.write(out)?;
                for stat in &mut [&mut stats.tagged_tuple, &mut stats.tagged_header] {
                    stat.entries += 1;
                    stat.own_bytes += result;
                    stat.total_bytes += result;
                }
                Ok(result)
            },
            Item::Encoded(ref vec) => {
                out.write_all(&vec)?;
                let result = vec.len();
                let stats = match self.nature {
                    Nature::Bool => Some(&mut stats.bool),
                    Nature::Float => Some(&mut stats.float),
                    Nature::ListHeader => Some(&mut stats.list_header),
                    _ => None
                };
                if let Some(stats) = stats {
                    stats.entries += 1;
                    stats.own_bytes += result;
                    stats.total_bytes += result;
                    stats.shallow_bytes += result;
                }
                Ok(result)
            },
            Item::List(ref items) => {
                let mut shallow_bytes = 0;
                let mut total_bytes = 0;
                match self.nature {
                    Nature::ListWhole => {
                        // Compute byte length
                        let mut buf = Vec::with_capacity(1024);
                        for item in items {
                            let len = item.write(&mut buf, stats)?;
                            if let Item::List(_) = item.item {
                                // These bytes are not part of the shallow count.
                            } else {
                                shallow_bytes += len;
                            }
                        }
                        // Write byte length
                        let bytelen_len = out.write_varnum(buf.len() as u32)?;
                        total_bytes += bytelen_len;
                        // Write data
                        out.write_all(&buf)?;
                        total_bytes += buf.len();

                        // Update statistics
                        stats.list.entries += 1;
                        stats.list.own_bytes += bytelen_len;
                        stats.list.total_bytes += total_bytes;
                        stats.list.shallow_bytes += shallow_bytes;

                        stats.list_header.own_bytes += bytelen_len;
                        stats.list_header.total_bytes += bytelen_len;
                        stats.list_header.shallow_bytes += bytelen_len;

                        match stats.list_lengths.entry(items.len()) {
                            vec_map::Entry::Occupied(mut entry) => {
                                let borrow = entry.get_mut();
                                *borrow += 1;
                            }
                            vec_map::Entry::Vacant(entry) => {
                                entry.insert(1);
                            }
                        }
                    }
                    Nature::TaggedTupleWhole => {
                        assert!(items.len() > 0);

                        // Size of the first element. Useful for statistics.
                        let mut first_size = None;
                        for item in items {
                            let len = item.write(out, stats)?;
                            if first_size.is_none() {
                                first_size = Some(len);
                            }
                            total_bytes += len;
                            if let Item::List(_) = item.item {
                                // These bytes are not part of the shallow count.
                            } else {
                                shallow_bytes += len;
                            }
                        }
                        let first_size = first_size.unwrap(); // We checked above that `items.len() > 0`.
                        stats.tagged_tuple.entries += 1;
                        stats.tagged_tuple.total_bytes += total_bytes;
                        stats.tagged_tuple.shallow_bytes += shallow_bytes;

                        // Update statistics.
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
                                    borrow.total_bytes += total_bytes;
                                    borrow.own_bytes += first_size;
                                    borrow.shallow_bytes += shallow_bytes;
                                }
                                vec_map::Entry::Vacant(entry) => {
                                    entry.insert(NodeStatistics {
                                        entries: 1,
                                        shallow_bytes,
                                        total_bytes,
                                        own_bytes: first_size,
                                    });
                                }
                            }
                        } else {
                            panic!("Internal error: Tagged tuple doesn't have the expected structure")
                        }
                    }
                    _ => {
                        for item in items {
                            total_bytes += item.write(out, stats)?;
                        }
                    }
                }
                Ok(total_bytes)
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

        println!("Statistics: {}", self.statistics);
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
pub struct SectionStatistics {
    /// Number of entries in this table.
    entries: usize,

    /// Number of bytes prior to compression.
    uncompressed_bytes: usize,

    /// Number of bytes after compression.
    compressed_bytes: usize,
}

#[derive(Clone, Debug, Default)]
pub struct NodeStatistics {
    /// Total number of entries of this node.
    entries: usize,

    /// Number of bytes used to represent the node, minus subnodes
    /// (e.g. the length of the entry in the grammar table).
    own_bytes: usize,

    /// Number of bytes used to represent the node, including primitive
    /// subnodes (e.g. anything but lists) but not compound subnodes.
    shallow_bytes: usize,

    /// Number of bytes used to represent the node, including all subnodes.
    total_bytes: usize,
}

#[derive(Debug, Default)]
pub struct Statistics {
    grammar_table: SectionStatistics,
    strings_table: SectionStatistics,
    tree: SectionStatistics,

    per_kind_index: VecMap<NodeStatistics>,
    per_kind_name: HashMap<NodeName, NodeStatistics>,
    list_lengths: VecMap<usize>,

    bool: NodeStatistics,
    float: NodeStatistics,
    string: NodeStatistics,
    list: NodeStatistics,
    list_header: NodeStatistics,
    tagged_header: NodeStatistics,
    tagged_tuple: NodeStatistics,
}

struct NodeNameAndStatistics(Vec<(NodeName, NodeStatistics)>);

struct ListLengthsAndNumber(Vec<(usize, usize)>);

impl Display for NodeNameAndStatistics {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        // FIXME: Ugly. Find a better way to handle indentation.
        for &(ref name, ref stats) in &self.0 {
            write!(f, "\t\t{}:\n", name.to_string())?;
            write!(f, "\t\t\tEntries: {}\n", stats.entries)?;
            write!(f, "\t\t\tOwn bytes: {}\n", stats.own_bytes)?;
            write!(f, "\t\t\tShallow bytes: {}\n", stats.shallow_bytes)?;
            write!(f, "\t\t\tTotal bytes: {}\n", stats.total_bytes)?;
        }
        Ok(())
    }
}

impl Display for ListLengthsAndNumber {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        // FIXME: Ugly. Find a better way to handle indentation.
        for &(ref length, ref number) in &self.0 {
            write!(f, "\t\tlength {} x {}\n", length, number)?;
        }
        Ok(())
    }
}

impl Display for Statistics {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        // Sort entries by number of uses.
        let mut per_kind : Vec<_> = self.per_kind_name.iter()
            .map(|(a, b)| (a.clone(), b.clone()))
            .collect();
        per_kind.sort_unstable_by(|a, b| usize::cmp(&b.1.entries, &a.1.entries));

        let mut per_size : Vec<_> = self.list_lengths.iter()
            .map(|(a, b)| (a.clone(), b.clone()))
            .collect();
        per_size.sort_unstable_by(|a, b| usize::cmp(&b.1, &a.1));

        write!(f, "
Statistics
\tSections:
\t\tGrammar:
\t\t\tEntries: {}
\t\t\tUncompressed bytes: {}
\t\t\tCompressed bytes: {}
\t\tStrings:
\t\t\tEntries: {}
\t\t\tUncompressed bytes: {}
\t\t\tCompressed bytes: {}
\t\tTree:
\t\t\tEntries: {}
\t\t\tUncompressed bytes: {}
\t\t\tCompressed bytes: {}
\tNodes:
{}
\tKinds:
\t\tBool:
\t\t\tEntries: {}
\t\t\tBytes: {}
\t\tFloat:
\t\t\tEntries: {}
\t\t\tBytes: {}
\t\tString indices:
\t\t\tEntries: {}
\t\t\tBytes: {}
\t\tList:
\t\t\tEntries: {}
\t\t\tOwn bytes: {}
\t\t\tHeader bytes: {}
\t\t\tShallow bytes: {}
\t\t\tTotal bytes: {}
\t\tTagged tuples:
\t\t\tEntries: {}
\t\t\tOwn bytes: {}
\t\t\tHeader bytes: {}
\t\t\tShallow bytes: {}
\t\t\tTotal bytes: {}
\tList sizes
{}
",
        self.grammar_table.entries,
        self.grammar_table.uncompressed_bytes,
        self.grammar_table.compressed_bytes,
        self.strings_table.entries,
        self.strings_table.uncompressed_bytes,
        self.strings_table.compressed_bytes,
        self.tree.entries,
        self.tree.uncompressed_bytes,
        self.tree.compressed_bytes,
        NodeNameAndStatistics(per_kind),
        self.bool.entries,
        self.bool.own_bytes,
        self.float.entries,
        self.float.own_bytes,
        self.string.entries,
        self.string.own_bytes,
        self.list.entries,
        self.list.own_bytes,
        self.list_header.own_bytes,
        self.list.shallow_bytes,
        self.list.total_bytes,
        self.tagged_tuple.entries,
        self.tagged_tuple.own_bytes,
        self.tagged_header.own_bytes,
        self.tagged_tuple.shallow_bytes,
        self.tagged_tuple.total_bytes,
        ListLengthsAndNumber(per_size)
        )
    }
}
