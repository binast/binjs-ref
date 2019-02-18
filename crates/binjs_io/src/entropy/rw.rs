//! Constants and data structures shared between entropy read and entropy write.

use entropy::dictionary::{LinearTable, TableRef};
use std::marker::PhantomData;

use smallvec::SmallVec;

/// The magic number at the start of the file.
///
/// The early \0 is used to ensure tools such as github detect the file as binary.
/// The various \r, \n, \0 are there as canaries to avoid ASCII<->BINARY transcoding
/// errors.
pub const GLOBAL_HEADER_START: &'static [u8; 8] = b"\x89BJS\r\n\0\n";

/// The main section, containing the entropy-encoded stream.
pub const SECTION_MAIN: &'static [u8] = b"[[main]]";
pub const SECTION_MAIN_WITHOUT_BRACKETS: &'static [u8] = b"main";

/// The prelude section, containing the dictionary extensions,
/// encoded using e.g. brotli.
pub const SECTION_PRELUDE: &'static [u8] = b"[[prelude]]";
// pub const SECTION_PRELUDE_WITHOUT_BRACKETS: &'static [u8] = b"prelude";

/// The content section, containing the streams of indices of
/// user-extensible values, encoded e.g. using brotli.
pub const SECTION_CONTENT: &'static [u8] = b"[[content]]";
pub const SECTION_CONTENT_WITHOUT_BRACKETS: &'static [u8] = b"content";

/// Indicating that the stream is compressed with format "entropy 0.2".
pub const FORMAT_ENTROPY_0: &'static [u8] = b"entropy0.2;";
// pub const FORMAT_IDENTITY: &'static [u8] = b"identity;";

/// Indicating that the stream is compressed with format "brotli".
pub const FORMAT_BROTLI: &'static [u8] = b"br;";

/// Prelude data.
///
/// The prelude contains streams to read or write dictionary extensions for
/// user-extensible symbols (aka "prelude dictionaries").
///
/// Examples of `T`: `LazyStream` (for writing) or `Cursor<Vec<u8>>` (for reading).
pub struct PreludeStreams<T> {
    /// Instances of IdentifierName.
    ///
    /// To aid with compression, we typically store user-extensible strings
    /// as two distinct data structures:
    ///
    /// - a single string, containing all concatenated strings without delimiter;
    /// - a list of lengths, used to split the string into the successive strings.
    pub identifier_names: T,
    pub identifier_names_len: T,

    /// Instances of PropertyKey
    ///
    /// To aid with compression, we typically store user-extensible strings
    /// as two distinct data structures:
    ///
    /// - a single string, containing all concatenated strings without delimiter;
    /// - a list of lengths, used to split the string into the successive strings.
    pub property_keys: T,
    pub property_keys_len: T,

    /// Instances of string literals.
    ///
    /// To aid with compression, we typically store user-extensible strings
    /// as two distinct data structures:
    ///
    /// - a single string, containing all concatenated strings without delimiter;
    /// - a list of lengths, used to split the string into the successive strings.
    pub string_literals: T,
    pub string_literals_len: T,

    /// Instances of InterfaceName
    pub interface_names: T,

    /// Instances of string enums.
    pub string_enums: T,

    /// Instances of list lengths.
    pub list_lengths: T,

    /// Instances of floating-point numbers.
    pub floats: T,

    /// Instances of unsigned longs.
    pub unsigned_longs: T,
}
impl<T> PreludeStreams<T> {
    /// Create a new PreludeStreams.
    pub fn with<F>(f: F) -> Self
    where
        F: Fn(&str) -> T,
    {
        PreludeStreams {
            identifier_names: f("identifier_names"),
            identifier_names_len: f("identifier_names_len"),
            property_keys: f("property_keys"),
            property_keys_len: f("property_keys_len"),
            string_literals: f("string_literals"),
            string_literals_len: f("string_literals_len"),
            interface_names: f("interface_names"),
            string_enums: f("string_enums"),
            list_lengths: f("list_lengths"),
            floats: f("floats"),
            unsigned_longs: f("unsigned_longs"),
        }
    }

    /// Iterate through fields of PreludeStreams.
    pub fn into_iter(self) -> impl Iterator<Item = (&'static str, T)> {
        vec![
            ("identifier_names", self.identifier_names),
            ("identifier_names_len", self.identifier_names_len),
            ("property_keys", self.property_keys),
            ("property_keys_len", self.property_keys_len),
            ("string_literals", self.string_literals),
            ("string_literals_len", self.string_literals_len),
            ("interface_names", self.interface_names),
            ("string_enums", self.string_enums),
            ("list_lengths", self.list_lengths),
            ("floats", self.floats),
            ("unsigned_longs", self.unsigned_longs),
        ]
        .into_iter()
    }

    /// Access a field by its name, specified as a sequence of bytes.
    ///
    /// This method is typically used to simplify parsing a file that
    /// contains sections explicitly labelled "identifier_names",
    /// "property_keys", etc.
    /// In such case, `field_name` is expected to be a user input.
    ///
    /// Return `None` if `field_name` is not one of the field names.
    pub fn get_mut_b(&mut self, field_name: &[u8]) -> Option<&mut T> {
        match field_name {
            b"identifier_names" => Some(&mut self.identifier_names),
            b"identifier_names_len" => Some(&mut self.identifier_names_len),
            b"property_keys" => Some(&mut self.property_keys),
            b"property_keys_len" => Some(&mut self.property_keys_len),
            b"string_literals" => Some(&mut self.string_literals),
            b"string_literals_len" => Some(&mut self.string_literals_len),
            b"interface_names" => Some(&mut self.interface_names),
            b"string_enums" => Some(&mut self.string_enums),
            b"list_lengths" => Some(&mut self.list_lengths),
            b"floats" => Some(&mut self.floats),
            b"unsigned_longs" => Some(&mut self.unsigned_longs),
            _ => None,
        }
    }
}

/// A stack-allocated buffer for names of sections and streams.
pub const NAME_MAX_LEN: usize = 32;
pub type NameData = SmallVec<[u8; NAME_MAX_LEN]>;

/// State of a input/output stream of `TableRef`.
///
/// Used to (de)serialize `TableRef`, as the encoding of an `TableRef`
/// may depend on that of previous values.
///
/// `TableRef` values are represented as follows:
/// - a value `0` represents an `TableRef::Prelude` with index 1 + the latest `TableRef::Prelude` value we have encoded/decoded;
/// - a value `v` in `[1, length of the shared dictionary]` represent `TableRef::Shared` with index `v - 1`;
/// - a value `v > length of the shared dictionary` represents `TableRef::Prelude` with index `v - 1`.
pub struct TableRefStreamState<T> {
    /// The latest `TableRef::Prelude` value.
    ///
    /// Updated whenever we
    /// - encode a new `TableRef::Prelude`;
    /// - decode a value to an `TableRef::Prelude`, including the special value `0`.
    latest_in_prelude: Option<u32>,

    /// Phantom type, to ensure that we only ever use a given `TableRefStreamState` with
    /// a single (type of) LinearTable.
    phantom: PhantomData<T>,
}
impl<T> TableRefStreamState<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    /// Create a new `TableRefStreamState`.
    pub fn new() -> Self {
        TableRefStreamState {
            latest_in_prelude: None,
            phantom: PhantomData,
        }
    }

    /// The value of `latest_in_prelude + 1`, or `0` if `latest_in_prelude`
    /// has never been set.
    fn next_in_prelude(&self) -> u32 {
        match self.latest_in_prelude {
            Some(latest) => latest + 1,
            None => 0,
        }
    }

    /// Decode an `TableRef` from a `u32`.
    pub fn from_u32(&mut self, value: u32, table: &LinearTable<T>) -> Option<TableRef> {
        if value == 0 {
            // Value `0` is reserved to represent `latest_in_prelude + 1` (or `0` if `latest_in_prelude` is `None`).
            let next_in_prelude = self.next_in_prelude();
            self.latest_in_prelude = Some(next_in_prelude);
            return Some(TableRef::Prelude(next_in_prelude as usize));
        }
        // Renormalize from 0.
        let value = value - 1;
        if (value as usize) < table.shared_len() {
            return Some(TableRef::Shared(value as usize));
        }
        // Values beyond `table.shared_len()` represent prelude indices.
        if (value as usize) < table.len() {
            self.latest_in_prelude = Some(value);
            return Some(TableRef::Prelude(value as usize));
        }
        None
    }

    /// Encode an `TableRef` into a `u32`.
    ///
    /// This method does *not* perform any kind of sanity check on the `TableRef`.
    pub fn into_u32(&mut self, value: TableRef) -> u32 {
        match value {
            // References to the shared dictionary are shifted by 1.
            TableRef::Shared(i) => (i + 1) as u32,
            // References to the prelude dictionary may be 0 (for next) or shifted by 1.
            TableRef::Prelude(i) => {
                let i = i as u32;
                let next_in_prelude = self.next_in_prelude();
                self.latest_in_prelude = Some(i);
                if next_in_prelude == i {
                    0
                } else {
                    i + 1
                }
            }
        }
    }
}
