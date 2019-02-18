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
/// - a value `v` in `[1, window len]` represents a repeat of a previous value: 1 it the latest value, 2 the second latest, etc.
/// - a value `v` in `[window len + 1, window len + length of the shared dictionary]` represent `TableRef::Shared` with index `v - window len - 1`;
/// - a value `v > window len + length of the shared dictionary` represents `TableRef::Prelude` with index `v - window len - length of shared dictionary - 1`.
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

    /// The latest references encountered.
    ///
    /// window[0] is the latest reference encountered,
    /// window[1] the second latest
    /// ...
    ///
    /// Invariant:
    /// - length is always <= window_max_len
    /// - if i ≠ j, window[i] ≠ window[j]
    window: Vec<TableRef>,

    /// The maximal length of `self.window`, which is also the number of
    /// integers to reserve to represent backreferences to values encountered
    /// recently.
    window_max_len: usize,
}
impl<T> TableRefStreamState<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    /// Create a new `TableRefStreamState`.
    ///
    /// `window_max_len` represents the number of integers to reserve to
    /// represent backreferences to values encountered recently.
    pub fn new(window_max_len: usize) -> Self {
        TableRefStreamState {
            latest_in_prelude: None,
            phantom: PhantomData,
            window_max_len,
            window: Vec::with_capacity(window_max_len),
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
        debug!(target: "rw", "TableRefStreamState::from_u32 {} with window_len {}, shared_len {}, total len {}",
            value,
            self.window_max_len,
            table.shared_len(),
            table.len());
        let index = loop {
            // Fake loop, used simply to be able to `break` out.
            if value == 0 {
                // Value `0` is reserved to represent `latest_in_prelude + 1` (or `0` if `latest_in_prelude` is `None`).
                let next_in_prelude = self.next_in_prelude();
                break TableRef::Prelude(next_in_prelude as usize);
            }
            // Renormalize from 0.
            let value = value - 1;
            debug!(target: "rw", "TableRefStreamState::from_u32, value is not 0, renormalizing to {}",
                value);
            if (value as usize) < self.window_max_len {
                // this is a hit in the floating window.
                break *self.window.get(value as usize)?;
            }

            // Renormalize from 0 again.
            let value = value - self.window_max_len as u32;
            debug!(target: "rw", "TableRefStreamState::from_u32, value is not in window, renormalizing to {}",
                value);
            if (value as usize) < table.shared_len() {
                break TableRef::Shared(value as usize);
            }

            // Values beyond `table.shared_len()` represent prelude indices.
            if (value as usize) < table.len() {
                break TableRef::Prelude(value as usize);
            }
            return None;
        };
        // Update caches (latest in prelude, position in window).
        self.update_position_in_window(index);
        if let TableRef::Prelude(ref index_in_prelude) = index {
            self.latest_in_prelude = Some(*index_in_prelude as u32)
        }
        Some(index)
    }

    /// Update `window` using a LRU eviction strategy.
    ///
    /// Return:
    /// - `Some(pos)` if `value` was already in `window` at position `pos`;
    /// - `None` otherwise.
    fn update_position_in_window(&mut self, value: TableRef) -> Option<usize> {
        if self.window_max_len == 0 {
            return None;
        }
        if let Some(pos) = self.window.iter().position(|i| i == &value) {
            if pos == 0 {
                // `value` was already the latest value in `window`, nothing to update.
            } else {
                // Rotate the value to the first position.
                let ref mut slice = self.window.as_mut_slice()[0..pos + 1];
                slice.rotate_right(1);
            }
            return Some(pos);
        }
        if self.window.len() < self.window_max_len {
            // Insert a new latest value in the window.
            self.window.insert(0, value);
        } else {
            // Insert a new value at 0, shifting other values and getting
            // rid of the last value in window.
            let len = self.window.len();
            self.window[len - 1] = value;
            self.window.rotate_right(1);
        }
        assert!(self.window[0] == value);
        assert!(self.window.len() <= self.window_max_len);
        None
    }

    /// Encode an `TableRef` into a `u32`.
    ///
    /// This method does *not* perform any kind of sanity check on the `TableRef`.
    pub fn into_u32(&mut self, value: TableRef) -> u32 {
        let maybe_position_in_window = self.update_position_in_window(value);

        match value {
            TableRef::Shared(i) => {
                match maybe_position_in_window {
                    // If we repeat a recent value, emit its position.
                    // Use interval [1, self.window_max_len].
                    Some(pos) => (pos + 1) as u32,
                    // Otherwise, emit a direct reference to the shared dictionary.
                    // Use interval ]self.window_max_len, self.window_max_len + self.shared_len]
                    None => (i + self.window_max_len + 1) as u32,
                }
            }
            // References to the prelude dictionary may be 0 (for next) or shifted by 1.
            TableRef::Prelude(i) => {
                let i = i as u32;
                let next_in_prelude = self.next_in_prelude();
                self.latest_in_prelude = Some(i);
                if next_in_prelude == i {
                    // 0 is reserved exactly for this.
                    return 0;
                }
                match maybe_position_in_window {
                    // If we repeat a recent value, emit its position.
                    // Use interval [1, self.window_max_len].
                    Some(pos) => (pos + 1) as u32,
                    // Otherwise, emit a direct reference to the prelude dictionary.
                    // Use interval ]self.window_max_len + self.shared_len, ..]
                    None => (i + self.window_max_len as u32 + 1),
                }
            }
        }
    }
}
