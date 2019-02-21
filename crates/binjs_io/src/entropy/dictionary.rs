use entropy::predict::{PathPredict, WindowPredict};
use entropy::probabilities::{InstancesToProbabilities, SymbolIndex, SymbolInfo};

use io::TokenWriter;
use TokenWriterError;

use binjs_shared::{
    FieldName, IdentifierName, InterfaceName, Node, PropertyKey, SharedString, F64,
};

use itertools::Itertools;

use std;
use std::collections::HashMap;

pub type IOPath = binjs_shared::ast::Path<
    InterfaceName,
    (
        /* child index */ usize,
        /* field name */ FieldName,
    ),
>;

pub use entropy::predict::Instances;

/// A newtype for `usize` used to count the number of some item in a given file.
#[derive(
    Default,
    Serialize,
    Deserialize,
    From,
    Into,
    AddAssign,
    Clone,
    Copy,
    Debug,
    PartialOrd,
    Ord,
    PartialEq,
    Eq,
)]
pub struct InstancesInFile(pub usize);

/// A newtype for `usize` used to count the number of files containing some item.
#[derive(
    Default,
    Display,
    Serialize,
    Deserialize,
    From,
    Into,
    AddAssign,
    Clone,
    Copy,
    Debug,
    PartialOrd,
    Ord,
    PartialEq,
    Eq,
)]
pub struct FilesContaining(pub usize);

/// Add a single symbol to the table of probabilities for a given path.
///
/// Note that this macro could not be implemented as a simple method, as we need to adapt it to different field names.
///
/// Usage:
/// `update_in_context!(self, name_of_the_probability_table, "Description, used for debugging", value_to_encode, path_in_the_ast)`
macro_rules! update_in_context {
    ( $me: ident, $table: ident, $description: expr, $path:expr, $value: expr ) => {{
        use std::borrow::Borrow;

        let path = $path.borrow();
        $me.dictionary.$table.add(path, $value);

        Ok(())
    }};
}

/// Add a single symbol to the table of probabilities for a prediction window
/// (see `predict::WindowPredict` for details).
///
/// Note that this macro could not be implemented as a simple method, as we need to adapt it to different field names.
///
/// Usage:
/// `update_in_window!(self, name_of_the_probability_table, value_to_encode)`
macro_rules! update_in_window {
    ( $me: ident, $table:ident, $value: expr ) => {{
        $me.dictionary.$table.add($value);

        Ok(())
    }};
}

/// Count one instance of a user-extensible value in the AST. Used to count the number
/// of instances of e.g. each specific string, each specific number, etc.
///
/// Note that this macro could not be implemented as a simple method, as we need to adapt it to different field names.
///
/// Usage:
/// `increment_instance_count!(self, name_of_the_probability_table, value_to_count)`
macro_rules! increment_instance_count {
    ( $me: ident, $table: ident, $value: expr ) => {
        debug!(target: "dictionary", "Dictionary: Inserting instance {:?}", $value);
        $me.instances_of_user_extensible_data_in_current_file.$table
            .entry($value)
            .and_modify(|instances| {
                *instances += InstancesInFile(1) // We have already seen this string in this file, increment.
            }).or_insert(InstancesInFile(1));    // First time we see this string in this file, store 1.
    }
}

/// Result of a fetch operation in a LinearTable.
#[derive(Clone, Copy, Debug)]
pub enum Fetch {
    /// The value was already in the cache at the given index.
    Hit(TableRef),

    /// The value was not in the cache. A slot has been allocated at the given index,
    /// but the definition still needs to be added.
    Miss(TableRef),
}
impl Fetch {
    /// Return `true` if this result represents a hit.
    pub fn is_hit(&self) -> bool {
        match *self {
            Fetch::Hit(_) => true,
            _ => false,
        }
    }

    pub fn table_ref(&self) -> TableRef {
        match *self {
            Fetch::Hit(result) => result,
            Fetch::Miss(result) => result,
        }
    }
}

/// An index in an LinearTable.
#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum TableRef {
    /// The index represents a value in a shared dictionary.
    Shared(usize),

    /// The index represents a value in a prelude dictionary.
    Prelude(usize),
}
impl TableRef {
    /// Return the `usize` carried by this `TableRef`.
    fn raw(&self) -> usize {
        match *self {
            TableRef::Shared(raw) => raw,
            TableRef::Prelude(raw) => raw,
        }
    }
}

/// The initial size of LinearTables, in elements.
const INDEXED_TABLE_INITIAL_CAPACITY: usize = 1024;

/// A data structure designed to cache information accessible
/// either by index or by value.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LinearTable<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    /// The values in the table, in the order in which they were added.
    /// Used to perform lookup with `at_index`.
    values: Vec<T>,

    /// A mapping to value back to the indices used to access them.
    refs_by_value: HashMap<T, TableRef>,

    /// The number of values representing shared dictionary entries in this table.
    shared_len: usize,
}
impl<T> LinearTable<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    /// The number of values representing shared dictionary entries in this table.
    pub fn shared_len(&self) -> usize {
        self.shared_len
    }

    /// The number of values representing prelude dictionary entries in this table.
    pub fn prelude_len(&self) -> usize {
        self.values.len() - self.shared_len
    }

    /// The number of values in this table.
    pub fn len(&self) -> usize {
        self.values.len()
    }
}
impl<T> LinearTable<T>
where
    T: Eq + std::hash::Hash + Clone + std::fmt::Debug + Ord,
{
    /// Create a new LinearTable from a list of instances.
    ///
    /// Values appearing in `instances` that have `threshold` instances or less are ignored.
    pub fn new(
        value_to_instances: HashMap<T, FilesContaining>,
        threshold: FilesContaining,
    ) -> Self {
        debug!(target: "dictionary", "Creating a LinearTable with a threshold of {}", Into::<usize>::into(threshold));
        let value_to_instances = value_to_instances.into_iter().sorted(); // We sort to enforce traversal order.
        let mut values = Vec::with_capacity(INDEXED_TABLE_INITIAL_CAPACITY);
        let mut refs_by_value = HashMap::with_capacity(INDEXED_TABLE_INITIAL_CAPACITY);
        for (value, instances) in value_to_instances {
            debug!(target: "dictionary", "Should we add {:?} to the LinearTable ({} instances)?", value, instances);
            if instances <= threshold {
                // Too few instances, skipping.
                debug!(target: "dictionary", "Too few instances: {} <= {} for {:?}", instances, threshold, value);
                continue;
            }
            let len = TableRef::Shared(values.len());
            values.push(value.clone());
            let prev = refs_by_value.insert(value, len);
            assert!(prev.is_none());
        }
        let shared_len = values.len();
        let result = LinearTable {
            values,
            refs_by_value,
            shared_len,
        };
        debug!(target: "dictionary", "Dictionary: LinearTable contains {:?}", result);
        result
    }

    /// Create an empty `LinearTable`.
    pub fn with_capacity(len: usize) -> Self {
        LinearTable {
            values: Vec::with_capacity(len),
            refs_by_value: HashMap::with_capacity(len),
            shared_len: 0,
        }
    }

    /// Attempt to get the index for a value from the `LinearTable`.
    ///
    /// If the value is already in the cache, return `Fetch::Hit(index)`, where `index` is the
    /// immutable index of the value. Otherwise, allocate a new slot `index` and return
    /// `Fetch::Miss(index)`.
    pub fn fetch_index(&mut self, value: &T) -> Fetch {
        use std::collections::hash_map::Entry::*;
        debug!(target: "dictionary", "Dictionary: 'I'm looking for {:?} in {:?}", value, self);
        let len = self.values.len();
        let result = match self.refs_by_value.entry(value.clone()) {
            Occupied(slot) => return Fetch::Hit(*slot.get()),
            Vacant(slot) => {
                let result = TableRef::Prelude(len);
                slot.insert(result.clone());
                result
            }
        };
        self.values.push(value.clone());
        Fetch::Miss(result)
    }

    /// Create a copy of the current LinearTable with an added prelude dictionary.
    pub fn with_prelude<'a>(&self, prelude: &'a [T]) -> Result<Self, &'a T> {
        let mut clone = self.clone();
        for item in prelude {
            if clone.fetch_index(item).is_hit() {
                // The prelude shouldn't duplicate anything from the dictionary.
                return Err(item);
            }
        }
        Ok(clone)
    }

    /// Access the contents of this table by index.
    pub fn at_index(&self, index: &TableRef) -> Option<&T> {
        let index = index.raw();
        self.values.get(index)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Dictionary<T> {
    // --- Non-extensible sets of symbols, predicted by path.
    // Used for entropy coding.
    // ---
    /// All booleans appearing in the AST, predicted by path.
    bool_by_path: PathPredict<Option<bool>, T>,

    /// All string enumerations, predicted by path.
    string_enum_by_path: PathPredict<SharedString, T>,

    /// All interface names, predicted by path.
    interface_name_by_path: PathPredict<InterfaceName, T>,

    // --- Extensible sets of symbols, predicted by path.
    // Used for experiments with entropy coding, but so far, not very
    // good with extensibility. There are good chances that this section
    // will disappear in future versions.
    // ---
    /// All floats appearing in the AST.
    float_by_path: PathPredict<Option<F64>, T>,

    /// All unsigned longs appearing in the AST.
    unsigned_long_by_path: PathPredict<u32, T>,

    /// All property keys.
    property_key_by_path: PathPredict<Option<PropertyKey>, T>,

    /// All identifier names, predicted by path.
    identifier_name_by_path: PathPredict<Option<IdentifierName>, T>,

    /// All string literals, predicted by path.
    string_literal_by_path: PathPredict<Option<SharedString>, T>,

    /// All list lengths, predicted by path.
    list_length_by_path: PathPredict<Option<u32>, T>,

    // --- Extensible sets of symbols, predicted by window.
    // Used for experiments for extensibility of entropy coding, but so far,
    // not very good at compression, and might cause serious alignment issues.
    // There are good chances that this section will disappear in future versions.
    // ---
    /// All property keys, predicted by window.
    property_key_by_window: WindowPredict<Option<PropertyKey>, T>,

    /// All identifier names, predicted by window.
    identifier_name_by_window: WindowPredict<Option<IdentifierName>, T>,

    /// All string literals, predicted by window.
    string_literal_by_window: WindowPredict<Option<SharedString>, T>,

    // --- Extensible sets of symbols, as indexed tables.
    // Used to represent instances of extensible sets of symbols as indices in
    // a table. Pretty good for extensibility, experiments pending on
    // compression-level and performance.
    // ---
    /// All unsigned longs.
    unsigned_longs: LinearTable<u32>,

    /// All string literals. `None` for `null`.
    string_literals: LinearTable<Option<SharedString>>,

    /// All identifier names. `None` for `null`.
    identifier_names: LinearTable<Option<IdentifierName>>,

    /// All property keys. `None` for `null`.
    property_keys: LinearTable<Option<PropertyKey>>,

    /// All list lenghts. `None` for `null`.
    list_lengths: LinearTable<Option<u32>>,

    /// All floats. `None` for `null`.
    floats: LinearTable<Option<F64>>,
    // Missing:
    // - offsets (cannot be predicted?)
    // - directives?
}
impl<T> Dictionary<T> {
    /// Create a new dictionary using paths of `depth` depth
    /// and windows of `width` width.
    pub fn new(depth: usize, width: usize) -> Self {
        Dictionary {
            // By path.
            bool_by_path: PathPredict::new(depth),
            float_by_path: PathPredict::new(depth),
            unsigned_long_by_path: PathPredict::new(depth),
            string_enum_by_path: PathPredict::new(depth),
            property_key_by_path: PathPredict::new(depth),
            identifier_name_by_path: PathPredict::new(depth),
            string_literal_by_path: PathPredict::new(depth),
            list_length_by_path: PathPredict::new(depth),
            interface_name_by_path: PathPredict::new(depth),

            // By window.
            property_key_by_window: WindowPredict::new(width),
            identifier_name_by_window: WindowPredict::new(width),
            string_literal_by_window: WindowPredict::new(width),

            // Linear tables.
            string_literals: LinearTable::with_capacity(0),
            identifier_names: LinearTable::with_capacity(0),
            property_keys: LinearTable::with_capacity(0),
            list_lengths: LinearTable::with_capacity(0),
            floats: LinearTable::with_capacity(0),
            unsigned_longs: LinearTable::with_capacity(0),
        }
    }

    // The following methods are read-only accessors and may be used regardless
    // of whether we're producing the dictionary or using it.

    pub fn bool_by_path(&self) -> &PathPredict<Option<bool>, T> {
        &self.bool_by_path
    }

    pub fn string_enum_by_path(&self) -> &PathPredict<SharedString, T> {
        &self.string_enum_by_path
    }

    pub fn interface_name_by_path(&self) -> &PathPredict<InterfaceName, T> {
        &self.interface_name_by_path
    }

    pub fn float_by_path(&self) -> &PathPredict<Option<F64>, T> {
        &self.float_by_path
    }

    pub fn unsigned_long_by_path(&self) -> &PathPredict<u32, T> {
        &self.unsigned_long_by_path
    }

    pub fn property_key_by_path(&self) -> &PathPredict<Option<PropertyKey>, T> {
        &self.property_key_by_path
    }

    pub fn identifier_name_by_path(&self) -> &PathPredict<Option<IdentifierName>, T> {
        &self.identifier_name_by_path
    }

    pub fn string_literal_by_path(&self) -> &PathPredict<Option<SharedString>, T> {
        &self.string_literal_by_path
    }

    pub fn list_length_by_path(&self) -> &PathPredict<Option<u32>, T> {
        &self.list_length_by_path
    }

    pub fn property_key_by_window(&self) -> &WindowPredict<Option<PropertyKey>, T> {
        &self.property_key_by_window
    }

    pub fn identifier_name_by_window(&self) -> &WindowPredict<Option<IdentifierName>, T> {
        &self.identifier_name_by_window
    }

    pub fn string_literal_by_window(&self) -> &WindowPredict<Option<SharedString>, T> {
        &self.string_literal_by_window
    }

    pub fn unsigned_longs(&self) -> &LinearTable<u32> {
        &self.unsigned_longs
    }

    pub fn string_literals(&self) -> &LinearTable<Option<SharedString>> {
        &self.string_literals
    }

    pub fn identifier_names(&self) -> &LinearTable<Option<IdentifierName>> {
        &self.identifier_names
    }

    pub fn property_keys(&self) -> &LinearTable<Option<PropertyKey>> {
        &self.property_keys
    }

    pub fn list_lengths(&self) -> &LinearTable<Option<u32>> {
        &self.list_lengths
    }

    pub fn floats(&self) -> &LinearTable<Option<F64>> {
        &self.floats
    }

    // The following methods are read-write accessors. They are provided as an intermediate step
    // but will disappear soon, as they are dangerous whenever we reuse a dictionary for two
    // different files.

    pub fn unsigned_longs_mut(&mut self) -> &mut LinearTable<u32> {
        &mut self.unsigned_longs
    }

    pub fn string_literals_mut(&mut self) -> &mut LinearTable<Option<SharedString>> {
        &mut self.string_literals
    }

    pub fn identifier_names_mut(&mut self) -> &mut LinearTable<Option<IdentifierName>> {
        &mut self.identifier_names
    }

    pub fn property_keys_mut(&mut self) -> &mut LinearTable<Option<PropertyKey>> {
        &mut self.property_keys
    }

    pub fn list_lengths_mut(&mut self) -> &mut LinearTable<Option<u32>> {
        &mut self.list_lengths
    }

    pub fn floats_mut(&mut self) -> &mut LinearTable<Option<F64>> {
        &mut self.floats
    }

    /// Return the depth of the current dictionary.
    pub fn depth(&self) -> usize {
        assert_eq!(self.bool_by_path.depth(), self.string_enum_by_path.depth());
        assert_eq!(
            self.bool_by_path.depth(),
            self.interface_name_by_path.depth()
        );
        self.bool_by_path.depth()
    }

    /// Return the number of states in this dictionary.
    pub fn len(&self) -> usize {
        // Make sure that we don't forget a field.
        let Dictionary {
            ref bool_by_path,
            ref float_by_path,
            ref unsigned_long_by_path,
            ref string_enum_by_path,
            ref property_key_by_path,
            ref identifier_name_by_path,
            ref string_literal_by_path,
            ref list_length_by_path,
            ref interface_name_by_path,
            property_key_by_window: _,
            string_literal_by_window: _,
            identifier_name_by_window: _,
            string_literals: _,
            identifier_names: _,
            property_keys: _,
            list_lengths: _,
            floats: _,
            unsigned_longs: _,
        } = *self;

        bool_by_path.len()
            + float_by_path.len()
            + unsigned_long_by_path.len()
            + string_enum_by_path.len()
            + property_key_by_path.len()
            + identifier_name_by_path.len()
            + interface_name_by_path.len()
            + string_literal_by_path.len()
            + list_length_by_path.len()
            + interface_name_by_path.len()
    }
}

impl Dictionary<Instances> {
    pub fn bool_by_path_mut(&mut self) -> &mut PathPredict<Option<bool>, Instances> {
        &mut self.bool_by_path
    }

    pub fn string_enum_by_path_mut(&mut self) -> &mut PathPredict<SharedString, Instances> {
        &mut self.string_enum_by_path
    }

    pub fn interface_name_by_path_mut(&mut self) -> &mut PathPredict<InterfaceName, Instances> {
        &mut self.interface_name_by_path
    }

    pub fn float_by_path_mut(&mut self) -> &mut PathPredict<Option<F64>, Instances> {
        &mut self.float_by_path
    }

    pub fn unsigned_long_by_path_mut(&mut self) -> &mut PathPredict<u32, Instances> {
        &mut self.unsigned_long_by_path
    }

    pub fn property_key_by_path_mut(&mut self) -> &mut PathPredict<Option<PropertyKey>, Instances> {
        &mut self.property_key_by_path
    }

    pub fn identifier_name_by_path_mut(
        &mut self,
    ) -> &mut PathPredict<Option<IdentifierName>, Instances> {
        &mut self.identifier_name_by_path
    }

    pub fn string_literal_by_path_mut(
        &mut self,
    ) -> &mut PathPredict<Option<SharedString>, Instances> {
        &mut self.string_literal_by_path
    }

    pub fn list_length_by_path_mut(&mut self) -> &mut PathPredict<Option<u32>, Instances> {
        &mut self.list_length_by_path
    }

    pub fn property_key_by_window_mut(
        &mut self,
    ) -> &mut WindowPredict<Option<PropertyKey>, Instances> {
        &mut self.property_key_by_window
    }

    pub fn identifier_name_by_window_mut(
        &mut self,
    ) -> &mut WindowPredict<Option<IdentifierName>, Instances> {
        &mut self.identifier_name_by_window
    }

    pub fn string_literal_by_window_mut(
        &mut self,
    ) -> &mut WindowPredict<Option<SharedString>, Instances> {
        &mut self.string_literal_by_window
    }

    /// Combine a dictionary obtained by sampling (`self`) and a baseline dictionary
    /// (obtained by `entropy::baseline`) to produce a dictionary able to handle
    /// values that grammatically correct but have not been witnessed during
    /// sampling.
    pub fn with_grammar_fallback(self, fallback: Dictionary<Instances>) -> Self {
        let mut result = self;
        let original_len = result.len();
        result.bool_by_path.add_fallback(fallback.bool_by_path);
        result
            .string_enum_by_path
            .add_fallback(fallback.string_enum_by_path);
        result
            .interface_name_by_path
            .add_fallback(fallback.interface_name_by_path);

        debug!(target: "dictionary", "Added fallback to dictionary {} states => {} states",
            original_len,
            result.len());

        result
    }
}

impl InstancesToProbabilities for Dictionary<Instances> {
    type AsProbabilities = Dictionary<SymbolInfo>;

    /// Convert a dictionary counting instances into a dictionary that
    /// counts probabilities.
    fn instances_to_probabilities(self, _description: &str) -> Dictionary<SymbolInfo> {
        Dictionary {
            // By path.
            bool_by_path: self.bool_by_path.instances_to_probabilities("bool_by_path"),
            float_by_path: self
                .float_by_path
                .instances_to_probabilities("float_by_path"),
            unsigned_long_by_path: self
                .unsigned_long_by_path
                .instances_to_probabilities("unsigned_long_by_path"),
            string_enum_by_path: self
                .string_enum_by_path
                .instances_to_probabilities("string_enum_by_path"),
            property_key_by_path: self
                .property_key_by_path
                .instances_to_probabilities("property_key_by_path"),
            identifier_name_by_path: self
                .identifier_name_by_path
                .instances_to_probabilities("identifier_name_by_path"),
            interface_name_by_path: self
                .interface_name_by_path
                .instances_to_probabilities("interface_name_by_path"),
            string_literal_by_path: self
                .string_literal_by_path
                .instances_to_probabilities("string_literal_by_path"),
            list_length_by_path: self
                .list_length_by_path
                .instances_to_probabilities("list_length_by_path"),

            // By window.
            property_key_by_window: self
                .property_key_by_window
                .instances_to_probabilities("property_key_by_window"),
            identifier_name_by_window: self
                .identifier_name_by_window
                .instances_to_probabilities("identifier_name_by_window"),
            string_literal_by_window: self
                .string_literal_by_window
                .instances_to_probabilities("string_literal_by_window"),

            // Linear tables.
            string_literals: self.string_literals,
            floats: self.floats,
            list_lengths: self.list_lengths,
            identifier_names: self.identifier_names,
            property_keys: self.property_keys,
            unsigned_longs: self.unsigned_longs,
        }
    }
}

/// A container for all the user-extensible data in files,
/// e.g. string literals or numbers.
///
/// This container is used to collect statistics, such as the number
/// of instances of a given string in a file, or the number of files
/// that contain a given string.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct UserExtensibleData<T> {
    /// Instances of IdentifierName.
    pub identifier_name_instances: HashMap<Option<IdentifierName>, T>,

    /// Instances of PropertyKey
    pub property_key_instances: HashMap<Option<PropertyKey>, T>,

    /// Instances of InterfaceName
    pub interface_name_instances: HashMap<InterfaceName, T>,

    /// Instances of string literals.
    pub string_literal_instances: HashMap<Option<SharedString>, T>,

    /// Instances of string enums.
    pub string_enum_instances: HashMap<SharedString, T>,

    /// Instances of list lengths.
    pub list_length_instances: HashMap<Option<u32>, T>,

    /// Instances of floating-point numbers.
    pub float_instances: HashMap<Option<F64>, T>,

    /// Instances of unsigned longs.
    pub unsigned_long_instances: HashMap<u32, T>,
}
impl<T> UserExtensibleData<T> {
    pub fn len(&self) -> usize {
        // Make sure that we don't forget a field.
        let UserExtensibleData {
            ref identifier_name_instances,
            ref property_key_instances,
            ref interface_name_instances,
            ref string_literal_instances,
            ref string_enum_instances,
            ref list_length_instances,
            ref float_instances,
            ref unsigned_long_instances,
        } = *self;
        identifier_name_instances.len()
            + property_key_instances.len()
            + string_literal_instances.len()
            + string_enum_instances.len()
            + interface_name_instances.len()
            + list_length_instances.len()
            + float_instances.len()
            + unsigned_long_instances.len()
    }
}

impl<K> InstancesToProbabilities for HashMap<K, FilesContaining>
where
    K: Eq + std::hash::Hash,
{
    type AsProbabilities = HashMap<K, SymbolInfo>;

    fn instances_to_probabilities(self, _description: &str) -> HashMap<K, SymbolInfo> {
        use std::cell::RefCell;
        use std::rc::Rc;

        let instances = self
            .values()
            .map(|x| {
                let x: usize = x.clone().into();
                x as u32
            })
            .collect();
        let distribution = Rc::new(RefCell::new(
            range_encoding::CumulativeDistributionFrequency::new(instances),
        ));

        self.into_iter()
            .enumerate()
            .map(|(index, (key, _))| {
                (
                    key,
                    SymbolInfo {
                        index: SymbolIndex::from(index),
                        distribution: distribution.clone(),
                    },
                )
            })
            .collect()
    }
}

/// A structure used to build a dictionary based on a sample of files.
pub struct DictionaryBuilder {
    /// A dictionary.
    ///
    /// This is a shared reference as we typically wish to
    /// access this field after the DictionaryBuilder
    /// has been consumed and released by a `Serializer`.
    dictionary: Dictionary<Instances>,

    /// Number of instances of each string in the current file.
    instances_of_user_extensible_data_in_current_file: UserExtensibleData<InstancesInFile>,

    /// Number of files in which each string appears.
    files_containing_user_extensible_data: UserExtensibleData<FilesContaining>,
}

impl DictionaryBuilder {
    /// Create a new dictionary builder using paths of `depth` depth
    /// and windows of `width` width.
    ///
    /// Use `DictionaryBuilder::done` to convert it into a `Dictionary`.
    pub fn new(depth: usize, width: usize) -> Self {
        DictionaryBuilder {
            dictionary: Dictionary::new(depth, width),
            instances_of_user_extensible_data_in_current_file: UserExtensibleData::default(),
            files_containing_user_extensible_data: UserExtensibleData::default(),
        }
    }

    /// Return a dictionary containing all the paths collected and all
    /// the user-extensible content that appear in more than one file.
    pub fn done(self, threshold: FilesContaining) -> Dictionary<Instances> {
        let mut dictionary = self.dictionary;

        // Generate indexed tables for user-extensible values.
        dictionary.identifier_names = LinearTable::new(
            self.files_containing_user_extensible_data
                .identifier_name_instances,
            threshold,
        );
        dictionary.property_keys = LinearTable::new(
            self.files_containing_user_extensible_data
                .property_key_instances,
            threshold,
        );
        dictionary.list_lengths = LinearTable::new(
            self.files_containing_user_extensible_data
                .list_length_instances,
            threshold,
        );
        dictionary.floats = LinearTable::new(
            self.files_containing_user_extensible_data.float_instances,
            threshold,
        );
        dictionary.unsigned_longs = LinearTable::new(
            self.files_containing_user_extensible_data
                .unsigned_long_instances,
            threshold,
        );
        dictionary.string_literals = LinearTable::new(
            self.files_containing_user_extensible_data
                .string_literal_instances,
            threshold,
        );

        dictionary
    }

    pub fn len(&self) -> usize {
        self.dictionary.len()
    }

    /// Access statistics on the number of files containing specific user-extensible values.
    pub fn files_containing(&self) -> &UserExtensibleData<FilesContaining> {
        &self.files_containing_user_extensible_data
    }

    /// Access statistics on the number of instances of a specific user-extensible value.
    pub fn instances_in_file(&self) -> &UserExtensibleData<InstancesInFile> {
        &self.instances_of_user_extensible_data_in_current_file
    }

    /// Take all strings of a given nature present in a file (as stored
    /// in `self.instances_of_user_extensible_data_in_current_file`) and mark them as
    /// appearing in one more file (as stored in `self.files_containing_user_extensible_data`).
    ///
    /// The caller is responsible for making sure that `source` is a
    /// `self.instances_of_user_extensible_data_in_current_file.XXX` and `destination`
    /// is the corresponding `self.files_containing_user_extensible_data.XXX`.
    ///
    /// Note: This is a function rather than a method because making it a method
    /// would require us to borrow mutably `source` *and* while calling into `self`.
    /// Not very borrow-checker-compatible.
    fn transfer_instances<V>(
        source: &mut HashMap<V, InstancesInFile>,
        destination: &mut HashMap<V, FilesContaining>,
    ) where
        V: std::hash::Hash + Eq + Clone + std::fmt::Debug,
    {
        for (k, _) in source.drain() {
            // Increase the number of files in `destination` that contain `k` by 1,
            // ignoring the number of instances of `k` in `source`.
            destination
                .entry(k)
                .and_modify(|instances| *instances += FilesContaining(1))
                .or_insert(FilesContaining(1));
        }
    }

    fn done_with_file(&mut self) {
        // Count the number of files in which user-extensible instances appear.
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .identifier_name_instances,
            &mut self
                .files_containing_user_extensible_data
                .identifier_name_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .property_key_instances,
            &mut self
                .files_containing_user_extensible_data
                .property_key_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .interface_name_instances,
            &mut self
                .files_containing_user_extensible_data
                .interface_name_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .string_literal_instances,
            &mut self
                .files_containing_user_extensible_data
                .string_literal_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .string_enum_instances,
            &mut self
                .files_containing_user_extensible_data
                .string_enum_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .list_length_instances,
            &mut self
                .files_containing_user_extensible_data
                .list_length_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .float_instances,
            &mut self.files_containing_user_extensible_data.float_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .unsigned_long_instances,
            &mut self
                .files_containing_user_extensible_data
                .unsigned_long_instances,
        );
    }
}

impl<'a> TokenWriter for &'a mut DictionaryBuilder {
    type Data = [u8; 0]; // Placeholder

    fn done(self) -> Result<Self::Data, TokenWriterError> {
        self.done_with_file();
        debug!(target: "entropy", "Built a dictionary with len: {}", self.dictionary.len());
        Ok([])
    }

    fn bool_at(&mut self, value: Option<bool>, path: &IOPath) -> Result<(), TokenWriterError> {
        (*self).bool_at(value, path)
    }

    fn float_at(&mut self, value: Option<f64>, path: &IOPath) -> Result<(), TokenWriterError> {
        (*self).float_at(value, path)
    }

    fn unsigned_long_at(&mut self, value: u32, path: &IOPath) -> Result<(), TokenWriterError> {
        (*self).unsigned_long_at(value, path)
    }

    fn string_enum_at(
        &mut self,
        value: &SharedString,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        (*self).string_enum_at(value, path)
    }

    fn string_at(
        &mut self,
        value: Option<&SharedString>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        (*self).string_at(value, path)
    }

    fn property_key_at(
        &mut self,
        value: Option<&PropertyKey>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        (*self).property_key_at(value, path)
    }

    fn identifier_name_at(
        &mut self,
        value: Option<&IdentifierName>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        (*self).identifier_name_at(value, path)
    }

    fn enter_list_at(&mut self, len: usize, path: &IOPath) -> Result<(), TokenWriterError> {
        (*self).enter_list_at(len, path)
    }

    fn enter_tagged_tuple_at(
        &mut self,
        node: &Node,
        tag: &InterfaceName,
        children: &[&FieldName],
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        (*self).enter_tagged_tuple_at(node, tag, children, path)
    }

    fn offset_at(&mut self, path: &IOPath) -> Result<(), TokenWriterError> {
        (*self).offset_at(path)
    }
}

impl TokenWriter for DictionaryBuilder {
    type Data = [u8; 0]; // Placeholder

    fn done(mut self) -> Result<Self::Data, TokenWriterError> {
        self.done_with_file();
        debug!(target: "entropy", "Built a dictionary with len: {}", self.dictionary.len());
        Ok([])
    }

    fn bool_at(&mut self, value: Option<bool>, path: &IOPath) -> Result<(), TokenWriterError> {
        update_in_context!(self, bool_by_path, "bool_by_path", path, value)?;
        Ok(())
    }

    fn float_at(&mut self, value: Option<f64>, path: &IOPath) -> Result<(), TokenWriterError> {
        let value = value.map(|x| x.into());
        update_in_context!(self, float_by_path, "float_by_path", path, value)?;
        increment_instance_count!(self, float_instances, value);
        Ok(())
    }

    fn unsigned_long_at(&mut self, value: u32, path: &IOPath) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            unsigned_long_by_path,
            "unsigned_long_by_path",
            path,
            value
        )?;
        increment_instance_count!(self, unsigned_long_instances, value);
        Ok(())
    }

    fn string_enum_at(
        &mut self,
        value: &SharedString,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            string_enum_by_path,
            "string_enum_by_path",
            path,
            value.clone()
        )?;
        Ok(())
    }

    fn string_at(
        &mut self,
        value: Option<&SharedString>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            string_literal_by_path,
            "string_literal_by_path",
            path,
            value.cloned()
        )?;
        update_in_window!(self, string_literal_by_window, value.cloned())?;
        increment_instance_count!(self, string_literal_instances, value.cloned());
        Ok(())
    }

    fn property_key_at(
        &mut self,
        value: Option<&PropertyKey>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            property_key_by_path,
            "property_key_by_path",
            path,
            value.cloned()
        )?;
        update_in_window!(self, property_key_by_window, value.cloned())?;
        increment_instance_count!(self, property_key_instances, value.cloned());
        Ok(())
    }

    fn identifier_name_at(
        &mut self,
        value: Option<&IdentifierName>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            identifier_name_by_path,
            "identifier_name_by_path",
            path,
            value.cloned()
        )?;
        update_in_window!(self, identifier_name_by_window, value.cloned())?;
        increment_instance_count!(self, identifier_name_instances, value.cloned());
        Ok(())
    }

    fn enter_list_at(&mut self, len: usize, path: &IOPath) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            list_length_by_path,
            "list_length_by_path",
            path,
            Some(len as u32)
        )?;
        increment_instance_count!(self, list_length_instances, Some(len as u32));
        Ok(())
    }

    fn enter_tagged_tuple_at(
        &mut self,
        _node: &Node,
        tag: &InterfaceName,
        _children: &[&FieldName],
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            interface_name_by_path,
            "interface_name_by_path",
            path,
            tag.clone()
        )?;
        increment_instance_count!(self, interface_name_instances, tag.clone());
        Ok(())
    }

    fn offset_at(&mut self, _path: &IOPath) -> Result<(), TokenWriterError> {
        Ok(())
    }
}
