use entropy::predict::{ PathPredict };
use entropy::probabilities::{ InstancesToProbabilities, Symbol };

use io::TokenWriter;
use ::TokenWriterError;

use binjs_shared::{ F64, FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString };

use std;
use std::collections::HashMap;

pub type IOPath = binjs_shared::ast::Path<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;

/// A newtype for `usize` used to count instances of some item in a given file.
int_alias!(InstancesInFile, usize);

/// A newtype for `usize` used to count the number of files containing some item.
int_alias!(FilesContaining, usize);


#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Dictionary<T> {
    /// All booleans appearing in the AST, predicted by path.
    pub bool_by_path: PathPredict<Option<bool>, T>,

    /// All floats appearing in the AST, predicted by path.
    pub float_by_path: PathPredict<Option<F64>, T>,

    /// All unsigned longs appearing in the AST, predicted by path.
    pub unsigned_long_by_path: PathPredict<u32, T>,

    /// All string enumerations, predicted by path.
    pub string_enum_by_path: PathPredict<SharedString, T>,

    /// All property keys, predicted by path.
    pub property_key_by_path: PathPredict<Option<PropertyKey>, T>,

    /// All identifier names, predicted by path.
    pub identifier_name_by_path: PathPredict<Option<IdentifierName>, T>,

    /// All interface names, predicted by path.
    pub interface_name_by_path: PathPredict<InterfaceName, T>,

    /// All string literals, predicted by path.
    pub string_literal_by_path: PathPredict<Option<SharedString>, T>,

    /// All list lengths, predicted by path.
    pub list_length_by_path: PathPredict<Option<u32>, T>,

    // Missing:
    // - offsets (cannot be predicted?)
    // - property keys predicted by window?
    // - identifier names predicted by window?
    // - literal strings by window?
    // - directives?
}
impl<T> Dictionary<T> {
    /// Return the number of states in this dictionary.
    pub fn len(&self) -> usize {
          self.bool_by_path.len()
        + self.float_by_path.len()
        + self.unsigned_long_by_path.len()
        + self.string_enum_by_path.len()
        + self.property_key_by_path.len()
        + self.identifier_name_by_path.len()
        + self.interface_name_by_path.len()
        + self.string_literal_by_path.len()
        + self.list_length_by_path.len()
    }
}
impl InstancesToProbabilities for Dictionary<usize> {
    type AsProbabilities = Dictionary<Symbol>;

    /// Convert a dictionary counting instances into a dictionary that
    /// counting probabilities.
    fn instances_to_probabilities(self) -> Dictionary<Symbol> {
        Dictionary {
            bool_by_path: self.bool_by_path.instances_to_probabilities(),
            float_by_path: self.float_by_path.instances_to_probabilities(),
            unsigned_long_by_path: self.unsigned_long_by_path.instances_to_probabilities(),
            string_enum_by_path: self.string_enum_by_path.instances_to_probabilities(),
            property_key_by_path: self.property_key_by_path.instances_to_probabilities(),
            identifier_name_by_path: self.identifier_name_by_path.instances_to_probabilities(),
            interface_name_by_path: self.interface_name_by_path.instances_to_probabilities(),
            string_literal_by_path: self.string_literal_by_path.instances_to_probabilities(),
            list_length_by_path: self.list_length_by_path.instances_to_probabilities(),
        }
    }
}

/// Maps from the various kinds of strings in the AST to T.
///
/// This container is used to collect statistics, such as the number
/// of instances of a given string in a file, or the number of files
/// that contain a given string.
#[derive(Debug, Default)]
pub struct KindedStringMap<T> {
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
}

impl InstancesToProbabilities for StringsInFile<FilesContaining> {
    type AsProbabilities = StringsInFile<Symbol>;

    /// Convert a dictionary counting instances into a dictionary
    /// counting probabilities.
    fn instances_to_probabilities(self) -> StringsInFile<Symbol> {
        StringsInFile {
            identifier_name_instances: self.identifier_name_instances.instances_to_probabilities(),
            property_key_instances: self.property_key_instances.instances_to_probabilities(),
            interface_name_instances: self.interface_name_instances.instances_to_probabilities(),
            string_literal_instances: self.string_literal_instances.instances_to_probabilities(),
            string_enum_instances: self.string_enum_instances.instances_to_probabilities(),
        }
    }
}

impl<K> InstancesToProbabilities for HashMap<K, FilesContaining>
    where K: Eq + std::hash::Hash
{
    type AsProbabilities = HashMap<K, Symbol>;

    fn instances_to_probabilities(self) -> HashMap<K, Symbol> {
        use std::cell::RefCell;
        use std::rc::Rc;

        let instances = self.values()
            .map(|x| x.0 as u32)
            .collect();
        let distribution = Rc::new(RefCell::new(range_encoding::CumulativeDistributionFrequency::new(instances)
            .unwrap())); // FISME: Handle the empty case.

        self.into_iter()
            .enumerate()
            .map(|(index, (key, _))| {
                (key, Symbol {
                    index,
                    distribution: distribution.clone()
                })
            })
            .collect()
    }
}

/// A structure used to build a dictionary based on a sample of files.
///
/// In this version, we use several dictionary lengths, to determine the
/// best version for a file.
pub struct DictionaryBuilder<'a> {
    /// Attempt to build predictions with a depth of 0 (no context) up to max_path_depth ancestors.
    max_path_depth: usize,

    /// A dictionary.
    ///
    /// This is a shared reference as we typically wish to
    /// access this field after the DictionaryBuilder
    /// has been consumed and released by a `Serializer`.
    dictionary: &'a mut Dictionary</* instances */ usize>,

    /// Number of instances of each string in the current file.
    instances_of_strings_in_current_file: KindedStringMap<InstancesInFile>,

    /// Number of files in which each string appears.
    files_containing_string: &'a mut KindedStringMap<FilesContaining>,
}

impl<'a> DictionaryBuilder<'a> {
    pub fn new(max_path_depth: usize, dictionary: &'a mut Dictionary<usize>, files_containing_string: &'a mut KindedStringMap<FilesContaining>) -> Self {
        DictionaryBuilder {
            max_path_depth,
            dictionary,
            instances_of_strings_in_current_file: KindedStringMap::default(),
            files_containing_string
        }
    }

    /// Increment the number of cases in which `value` is a possible value for `path`.
    ///
    /// Since we do not want to store every single possible path, we use tails of
    /// `path` limited to `max_path_depth` items. We also store shorter path, to
    /// let us analyze later, on a case-by-case basis, whether having longer paths
    /// is useful or just increases the size of the dictionary.
    ///
    /// Note: This is a function rather than a method because making it a method
    /// would require us to borrow mutably `source` *and* while calling into `self`.
    /// Not very borrow-checker-compatible.
    fn add_instance_to_path<V>(max_path_depth: usize, value: V, path: &IOPath, source: &mut PathPredict<V, usize>)
        where
            V: 'a + std::hash::Hash + Eq + Clone + std::fmt::Debug,
    {
        let len = usize::min(max_path_depth, path.len()) + 1;
        // Update predictions with a path length of 0..len.
        for len in 0 .. len {
            let tail = path.tail(len);
            source.entry(tail, &value)
                .value
                .and_modify(|instances| {
                    *instances += 1 // We already have at least one association `tail` => `value`, increment.
                }).or_insert(1);    // First time we associate `tail` => `value`, store 1.
        }
    }

    /// Count the string `value` as used in the current file.
    fn add_instance_to_strings<V>(value: V, bucket: &mut HashMap<V, InstancesInFile>)
        where
            V: std::hash::Hash + Eq + Clone + std::fmt::Debug
    {
        bucket.entry(value)
            .and_modify(|instances| {
                *instances += InstancesInFile(1) // We have already seen this string in this file, increment.
            }).or_insert(InstancesInFile(1));    // First time we see this string in this file, store 1.
    }

    /// Take all strings of a given nature present in a file (as stored
    /// in `self.instances_of_strings_in_current_file`) and mark them as
    /// appearing in one more file (as stored in `self.files_containing_string`).
    ///
    /// The caller is responsible for making sure that `source` is a
    /// `self.instances_of_strings_in_current_file.XXX` and `destination`
    /// is the corresponding `self.files_containing_string.XXX`.
    ///
    /// Note: This is a function rather than a method because making it a method
    /// would require us to borrow mutably `source` *and* while calling into `self`.
    /// Not very borrow-checker-compatible.
    fn transfer_instances_of_strings<V>(source: &mut HashMap<V, InstancesInFile>, destination: &mut HashMap<V, FilesContaining>)
        where
            V: std::hash::Hash + Eq + Clone + std::fmt::Debug
    {
        for (k, _) in source.drain() {
            // Increase the number of files in `destination` that contain `k` by 1,
            // ignoring the number of instances of `k` in `source`.
            destination.entry(k)
                .and_modify(|instances| {
                    *instances += FilesContaining(1)
                }).or_insert(FilesContaining(1));
        }
    }

    fn done_with_file(&mut self) {
        // Count the number of files in which string instances appear.
        Self::transfer_instances_of_strings(
                &mut self.instances_of_strings_in_current_file.identifier_name_instances,
                &mut self.files_containing_string.identifier_name_instances
        );
        Self::transfer_instances_of_strings(
                &mut self.instances_of_strings_in_current_file.property_key_instances,
                &mut self.files_containing_string.property_key_instances
        );
        Self::transfer_instances_of_strings(
                &mut self.instances_of_strings_in_current_file.interface_name_instances,
                &mut self.files_containing_string.interface_name_instances
        );
        Self::transfer_instances_of_strings(
                &mut self.instances_of_strings_in_current_file.string_literal_instances,
                &mut self.files_containing_string.string_literal_instances
        );
        Self::transfer_instances_of_strings(
                &mut self.instances_of_strings_in_current_file.string_enum_instances,
                &mut self.files_containing_string.string_enum_instances
        );
    }
}

impl<'a> TokenWriter for DictionaryBuilder<'a> {
    type Statistics = usize /* placeholder */;
    type Data = [u8;0];

    fn done(mut self) -> Result<(Self::Data, Self::Statistics), TokenWriterError> {
        self.done_with_file();
        debug!(target: "entropy", "Built a dictionary with len: {}", self.dictionary.len());
        Ok(([], 0))
    }

    fn bool_at(&mut self, value: Option<bool>, path: &IOPath) -> Result<(), TokenWriterError> {
        Self::add_instance_to_path(self.max_path_depth, value, path, &mut self.dictionary.bool_by_path);
        Ok(())
    }

    fn float_at(&mut self, value: Option<f64>, path: &IOPath) -> Result<(), TokenWriterError> {
        let value = value.map(|x| x.into());
        Self::add_instance_to_path(self.max_path_depth, value, path, &mut self.dictionary.float_by_path);
        Ok(())
    }

    fn unsigned_long_at(&mut self, value: u32, path: &IOPath) -> Result<(), TokenWriterError> {
        Self::add_instance_to_path(self.max_path_depth, value, path, &mut self.dictionary.unsigned_long_by_path);
        Ok(())
    }

    fn string_enum_at(&mut self, value: &SharedString, path: &IOPath) -> Result<(), TokenWriterError> {
        Self::add_instance_to_path(self.max_path_depth, value.clone(), path, &mut self.dictionary.string_enum_by_path);
        Self::add_instance_to_strings(value.clone(), &mut self.instances_of_strings_in_current_file.string_enum_instances);
        Ok(())
    }

    fn string_at(&mut self, value: Option<&SharedString>, path: &IOPath) -> Result<(), TokenWriterError> {
        Self::add_instance_to_path(self.max_path_depth, value.cloned(), path, &mut self.dictionary.string_literal_by_path);
        Self::add_instance_to_strings(value.cloned(), &mut self.instances_of_strings_in_current_file.string_literal_instances);
        Ok(())
    }

    fn property_key_at(&mut self, value: Option<&PropertyKey>, path: &IOPath) -> Result<(), TokenWriterError> {
        Self::add_instance_to_path(self.max_path_depth, value.cloned(), path, &mut self.dictionary.property_key_by_path);
        Self::add_instance_to_strings(value.cloned(), &mut self.instances_of_strings_in_current_file.property_key_instances);
        Ok(())
    }

    fn identifier_name_at(&mut self, value: Option<&IdentifierName>, path: &IOPath) -> Result<(), TokenWriterError> {
        Self::add_instance_to_path(self.max_path_depth, value.cloned(), path, &mut self.dictionary.identifier_name_by_path);
        Self::add_instance_to_strings(value.cloned(), &mut self.instances_of_strings_in_current_file.identifier_name_instances);
        Ok(())
    }

    fn enter_list_at(&mut self, len: usize, path: &IOPath) -> Result<(), TokenWriterError> {
        Self::add_instance_to_path(self.max_path_depth, Some(len as u32), path, &mut self.dictionary.list_length_by_path);
        Ok(())
    }

    fn enter_tagged_tuple_at(&mut self, tag: &InterfaceName, _children: &[&FieldName], path: &IOPath)  -> Result<(), TokenWriterError> {
        Self::add_instance_to_path(self.max_path_depth, tag.clone(), path, &mut self.dictionary.interface_name_by_path);
        Self::add_instance_to_strings(tag.clone(), &mut self.instances_of_strings_in_current_file.interface_name_instances);
        Ok(())
    }

    fn offset_at(&mut self, _path: &IOPath) -> Result<(), TokenWriterError> {
        Ok(())
    }
}
