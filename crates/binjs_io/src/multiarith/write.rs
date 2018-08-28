
use multiarith::{ EncodingModel, Model };
use multiarith::tree:: { F64, Label, Path, ScopeIndex, SharedTree, SubTree, Tag, Visitor, WalkTree };
use multiarith::predict::{ ContextPredict, PathPredict, Symbol };

use io::TokenWriter;
use ::TokenWriterError;
use util::GenericCounter;

use range_encoding::opus;

use std;
use std::cell::RefCell;
use std::io::Read;
use std::rc::Rc;

const EMPTY_16_BYTES_ARRAY : [u8; 16] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

/// A constant used to initialize the maximal path depth.
/// Changing it will only affect performance.
const EXPECTED_PATH_DEPTH: usize = 2048;

/// A constant used to initialize the maximal scope depth.
/// Changing it will only affect performance.
const EXPECTED_SCOPE_DEPTH: usize = 128;

struct ExactEncodingModelData<T> {
    /// Tag prediction based on path (depth 1 as of this writing).
    tags: PathPredict<Tag, T>,

    /// Non-identifier string prediction based on path (depth 1 as of this writing).
    strings: PathPredict<Option<Rc<String>>, T>,

    /// Number prediction based on path.
    numbers: PathPredict<Option<F64>, T>,

    /// Bool prediction.
    bools: PathPredict<Option<bool>, T>,

    /// Identifier prediction based on scope.
    identifiers: ContextPredict<Option<ScopeIndex>, Rc<String>, T>,

    /// List length prediction based on path.
    list_lengths: PathPredict<Option<u32>, T>,

    /// A trivial model for bools with equal frequency for either value.
    ///
    /// Used to represent trivial dictionaries.
    iso_bit_model: ContextPredict<(), bool, T>,
}

/// Initialize the ExactEncodingModel
impl Visitor for ExactEncodingModelData</* Number of instances */ usize> {
    type Error = ();
    fn enter_label(&mut self, label: &Label, path: &Path<(Tag, usize)>, scopes: &Path<ScopeIndex>) -> Result<(), Self::Error> {
        match label {
            Label::Tag(ref tag) => {
                let (_, entry) = self.tags.entry(path, tag);
                entry.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            Label::String(ref string) => {
                let (_, entry) = self.strings.entry(path, string);
                entry.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            Label::Number(ref num) => {
                let (_, entry) = self.numbers.entry(path, num);
                entry.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            Label::List(ref len) => {
                let (_, entry) = self.list_lengths.entry(path, len);
                entry.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            Label::Declare(Some(ref string)) | Label::LiteralReference(Some(ref string)) => {
                let scope = scopes.last()
                    .map(Clone::clone);
                let (_, entry) = self.identifiers.entry(scope, string);
                entry.and_modify(|instances| {
                        *instances += 1
                    }).or_insert(1);
            }
            Label::Bool(ref value) => {
                let (_, entry) = self.bools.entry(path, value);
                entry.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            _ => {
                warn!(target: "multiarith", "Skipping initialization of predictor for label {:?} (not implemented yet)", label);
            }
        }
        Ok(())
    }
}

/// An encoding model which starts by analyzing the full AST to determine
/// exact statistics.
pub struct ExactEncodingModel {
    probabilities: ExactEncodingModelData<Symbol>,
}
impl ExactEncodingModel {
    fn get_from_path<T>(predictor: &mut PathPredict<T, Symbol>, value: &T, path: &Path<(Tag, usize)>) -> Result<Symbol, ()>
        where T: Eq + std::hash::Hash + Clone + std::fmt::Debug
    {
        let segment = predictor.get_mut(path, value)
            .ok_or(())?;
        let result = segment.clone();
        Ok(result)
    }
}
impl EncodingModel for ExactEncodingModel {
    fn string_frequency_for_encoding(&mut self, value: &Option<Rc<String>>, path: &Path<(Tag, usize)>) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.strings, value, path)
    }
    fn tag_frequency_for_encoding(&mut self, value: &Tag, path: &Path<(Tag, usize)>) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.tags, value, path)
    }
    fn bool_frequency_for_encoding(&mut self, value: &Option<bool>, path: &Path<(Tag, usize)>) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.bools, value, path)
    }
    fn number_frequency_for_encoding(&mut self, value: &Option<F64>, path: &Path<(Tag, usize)>) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.numbers, value, path)
    }
    fn list_length_frequency_for_encoding(&mut self, value: &Option<u32>, path: &Path<(Tag, usize)>) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.list_lengths, value, path)
    }
    fn iso_bit_frequency_for_encoding(&mut self, value: bool) -> Result<Symbol, ()> {
        let segment = self.probabilities.iso_bit_model.get_mut(&mut (), &value)
            .ok_or(())?;
        Ok(segment.clone())
    }
    fn identifier_frequency_for_encoding(&mut self, string: &Rc<String>, scopes: &Path<ScopeIndex>) -> Result<Symbol, ()> {
        let scope = scopes.last()
            .cloned();
        let segment = self.probabilities.identifiers.get_mut(&scope, string)
            .ok_or(())?;
        let result = segment.clone();
        Ok(result)
    }
}
impl ExactEncodingModel {
    pub fn new(tree: &SharedTree) -> Self {
        // Compute number of instances
        let mut instances = ExactEncodingModelData {
            tags: PathPredict::new(1), // FIXME: Test with other depths
            strings: PathPredict::new(1), // FIXME: Test with other depths
            list_lengths: PathPredict::new(1), // FIXME: Test with other depths
            numbers: PathPredict::new(1), // Fairly confident that a depth of 1 should be sufficient for numbers.
            bools: PathPredict::new(1),
            identifiers: ContextPredict::new(),
            iso_bit_model: ContextPredict::new(),
        };

        // Initialize trivial `iso_bit_model`.
        for bit in [false, true].into_iter() {
            let (_, entry) = instances.iso_bit_model.entry((), &bit);
            entry.and_modify(|instances| {
                *instances += 1
            }).or_insert(1);
        }

        tree.walk(&mut instances,
            &mut Path::with_capacity(EXPECTED_PATH_DEPTH),
            &mut Path::with_capacity(EXPECTED_SCOPE_DEPTH))
            .expect("Could not compute number of instances");

        // Deduce probabilities.

        Self {
            probabilities: ExactEncodingModelData {
                tags: instances.tags.instances_to_probabilities(),
                strings: instances.strings.instances_to_probabilities(),
                identifiers: instances.identifiers.instances_to_probabilities(),
                bools: instances.bools.instances_to_probabilities(),
                numbers: instances.numbers.instances_to_probabilities(),
                list_lengths: instances.list_lengths.instances_to_probabilities(),
                iso_bit_model: instances.iso_bit_model.instances_to_probabilities(),
            }
        }
    }
}

pub struct TreeTokenWriter<'a> {
    root: SharedTree,
    scope_counter: GenericCounter<ScopeIndex>,
    model: &'a Model,
    options: Options,
    encoder: opus::Writer<Vec<u8>>,
}

struct Compressor<'a> {
    model: Box<EncodingModel>,
    encoder: &'a mut opus::Writer<Vec<u8>>,
    options: Options,
}

impl<'a> Compressor<'a> {
    fn encode_uncompressed_bit(&mut self, value: bool) -> Result<(), std::io::Error> {
        let symbol = self.model.iso_bit_frequency_for_encoding(value)
            .unwrap();
        let mut distribution = symbol.distribution.borrow_mut();
        self.encoder.symbol(symbol.index, &mut distribution)?;
        Ok(())
    }
    fn encode_uncompressed_bits(&mut self, bits: &[bool]) -> Result<(), std::io::Error>
    {
        for bit in bits {
            self.encode_uncompressed_bit(*bit)?;
        }
        Ok(())
    }
    fn encode_uncompressed_varnum(&mut self, value: Option<u32>)-> Result<(), std::io::Error> {
        use bytes::varnum::*;

        // A few shennanigans to avoid heap allocating needlessly.
        let mut buf = EMPTY_16_BYTES_ARRAY;
        let mut buf = std::io::Cursor::new(&mut buf as &mut [u8]);
        buf.write_maybe_varnum(value)?;
        for byte in buf.bytes().map(Result::unwrap) { // Read from a Cursor's Bytes cannot fail.
            for shift in 0..7 {
                let bit = (byte & 1 << shift) != 0;
                self.encode_uncompressed_bit(bit)?;
            }
        }
        Ok(())
    }

    fn encode_uncompressed_float(&mut self, value: Option<f64>)-> Result<(), std::io::Error> {
        use bytes::float::*;

        // A few shennanigans to avoid heap allocating needlessly.
        let mut buf = EMPTY_16_BYTES_ARRAY;
        let mut buf = std::io::Cursor::new(&mut buf as &mut [u8]);
        buf.write_maybe_varfloat(value)?;
        for byte in buf.bytes() {
            let byte = byte.unwrap(); // Read from a Cursor cannot fail.
            for shift in 0..7 {
                let bit = (byte & 1 << shift) != 0;
                self.encode_uncompressed_bit(bit)?;
            }
        }
        Ok(())
    }

    fn encode_uncompressed_frequency(&mut self, value: u32) -> Result<(), std::io::Error> {
        self.encode_uncompressed_varnum(Some(value))
    }

    fn encode_value<F>(&mut self, symbol: Symbol, f: F) -> Result<(), std::io::Error>
        where F: FnOnce(&mut Self) -> Result<(), std::io::Error>
    {
        let mut distribution = symbol.distribution.borrow_mut();
        let requirements = self.encoder.symbol(symbol.index, &mut *distribution)?;
        if self.options.inline_dictionaries && requirements.symbol() {
            // First, write the symbol itself.
            f(self)?;
            // Then, write its frequency.
            let segment = distribution.at_index(symbol.index).unwrap();
                // We would have failed in `self.encoder.symbol` if `distribution.at_index` failed.
            self.encode_uncompressed_frequency(segment.width())?;
        }
        if self.options.inline_dictionaries && requirements.distribution_total() {
            // Finally, if needed, write the total frequency.
            self.encode_uncompressed_frequency(distribution.width())?;
        }
        Ok(())
    }

    fn encode_uncompressed_string(&mut self, value: &Rc<String>) -> Result<(), std::io::Error> {
        self.encode_uncompressed_varnum(Some(value.len() as u32))?;
        for byte in value.bytes() {
            for shift in 0..7 {
                let bit = (byte & 1 << shift) != 0;
                self.encode_uncompressed_bit(bit)?;
            }
        }
        Ok(())
    }
    fn encode_uncompressed_maybe_string(&mut self, value: &Option<Rc<String>>) -> Result<(), std::io::Error> {
        match value {
            None => self.encode_uncompressed_varnum(None),
            Some(ref s) => self.encode_uncompressed_string(s)
        }
    }
}

impl<'a> Visitor for Compressor<'a> {
    type Error = std::io::Error;
    fn enter_label(&mut self, label: &Label, path: &Path<(Tag, usize)>, scopes: &Path<ScopeIndex>) -> Result<(), Self::Error> {
        match label {
            Label::Tag(ref tag) => {
                let symbol = self.model.tag_frequency_for_encoding(tag, path)
                    .expect("Could not compute tag frequency");
                if !self.options.encode_tags {
                    return Ok(())
                }
                self.encode_value(symbol, move |me| me.encode_uncompressed_string(tag.content()))?;
            }
            Label::Number(ref value) => {
                let symbol = self.model.number_frequency_for_encoding(value, path)
                    .expect("Could not compute number frequency");
                if !self.options.encode_numbers {
                    return Ok(())
                }
                self.encode_value(symbol, move |me| me.encode_uncompressed_float(value.map(|x| x.0.clone())))?;
            }
            Label::List(ref len) => {
                let symbol = self.model.list_length_frequency_for_encoding(len, path)
                    .expect("Could not compute list length frequency");
                if !self.options.encode_list_lengths {
                    return Ok(())
                }
                self.encode_value(symbol, move |me| me.encode_uncompressed_varnum(len.map(|x| x.clone())))?;
            }
            Label::String(ref string) => {
                let symbol = self.model.string_frequency_for_encoding(string, path)
                    .expect("Could not compute string frequency");
                if !self.options.encode_strings {
                    return Ok(())
                }
                self.encode_value(symbol, move |me| me.encode_uncompressed_maybe_string(string))?;
            }
            Label::Bool(ref value) => {
                let symbol = self.model.bool_frequency_for_encoding(value, path)
                    .expect("Could not compute bool frequency");
                if !self.options.encode_bools {
                    return Ok(())
                }
                self.encode_value(symbol, move |me| {
                    match value { // FIXME: Would be more efficient if we knew ahead of time whether it's a bool or an Option<bool>
                        None => me.encode_uncompressed_bits(&[true, true]),
                        Some(true) => me.encode_uncompressed_bits(&[false, true]),
                        Some(false) => me.encode_uncompressed_bits(&[false, false]),
                    }
                })?;
            }
            Label::Declare(Some(ref string)) | Label::LiteralReference(Some(ref string)) => {
                let symbol = self.model.identifier_frequency_for_encoding(string, scopes)
                    .expect("Could not compute identifier frequency");
                if !self.options.encode_identifiers {
                    return Ok(())
                }
                self.encode_value(symbol, move |me| me.encode_uncompressed_maybe_string(&Some(string.clone())))?;
            }
            _ => {
                warn!(target: "multiarith", "Skipping serialization of label {:?} (not implemented yet)", label);
            }
        }
        Ok(())
    }
}

impl<'a> TreeTokenWriter<'a> {
    pub fn new(model: &'a Model, options: Options) -> Self {
        Self {
            scope_counter: GenericCounter::new(),
            model,
            options,
            encoder: opus::Writer::new(Vec::new()),
            root: Rc::new(RefCell::new(SubTree {
                label: Label::String(None),
                children: vec![]
            }))
        }
    }
    fn new_tree(&mut self, tree: SubTree) -> Result<SharedTree, TokenWriterError> {
        self.root = Rc::new(RefCell::new(tree));
        Ok(self.root.clone())
    }
}


impl<'a> TokenWriter for TreeTokenWriter<'a> {
    type Statistics = usize; // Placeholder
    type Tree = SharedTree;
    type Data = Vec<u8>;

    fn tagged_tuple(&mut self, tag: &str, children: &[(&str, Self::Tree)]) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Tag(Tag::new(tag)),
            children: children.iter()
                .map(|(_, tree)| tree.clone())
                .collect()
        })
    }

    fn tagged_scope_tuple(&mut self, tag: &str, children: &[(&str, Self::Tree)]) -> Result<Self::Tree, TokenWriterError> {
        let tuple = self.tagged_tuple(tag, children)?;
        let index = self.scope_counter.next();
        self.new_tree(SubTree {
            label: Label::Scope(index),
            children: vec![tuple]
        })
    }

    fn identifier_definition(&mut self, name: Option<&str>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Declare(name.map(|name|
                Rc::new(name.to_string())
            )),
            children: vec![]
        })
    }

    fn identifier_reference(&mut self, name: Option<&str>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::LiteralReference(name.map(|name|
                Rc::new(name.to_string())
            )),
            children: vec![]
        })
    }

    fn offset(&mut self) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }

    fn bool(&mut self, value: Option<bool>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Bool(value),
            children: vec![]
        })
    }

    fn float(&mut self, value: Option<f64>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Number(value.map(F64)),
            children: vec![]
        })
    }

    fn string(&mut self, value: Option<&str>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::String(value.map(|x| Rc::new(x.to_string()))),
            children: vec![]
        })
    }

    fn string_enum(&mut self, value: &str) -> Result<Self::Tree, TokenWriterError> {
        self.tagged_tuple(value, &[])
    }

    fn list(&mut self, children: Vec<Self::Tree>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::List(Some(children.len() as u32)),
            children
        })
    }

    fn untagged_tuple(&mut self, _: &[Self::Tree]) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }

    fn done(mut self) -> Result<(Self::Data, Self::Statistics), TokenWriterError> {
        let model = self.model.encoding(&self.root);
        let root = self.root.clone();
        {
            let mut compressor = Compressor {
                model,
                options: self.options.clone(),
                encoder: &mut self.encoder
            };
            root.walk(&mut compressor,
                &mut Path::with_capacity(EXPECTED_PATH_DEPTH),
                &mut Path::with_capacity(EXPECTED_SCOPE_DEPTH)
            ).unwrap(); // FIXME: Handle errors
        }
        let data = self.encoder.done()
            .unwrap(); // FIXME: Handle errors

        Ok((data, 0))
    }
}

/// Options to customize the encoding process.
#[derive(Clone)]
pub struct Options {
    pub inline_dictionaries: bool,

    /// If `true`, encode the tree structure in the file
    /// (tag names, string enums, booleans)
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_tags: bool,

    /// If `true`, encode the bools in the file
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_bools: bool,

    /// If `true`, encode numbers in the file.
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_numbers: bool,

    /// If `true`, encode list lengths in the file.
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_list_lengths: bool,

    /// If `true`, encode strings proper (i.e. not identifiers,
    /// not string enums) in the file.
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_strings: bool,

    /// If `true`, encode identifiers (i.e. not identifiers)
    /// in the file.
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_identifiers: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            inline_dictionaries: true,
            encode_tags: true,
            encode_bools: true,
            encode_strings: true,
            encode_identifiers: true,
            encode_numbers: true,
            encode_list_lengths: true,
        }
    }
}
impl Options {
    pub const KEYS : &'static [&'static str] = &[
        "dictionaries",
        "no-dictionaries",
        "tags",
        "no-tags",
        "strings",
        "no-strings",
        "numbers",
        "no-numbers",
        "bools",
        "no-bools",
    ];
    pub fn with_option(self, option: &str) -> Option<Self> {
        debug!(target: "multiarith", "Applying option {}", option);
        match option {
            "dictionaries" => {
                Some(Self {
                    inline_dictionaries: true,
                    .. self
                })
            }
            "no-dictionaries" => {
                Some(Self {
                    inline_dictionaries: false,
                    .. self
                })
            }
            "tags" => {
                Some(Self {
                    encode_tags: true,
                    .. self
                })
            }
            "no-tags" => {
                Some(Self {
                    encode_tags: false,
                    .. self
                })
            }
            "bools" => {
                Some(Self {
                    encode_bools: true,
                    .. self
                })
            }
            "no-bools" => {
                Some(Self {
                    encode_bools: false,
                    .. self
                })
            }
            "strings" => {
                Some(Self {
                    encode_strings: true,
                    .. self
                })
            }
            "no-strings" => {
                Some(Self {
                    encode_strings: false,
                    .. self
                })
            }
            "numbers" => {
                Some(Self {
                    encode_strings: true,
                    .. self
                })
            }
            "no-numbers" => {
                Some(Self {
                    encode_strings: false,
                    .. self
                })
            }
            _ => None
        }
    }
}