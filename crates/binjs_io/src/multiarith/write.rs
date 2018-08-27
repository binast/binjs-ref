
use multiarith::{ ContextPredict, EncodingModel, F64, Label, Model, Path, PathPredict, ScopeIndex, SharedTree, SubTree, Symbol, Tag, Visitor, WalkTree };

use io::TokenWriter;
use ::TokenWriterError;
use util::GenericCounter;

use range_encoding::AlreadyEncountered;
use range_encoding::opus;

use std;
use std::cell::RefCell;
use std::rc::Rc;

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

    identifiers: ContextPredict<Option<ScopeIndex>, Rc<String>, T>,
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
            Label::Declare(Some(ref string)) | Label::LiteralReference(Some(ref string)) => {
                let scope = scopes.last()
                    .map(Clone::clone);
                let (_, entry) = self.identifiers.entry(scope, string);
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
    fn string_frequency_for_encoding(&mut self, string: &Option<Rc<String>>, path: &Path<(Tag, usize)>) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.strings, string, path)
    }
    fn tag_frequency_for_encoding(&mut self, tag: &Tag, path: &Path<(Tag, usize)>) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.tags, tag, path)
    }
    fn number_frequency_for_encoding(&mut self, number: &Option<F64>, path: &Path<(Tag, usize)>) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.numbers, number, path)
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
            numbers: PathPredict::new(1), // Fairly confident that a depth of 1 should be sufficient for numbers.
            identifiers: ContextPredict::new(),
        };

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
                numbers: instances.numbers.instances_to_probabilities(),
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
                let mut distribution = symbol.distribution.borrow_mut();
                if let AlreadyEncountered(false) = self.encoder.symbol(symbol.index, &mut *distribution)? {
                    warn!(target: "multiarith", "FIXME: Append definition of the current tag {:?} in {:?}", tag, path.len());
                }
            }
            Label::Number(ref number) => {
                let symbol = self.model.number_frequency_for_encoding(number, path)
                    .expect("Could not compute number frequency");
                if !self.options.encode_numbers {
                    return Ok(())
                }
                let mut distribution = symbol.distribution.borrow_mut();
                if let AlreadyEncountered(false) = self.encoder.symbol(symbol.index, &mut *distribution)? {
                    warn!(target: "multiarith", "FIXME: Append definition of the current number {:?} in {:?}", number, path.len());
                }
            }
            Label::String(ref string) => {
                let symbol = self.model.string_frequency_for_encoding(string, path)
                    .expect("Could not compute string frequency");
                if !self.options.encode_strings {
                    return Ok(())
                }
                let mut distribution = symbol.distribution.borrow_mut();
                if let AlreadyEncountered(false) = self.encoder.symbol(symbol.index, &mut *distribution)? {
                    // self.encoder.flush_symbols()?; // FIXME: Should we flush?
                    warn!(target: "multiarith", "FIXME: Append definition of the current string {:?} in {:?}", string, path.len());
                }
            }
            Label::Declare(Some(ref string)) | Label::LiteralReference(Some(ref string)) => {
                let symbol = self.model.identifier_frequency_for_encoding(string, scopes)
                    .expect("Could not compute identifier frequency");
                if !self.options.encode_identifiers {
                    return Ok(())
                }
                let mut distribution = symbol.distribution.borrow_mut();
                if let AlreadyEncountered(false) = self.encoder.symbol(symbol.index, &mut *distribution)? {
                    // self.encoder.flush_symbols()?; // FIXME: Should we flush?
                    warn!(target: "multiarith", "FIXME: Append definition of the current identifier {:?} in {:?}", string, path.len());
                }
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
        // FIXME: Once we start introducing dictionaries, we won't want to use strings anymore.
        let string = match value {
            None => "",
            Some(true) => "true",
            Some(false) => "false"
        };
        self.tagged_tuple(&string, &[])
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
    /// If `true`, encode the tree structure in the file
    /// (tag names, string enums, booleans)
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with file sizes.
    pub encode_tags: bool,

    /// If `true`, encode numbers in the file.
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with file sizes.
    pub encode_numbers: bool,

    /// If `true`, encode strings proper (i.e. not identifiers,
    /// not string enums) in the file.
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with file sizes.
    pub encode_strings: bool,

    /// If `true`, encode identifiers (i.e. not identifiers)
    /// in the file.
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with file sizes.
    pub encode_identifiers: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            encode_tags: true,
            encode_strings: true,
            encode_identifiers: true,
            encode_numbers: true,
        }
    }
}