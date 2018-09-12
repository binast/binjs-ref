use entropy::{ DecodingModel, EncodingModel, Model };
use entropy::predict::{ ContextPredict, PathPredict, Symbol };
use entropy::tree::{ EXPECTED_PATH_DEPTH, EXPECTED_SCOPE_DEPTH, ASTPath, F64, Label, ScopeIndex, ScopePath, SharedTree, Tag, Visitor, WalkTree };

use binjs_shared::ast::PathItem;

use std;
use std::rc::Rc;

pub struct ExactModel;

impl Model for ExactModel {
    fn encoding(&self, tree: &SharedTree) -> Box<EncodingModel> {
        Box::new(ExactEncodingModel::new(tree))
    }
}


/// An encoding model which starts by analyzing the full AST to determine
/// exact statistics.
pub struct ExactEncodingModel {
    probabilities: ExactEncodingModelData<Symbol>,
}
impl ExactEncodingModel {
    fn get_from_path<T>(predictor: &mut PathPredict<T, Symbol>, value: &T, path: &ASTPath) -> Result<Symbol, ()>
        where T: Eq + std::hash::Hash + Clone + std::fmt::Debug
    {
        let segment = predictor.get_mut(path, value)
            .ok_or(())?;
        let result = segment.clone();
        Ok(result)
    }
}
impl EncodingModel for ExactEncodingModel {
    fn string_frequency_for_encoding(&mut self, value: &Option<Rc<String>>, path: &ASTPath) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.strings, value, path)
    }
    fn tag_frequency_for_encoding(&mut self, value: &Tag, path: &ASTPath) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.tags, value, path)
    }
    fn bool_frequency_for_encoding(&mut self, value: &Option<bool>, path: &ASTPath) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.bools, value, path)
    }
    fn number_frequency_for_encoding(&mut self, value: &Option<F64>, path: &ASTPath) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.numbers, value, path)
    }
    fn list_length_frequency_for_encoding(&mut self, value: &Option<u32>, path: &ASTPath) -> Result<Symbol, ()> {
        Self::get_from_path(&mut self.probabilities.list_lengths, value, path)
    }
    fn iso_bit_frequency_for_encoding(&mut self, value: bool) -> Result<Symbol, ()> {
        let segment = self.probabilities.iso_bit_model.get_mut(&mut (), &value)
            .ok_or(())?;
        Ok(segment.clone())
    }
    fn identifier_frequency_for_encoding(&mut self, string: &Rc<String>, scopes: &ScopePath) -> Result<Symbol, ()> {
        let scope = scopes.get(0)
            .map(PathItem::interface)
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
            let mut entry = instances.iso_bit_model.entry((), &bit);
            entry.value.and_modify(|instances| {
                *instances += 1
            }).or_insert(1);
        }

        tree.walk(&mut instances,
            &mut ASTPath::with_capacity(EXPECTED_PATH_DEPTH),
            &mut ScopePath::with_capacity(EXPECTED_SCOPE_DEPTH))
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

pub struct ExactEncodingModelData<T> {
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
    fn enter_label(&mut self, label: &Label, path: &ASTPath, scopes: &ScopePath) -> Result<(), Self::Error> {
        match label {
            Label::Tag(ref tag) => {
                let mut entry = self.tags.entry(path, tag);
                entry.value.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            Label::String(ref string) => {
                let mut entry = self.strings.entry(path, string);
                entry.value.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            Label::Number(ref num) => {
                let mut entry = self.numbers.entry(path, num);
                entry.value.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            Label::List(ref len) => {
                let mut entry = self.list_lengths.entry(path, len);
                entry.value.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            Label::Declare(Some(ref string)) | Label::LiteralReference(Some(ref string)) => {
                let scope = scopes.get(0)
                    .map(PathItem::interface)
                    .map(Clone::clone);
                let mut entry = self.identifiers.entry(scope, string);
                entry.value.and_modify(|instances| {
                        *instances += 1
                    }).or_insert(1);
            }
            Label::Bool(ref value) => {
                let mut entry = self.bools.entry(path, value);
                entry.value.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            _ => {
                warn!(target: "entropy", "Skipping initialization of predictor for label {:?} (not implemented yet)", label);
            }
        }
        Ok(())
    }
}