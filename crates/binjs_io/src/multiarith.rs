//! A format in which we adopt an arithmetic encoding per nature.
//!
//! # Overview
//!
//! The general idea of arithmetic coding is that we can achieve a high
//! density of information by encoding a sequence of symbols as a single
//! rational number in [0, 1), with the actual number being determined
//! from the probability of encountering each successive symbol in a
//! given context.
//!
//! For instance, consider `BinaryOperation(op, left, right)` in a context C.
//! For this example, let's assume that we are starting from the full segment
//! [0, 1). In context C, we may encounter a number of subtrees labelled S_1,
//! S_2, ..., S_n with probabilities p(C, S_1), p(C, S_2), ..., p(C, S_n).
//! For simplicity, let's assume that label `BinaryOperation` is S_1
//! (so, with probability p(C, S_1))
//! Any number in [0, p(C, S_1)) may be used to represent `BinaryOperation`.
//!
//! Now, we proceed to encode `op` in a context C + `BinaryOperation`. Again,
//! we have a number of possible values for `op`: op_1, ... op_n'. If `op`
//! is `op_1`, any number of [0, p(C, S_1) * P(C + BinaryOperation, op_1))
//! may be used to represent `op`.
//!
//! Similarly, we'll encode `left` and `right` in context C + `BinaryOperation`.
//!
//!
//! # Extensibility
//!
//! While we can have a built-in initial dictionary, the list of symbols cannot be
//! fully hardcoded, as there is no guarantee that the decoder won't be designed
//! for a more recent version of the language, which may have different nodes/enums,
//! etc.
//!
//!
//!
//! # Encoding
//!
//! Pass 1: walk the tree, compute the probabilities for each symbol.
//! Pass 2: use probabilities to actually encode the symbols.
//!   - Whenever we encounter a symbol that we have never seen in a given context,
//!     the symbol is followed by its definition as soon as possible.
//!
//! FIXME: Some symbols will need to be defined more than once. Need to estimate
//! how often this appears.

use io::TokenWriter;
use ::{ DictionaryPlacement, CompressionTarget, TokenWriterError };
use util:: { Counter, GenericCounter };

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{ Hash, Hasher };
use std::io::Write;
use std::rc::Rc;

use itertools::Itertools;
use vec_map::VecMap;

struct BitWriter; // Placeholder

enum Appearance {
    /// This is the first time the symbol appears, so it must be defined.
    First,
    /// The symbol has already appeared at least once, no need to redefine it.
    Next
}

/// Instructions for serializing a symbol.
struct Symbol {
    /// The index for this symbol.
    ///
    /// For instance, in
    /// ```idl
    /// enum VariableDeclarationKind {
    ///    "var",
    ///    "let",
    ///    "const"
    /// };
    /// ```
    ///
    /// we may decide that "var" has index 0, "let" has index 1, "const" has index 2.
    ///
    /// Note that, since `VariableDeclarationKind` may appear in several context, there
    /// may be several indices for "var" depending on the context. For instance, a
    /// different numbering in `ForInOfBinding` and in `VariableDeclaration`.
    ///
    /// FIXME: How do we make this future-proof? If the order of values changes in
    /// the `enum` or if we add a new value in the middle? Or should we forbid this?
    index: usize,
    instances: usize,
    low: usize,
    high: usize,
}

/*
trait Model {
    type Context;
    type Label;
    fn get_frequency(&mut self, context: &Self::Context, label: &Self::Label) -> Result<Serial, ()>;
}

struct Compressor<M> where M: Model {
    model: M,
    sink: BitWriter,
}
impl<M> Compressor<M> where M: Model {
    /// Change internal state to introduce a new symbol.
    ///
    /// This symbol will eventually be written to the sink.
    ///
    /// `Ok(true)` if this is the first time the symbol is written
    /// and we need to call `add_definition` to define it.
    fn compress(&mut self, context: &M::Context, label: &M::Label) -> Result<bool, std::io::Error> {
        unimplemented!()
    }
    fn add_definition(&mut self, data: &[u8]) -> Result<(), std::io::Error> {
        unimplemented!()
    }
    fn flush(&mut self) -> Result<(), std::io::Error> {
        unimplemented!()
    }
    fn done(self) -> (M, BitWriter) {
        unimplemented!()
    }
}
*/

#[derive(Clone, Debug)]
pub struct SubTree {
    label: Label,
    children: Vec<SharedTree>,
}
pub type SharedTree = Rc<RefCell<SubTree>>;

#[derive(Clone, Copy, Debug)]
enum Direction {
    Enter,
    Exit
}

struct PerCategory<T> {
    tags: T,
}

struct Context<'a> {
    tree: &'a SharedTree,
    parent: Option<(&'a SharedTree, usize)>,
}

/// One-level prediction, using the label of the parent and the index of the child.
#[derive(Default)]
struct Predict1<K, T> where K: Eq + Hash {
    by_parent: HashMap<Option<Tag> /* parent */, VecMap< /* child index */ HashMap<K /* child */, T>>>,
}
struct Compressor {
    tags: Predict1<Tag, (Symbol, RefCell<Appearance>)>,
}
impl Compressor {
    fn compute_instances(predict_1: &mut Predict1<Tag, /* instances */ usize>, subtree: &SharedTree, parent: Option<(&Tag, usize)>)
    {
        use std::collections::hash_map::Entry::*;
        let borrow = subtree.borrow();
        match borrow.label {
            Label::Tag(ref tag) => {
                let mut by_index = match parent {
                    None => {
                        let mut by_parent = predict_1.by_parent.entry(None)
                            .or_insert_with(|| VecMap::with_capacity(1));
                        by_parent.entry(0)
                            .or_insert_with(|| HashMap::new())
                    }
                    Some((parent, index)) => {
                        let mut by_parent = predict_1.by_parent.entry(Some(parent.clone()))
                            .or_insert_with(|| VecMap::with_capacity(5));
                        by_parent.entry(index)
                            .or_insert_with(|| HashMap::new())
                    }
                };
                let symbols = by_index.len();
                by_index.entry(tag.clone())
                    .and_modify(|instances| {
                        *instances += 1
                    })
                    .or_insert(1);

            }
            _ => {
                warn!(target: "multiarith", "Skipping initialization of predictor for label {:?} (not implemented yet)", borrow.label);
            }
        }
        // Recur towards children.
        match borrow.label {
            Label::Tag(ref tag) => {
                for (index, child) in borrow.children.iter().enumerate() {
                    Self::compute_instances(predict_1, child, Some((tag, index)));
                }
            }
            _ => {
                for (index, child) in borrow.children.iter().enumerate() {
                    Self::compute_instances(predict_1, child, parent);
                }
            }
        }
    }
    fn new(tree: &SharedTree) -> Self {
        let mut predict_instances_1 = Predict1::default();
        // Initialize number of instances.
        Self::compute_instances(&mut predict_instances_1, tree, None);

        // Deduce probabilities.
        let probabilities = predict_instances_1.by_parent.drain()
            .map(|(parent, mut by_child_index)| {
                let by_child_index = by_child_index.drain()
                    .map(|(child_index, mut by_symbol)| {
                        let number_of_symbols = by_symbol.len();
                        let total_instances : usize = by_symbol.values()
                            .sum();
                        let mut cursor = 0;
                        let by_symbol = by_symbol.drain()
                            .enumerate()
                            .map(|(index, (tag, instances))| {
                                let low = cursor;
                                cursor += instances;
                                let high = cursor;
                                let symbol = Symbol {
                                    low,
                                    high,
                                    instances,
                                    index
                                };
                                (tag, (symbol, RefCell::new(Appearance::First)))
                            })
                            .collect();
                        (child_index, by_symbol)
                    })
                    .collect();
                (parent, by_child_index)
            }).collect();
        Compressor {
            tags: Predict1 {
                by_parent: probabilities
            }
        }
    }
    fn compress(&mut self, subtree: &SharedTree, parent: Option<(&Tag, usize)>) -> Result<(), std::io::Error> {
        let borrow = subtree.borrow();
        match borrow.label {
            Label::Tag(ref tag) => {
                let (parent, index) = match parent {
                    None => (None, 0),
                    Some((parent, index)) => (Some(parent.clone()), index)
                };
                let (ref symbol, ref appearance) = self.tags.by_parent.get(&parent)
                    .expect("Could not find parent")
                    .get(index)
                    .expect("Could not find index")
                    .get(tag)
                    .expect("Could not find tag");
                unimplemented!("FIXME: We now have the probability of `(subtree, parent)`. Use it to refine the current segment");
                let appearance = appearance.borrow_mut();
                if let Appearance::First = *appearance {
                    unimplemented!("FIXME: Add definition of the current label");
                }
            }
            _ => {
                warn!(target: "multiarith", "Skipping serialization of label {:?} (not implemented yet)", borrow.label);
            }
        }
        // Recur towards children.
        match borrow.label {
            Label::Tag(ref tag) => {
                for (index, child) in borrow.children.iter().enumerate() {
                    self.compress(child, Some((tag, index)))?;
                }
            }
            _ => {
                for (index, child) in borrow.children.iter().enumerate() {
                    self.compress(child, parent)?;
                }
            }
        }
        Ok(())
    }
}

impl SubTree {
    fn with_labels<F: FnMut(&Label)>(&self, f: &mut F) {
        f(&self.label);
        for child in &self.children {
            child.borrow().with_labels(f);
        }
    }
    fn with_labels_mut<F: FnMut(Direction, &mut Label)>(&mut self, f: &mut F) {
        f(Direction::Enter, &mut self.label);
        for child in &self.children {
            child.borrow_mut().with_labels_mut(f);
        }
        f(Direction::Exit, &mut self.label);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ScopeIndex(usize);
impl Counter for ScopeIndex {
    fn internal_make(value: usize) -> Self {
        ScopeIndex(value)
    }
}

/// A trivial wrapping of f64 with Hash and Eq.
#[derive(Clone, Debug, PartialEq, PartialOrd, Copy)]
pub struct F64(pub f64);
impl Eq for F64 { } // Not strictly true.
impl Ord for F64 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
} // Not strictly true.
impl Hash for F64 {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.0.to_bits().hash(state)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Tag(Rc<String>);
impl Default for Tag {
    fn default() -> Tag {
        Self::new("")
    }
}
impl Tag {
    fn new(s: &str) -> Self {
        Tag(Rc::new(s.to_string()))
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord)]
pub enum Label {
    String(Option<Rc<String>>),
    Number(Option<F64>),
    Bool(Option<bool>),
    List(Option<u32>),
    Tag(Tag),
    /// Scope. Any `Declare` within the `Scope`
    /// stays in the `Scope`.
    Scope(ScopeIndex),
    /// Declare a variable throughout the current `Scope`.
    Declare(Option<Rc<String>>),
    /// Reference a variable throughout the current `Scope`.
    ///
    /// Initially entered as `LiteralReference`, then processed to a `NumberedReference`.
    LiteralReference(Option<Rc<String>>),
    NumberedReference(Option<u32>),
}

impl Eq for Label { /* Yes, it's probably not entirely true for f64 */ }
impl Hash for Label { /* Again, not entirely true for f64 */
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        use self::Label::*;
        match *self {
            String(ref s) => s.hash(state),
            Bool(ref b) => b.hash(state),
            List(ref l) => l.hash(state),
            Tag(ref s) => s.hash(state),
            Number(ref num) => num.map(|x| f64::to_bits(x.0)).hash(state), // Not really true for a f64.
            Scope(ref s) => s.hash(state),
            Declare(ref d) => d.hash(state),
            LiteralReference(ref r) => r.hash(state),
            NumberedReference(ref r) => r.hash(state),
        }
    }
}


pub struct TreeTokenWriter {
    root: SharedTree,
    scope_counter: GenericCounter<ScopeIndex>,
}
impl TreeTokenWriter {
    pub fn new() -> Self {
        Self {
            scope_counter: GenericCounter::new(),
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

    fn number_references(&mut self) -> Result<SharedTree, TokenWriterError> {
        // Undeclared references
        let top = Rc::new(RefCell::new(vec![]));
        let stack = Rc::new(RefCell::new(vec![top.clone()]));
        self.root.borrow_mut().with_labels_mut(&mut |direction, label| {
            let rewrite = match (direction, &label) {
                (Direction::Enter, Label::Scope(_)) => {
                    let mut borrow_stack = stack.borrow_mut();
                    borrow_stack.push(Rc::new(RefCell::new(vec![])));
                    None
                }
                (Direction::Exit, Label::Scope(_)) => {
                    let mut borrow_stack = stack.borrow_mut();
                    borrow_stack.pop();
                    None
                }
                (Direction::Enter, Label::Declare(Some(ref s))) => {
                    let borrow_stack = stack.borrow();
                    let mut borrow_frame = borrow_stack.last()
                        .unwrap()
                        .borrow_mut();
                    borrow_frame
                        .push(s.clone());
                    None
                }
                (Direction::Enter, Label::LiteralReference(None)) => {
                    Some(Label::NumberedReference(None))
                }
                (Direction::Enter, Label::LiteralReference(Some(ref s))) => {
                    let mut depth = 0;
                    let mut found = None;
                    {
                        let borrow_stack = stack.borrow();
                        'find_in_stack: for frame in borrow_stack.iter().rev() {
                            let borrow_frame = frame.borrow();
                            if let Some(index) = borrow_frame.iter()
                                .position(|name| name == s)
                            {
                                found = Some(index);
                                break 'find_in_stack;
                            } else {
                                depth += borrow_frame.len()
                            }
                        }
                    }
                    let index = match found {
                        Some(found) => found,
                        None => {
                            let mut borrow_top = top.borrow_mut();
                            borrow_top.push(s.clone());
                            borrow_top.len()
                        }
                    };
                    Some(Label::NumberedReference(Some((depth + index) as u32)))
                }
                _ => None,
            };
            if let Some(rewrite) = rewrite {
                *label = rewrite;
            }
        });

        // Now declare all undeclared variables.
        let root = self.root.clone();
        let top = top.borrow()
            .iter()
            .map(|name| Rc::new(RefCell::new(SubTree {
                label: Label::Declare(Some(name.clone())),
                children: vec![]
            })))
            .collect();
        let declared_undeclared_variables = self.new_tree(SubTree {
            label: Label::Tag(Tag::new("_undeclared_variables")),
            children: top
        })?;
        let scope_index = self.scope_counter.next();
        self.new_tree(SubTree {
            label: Label::Scope(scope_index),
            children: vec![
                declared_undeclared_variables,
                root
            ]
        })
    }
}
impl TokenWriter for TreeTokenWriter {
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
        use labels:: { ExplicitIndexLabeler, /*MRULabeler,*/ ParentPredictionFrequencyLabeler, RawLabeler };
        self.number_references()?;

        unimplemented!()
    }
}
