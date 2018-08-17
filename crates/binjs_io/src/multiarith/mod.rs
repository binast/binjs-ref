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
//! # Random nodes on the format
//!
//! - We don't want too many dependencies between the dictionaries of two lazy
//!  functions, as we generally don't know in which order they will be loaded.
//!  The exception is if each lazy function has its own header and this header
//!  contains a dictionary, in which case we may easily load the headers and
//!  dictionaries in reading order. I guess we should do that.
//!
//! # Format details // DEPRECATED!
//!
//! Magic Header:
//!  TBD
//!
//! Grammar Header:
//! - reference to an existing grammar + probability set; or
//! - start from nothing
//!
//! Parameters Header:
//!  packet_structure_len (number of bits to be read in a row)
//!  TBD
//!
//! Tree:
//!  Packet:
//!    - `packet_structure_len` bits, to be arithmetically decoded as a variable number
//!         of u32, based on known probability distribution.
//!         The mapping from u32 to the AST is heavily dependent on the context.
//!         The grammar determines the nature of the value:
//!         - tag;
//!         - list length;
//!         - string;
//!         - string enum;
//!         - number;
//!         - boolean;
//!         - variable declaration;
//!         - variable reference;
//!         - nullable variants.
//!
//!     - arbitrary length definition
//!
//! # Decoding and interpreting packet structure
//!
//! Decoding a packet structure depends on:
//! - context;
//! - probabilities, which are given by context.
//!
//! In many cases, we won't be able to **extract** the next number of a packet structure
//! until we have fully **interpreted** the previous numbers from the same packet structure,
//! which may in turn need us to have read and interpreted a **definition** that appears
//! after the packet structure.
//!
//! Conceptually, we have two streams:
//! - one stream of u32 (*tree stream*), which reads from `packet_structure_len` bits;
//! - one stream of *definitions*, which reads from the definition just after the `packet_structure_len` bits.
//!
//! Reading from those streams is interleaved, which may make things a bit complicated.
//!
//! # Extensibility
//!
//! While we can have a built-in initial dictionary, the list of symbols cannot be
//! fully hardcoded, as there is no guarantee that the decoder won't be designed
//! for a more recent version of the language, which may have different nodes/enums,
//! etc.
//!
//! # Decoding a number
//!
//! Depending on the nature of the value:
//! - boolean
//!   TreeStream: 0 => false (no definition needed)
//!   TreeStream: 1 => true (no definition needed)
//!
//! - tag in context C for which we have no definition
//!    DefStream: tag id (varnum)
//!     - if `tag id` is unknown
//!         DefStream: tag grammar definition (see below)
//!     - else
//!         DefStream: `probabilities id` (varnum?)
//!         - if `tag id::probabilities id` is unknown
//!           DefStream: tag probabilities definition(number of items matching `tag id`) (see below)
//!     now that tag has a definition, proceed
//!
//! - tag in context C for which we have a definition
//!    TreeStream: n => tag[n]
//!
//! - tag grammar definition
//!    DefStream: number of items (varnum)
//!    - for i in 0..number of items
//!       DefStream: name length (varnum)
//!       DefStream: tag name (WTF-8)
//!         may contain a special "null" tag name "", to represent null values
//!         may contain a special "escape" tag name "?", to force loading a new list of items
//!    DefStream: `probabilities id` (varnum?)
//!    DefStream: tag probabilities definition(number of items) (see below)
//!
//! // FIXME: We probably want to predefine the tags in a header.
//! // FIXME: Pass as argument a prepopulated probability distribution, generated from the library. By default, everything is set to 1.
//!
//! - tag probabilities definition(number of items)
//!   - for i in 0..number of items
//!     DefStream: relative number of instances of name `tag name[i]`, renormalized so that max == 255 (8 bits)
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
//!
//! FIXME: Wait, no, that doesn't work. If we want this to work, we first need
//! to deliver the probability table for everything. That's not what we want.
//!
//! Option: we may initialize the dictionary as follows:
//! - initial probability of a new symbol is 1.
//! - whenever we create a symbol, we followup with its actual probability
//! - the probability of new symbols decreases each time we add a symbol
//!
//!
//! ----- Initially, start with everything equi-likely. We'll add a predefined
//! and/or custom dictionary later.

mod bit;
pub mod write;

use util::Counter;

use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{ Hash, Hasher };
use std::rc::Rc;
use std;


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
    LiteralReference(Option<Rc<String>>),
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
        }
    }
}


#[derive(Clone, Debug)]
pub struct SubTree {
    label: Label,
    children: Vec<SharedTree>,
}
pub type SharedTree = Rc<RefCell<SubTree>>;

pub trait Visitor {
    type Error;
    fn enter_label(&mut self, _label: &Label, _path: &Path<(Tag, usize)>, _scopes: &Path<ScopeIndex>) -> Result<(), Self::Error> {
        Ok(())
    }
    fn exit_label(&mut self, _label: &Label, _path: &Path<(Tag, usize)>, _scopes: &Path<ScopeIndex>) -> Result<(), Self::Error> {
        Ok(())
    }
}
pub trait WalkTree {
    fn walk<V>(&self, visitor: &mut V, path: &mut Path<(Tag, usize)>, scopes: &mut Path<ScopeIndex>) -> Result<(), V::Error>
        where V: Visitor;
}

impl WalkTree for SubTree {
    fn walk<V>(&self, visitor: &mut V, path: &mut Path<(Tag, usize)>, scopes: &mut Path<ScopeIndex>) -> Result<(), V::Error>
        where V: Visitor
    {
        visitor.enter_label(&self.label, path, scopes)?;
        match self.label {
            Label::Tag(ref tag) => {
                for (index, child) in self.children.iter().enumerate() {
                    path.push((tag.clone(), index));
                    child.walk(visitor, path, scopes)?;
                    path.pop();
                }
            }
            Label::Scope(ref scope) => {
                scopes.push(scope.clone());
                for child in self.children.iter() {
                    child.walk(visitor, path, scopes)?;
                }
                scopes.pop();
            }
            _ => {
                for child in self.children.iter() {
                    child.walk(visitor, path, scopes)?;
                }
            }
        }
        visitor.exit_label(&self.label, path, scopes)?;
        Ok(())
    }
}

impl WalkTree for SharedTree {
    fn walk<V>(&self, visitor: &mut V, path: &mut Path<(Tag, usize)>, scopes: &mut Path<ScopeIndex>) -> Result<(), V::Error>
        where V: Visitor
    {
        self.borrow().walk(visitor, path, scopes)
    }
}

/// A trivial wrapping of f64 with Hash and Eq.
///
/// FIXME: Check whether it's still needed
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
pub struct ScopeIndex(usize);
impl Counter for ScopeIndex {
    fn internal_make(value: usize) -> Self {
        ScopeIndex(value)
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Path<T> where T: Clone {
    items: Vec<T>
}
impl<T> Path<T> where T: Clone {
    pub fn with_capacity(len: usize) -> Self {
        Path {
            items: Vec::with_capacity(len)
        }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn push(&mut self, value: T) {
        self.items.push(value);
    }

    pub fn pop(&mut self) {
        self.items.pop()
            .expect("Popping from an empty path");
    }

    pub fn last(&self) -> Option<&T> {
        self.items.last()
    }

    pub fn tail(&self, len: usize) -> Self {
        let start = if self.items.len() <= len {
            0
        } else {
            self.items.len() - len
        };
        let (_, tail) = &self.items[..].split_at(start);
        let tail = tail.iter()
            .cloned()
            .collect();
        Path {
            items: tail
        }
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
        self.items.iter()
    }
}

/// A generic predictor, associating a context and a key to a value.
pub struct ContextPredict<C, K, T> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    by_context: HashMap<C, HashMap<K, T>>,
}
impl<C, K, T> ContextPredict<C, K, T> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    pub fn new() -> Self {
        Self {
            by_context: HashMap::new()
        }
    }
    pub fn get_mut(&mut self, context: &C, key: &K) -> Option<&mut T> {
        let by_key = self.by_context.get_mut(context)?;
        by_key.get_mut(key)
    }

    pub fn entry(&mut self, context: C, key: &K) -> (usize, std::collections::hash_map::Entry<K, T>) {
        let by_key = self.by_context.entry(context)
            .or_insert_with(|| HashMap::new());
        (by_key.len(), by_key.entry(key.clone()))
    }
}
impl<C, K> ContextPredict<C, K, usize> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    /// Utility: convert a number of instances for each symbol into
    /// a probability distribution.
    pub fn instances_to_probabilities(mut self) -> ContextPredict<C, K, Segment> {
        let probabilities = self.by_context.drain()
            .map(|(path, mut by_value)| {
                let total_instances : usize = by_value.values()
                    .sum();
                let mut cursor = 0;
                let probability_by_value = by_value.drain()
                    .map(|(key, instances)| {
                        let low = cursor;
                        cursor += instances;
                        let segment = Segment {
                            low: low as u32,
                            length: instances as u32,
                            context_length: total_instances as u32,
                            needs_definition: true,
                        };
                        (key, segment)
                    })
                    .collect();
                assert_eq!(cursor, total_instances);
                (path, probability_by_value)
            })
            .collect();
        ContextPredict {
            by_context: probabilities
        }
    }
}


/// A predictor used to predict the probability of a symbol based on
/// its position in a tree. The predictor is typically customized to
/// limit the prediction depth, e.g. to 0 (don't use any context),
/// 1 (use only the parent + child index) or 2 (use parent +
/// grand-parent and both child indices).
pub struct PathPredict<K, T> where K: Eq + Hash + Clone {
    /// Depth to use for prediction.
    ///
    /// 0: no context
    /// 1: use parent
    /// 2: use parent + grand parent
    /// ...
    depth: usize,

    context_predict: ContextPredict<Path<(Tag, usize)>, K, T>,
}

impl<K> PathPredict<K, usize> where K: Eq + Hash + Clone {
    /// Utility: convert a number of instances for each symbol into
    /// a probability distribution.
    pub fn instances_to_probabilities(self) -> PathPredict<K, Segment> {
        PathPredict {
            depth: self.depth,
            context_predict: self.context_predict.instances_to_probabilities()
        }
    }
}

impl<K, T> PathPredict<K, T> where K: Eq + Hash + Clone + std::fmt::Debug {
    pub fn new(depth: usize) -> Self {
        PathPredict {
            depth,
            context_predict: ContextPredict::new(),
        }
    }

    pub fn get_mut(&mut self, path: &Path<(Tag, usize)>, key: &K) -> Option<&mut T> {
        self.context_predict.get_mut(&path.tail(self.depth), key)
    }

    pub fn entry(&mut self, path: &Path<(Tag, usize)>, key: &K) -> (usize, std::collections::hash_map::Entry<K, T>) {
        self.context_predict.entry(path.tail(self.depth), key)
    }
}


#[derive(Clone, Debug)]
pub struct Segment { // FIXME: Maybe we don't want `u32` but `u16` or `u8`.
    /// Low value for this segment.
    ///
    /// The probability of the segment is `length/context_length`.
    low:  u32,

    /// Length of the segment.
    ///
    /// MUST never be 0.
    length: u32,


    /// The highest possible value of `high` in this context.
    ///
    /// MUST be consistent across segments for the same context.
    ///
    /// MUST be greater or equal to `high`.
    ///
    /// MUST be > 0.
    context_length: u32,

    /// If `true`, this is the first occurrence of this symbol in the
    /// context, so a definition must be injected.
    needs_definition: bool,
}

pub trait EncodingModel {
    fn tag_frequency_for_encoding(&mut self, tag: &Tag, path: &Path<(Tag, usize)>) -> Result<Segment, ()>;
    fn string_frequency_for_encoding(&mut self, string: &Option<Rc<String>>, path: &Path<(Tag, usize)>) -> Result<Segment, ()>;
    fn identifier_frequency_for_encoding(&mut self, string: &Rc<String>, scopes: &Path<ScopeIndex>) -> Result<Segment, ()>;
}

pub trait Model {
    fn encoding(&self, &SharedTree) -> Box<EncodingModel>;
}

pub struct ExactModel;

impl Model for ExactModel {
    fn encoding(&self, tree: &SharedTree) -> Box<EncodingModel> {
        Box::new(write::ExactEncodingModel::new(tree))
    }
}

