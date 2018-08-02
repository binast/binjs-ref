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
//! # Format // DEPRECATED!
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
//!    `packet_structure_len` bits, to be arithmetically decoded as a variable number
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
//!     arbitrary length definition
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

mod write;

use util::Counter;

use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{ Hash, Hasher };
use std::rc::Rc;
use std;

use vec_map::VecMap;


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


#[derive(Clone, Debug)]
pub struct SubTree {
    label: Label,
    children: Vec<SharedTree>,
}
pub type SharedTree = Rc<RefCell<SubTree>>;

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

/// One-level prediction, using the label of the parent and the index of the child.
#[derive(Default)]
pub struct Predict1<K, T> where K: Eq + Hash {
    pub by_parent: HashMap<Option<Tag> /* parent */, VecMap< /* child index */ HashMap<K /* child */, T>>>,
}
