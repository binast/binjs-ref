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

mod predict;
mod tree;
pub mod write;

use self::predict::Symbol;
use self::tree::{ F64, Path };

use std::rc::Rc;

pub trait EncodingModel {
    fn tag_frequency_for_encoding(&mut self, tag: &tree::Tag, path: &Path<(tree::Tag, usize)>) -> Result<Symbol, ()>;
    fn bool_frequency_for_encoding(&mut self, value: &Option<bool>, path: &Path<(tree::Tag, usize)>) -> Result<Symbol, ()>;
    fn string_frequency_for_encoding(&mut self, string: &Option<Rc<String>>, path: &Path<(tree::Tag, usize)>) -> Result<Symbol, ()>;
    fn number_frequency_for_encoding(&mut self, number: &Option<F64>, path: &Path<(tree::Tag, usize)>) -> Result<Symbol, ()>;
    fn identifier_frequency_for_encoding(&mut self, string: &Rc<String>, scopes: &Path<tree::ScopeIndex>) -> Result<Symbol, ()>;
    fn list_length_frequency_for_encoding(&mut self, value: &Option<u32>, path: &Path<(tree::Tag, usize)>) -> Result<Symbol, ()>;

    /// Utility: return the frequency information for a true/false value in which
    /// either value has the same frequency.
    fn iso_bit_frequency_for_encoding(&mut self, value: bool) -> Result<Symbol, ()>;
}

pub trait Model {
    fn encoding(&self, &tree::SharedTree) -> Box<EncodingModel>;
}

pub struct ExactModel;

impl Model for ExactModel {
    fn encoding(&self, tree: &tree::SharedTree) -> Box<EncodingModel> {
        Box::new(write::ExactEncodingModel::new(tree))
    }
}

