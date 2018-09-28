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
//! # Random notes on the format
//!
//! - We don't want too many dependencies between the dictionaries of two lazy
//!  functions, as we generally don't know in which order they will be loaded.
//!  The exception is if each lazy function has its own header and this header
//!  contains a dictionary, in which case we may easily load the headers and
//!  dictionaries in reading order. I guess we should do that.
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

pub mod model;
mod predict;
mod tree;
pub mod read;
pub mod write;

use self::predict::Symbol;
use self::tree::F64;

use std::rc::Rc;

use clap;
use range_encoding::CumulativeDistributionFrequency;

pub type ASTPath = tree::ASTPath;
pub type ScopePath = tree::ScopePath;

pub trait EncodingModel {
    fn tag_frequency_for_encoding(&mut self, tag: &tree::Tag, path: &ASTPath) -> Result<Symbol, ()>;
    fn bool_frequency_for_encoding(&mut self, value: &Option<bool>, path: &ASTPath) -> Result<Symbol, ()>;
    fn string_frequency_for_encoding(&mut self, string: &Option<Rc<String>>, path: &ASTPath) -> Result<Symbol, ()>;
    fn number_frequency_for_encoding(&mut self, number: &Option<F64>, path: &ASTPath) -> Result<Symbol, ()>;
    fn identifier_frequency_for_encoding(&mut self, string: &Rc<String>, scopes: &ScopePath) -> Result<Symbol, ()>;
    fn list_length_frequency_for_encoding(&mut self, value: &Option<u32>, path: &ASTPath) -> Result<Symbol, ()>;

    /// Utility: return the frequency information for a true/false value in which
    /// either value has the same frequency.
    fn iso_bit_frequency_for_encoding(&mut self, value: bool) -> Result<Symbol, ()>;
}

pub trait Model {
    fn encoding(&self, &tree::SharedTree) -> Box<EncodingModel>;
}


pub trait DecodingModel {
    // FIXME: We need to be able to handle
    // - 1. Case in which we have the CDF and can simply give it.
    // - 2. Case in which we have the CDF and can give it, and it will somehow be incremented.
    // - 3. Case in which we do not have the CDF yet, the decoder needs to give us one.
    fn tag_frequency_for_decoding(&mut self, path: &ASTPath) -> Option<&mut CumulativeDistributionFrequency>;
    fn init_tag_frequency_for_decoding(&mut self, path: &ASTPath, cdf: Vec<u32>);
}


/// Command-line management.

/// Command-line management.
pub struct FormatProvider;
impl ::FormatProvider for FormatProvider {
    fn subcommand<'a, 'b>(&self) -> clap::App<'a, 'b> {
        use clap::*;
        SubCommand::with_name("entropy")
            .about("(EXPERIMENTAL) Encode using entropy compression. This format should eventually produce very good compression ratio.")
            .arg(Arg::with_name("include-dictionaries")
                .help("Include the dictionaries")
                .long("dictionaries")
                .takes_value(true)
                .default_value("false")
                .validator(|s| s.parse::<bool>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid bool {}", e)))
                )
            .arg(Arg::with_name("encode-tags")
                .help("Encode tags")
                .long("tags")
                .takes_value(true)
                .default_value("true")
                .validator(|s| s.parse::<bool>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid bool {}", e)))
                )
            .arg(Arg::with_name("encode-bools")
                .help("Encode booleans")
                .long("bools")
                .takes_value(true)
                .default_value("true")
                .validator(|s| s.parse::<bool>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid bool {}", e)))
                )
            .arg(Arg::with_name("encode-strings")
                .help("Encode strings")
                .long("strings")
                .takes_value(true)
                .default_value("true")
                .validator(|s| s.parse::<bool>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid bool {}", e)))
                )
            .arg(Arg::with_name("encode-identifiers")
                .help("Encode identifiers")
                .long("identifiers")
                .takes_value(true)
                .default_value("true")
                .validator(|s| s.parse::<bool>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid bool {}", e)))
                )
            .arg(Arg::with_name("encode-list-lengths")
                .help("Encode list lengths")
                .long("list-lengths")
                .takes_value(true)
                .default_value("true")
                .validator(|s| s.parse::<bool>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid bool {}", e)))
                )
            .arg(Arg::with_name("encode-numbers")
                .help("Encode numbers")
                .long("numbers")
                .takes_value(true)
                .default_value("true")
                .validator(|s| s.parse::<bool>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid bool {}", e)))
                )
    }

    fn handle_subcommand(&self, matches: Option<&clap::ArgMatches>) -> Result<::Format, ::std::io::Error> {
        let as_bool = |name| {
            matches.unwrap() // We only call this if `matches.is_some()`.
                .value_of(name)
                .unwrap() // Already checked by App.
                .parse::<bool>()
                .unwrap() // Already checked by App.
        };
        let options = {
            if matches.is_some() {
                write::Options {
                    inline_dictionaries: as_bool("include-dictionaries"),
                    encode_tags: as_bool("encode-tags"),
                    encode_bools: as_bool("encode-bools"),
                    encode_strings: as_bool("encode-strings"),
                    encode_numbers: as_bool("encode-numbers"),
                    encode_identifiers: as_bool("encode-identifiers"),
                    encode_list_lengths: as_bool("encode-list-lengths"),
                }
            } else {
                write::Options::default()
            }
        };
        Ok(::Format::Entropy {
            options,
            model: Box::new(model::ExactModel)
        })
    }
}
