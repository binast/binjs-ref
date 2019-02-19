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
//!
//! # General structure
//!
//! ```md
//! - Magic header
//! - `[[prelude]]` (this section contains the extension of dictionaries used by this file)
//! - the following streams may appear in any order and are all optional
//!    - (TBD)
//! - `[[content]]` (this section contains the content streams, which represent user-extensible values)
//!    - (TBD)
//! - `[[main]]` (this section contains the main stream, encoded with entropy)
//!    - (TBD)
//! ```

mod baseline;
pub mod dictionary;

/// Reading a compressed stream.
pub mod read;

/// Tools shared by `read` and `write`.
pub mod rw;

/// Misc convenience utilities.
mod util;

/// Writing to a compressed stream.
pub mod write;

mod predict;
pub mod probabilities;

use self::dictionary::Dictionary;
use self::probabilities::SymbolInfo;

use io::statistics::{Bytes, BytesAndInstances, ContentInfo, Instances};

use std::cell::RefCell;
use std::rc::Rc;

/// The number of indices to reserve to recall
/// recently used values in a content stream.
///
/// In issue #302, we have established that identifier_names
/// seems to benefit of a window length of 8, and others
/// a window length of 0.
const DEFAULT_WINDOW_LEN_BOOLS: usize = 0;
const DEFAULT_WINDOW_LEN_FLOATS: usize = 0;
const DEFAULT_WINDOW_LEN_UNSIGNED_LONGS: usize = 0;
const DEFAULT_WINDOW_LEN_STRING_ENUMS: usize = 0;
const DEFAULT_WINDOW_LEN_PROPERTY_KEYS: usize = 0;
const DEFAULT_WINDOW_LEN_IDENTIFIER_NAMES: usize = 8;
const DEFAULT_WINDOW_LEN_INTERFACE_NAMES: usize = 0;
const DEFAULT_WINDOW_LEN_STRING_LITERALS: usize = 0;
const DEFAULT_WINDOW_LEN_LIST_LENGTHS: usize = 0;

#[derive(Clone)]
pub struct Options {
    /// The (shared) AST probability tables, generally shipped separately
    /// from the compressed files and used to predict the probability
    /// of a symbol occurring at a specific position in the AST.
    probability_tables: Dictionary<SymbolInfo>,

    /// Statistics obtained while writing: number of bytes written.
    /// If several files are written with the same options, we accumulate
    /// statistics.
    content_lengths: Rc<RefCell<ContentInfo<Bytes>>>,

    /// Statistics obtained while writing: number of instances of each
    /// kind written. If several files are written with the same options,
    /// we accumulate statistics.
    content_instances: Rc<RefCell<ContentInfo<Instances>>>,

    /// For each content section, the number of indices reserved to reference
    /// recently used values.
    content_window_len: ContentInfo<usize>,

    /// If `true`, when compressing a file, also write streams to separate files,
    /// for analysis purposes.
    split_streams: bool,
}
impl Options {
    pub fn new(spec: &binjs_meta::spec::Spec, dictionary: Option<Dictionary<Instances>>) -> Self {
        use entropy::probabilities::InstancesToProbabilities;
        let baseline = baseline::build(spec);
        let probability_tables = match dictionary {
            None => baseline,
            Some(dictionary) => dictionary.with_grammar_fallback(baseline),
        };
        Options {
            probability_tables: probability_tables.instances_to_probabilities("dictionary"),
            content_lengths: Rc::new(RefCell::new(ContentInfo::default())),
            content_instances: Rc::new(RefCell::new(ContentInfo::default())),
            split_streams: false,
            content_window_len: ContentInfo {
                bools: DEFAULT_WINDOW_LEN_BOOLS,
                floats: DEFAULT_WINDOW_LEN_FLOATS,
                unsigned_longs: DEFAULT_WINDOW_LEN_UNSIGNED_LONGS,
                string_enums: DEFAULT_WINDOW_LEN_STRING_ENUMS,
                property_keys: DEFAULT_WINDOW_LEN_PROPERTY_KEYS,
                identifier_names: DEFAULT_WINDOW_LEN_IDENTIFIER_NAMES,
                interface_names: DEFAULT_WINDOW_LEN_INTERFACE_NAMES,
                string_literals: DEFAULT_WINDOW_LEN_STRING_LITERALS,
                list_lengths: DEFAULT_WINDOW_LEN_LIST_LENGTHS,
            },
        }
    }

    /// Return the statistics as (number of instances, number of bytes).
    pub fn statistics_for_write(&self) -> ContentInfo<BytesAndInstances> {
        let borrow_lengths = self.content_lengths.borrow();
        let borrow_instances = self.content_instances.borrow();
        ContentInfo {
            bools: BytesAndInstances::new(borrow_lengths.bools, borrow_instances.bools),
            floats: BytesAndInstances::new(borrow_lengths.floats, borrow_instances.floats),
            unsigned_longs: BytesAndInstances::new(
                borrow_lengths.unsigned_longs,
                borrow_instances.unsigned_longs,
            ),
            string_enums: BytesAndInstances::new(
                borrow_lengths.string_enums,
                borrow_instances.string_enums,
            ),
            property_keys: BytesAndInstances::new(
                borrow_lengths.property_keys,
                borrow_instances.property_keys,
            ),
            identifier_names: BytesAndInstances::new(
                borrow_lengths.identifier_names,
                borrow_instances.identifier_names,
            ),
            interface_names: BytesAndInstances::new(
                borrow_lengths.interface_names,
                borrow_instances.interface_names,
            ),
            string_literals: BytesAndInstances::new(
                borrow_lengths.string_literals,
                borrow_instances.string_literals,
            ),
            list_lengths: BytesAndInstances::new(
                borrow_lengths.list_lengths,
                borrow_instances.list_lengths,
            ),
        }
    }
}

/// Command-line management.
use clap;

pub struct FormatProvider;
impl ::FormatProvider for FormatProvider {
    fn subcommand<'a, 'b>(&self) -> clap::App<'a, 'b> {
        use clap::*;
        SubCommand::with_name("entropy")
            .about("(EXPERIMENTAL) Encode using entropy compression. This format should eventually produce very good compression ratio.")
            .arg(Arg::with_name("dictionary")
                .help("Path to external probability tables dictionary (generated by binjs_generate_prediction_tables)")
                .long("dictionary")
                .takes_value(true)
                .required(false)
            )
            .arg(Arg::with_name("path-depth")
                .long("path-depth")
                .takes_value(true)
                .default_value("3")
            )
            .arg(Arg::with_name("split-streams")
                .long("split-streams")
                .takes_value(false)
            )
    }

    fn handle_subcommand(
        &self,
        spec: &binjs_meta::spec::Spec,
        matches: Option<&clap::ArgMatches>,
    ) -> Result<::Format, ::std::io::Error> {
        use bincode;

        let matches = matches.unwrap();

        let dictionary = match matches.value_of("dictionary") {
            None => None,
            Some(path) => {
                // Load a dictionary from disk.
                let source = std::fs::File::open(&path).expect("Could not open dictionary");
                let surface_dictionary: Dictionary<Instances> =
                    bincode::deserialize_from(source).expect("Could not decode dictionary");
                Some(surface_dictionary)
            }
        };

        let split_streams = matches.is_present("split-streams");

        Ok(::Format::Entropy {
            options: Options {
                content_lengths: Rc::new(RefCell::new(ContentInfo::default())),
                content_instances: Rc::new(RefCell::new(ContentInfo::default())),
                split_streams,
                ..Options::new(spec, dictionary)
            },
        })
    }
}
