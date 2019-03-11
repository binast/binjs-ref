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

use io::statistics::{Bytes, BytesAndInstances, Instances, PerUserExtensibleKind};

use std::cell::RefCell;
use std::rc::Rc;

/// From a set of macro definitions, derive a module with a set of matching `&'static str`.
///
/// Example:
/// ```rust,no_run
/// const_with_str {
///    const FOO: usize = 0;
///    mod my_strings;
/// }
/// ```
///
/// will produce
///
/// ```rust,no_run
///
/// const FOO: usize = 0;
/// mod my_strings {
///   const FOO: &'static str = "0";
/// }
/// ```
macro_rules! const_with_str {
    ( $(const $const_name: ident: $ty: ty = $init: expr;)* mod $mod_name: ident;) => {
        mod $mod_name {
            $(pub const $const_name: &'static str = stringify!($init);)*
        }
        $(const $const_name: $ty = $init;)*
    }
}

/// The number of indices to reserve to recall
/// recently used values in a content stream.
///
/// In issue #302, we have established that identifier_names
/// seems to benefit of a window length of 8, and others
/// a window length of 0.
///
/// The macro call generates a module `arg_as_str` holding the
/// string representations of the consts. Used for the `clap`
/// default args.
const_with_str! {
    const DEFAULT_WINDOW_LEN_FLOATS: usize = 0;
    const DEFAULT_WINDOW_LEN_UNSIGNED_LONGS: usize = 0;
    const DEFAULT_WINDOW_LEN_PROPERTY_KEYS: usize = 0;
    const DEFAULT_WINDOW_LEN_IDENTIFIER_NAMES: usize = 8;
    const DEFAULT_WINDOW_LEN_STRING_LITERALS: usize = 0;
    const DEFAULT_WINDOW_LEN_LIST_LENGTHS: usize = 0;

    mod arg_as_str;
}

#[derive(Clone)]
pub struct Options {
    /// The (shared) AST probability tables, generally shipped separately
    /// from the compressed files and used to predict the probability
    /// of a symbol occurring at a specific position in the AST.
    probability_tables: Dictionary<SymbolInfo>,

    /// Statistics obtained while writing: number of bytes written.
    /// If several files are written with the same options, we accumulate
    /// statistics.
    content_lengths: Rc<RefCell<PerUserExtensibleKind<Bytes>>>,

    /// Statistics obtained while writing: number of instances of each
    /// kind written. If several files are written with the same options,
    /// we accumulate statistics.
    content_instances: Rc<RefCell<PerUserExtensibleKind<Instances>>>,

    /// For each content section, the number of indices reserved to reference
    /// recently used values.
    content_window_len: PerUserExtensibleKind<usize>,

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
            content_lengths: Rc::new(RefCell::new(PerUserExtensibleKind::default())),
            content_instances: Rc::new(RefCell::new(PerUserExtensibleKind::default())),
            split_streams: false,
            content_window_len: PerUserExtensibleKind {
                floats: DEFAULT_WINDOW_LEN_FLOATS,
                unsigned_longs: DEFAULT_WINDOW_LEN_UNSIGNED_LONGS,
                property_keys: DEFAULT_WINDOW_LEN_PROPERTY_KEYS,
                identifier_names: DEFAULT_WINDOW_LEN_IDENTIFIER_NAMES,
                string_literals: DEFAULT_WINDOW_LEN_STRING_LITERALS,
                list_lengths: DEFAULT_WINDOW_LEN_LIST_LENGTHS,
            },
        }
    }

    /// Return the statistics as (number of instances, number of bytes).
    pub fn statistics_for_write(&self) -> PerUserExtensibleKind<BytesAndInstances> {
        let borrow_lengths = self.content_lengths.borrow();
        let borrow_instances = self.content_instances.borrow();
        PerUserExtensibleKind {
            floats: BytesAndInstances::new(borrow_lengths.floats, borrow_instances.floats),
            unsigned_longs: BytesAndInstances::new(
                borrow_lengths.unsigned_longs,
                borrow_instances.unsigned_longs,
            ),
            property_keys: BytesAndInstances::new(
                borrow_lengths.property_keys,
                borrow_instances.property_keys,
            ),
            identifier_names: BytesAndInstances::new(
                borrow_lengths.identifier_names,
                borrow_instances.identifier_names,
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

    /// Configure the `split-streams` option.
    ///
    /// If `value == true`, dump the individual streams for forensics purposes,
    /// otherwise don't.
    pub fn with_split_streams(&mut self, value: bool) -> &mut Self {
        self.split_streams = value;
        self
    }
}

/// Command-line management.
use clap;

pub struct FormatProvider;
impl ::FormatProvider for FormatProvider {
    fn subcommand<'a, 'b>(&self) -> clap::App<'a, 'b> {
        use clap::*;

        let validate_usize = |s: String| {
            usize::from_str_radix(&s, 10)
                .map(|_| ())
                .map_err(|e| format!("{}", e))
        };

        SubCommand::with_name("entropy")
            .about("(EXPERIMENTAL) Encode using entropy compression. This format produces a very good compression ratio in presence of a good dictionary.")
            .arg(Arg::with_name("dictionary")
                .help("(Recommended) path to external dictionary generated by binjs_generate_prediction_tables.")
                .long("dictionary")
                .takes_value(true)
                .required(false)
            )
            .arg(Arg::with_name("split-streams")
                .long("split-streams")
                .help("If specified, dump all streams to the disk, for forensics purposes.")
                .takes_value(false)
            )
            .arg(Arg::with_name("window-len-floats")
                .long("window-len-floats")
                .help("The length of the recall window for floats, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_FLOATS)
            )
            .arg(Arg::with_name("window-len-unsigned-longs")
                .long("window-len-unsigned-longs")
                .help("The length of the recall window for unsigned longs, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_UNSIGNED_LONGS)
            )
            .arg(Arg::with_name("window-len-property-keys")
                .long("window-len-property-keys")
                .help("The length of the recall window for property keys, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_PROPERTY_KEYS)
            )
            .arg(Arg::with_name("window-len-identifier-names")
                .long("window-len-identifier-names")
                .help("The length of the recall window for identifier names, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_IDENTIFIER_NAMES)
            )
            .arg(Arg::with_name("window-len-string-literals")
                .long("window-len-string-literals")
                .help("The length of the recall window for string literals, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_STRING_LITERALS)
            )
            .arg(Arg::with_name("window-len-list-lengths")
                .long("window-len-list-lengths")
                .help("The length of the recall window for list lengths, in indices.")
                .takes_value(true)
                .validator(validate_usize)
                .default_value(arg_as_str::DEFAULT_WINDOW_LEN_LIST_LENGTHS)
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

        let convert_usize = |key: &str| {
            usize::from_str_radix(&matches.value_of(key).unwrap(), 10)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, format!("{}", e)))
        };

        let content_window_len = PerUserExtensibleKind {
            floats: convert_usize("window-len-floats")?,
            unsigned_longs: convert_usize("window-len-unsigned-longs")?,
            property_keys: convert_usize("window-len-property-keys")?,
            identifier_names: convert_usize("window-len-identifier-names")?,
            string_literals: convert_usize("window-len-string-literals")?,
            list_lengths: convert_usize("window-len-list-lengths")?,
        };

        Ok(::Format::Entropy {
            options: Options {
                content_lengths: Rc::new(RefCell::new(PerUserExtensibleKind::default())),
                content_instances: Rc::new(RefCell::new(PerUserExtensibleKind::default())),
                split_streams,
                content_window_len,
                ..Options::new(spec, dictionary)
            },
        })
    }
}
