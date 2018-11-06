#![feature(box_patterns)]
#![feature(vec_resize_default)]

extern crate bincode; // Used to store dictionaries. This is a temporary format.
extern crate binjs_shared;

extern crate brotli;
extern crate clap;
extern crate flate2;
extern crate itertools;
extern crate lzw;
#[macro_use]
extern crate log;
#[macro_use]
extern crate num_alias;
extern crate rand;
extern crate range_encoding;
#[macro_use]
extern crate serde_derive;
extern crate serde;

extern crate vec_map;
extern crate xml as xml_rs;

use std::fmt::{ Debug, Formatter };
use std::cell::RefCell;
use std::rc::Rc;

use rand::{ Rand, Rng };

pub use bytes::compress::Compression;

#[derive(Debug)]
pub enum TokenWriterError {
    InvalidOffsetField,
    NotInDictionary(String),
    WriteError(std::io::Error),
}

#[derive(Debug)]
pub enum TokenReaderError {
    NotInDictionary(String),
    ReadError(std::io::Error),
    BadLength { expected: usize, got: usize },
    BadHeader,
    BadCompression(std::io::Error),
    EndOffsetError {
        start: u64,
        expected: u64,
        found: u64,
        description: String,
    },
    BadStringIndex(u32),
    InvalidValue,
    BadKindIndex(u32),
    Encoding(std::string::FromUtf8Error),
    EmptyNodeName,
    EmptyFieldName,
    EmptyVariant,
    EmptyBool,
    EmptyString,
    EmptyList,
    BadEnumVariant,
}
impl TokenReaderError {
    pub fn invalid_value<T: std::fmt::Debug>(value: &T) -> Self {
        error!(target: "token_reader", "InvalidValue {:?}", value);
        TokenReaderError::InvalidValue
    }
}


/// Byte-level utilities for writing token readers/writers.
pub mod bytes;

/// Definition of TokenReader/TokenWriter traits.
#[macro_use]
mod io;
pub use io::*;

#[cfg(multistream)]
pub mod labels;

/// A simple implementation of TokenReader/TokenWriter,
/// designed specifically to help debug implementations
/// of grammar encoders/decoders.
pub mod simple;

/// An optimization of TokenReader/TokenWriter,
/// designed to minimize the size of the file.
pub mod multipart;

/// An encoding using entropy coding.
pub mod entropy;


#[cfg(multistream)]
pub mod multistream;

/// A tree comperssion mechanism.
#[cfg(multistream)]
pub mod repair;
// pub mod repair2;

pub mod xml;

mod util;

mod escaped_wtf8;

const ADVANCED_COMMAND: &str = "advanced";

/// A strategy for placing the dictionary.
#[derive(Clone, Debug)]
pub enum DictionaryPlacement {
    /// Place the entire dictionary before the contents.
    Header,

    /// Inline the dictionary. The first instance of a node is followed
    /// immediately by its definition.
    Inline
}

#[cfg(multistream)]
/// A strategy for numbering nodes, labels, strings, ...
#[derive(Clone, Debug)]
pub enum NumberingStrategy {
    /// Relative to the most recently used.
    ///
    /// Using twice the same value in a row will mean that the second index will be 0.
    MRU,

    /// Use global frequencey.
    ///
    /// The most common value will be numbered 0, the second most common will be numbered
    /// 1, etc.
    GlobalFrequency,

    Prediction,
}

#[derive(Clone, Debug)]
enum Compressing {
    Uncompressed(Rc<RefCell<Vec<u8>>>),
    Compressed {
        data: Rc<Vec<u8>>,
        result: bytes::compress::CompressionResult,
    },
}
/// Instructions for a single section (grammar, strings, tree, ...)
#[derive(Clone)]
pub struct CompressionTarget {
    data: Compressing,
    format: bytes::compress::Compression,
}
impl Debug for CompressionTarget {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        self.format.fmt(f)
    }
}
impl CompressionTarget {
    pub fn new(format: bytes::compress::Compression) -> Self {
        Self {
            data: Compressing::Uncompressed(Rc::new(RefCell::new(vec![]))),
            format,
        }
    }
    pub fn done(&mut self) -> std::result::Result<(Rc<Vec<u8>>, bytes::compress::CompressionResult), std::io::Error> {
        let (data, result) = match self.data {
            Compressing::Compressed { ref result, ref data } => return Ok((data.clone(), result.clone())),
            Compressing::Uncompressed(ref data) => {
                let mut buf = vec![];
                let result = self.format.compress(&data.borrow().as_ref(), &mut buf)?;
                (Rc::new(buf), result)
            }
        };
        self.data = Compressing::Compressed {
            result: result.clone(),
            data: data.clone(),
        };
        Ok((data, result))
    }
    pub fn reset(&mut self) {
        self.data = Compressing::Uncompressed(Rc::new(RefCell::new(vec![])));
    }
    pub fn len(&self) -> usize {
        match self.data {
            Compressing::Uncompressed(ref data) => data.borrow().len(),
            Compressing::Compressed { ref result, .. } => result.before_bytes,
        }
    }
}

/// Support picking a random compression target.
/// Used for testing.
impl Rand for CompressionTarget {
    fn rand<R: Rng>(rng: &mut R) -> Self {
        Self::new(Compression::rand(rng))
    }
}
impl std::io::Write for CompressionTarget {
    fn write(&mut self, data: &[u8]) -> std::result::Result<usize, std::io::Error> {
        match self.data {
            Compressing::Uncompressed(ref buf) => {
                let mut borrow = buf.borrow_mut();
                borrow.extend_from_slice(data);
                Ok(data.len())
            },
            _ => panic!("Attempting to add data to a CompressionTarget that is already closed")
        }
    }
    fn flush(&mut self) -> std::result::Result<(), std::io::Error> {
        Ok(())
    }
}
impl Default for CompressionTarget {
    fn default() -> Self {
        Self::new(bytes::compress::Compression::Identity)
    }
}

/// Command-line management for a format
pub trait FormatProvider {
    /// Specify command-line arguments for this format.
    fn subcommand<'a, 'b>(&self) -> clap::App<'a, 'b>;

    /// Produce a format given command-line argument matches.
    fn handle_subcommand(&self, matches: Option<&clap::ArgMatches>) -> Result<::Format, ::std::io::Error>;
}

/// All the formats available for encoding/decoding.
pub enum Format {
    Simple {
        stats: Rc<RefCell<simple::Statistics>>
    },
    Multipart {
        targets: multipart::Targets,
        stats: Rc<RefCell<multipart::Statistics>>
    },
    #[cfg(multistream)]
    TreeRePair {
        options: repair::Options,
    },
    XML,
    #[cfg(multistream)]
    MultiStream {
        options: multistream::Options,
        targets: multistream::Targets,
    },
    Entropy {
        options: entropy::Options,
    }
}

/// Support picking a random format.
/// Used for testing.
impl Rand for Format {
    fn rand<'a, R: Rng>(rng: &'a mut R) -> Self {
        let generators = [
            Rc::new(|_| Self::simple()) as Rc<Fn(&'a mut R) -> Format>,
            Rc::new(|rng| {
                use multipart::{ Statistics, Targets };
                let stats = Rc::new(RefCell::new(Statistics::default()
                    .with_source_bytes(0)));

                Format::Multipart {
                    targets: Targets {
                        strings_table: CompressionTarget::rand(rng),
                        grammar_table: CompressionTarget::rand(rng),
                        tree: CompressionTarget::rand(rng),
                    },
                    stats
                }
            }),
            Rc::new(|_| Format::XML),
            #[cfg(multistream)]
            Rc::new(|_| Format::TreeRePair {
                    options: repair::Options {
                        max_rank: None,
                        dictionary_placement: DictionaryPlacement::Header,
                        numbering_strategy: NumberingStrategy::GlobalFrequency,
                    }
                }),
            #[cfg(multistream)]
            Rc::new(|rng| Format::MultiStream {
                options: multistream::Options::rand(rng),
                targets: multistream::Targets::rand(rng),
            })
        ];
        let pick : Rc<Fn(&'a mut R) -> Format> = rng.choose(&generators)
            .map(Rc::clone)
            .unwrap(); // Never empty
        pick(rng)
    }
}
impl Format {
    pub fn simple() -> Self {
        Format::Simple {
            stats: Rc::new(RefCell::new(simple::Statistics::default()))
        }
    }

    /// Pick a random set of options.
    ///
    /// Used for testing.
    pub fn randomize_options<R: rand::Rng>(self, rng: &mut R) -> Self {
        match self {
            Format::Simple { stats } => Format::Simple { stats },
            Format::XML => Format::XML,
            Format::Multipart { stats, .. } =>
                Format::Multipart {
                    targets: multipart::Targets {
                        strings_table: CompressionTarget::rand(rng),
                        grammar_table: CompressionTarget::rand(rng),
                        tree: CompressionTarget::rand(rng),
                    },
                    stats
                }
            ,
            Format::Entropy { .. } => unimplemented!()
        }
    }

    /// Return a human-readable name for this format.
    pub fn name(&self) -> String {
        match *self {
            Format::Simple { .. } => "Simple".to_string(),
            Format::Multipart { .. } => "Multipart".to_string(),
            Format::XML => "XML".to_string(),
            Format::Entropy { .. } => "Entropy".to_string(),
        }
    }

    pub fn with_sections<F, E>(&mut self, mut f: F) -> Result<(), E> where F: FnMut(&mut CompressionTarget, &str) -> Result<(), E> {
        match *self {
            Format::Simple { .. } |
            Format::XML => {
                // Nothing to do
                Ok(())
            }
            #[cfg(multistream)]
            Format::TreeRePair { .. } => {
                // Nothing to do
                Ok(())
            }
            Format::Entropy { ..} => {
                // Nothing to do
                Ok(())
            }
            Format::Multipart {
                targets: multipart::Targets {
                    ref mut grammar_table,
                    ref mut strings_table,
                    ref mut tree
                },
                ..
            } => {
                f(grammar_table, "grammar")?;
                f(strings_table, "strings")?;
                f(tree, "tree")?;
                Ok(())
            }
            #[cfg(multistream)]
            Format::MultiStream { ref mut targets, .. } => {
                let multistream::Targets {
                    ref mut contents,
                    ref mut header_identifiers,
                    ref mut header_strings,
                    ref mut header_tags,
                } = *targets;
                f(header_identifiers, "header_identifiers")?;
                f(header_strings, "header_strings")?;
                f(header_tags, "header_tags")?;
                let multistream::PerCategory {
                    ref mut declarations,
                    ref mut idrefs,
                    ref mut strings,
                    ref mut numbers,
                    ref mut bools,
                    ref mut lists,
                    ref mut tags
                } = *contents;
                f(declarations, "declarations")?;
                f(idrefs, "idrefs")?;
                f(strings, "strings")?;
                f(numbers, "numbers")?;
                f(bools, "bools")?;
                f(lists, "lists")?;
                f(tags, "tags")?;
                Ok(())
            }
        }
    }

    /// Return all existing format providers, to manage
    /// command-line arguments.
   fn providers() -> [&'static FormatProvider; 4] {
        [
            &multipart::FormatProvider,
            &simple::FormatProvider,
            &xml::FormatProvider,
            &entropy::FormatProvider,
        ]
    }

    /// The format provider to use if no format provider
    /// has been specified on the command-line.
    fn default_provider() -> &'static FormatProvider {
        &multipart::FormatProvider
    }

    /// Returns command-line argument for advanced.
    /// FormatProvider's subcommands are hidden behind "advanced" command.
    pub fn subcommand<'a, 'b>() -> clap::App<'a, 'b> {
        clap::SubCommand::with_name(ADVANCED_COMMAND)
            .subcommands(Format::providers().iter()
                .map(|x| x.subcommand())
            )
    }

    /// Create a Format based on command-line arguments.
    ///
    /// Pick the first format provider that was invoked by
    /// `matches` as a subcommand. If none, pick the default
    /// provider, without any command-line arguments.
    pub fn from_matches(matches: &clap::ArgMatches) -> Result<Self, std::io::Error> {
        if let Some(matches) = matches.subcommand_matches(ADVANCED_COMMAND) {
            for provider in Self::providers().into_iter() {
                let subcommand = provider.subcommand();
                let key = subcommand.get_name();
                if let Some(matches) = matches.subcommand_matches(key) {
                    return provider.handle_subcommand(Some(matches));
                }
            }
        }
        Self::default_provider()
            .handle_subcommand(None)
    }
}
