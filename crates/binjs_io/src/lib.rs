#![feature(box_patterns)]
#![feature(vec_resize_default)]

extern crate binjs_shared;

extern crate brotli;
extern crate flate2;
extern crate itertools;
extern crate lzw;
#[macro_use]
extern crate log;
extern crate rand;
extern crate range_encoding;
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
    WriteError(std::io::Error),
}

#[derive(Debug)]
pub enum TokenReaderError {
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

#[cfg(multistream)]
pub mod multistream;
pub mod entropy;

/// A tree comperssion mechanism.
#[cfg(multistream)]
pub mod repair;
// pub mod repair2;

pub mod xml;

mod util;

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
    Arithmetic {
        options: entropy::write::Options,
        model: Box<entropy::Model>,
    }
}

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
    pub fn parse(source: Option<&str>) -> Option<Self> {
        match source {
            None | Some("multipart") => {
                use multipart::{ Statistics, Targets };
                let stats = Rc::new(RefCell::new(Statistics::default()
                    .with_source_bytes(0)));
                Some(Format::Multipart {
                    targets: Targets {
                        strings_table: CompressionTarget::new(Compression::Identity),
                        grammar_table: CompressionTarget::new(Compression::Identity),
                        tree: CompressionTarget::new(Compression::Identity),
                    },
                    stats
                })
            },
#[cfg(multistream)]
            Some("trp") => {
                Some(Format::TreeRePair {
                    options: repair::Options {
                        max_rank: None,
                        dictionary_placement: DictionaryPlacement::Header,
                        numbering_strategy: NumberingStrategy::GlobalFrequency,
                    }
                })
            }
            Some("xml") => Some(Format::XML),
            Some("simple") => Some(Self::simple()),
            Some("arithmetic") => Some(Format::Arithmetic {
                options: entropy::write::Options::default(),
                model: Box::new(entropy::model::ExactModel)
            }),
            _ => None
        }
    }
    pub fn with_flags<'a, I>(self, flags: I) -> Result<Self, String>
        where I: Iterator<Item = &'a str>
    {
        match self {
            Format::Arithmetic { model, options } => {
                let mut options = options;
                for option in flags {
                    options = options.with_option(option)
                        .ok_or_else(|| format!("Invalid option {}", option))?;
                }
                Ok(Format::Arithmetic { model, options })
            }
            _ => Ok(self)
        }
    }

    #[cfg(multistream)]
    pub fn with_trp_rank(self, source: Option<&str>) -> Option<Self> {
        use Format::*;
        match self {
            TreeRePair { options } => {
                let max_rank = match source {
                    None | Some("none") => None,
                    Some(ref num) => {
                        if let Ok(rank) = usize::from_str_radix(num, 10) {
                            Some(rank)
                        } else {
                            return None
                        }
                    }
                };
                Some(TreeRePair {
                    options: repair::Options {
                        max_rank,
                        ..options
                    }
                })
            },
            _ if source.is_some() => panic!("Argument trp rank specified, but we're not performing trp"),
            _ => { Some(self) }
        }
    }

    #[cfg(multistream)]
    pub fn with_trp_numbering(self, source: Option<&str>) -> Option<Self> {
        match self {
            Format::TreeRePair { options } => {
                let numbering_strategy = match source {
                    None | Some("frequency") => NumberingStrategy::GlobalFrequency,
                    Some("mru") => NumberingStrategy::MRU,
                    Some("parent") => NumberingStrategy::Prediction,
                    Some(other) => return None
                };
                Some(Format::TreeRePair {
                    options: repair::Options {
                        numbering_strategy,
                        ..options
                    }
                })
            }
            _ if source.is_some() => panic!("Argument trp numbering specified, but we're not performing trp"),
            _ => { Some(self) }
        }
    }

    #[cfg(multistream)]
    pub fn with_dictionary_placement(self, source: Option<&str>) -> Option<Self> {
        match self {
            Format::TreeRePair { options } => {
                let dictionary_placement = match source {
                    None => DictionaryPlacement::Header,
                    Some("inline") => DictionaryPlacement::Inline,
                    Some("header") => DictionaryPlacement::Header,
                    Some(other) => return None
                };
                Some(Format::TreeRePair {
                    options: repair::Options {
                        dictionary_placement,
                        ..options
                    }
                })
            }
            _ if source.is_some() => panic!("Argument trp dictionary placement specified, but we're not performing trp"),
            _ => { Some(self) }
        }
    }

    pub fn with_compression(self, compression: Compression) -> Self {
        match self {
            Format::Multipart {
                stats,
                targets: _
            } => {
                Format::Multipart {
                    stats,
                    targets: multipart::Targets {
                        grammar_table: CompressionTarget::new(compression.clone()),
                        strings_table: CompressionTarget::new(compression.clone()),
                        tree: CompressionTarget::new(compression)
                    }
                }
            },
            #[cfg(multistream)]
            Format::MultiStream {
                options,
                targets: _
            } => {
                Format::MultiStream {
                    options,
                    targets: multistream::Targets {
                        contents: multistream::PerCategory {
                            declarations: CompressionTarget::new(compression.clone()),
                            idrefs: CompressionTarget::new(compression.clone()),
                            strings: CompressionTarget::new(compression.clone()),
                            numbers: CompressionTarget::new(compression.clone()),
                            bools: CompressionTarget::new(compression.clone()),
                            lists: CompressionTarget::new(compression.clone()),
                            tags: CompressionTarget::new(compression.clone()),
                        },
                        header_strings: CompressionTarget::new(compression.clone()), // FIXME: A different compression might be useful.
                        header_tags: CompressionTarget::new(compression.clone()),
                        header_identifiers: CompressionTarget::new(compression.clone()), // FIXME: A different compression might be useful.
                    }
                }
            },
            _ => self
        }
    }
    pub fn with_compression_str(self, source: Option<&str>) -> Option<Self> {
        let compression = Compression::parse(source)
            .unwrap_or(Compression::Identity);
        Some(self.with_compression(compression))
    }

    #[cfg(multistream)]
    pub fn with_numbering_strategy(mut self, source: Option<&str>) -> Option<Self> {
        let strategy = match source {
            None => NumberingStrategy::GlobalFrequency,
            Some("mru") => NumberingStrategy::MRU,
            Some("ppm") => NumberingStrategy::Prediction,
            _ => return None,
        };
        use self::Format::*;
        if let TreeRePair { ref mut options } = self {
            options.numbering_strategy = strategy;
        }
        Some(self)
    }
}

impl Format {
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
            Format::Arithmetic { ..} => {
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
}