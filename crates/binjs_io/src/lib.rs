#![feature(vec_resize_default)]

extern crate binjs_shared;

extern crate brotli;
extern crate flate2;
extern crate itertools;
extern crate lzw;
#[macro_use]
extern crate log;
extern crate priority_queue;
extern crate rand;
extern crate vec_map;
extern crate xml as xml_rs;

use std::cell::RefCell;
use std::rc::Rc;

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


/// Byte-level utilities for writing token readers/writers.
pub mod bytes;

/// Definition of TokenReader/TokenWriter traits.
mod io;
pub use io::*;

pub mod labels;

/// A simple implementation of TokenReader/TokenWriter,
/// designed specifically to help debug implementations
/// of grammar encoders/decoders.
pub mod simple;

/// An optimization of TokenReader/TokenWriter,
/// designed to minimize the size of the file.
pub mod multipart;

pub mod multistream;

/// A tree comperssion mechanism.
pub mod repair;

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

/// Instructions for a single section (grammar, strings, tree, ...)
#[derive(Clone, Debug)]
pub struct CompressionTarget {
    destination: Rc<RefCell<Vec<u8>>>,
    format: bytes::compress::Compression,
    stats: Option<bytes::compress::CompressionResult>,
}
impl CompressionTarget {
    pub fn new(format: bytes::compress::Compression) -> Self {
        Self {
            destination: Rc::new(RefCell::new(vec![])),
            stats: None,
            format,
        }
    }
    pub fn data(&self) -> &RefCell<Vec<u8>> {
        self.destination.as_ref()
    }
    pub fn reset(&mut self) -> Rc<RefCell<Vec<u8>>> {
        let mut buffer = Rc::new(RefCell::new(vec![]));
        std::mem::swap(&mut buffer, &mut self.destination);
        buffer
    }
    pub fn stats(&self) -> Option<bytes::compress::CompressionResult> {
        self.stats.clone()
    }
}
impl std::io::Write for CompressionTarget {
    fn write(&mut self, data: &[u8]) -> std::result::Result<usize, std::io::Error> {
        self.stats = Some(self.format.compress(data, &mut *self.destination.borrow_mut())?);
        Ok(data.len())
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
    TreeRePair {
        options: repair::Options,
    },
    XML,
    MultiStream {
        options: multistream::Options,
        targets: multistream::Targets,
    },
}
