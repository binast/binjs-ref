#![feature(vec_resize_default)]


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

/// A simple implementation of TokenReader/TokenWriter,
/// designed specifically to help debug implementations
/// of grammar encoders/decoders.
pub mod simple;

/// An optimization of TokenReader/TokenWriter,
/// designed to minimize the size of the file.
pub mod multipart;

/// A tree comperssion mechanism.
pub mod repair;

pub mod xml;

mod util;


pub enum Format {
    Simple {
        stats: Rc<RefCell<simple::Statistics>>
    },
    Multipart {
        options: multipart::WriteOptions,
        stats: Rc<RefCell<multipart::Statistics>>
    },
    TreeRePair,
    XML,
}
impl Format {
    pub fn new(format: &Format) -> Format {
        use multipart::WriteOptions;
        match *format {
            Format::Simple { .. } => Format::Simple { stats: Default::default() },
            Format::Multipart { ref options, .. } => Format::Multipart {
                stats: Default::default(),
                options: WriteOptions::new(options),
            },
            Format::TreeRePair => Format::TreeRePair,
            Format::XML => Format::XML,
        }
    }
}
