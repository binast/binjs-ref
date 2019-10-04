//! Encoding/decoding based on Huffman tables.
#![allow(dead_code)] // Silence dead code warnings until they make sense.

/// Format documentation.
pub mod format;
mod huffman;
mod strings;
mod varnum;

/// A four-char name embedded in the binary.
///
/// This may typically the name of a section or that of a compression format.
pub type Name = [u8; 4];
