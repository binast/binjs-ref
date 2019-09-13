//! Encoding/decoding based on Huffman tables.

/// Format documentation.
pub mod format;
mod prelude;
mod varnum;

/// A four-char name embedded in the binary.
///
/// This may typically the name of a section or that of a compression format.
pub type Name = [u8; 4];
