//! WIP: Context encoding/decoding.

mod prelude;

/// A four-char name embedded in the binary.
///
/// This may typically the name of a section or that of a compression format.
pub type Name = [u8; 4];
