//! Tools for manipulating byte-level data.

/// Encoding/decoding booleans.
pub mod bool;

/// Compressing/decompressing from/to common formats.
pub mod compress;

/// Encoding/decoding floating-point numbers.
pub mod float;

/// Serializing/deserializing traits.
pub mod serialize;

/// Encoding/decoding variable-length numbers.
pub mod varnum;
