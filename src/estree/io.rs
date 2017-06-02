//! Low-level API used to write individual components of a tree to a stream.
//!
//! Simple implementations may write bytes as they come, while complex
//! ones may decide to provide sophisticated compression.
//!
//! In practice, this API is kept as a trait to simplify unit testing and
//! experimentation of sophisticated compression schemes.

use estree::grammar::*;

use std::fmt::Debug;

/// Extracting values and structure from a stream of bytes.
pub trait Extractor where Self::Error: Debug, Self: Sized {
    /// An error returned by the extractor.
    ///
    /// Errors are *not* recoverable.
    ///
    /// For instance, if attempting to read with `string()`
    /// fails, any further attempt to use the `Extractor`
    /// or any of its parents will also raise an error.
    type Error;

    /// Skip to the end of the extractor.
    fn skip(&mut self) -> Result<(), Self::Error>;

    /// Extract a string.
    fn string(&mut self) -> Result<Option<String>, Self::Error>;

    /// Start reading a list.
    ///
    /// Returns an extractor for that list and the number of elements
    /// in the list.
    fn list(&mut self) -> Result<(u32, Self), Self::Error>;

    /// Extract a tag from the stream. If the stream was encoded
    /// properly, the tag is attached to an **ordered** tuple of
    /// fields that may be extracted **in order**.
    ///
    /// Returns the tag, the ordered array of fields in which
    /// the contents must be read, and a sub-extractor dedicated
    /// to that tuple.
    fn tag(&mut self) -> Result<(String, &[&Field], Self), Self::Error>;

    fn float(&mut self) -> Result<f64, Self::Error>;
    fn bool(&mut self) -> Result<bool, Self::Error>;
}

/// Build an in-memory representation of a BinTree.
///
/// Implementations may for instance introduce atoms,
/// maximal sharing, etc.
pub trait Builder {
    type Tree;
    type Error;

    /// Write a tuple.
    ///
    /// If an interface is specified, it MUST have a Tag.
    fn tuple(&mut self, Vec<Self::Tree>, Option<&InterfaceNode>) -> Result<Self::Tree, Self::Error>;
    fn list(&mut self, Vec<Self::Tree>) -> Result<Self::Tree, Self::Error>;

    /// Build a special string that represents the undefined string.
    fn no_string(&mut self) -> Result<Self::Tree, Self::Error>;
    fn string(&mut self, &str) -> Result<Self::Tree, Self::Error>;

    fn float(&mut self, f64) -> Result<Self::Tree, Self::Error>;
    fn bool(&mut self, bool) -> Result<Self::Tree, Self::Error>;
}