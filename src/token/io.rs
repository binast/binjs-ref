//! Mid-level API used to write individual components of an AST tree to a
//! stream of tokens.
//!
//! Simple implementations may write bytes as they come, while complex
//! ones may decide to provide sophisticated compression.
//!
//! In practice, this API is kept as a trait to simplify unit testing and
//! experimentation of sophisticated compression schemes.

use ast::grammar::*;

use std::fmt::Debug;
use std::rc::Rc;

/// An API for reading tokens.
///
/// Note that a `TokenReader` by itself *cannot* determine the nature of the
/// following token. Rather, the driver of the `TokenReader` must be able to
/// deduce the nature of the following token from what it has previously
/// read.
pub trait TokenReader where Self::Error: Debug, Self: Sized {
    /// An error returned by the extractor.
    ///
    /// Errors are *not* recoverable within a `TokenReader`.
    ///
    /// For instance, if attempting to read with `string()`
    /// fails, any further attempt to use the `TokenReader`
    /// or any of its parents will also raise an error.
    type Error;

    /// Skip to the end of the extractor.
    fn skip(&mut self) -> Result<(), Self::Error>;

    /// Read a single UTF-8 string.
    ///
    /// The returned string MUST be valid UTF-8.
    fn string(&mut self) -> Result<Option<String>, Self::Error>;

    /// Read a single `f64`. Note that all numbers are `f64`.
    fn float(&mut self) -> Result<Option<f64>, Self::Error>;

    /// Read a single `bool`.
    fn bool(&mut self) -> Result<Option<bool>, Self::Error>;

    /// Start reading a list.
    ///
    /// Returns an extractor for that list and the number of elements
    /// in the list. Before dropping the sub-extractor, callers MUST
    /// either reach the end of the list or call `skip()`.
    fn list(&mut self) -> Result<(u32, Self), Self::Error>;

    /// Start reading a tagged tuple. If the stream was encoded
    /// properly, the tag is attached to an **ordered** tuple of
    /// fields that may be extracted **in order**.
    ///
    /// Returns the tag name, the ordered array of fields in which
    /// the contents must be read, and a sub-extractor dedicated
    /// to that tuple. The sub-extractor MUST be consumed entirely.
    fn tagged_tuple(&mut self) -> Result<(String, Rc<Box<[Field]>>, Self), Self::Error>;

    /// Start reading an untagged tuple. The sub-extractor MUST
    /// be consumed entirely.
    fn untagged_tuple(&mut self) -> Result<Self, Self::Error>;
}

/// Build an in-memory representation of a BinTree.
///
/// Implementations may for instance introduce atoms,
/// maximal sharing, etc.
pub trait TokenWriter where Self::Error: Debug {
    /// The type of trees manipulated by this writer.
    type Tree;

    /// An error returned by this writer.
    ///
    /// Note that errors are *not* recoverable within the life
    /// of this `TokenWriter`.
    type Error;

    fn done(&mut self);

    /// Write a tagged tuple.
    ///
    /// The number of items is specified by the grammar, so it MAY not be
    /// recorded by the `TokenWriter`.
    ///
    /// The interface MUST have a Tag.
    fn tagged_tuple(&mut self, tag: &str, &[(&Field, Self::Tree)]) -> Result<Self::Tree, Self::Error>;

    /// Write an untagged tuple.
    ///
    /// The number of items is specified by the grammar, so it MAY not be
    /// recorded by the `TokenWriter`.
    fn untagged_tuple(&mut self, &[Self::Tree]) -> Result<Self::Tree, Self::Error>;

    /// Write a list.
    ///
    /// By opposition to a tuple, the number of items is variable and MUST
    /// be somehow recorded by the `TokenWriter`.
    fn list(&mut self, Vec<Self::Tree>) -> Result<Self::Tree, Self::Error>;

    /// Write a single UTF-8 string.
    ///
    /// If specified, the string MUST be UTF-8.
    fn string(&mut self, Option<&str>) -> Result<Self::Tree, Self::Error>;

    /// Write a single number.
    fn float(&mut self, Option<f64>) -> Result<Self::Tree, Self::Error>;

    /// Write single bool.
    fn bool(&mut self, Option<bool>) -> Result<Self::Tree, Self::Error>;
}