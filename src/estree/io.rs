use estree::grammar::*;

use std::cell::RefMut;
use std::fmt::Debug;

pub trait Extractor where Self::Error: Debug, Self: Sized {
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
    /// properly, the tag is attached to an **ordered** list of
    /// fields that may be extracted **in order**.
    ///
    /// Once reading the tuple is complete, the iterator may
    /// perform additional checks on byte length.
    ///
    /// The iterator MUST be exhausted by the client.
    fn tag(&mut self) -> Result<(Tag, Box<Iterator<Item=String>>), Self::Error>;

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
    /// If an interface is specified, add the `tag` of the interface. // FIXME: What if it has no tag?
    fn tuple(&mut self, Vec<Self::Tree>, Option<&Interface>) -> Result<Self::Tree, Self::Error>;
    fn list(&mut self, Vec<Self::Tree>) -> Result<Self::Tree, Self::Error>;

    /// Build a special string that represents the undefined string.
    fn no_string(&mut self) -> Result<Self::Tree, Self::Error>;
    fn string(&mut self, &str) -> Result<Self::Tree, Self::Error>;

    fn float(&mut self, f64) -> Result<Self::Tree, Self::Error>;
    fn bool(&mut self, bool) -> Result<Self::Tree, Self::Error>;
}