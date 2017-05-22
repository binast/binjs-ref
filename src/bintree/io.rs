use estree::grammar::*;

use std::fmt::Debug;

pub trait Extractor where Self::Error: Debug, Self: Sized {
    type Error;

    /// Extract a string.
    fn string(&mut self) -> Result<Option<String>, Self::Error>;

    /// Extract an iterator on a list. Actual reading of the
    /// contents of the list will be lazy, as the extractor
    /// does not have the necessary information to decode
    /// the contents of the list, only its layout in the source.
    ///
    /// Once reading the list is complete, the iterator may
    /// perform additional checks on byte length.
    ///
    /// The iterator MUST be exhausted by the client.
    fn list(&mut self) -> Box<Iterator<Item=Result<Self, Self::Error>>>;

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
    fn tuple(&mut self, Vec<Self::Tree>, Option<&Interface>) -> Self::Tree;
    fn list(&mut self, Vec<Self::Tree>) -> Self::Tree;

    /// Build a special string that represents the undefined string.
    fn no_string(&mut self) -> Self::Tree;
    fn string(&mut self, &str) -> Self::Tree;

    fn float(&mut self, f64) -> Self::Tree;
    fn bool(&mut self, bool) -> Self::Tree;

    fn label(&mut self, Self::Tree, &str) -> Self::Tree;
}