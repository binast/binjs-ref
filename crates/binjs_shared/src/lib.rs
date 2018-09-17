extern crate itertools;
extern crate json;
#[macro_use]
extern crate log;

mod json_conversion;
pub use json_conversion::*;

pub mod ast;
pub mod mru;
mod shared_string;
pub use shared_string::SharedString;


#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Offset(pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VisitMe<T> {
    /// Visit the children of this node.
    ///
    /// The value is a guard
    HoldThis(T),

    /// Skip the children of this node, skip the `exit_` method, return immediately.
    DoneHere,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct IdentifierName(pub SharedString);
pub type Identifier = IdentifierName;
impl IdentifierName {
    pub fn from_str(value: &'static str) -> Self {
        IdentifierName(SharedString::from_str(value))
    }
    pub fn from_string(value: String) -> Self {
        IdentifierName(SharedString::from_string(value))
    }
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
    pub fn as_shared_string(&self) -> &SharedString {
        &self.0
    }
}
impl<'a> PartialEq<&'a str> for IdentifierName {
    fn eq(&self, other: &&'a str) -> bool {
        self.0.eq(other)
    }
}
impl Default for IdentifierName {
    fn default() -> Self {
        Self::from_str("<uninitialized IdentifierName>")
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct PropertyKey(pub SharedString);
impl PropertyKey {
    pub fn from_str(value: &'static str) -> Self {
        PropertyKey(SharedString::from_str(value))
    }
    pub fn from_string(value: String) -> Self {
        PropertyKey(SharedString::from_string(value))
    }
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
    pub fn as_shared_string(&self) -> &SharedString {
        &self.0
    }
}
impl<'a> PartialEq<&'a str> for PropertyKey {
    fn eq(&self, other: &&'a str) -> bool {
        self.0.eq(other)
    }
}
impl Default for PropertyKey {
    fn default() -> Self {
        Self::from_str("<uninitialized PropertyKey>")
    }
}
