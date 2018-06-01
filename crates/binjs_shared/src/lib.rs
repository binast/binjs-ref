extern crate json;
extern crate itertools;

use std::rc::Rc;

mod json_conversion;
pub use json_conversion::*;

pub mod ast;

pub mod mru;

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Offset(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct IdentifierDefinition(pub Rc<String>);
impl IdentifierDefinition {
    pub fn name(&self) -> &Rc<String> {
        &self.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct IdentifierReference(pub Rc<String>);
impl IdentifierReference {
    pub fn name(&self) -> &Rc<String> {
        &self.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VisitMe<T> {
    /// Visit the children of this node.
    ///
    /// The value is a guard
    HoldThis(T),

    /// Skip the children of this node, skip the `exit_` method, return immediately.
    DoneHere,
}

