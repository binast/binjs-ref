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

/// An identifier, inside the grammar.
shared_string!(pub IdentifierName);
pub type Identifier = IdentifierName;

/// A property, inside the grammar.
shared_string!(pub PropertyKey);

/// An interface *of* the grammar.
shared_string!(pub InterfaceName);

/// A field name *of* the grammar.
shared_string!(pub FieldName);



