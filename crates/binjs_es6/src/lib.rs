//! Strongly-typed implementation of the ES6 AST.

#![feature(box_patterns)]
#![recursion_limit="128"] // We have deeply nested data structures...

#[macro_use]
extern crate binjs_io;
extern crate binjs_shared;

#[macro_use]
extern crate assert_matches;
extern crate itertools;
#[macro_use]
extern crate json;
#[macro_use]
extern crate log;

/// A strongly-typed AST for ES6.
pub mod ast;

/// Serialization/deserialization utilities.
pub mod io;

/// Computing scope information from a strongly-typed AST.
pub mod scopes;

/// Introducing laziness in an AST.
pub mod skip;
