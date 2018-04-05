//! Strongly-typed implementation of the ES6 AST.

#![feature(box_patterns)]
#![recursion_limit="128"] // We have deeply nested data structures...

extern crate binjs_io;
extern crate binjs_shared;

#[macro_use]
extern crate assert_matches;
#[macro_use]
extern crate log;
#[macro_use]
extern crate json;

/// A strongly-typed AST for ES6.
pub mod ast;

pub mod io;

/// Computing scope information from a strongly-typed AST.
pub mod scopes;
