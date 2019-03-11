//! A crate containing tools to manipulate ASTs in generic (i.e. JSON)
//! format.

extern crate binjs_meta;

#[macro_use]
extern crate json;
extern crate rand;

/// Generic instance of `Spec` representing the es6 AST.
pub mod es6;

/// Generating random ASTs (for fuzzing purposes).
pub mod pick;

/// Walkers, comparisons.
pub mod util;
