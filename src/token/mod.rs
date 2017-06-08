//! Transformations between an AST (as defined in `ast::*`) and stream of tokens
//! (as defined in `io::*`).
//!
//! # Specifications
//! TODO

/// Definition of token streams.
pub mod io;

/// Decode a TokenReader to an AST using a dynamically-loaded grammar.
pub mod decode;

/// Encode a AST to a TokenWriter using a dynamically-loaded grammar.
pub mod encode;

/// Simple implementation of TokenReader/TokenWriter for testing purposes.
pub mod simple;
