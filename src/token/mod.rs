//! Transformations between an AST (as defined in `ast::*`) and stream of tokens
//! (as defined in `io::*`).


use ast::grammar::{ FieldName, NodeName };
use std;

/// Definition of token streams.
pub mod io;

/// Decode a TokenReader to an AST using a dynamically-loaded grammar.
pub mod decode;

/// Encode a AST to a TokenWriter using a dynamically-loaded grammar.
pub mod encode;

/// Simple implementation of TokenReader/TokenWriter for testing purposes.
pub mod simple;

/// An implementation of TokenReader/TokenWriter that splits grammar, strings
/// and the tree itself into separate "parts", each of which may be individually
/// compressed.
pub mod multipart;

#[derive(Debug)]
pub enum GrammarError {
    NoSuchKind(String),
    NoSuchField {
        kind: NodeName,
        field: FieldName
    }
}

#[derive(Debug)]
pub enum TokenWriterError {
    GrammarError(GrammarError),
    WriteError(std::io::Error),
}
