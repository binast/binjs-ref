//! This module provides the tools to encode (respectively decode) an AST
//! represented in an ESTree-style format to (respectively from) a lower-level
//! BinTree.
//!
//! As a mechanism to aid with testing compatibility with successive evolutions of
//! EcmaScript, this module does not hardcode any information on the JavaScript
//! language itself. Rather, the actual rules that drive encoding/decoding may be
//! loaded dynamically, typically by being extracting directly from the text of
//! any version of the specifications of ESTree.
//!
//! Optimized implementations are expected to hardcode a specific version of ESTree.

/// Tools for describing the grammar.
pub mod grammar;

/// Decode a Reader to an AST using a dynamically-loaded grammar.
pub mod decode;

/// Encode an AST to a SerializeTree using a dynamically-loaded grammar.
pub mod encode;

/// Support for importing from/exporting to a lower-level format.
pub mod io;