//! This module defines the AST grammar and provides tools for manipulating
//! representations of this AST.
//!
//! As a mechanism to aid with testing compatibility with successive evolutions of
//! EcmaScript, this module does not hardcode any information on the JavaScript
//! language itself. Rather, the actual rules that drive encoding/decoding may be
//! loaded dynamically, possibly by being extracting directly from the text of
//! any version of the specifications of ESTree.
//!
//! Optimized implementations are expected to hardcode a specific version of ESTree.
//!
//! # Specifications
//! TODO

/// Tools for describing the grammar.
pub mod grammar;

/// A library of versions of EcmaScript.
pub mod library;
