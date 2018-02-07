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
//! See:
//!
//! - the documentation of module `grammar` for the grammar of grammars;
//! - the documentation of module `library` for the actual grammars of ES5, ES6, ...;

pub mod export_utils;

/// Definition of a grammar, tools for describing the grammar.
pub mod grammar;

/// A library of versions of EcmaScript.
pub mod library;

/// ES6 implementation
mod library_es6_generated;

/// Collecting annotations on the AST (e.g. bound variables).
pub mod annotation;

/// Tools for manipulating webidl.
pub mod webidl;