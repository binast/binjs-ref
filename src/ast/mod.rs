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
//!
//! # Specifications
//!
//! A Grammar is composed of Interface Declarations and Enumeration Declarations.
//!
//! Each Interface Declaration consists in *all* of:
//!
//! - one **Name** (e.g. `Expression`, `ExpressionStatement`, ...);
//! - a (possibly empty) list of **Name**s of Parent Interfaces.
//! - optionally, one **Tag** (e.g. `ExpressionStatement`, but not `Expression`);
//! - an object **Structure**.
//!
//! A **Name** is used to represent relationships between Interfaces. By opposition,
//! A **Tag** is attached to actual nodes of the AST. For instance, `Expression`
//! is the name of a Parent Interface of many interfaces. However, this specific
//! interface has no inhabitant and therefore no **Tag**.
//!
//! FIXME: specify Enumerations.
//!
//! FIXME: specify Structure.
//!
//! Each Field consists in *all* of:
//!
//! - one Property (e.g. `body`);
//! - one Type.
//!
//! Each Type is *one* of:
//!
//! - `OrNull(T)`, where T is a Type; or
//! - one or more Name of Interfaces; or
//! - the Name of an Enumeration; or
//! - `Array(T)`, where T is a Type; or
//! - `Boolean`; or
//! - `String`; or
//! - `Number`; or
//! - `Null`.
//!
//! # Inhabiting a grammar
//!
//! FIXME: Specify.
//!
//! # Evolving the Grammar
//!
//! FIXME: Specify.

/// Tools for describing the grammar.
pub mod grammar;

/// A library of versions of EcmaScript.
pub mod library;
