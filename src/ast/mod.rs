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
//! ## Interface Declaration
//!
//! Each Interface Declaration consists in *all* of:
//!
//! - *name*: `InterfaceName` (e.g. `Expression`, `ExpressionStatement`, ...);
//! - *tag*: `Tag`, optional (e.g. `ExpressionStatement`, but not `Expression`);
//! - *parents*: `[InterfaceName]` a (possibly empty) list of Parent Interfaces.
//! - *own_structure*: `Structure`
//!
//! Property *name* is used to represent relationships between Interfaces. By opposition,
//! Property *tag** is attached to actual nodes of the AST. For instance, `Expression`
//! is the name of a Parent Interface of many interfaces. However, this specific
//! interface has no inhabitant and therefore no *tag*.
//!
//! Property *own_structure* determines the list of fields defined by this interface.
//! This does not include fields defined by parent interfaces.
//!
//!
//! ## Enumeration Declaration
//!
//! Each Enumeration Declaration consists in *all* of:
//!
//! - *name*: `EnumerationName`;
//! - *cases*: `[String]` (not empty);
//!
//! Enumerations describe one of several possible strings, e.g.: `"+" | "-" | "/" | "-"`.
//!
//! ## Types
//!
//! Each `Type` is *one* of:
//!
//! - `Enumeration`: `EnumerationName`; or
//! - `Interfaces`: `([InterfaceName], Nullability)`;
//! - `Boolean`: empty; or
//! - `String`: empty; or
//! - `Number`: empty; or
//! - `Array`: `Type`
//!
//! `Nullability` is *one* of:
//! - `CanBeNull`: empty; or
//! - `CannotBeNull`: empty.
//!
//! ## Structures
//!
//! Each `Structure` is:
//! - fields: `[Field]`;
//!
//! Each `Field` consists in *all* of:
//!
//! - *name*: `FieldName` (e.g. `body`);
//! - *type*: `Type` (e.g. `Number`).
//!
//!
//! # Inhabiting a grammar
//!
//! **Warning** The AST described here may be more expressive in places than the JavaScript source
//! grammar. Therefore, it is possible that not all the inhabitants of the grammar are correct
//! (or even syntactically-correct) JavaScript ASTs.
//!
//! FIXME: Specify.
//!
//! # Evolving the Grammar
//!
//! ## Adding new interfaces or enumerations
//!
//! FIXME: Specify.
//!
//! ## Patching an interface.
//!
//! FIXME: Specify.
//!
//! ## Patching an enumeration.
//!
//! FIXME: Specify.

/// Tools for describing the grammar.
pub mod grammar;

/// A library of versions of EcmaScript.
pub mod library;

/// Collecting annotations on the AST (e.g. bound variables).
pub mod annotation;