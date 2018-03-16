//! Source-level operations.
//!
//! Reading a JavaScript text source file into an AST.

mod parser;
pub use self::parser::SourceParser;

/// Parsing JavaScript using the Shift source parser (in Node).
pub mod shift;
pub use self::shift::Shift;
