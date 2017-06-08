//! Source-level operations.
//!
//! Reading a JavaScript text source file into an AST.

mod parser;
pub use self::parser::SourceParser;

#[cfg(feature = "spidermonkey")]
/// Using an external SpiderMonkey shell to load an AST.
pub mod spidermonkey;

#[cfg(feature = "spidermonkey")]
pub use self::spidermonkey::SpiderMonkey;

/// Using Node.js + Esprima to load an AST.
pub mod esprima;
pub use self::esprima::Esprima;

/// Using Node.js + Babel to load an AST.
pub mod babel;
pub use self::babel::Babel;