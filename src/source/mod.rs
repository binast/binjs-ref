//! Source-level operations.
//!
//! Reading a JavaScript text source file into an AST.

#[cfg(feature = "spidermonkey")]
/// Using an external SpiderMonkey shell to load an AST.
pub mod spidermonkey;

#[cfg(feature = "spidermonkey")]
pub use self::spidermonkey::SpiderMonkey;

/// Using Node.js + Esprima to load an AST.
pub mod esprima;

pub use self::esprima::Esprima;