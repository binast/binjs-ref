//! Source-level operations.
//!
//! Reading a JavaScript text source file into an AST.

/// Using an external SpiderMonkey shell to load an AST.
pub mod spidermonkey;

pub use self::spidermonkey::SpiderMonkey;