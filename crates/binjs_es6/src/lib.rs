//! Strongly-typed implementation of the ES6 AST.

#![recursion_limit = "128"] // We have deeply nested data structures...

#[macro_use]
extern crate binjs_io;
extern crate binjs_shared;

#[macro_use]
extern crate assert_matches;
extern crate itertools;
#[macro_use]
extern crate serde;
#[macro_use]
extern crate log;

/// A strongly-typed AST for ES6.
pub mod ast;

/// Serialization/deserialization utilities.
pub mod io;

/// Errors encountered during a call to `Enrich`.
///
/// These errors generally mean that we have encountered a fragment of JavaScript
/// which we do not support yet.
#[derive(Debug)]
pub enum EnrichError {
    ScopeError(scopes::ScopeError),
}
impl From<scopes::ScopeError> for EnrichError {
    fn from(err: scopes::ScopeError) -> Self {
        EnrichError::ScopeError(err)
    }
}

/// A mechanism used to enrich an AST obtained from parsing with additional information
/// and/or domain-specific rewrites.
///
/// See the definition of fields for individual enrichments.
pub struct Enrich {
    /// If `true`, introduce `AssertedScope*` information.
    ///
    /// Default: `true`, as this is necessary for most tests and command-line tools.
    pub scopes: bool,

    /// Introduce laziness for all functions strictly below level `t`.
    ///
    /// Default: 0 (deactivated), as this is an optional, WIP, optimization.
    pub lazy_threshold: u32,

    /// If `Some(t)`, introduce scoped dictionaries around pure data expressions
    /// of size >= t.
    ///
    /// Default: `None` (deactivated), as this is an optional, WIP, optimization.
    pub pure_data_threshold: Option<usize>,
}

impl Default for Enrich {
    fn default() -> Self {
        Enrich {
            scopes: true,
            lazy_threshold: 0,
            pure_data_threshold: None,
        }
    }
}
impl Enrich {
    /// Perform enrichments.
    pub fn enrich(&self, script: &mut ast::Script) -> Result<(), EnrichError> {
        if self.lazy_threshold > 0 {
            let mut visitor = lazy::LazifierVisitor::new(self.lazy_threshold);
            visitor.annotate_script(script)?;
        }
        if self.scopes {
            let mut visitor = scopes::AnnotationVisitor::new();
            visitor.annotate_script(script)?;
        }
        if let Some(threshold) = self.pure_data_threshold {
            sublanguages::InjectVisitor::rewrite_script(threshold, script)?;
        }
        Ok(())
    }
}

/// A mechanism used to cleanup an AST previously enriched with `Enrich`.
///
/// See the definition of fields for individual enrichments.
pub struct Cleanup {
    /// If `true`, get rid of all instances of scoped dictionaries.
    /// Default: `false`.
    pub scoped_dictionaries: bool,
}
impl Default for Cleanup {
    fn default() -> Self {
        Cleanup {
            scoped_dictionaries: false,
        }
    }
}
impl Cleanup {
    /// Perform enrichments.
    pub fn cleanup(&self, script: &mut ast::Script) {
        if self.scoped_dictionaries {
            sublanguages::CleanupVisitor::rewrite_script(script);
        }
    }
}

/// Computing scope information from a strongly-typed AST.
mod scopes;

/// Introducing laziness in an AST.
mod lazy;

/// Rewriting language fragments.
mod sublanguages;
