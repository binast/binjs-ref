use std::fmt::Debug;
use std::path::Path;

/// A source that can parse files to ASTs.
pub trait SourceParser<AST> {
    type Error: Debug;

    /// Parse a string.
    fn parse_str(&self, source: &str) -> Result<AST, Self::Error>;

    /// Parse a file.
    fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<AST, Self::Error>;
}
