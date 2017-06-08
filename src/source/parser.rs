use std::fmt::Debug;
use std::path::Path;

use serde_json::Value as JSON;

/// A source that can parse files to JSON ASTs.
pub trait SourceParser {
    type Error: Debug;

    /// Parse a string.
    fn parse_str(&self, source: &str) -> Result<JSON, Self::Error>;

    /// Parse a file.
    fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<JSON, Self::Error>;
}