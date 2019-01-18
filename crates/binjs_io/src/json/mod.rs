//! A format for generating invalid file, to test decoder implementation.
//!
//! The expected usage is the following:
//!
//!   1. Convert .js file to this JSON format (write module)
//!   2. Modify the JSON file
//!   3. Convert JSON file to other BinAST format (parser, read modules)
//!   4. Feed the possibly-invalid BinAST to decoder

mod parser;

mod value;

pub mod read;

pub mod write;

use clap;

/// Command-line management.
pub struct FormatProvider;
impl ::FormatProvider for FormatProvider {
    fn subcommand<'a, 'b>(&self) -> clap::App<'a, 'b> {
        use clap::*;
        SubCommand::with_name("json").about("Encode to json. This is for testing purpose only.")
    }

    fn handle_subcommand(
        &self,
        _matches: Option<&clap::ArgMatches>,
    ) -> Result<::Format, ::std::io::Error> {
        Ok(::Format::JSON)
    }
}
