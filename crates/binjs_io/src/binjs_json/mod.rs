//! A format for generating invalid file, to test decoder implementation.
//!
//! The expected usage is the following:
//!
//!   1. Convert .js file to this JSON format (write module)
//!   2. Modify the JSON file, for instance:
//!       - Remove a field from an interface
//!       - Add a field to an interface
//!       - Replace a field value
//!   3. Convert JSON file to other BinAST format (read module)
//!   4. Feed the possibly-invalid BinAST to decoder
//!
//! The JSON format is in the following structure:
//!
//! tagged tuple
//! ```ignore
//! {
//!   "@TYPE": "tagged tuple",
//!   "@INTERFACE": "<interface name>",
//!   "@FIELDS": [
//!     {
//!       "@FIELD_NAME": "<field name>"
//!       "@FIELD_VALUE": <field value>
//!     },
//!     ...
//!   ]
//! }
//! ```
//! `<interface name>` and `<field name>` are the names of the interface and
//! the field in WebIDL, and `<field value>` the field's value.
//!
//! list
//! ```ignore
//! {
//!   "@TYPE": "list",
//!   "@VALUE": [
//!     <item>,
//!     ...
//!   ]
//! }
//! ```
//! `<item>` is the list's item.
//!
//! string
//! ```ignore
//! {
//!   "@TYPE": "string",
//!   "@VALUE": <value>
//! }
//! ```
//! `<value>` is the value of the string. if the value is null, it's `null`.
//! enum, identifier name, property key, float, unsigned long, bool also use
//! the same format, with corresponding "@TYPE" value

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
