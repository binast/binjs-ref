//! A multipart format, in which each part can be compressed independently.
//!
//! # Overview
//!
//! The file is divided in sections. Each section is prefixed by its bytelength, so as to permit
//! skipping a section and/or reading sections concurrently. Each section may be compressed
//! independently, possibly with different compression formats, with the expectation that this
//! will let compressors take best advantage of the distinct structures of each section.
//!
//! (future versions may allow file-wide compression, too)
//!
//! The sections are:
//!
//! 1. the grammar table;
//! 2. the strings table (which contains both strings and identifiers);
//! 3. the representation of the tree.
//!
//! The grammar table lists the AST nodes used in the file. Its primary role is to serve as a lightweight
//! versioning mechanism - for instance, older versions of JS may define a node `Function` with three fields
//! (`body`, `arguments` and optional `name`), while more recent versions of JS may define the same node
//! with *five* fields (`body`, `arguments`, `async`, `generator` and optional `name`). A BinAST file
//! may contain *either* variants of `Function`, depending on when it was created. The grammar table lets recent
//! parsers determine that some fields are omitted and should be replaced by their default value. In fact, a
//! BinAST file could even contain *both* variants of `Function`, for compression purposes. Also, when a
//! parser encounters a grammar table with nodes that either have an unknown name or contain unknown
//! fields, it may decide to reject the file immediately (it doesn't have to, mind you).
//!
//! The strings table lists all strings (including identifiers) in the file. Its primary role is to speed
//! up parsing by making sure that each string only needs to be parsed/checked/atomized once during parsing.
//! Its secondary role is compression.
//!
//! In the current version, the tree is a sequence of tokens. All these tokens are ambiguous and a stream may
//! only be tokenized by a client that knows both the grammar and the grammar table. Specific tokens (lists)
//! contain their byte length, so as to allow skipping them for purposes of lazy parsing and/or concurrent
//! parsing.
//!
//! # Format
//!
//! The entire file is formatted as:
//!
//! - the characters `"BINJS"`;
//! - a container version number (`varnum`, currently `0`);
//! - the compressed grammar table (see below);
//! - the compressed strings table (see below);
//! - the compressed tree (see below).
//!
//! ## Grammar table
//!
//! The grammar table serves to map tagged tuple indices to actual constructions in the JS grammar.
//!
//! - the characters `"[GRAMMAR]"`;
//! - a `prefix` identifying the compression format used for the grammar (one of "identity;", "br;", "gzip;", "compress;", "deflate;").
//! - the number of compressed bytes (`varnum`);
//! - compressed in the format identified by `prefix`:
//!    - the number of entries (`varnum`);
//!    - for each entry,
//!      - byte length of entry (`varnum`);
//!      - one of
//!        - the invalid strings [255, 0] (representing the null interface, only valid if byte length is 2);
//!        - a utf-8 encoded string (utf-8 encoded, `bytelen` bytes, no terminator).
//!
//! ## Strings table
//!
//! The grammar table serves to map tagged tuple indices to strings.
//!
//! - the characters `"[STRINGS]"`;
//! - a `prefix` identifying the compression format used for the grammar (one of "identity;", "br;", "gzip;", "compress;", "deflate;").
//! - the number of compressed bytes (`varnum`);
//! - compressed in the format identified by `prefix`;
//!    - the number of entries (`varnum`);
//!    - for each entry,
//!      - byte length of string (`varnum`);
//!      - one of
//!        - the invalid strings [255, 0] (representing the null string, only valid if byte length is 2);
//!        - a utf-8 encoded string (utf-8 encoded, `bytelen` bytes, no terminator).
//!
//! ## The tree
//!
//! This contains the actual tree for a specific grammar. The file does not contain all the information
//! to determine the nature of next token. Rather, this must be led by the grammar.
//!
//! - the characters `"[TREE]"`;
//! - a `prefix` identifying the compression format used for the grammar (one of "identity;", "br;", "gzip;", "compress;", "deflate;").
//! - the number of compressed bytes (`varnum`);
//! - compressed in the format identified by `prefix`:
//!   - one tree token.
//!
//! ### Tree token
//!
//!  A tree token is defined as one of
//!
//!   - a null float, represented as:
//!     - a low-endian IEEE764 64-bit floating point value signalling NaN (8 bytes),
//!   - a non-null float, represented as:
//!     - a low-endian IEEE764 64-bit floating point value non-signalling NaN (8 bytes),
//!   - a null boolean, represented as:
//!     -  a single byte with value `2` (one byte);
//!   - a non-null boolean, represented as:
//!     -  a single byte with value `0` (false) or `1` (true) (one byte);
//!   - a string, representing as
//!     - an entry in the table of strings (`varnum`);
//!   - a list, represented as
//!     - byte length (`varnum`);
//!     - `bytelen` bytes representing:
//!       - number of items (`varnum`);
//!       - for each item
//!          - the token;
//!   - a untagged tuple, represented as
//!     - for each item
//!       - the token;
//!   - a tagged tuple, represented as
//!     - an entry in the grammar table (`varnum`);
//!     - for each field
//!       - the token

/// Implementation of the token reader.
mod read;

/// Implementation of the token writer.
mod write;

/// The header of the strings table section.
const HEADER_STRINGS_TABLE : &str = "[STRINGS]";

/// The header of the grammars table section.
const HEADER_GRAMMAR_TABLE: &str = "[GRAMMAR]";

/// The header of the tree section.
const HEADER_TREE: &str = "[TREE]";

/// A trait specifying whether a piece of data needs the addition of a length index.
trait FormatInTable {
    const HAS_LENGTH_INDEX : bool;
}


impl FormatInTable for Option<String> {
    const HAS_LENGTH_INDEX : bool = false;
}

pub use self::read::TreeTokenReader;
pub use self::write::{ TreeTokenWriter, Statistics, WriteOptions };


#[test]
fn test_multipart_io() {
    println!("Multipart (de)tokenizer test starting");
    extern crate env_logger;
    env_logger::init();

    use io::{ Guard, TokenReader, TokenWriter };
    use multipart::*;

    use std::fs::*;
    use std::io::{ Cursor, Write };

    // All combinations of options for compression.
    let all_options = {
        use bytes::compress::Compression::*;
        let mut vec = vec![];
        let compressions = [Identity, Gzip, Deflate, /*Lzw, Brotli don't work yet*/];
        for grammar_table in &compressions {
            for strings_table in &compressions {
                for tree in &compressions {
                    vec.push(WriteOptions {
                        grammar_table: grammar_table.clone(),
                        strings_table: strings_table.clone(),
                        tree: tree.clone(),
                    });
                }
            }
        }
        vec
    };

    for options in all_options {
        println!("Options {:?}", options);
        let suffix = format!("{:?}-{:?}-{:?}", options.grammar_table, options.strings_table, options.tree);


        {
            let mut writer : ::multipart::TreeTokenWriter = TreeTokenWriter::new(options.clone());
            writer.string(Some("simple string"))
                .expect("Writing simple string");

            let (output, _) = writer.done()
                .expect("Finalizing data");
            File::create(format!("/tmp/test-simple-string-{}.binjs", suffix)).unwrap()
                .write_all(&output).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output))
                .expect("Creating reader");
            let simple_string = reader.string()
                .expect("Reading simple string")
                .expect("Non-null string");
            assert_eq!(&simple_string, "simple string");
        }


        {
            let data = "string with escapes \u{0}\u{1}\u{0}";
            let mut writer = TreeTokenWriter::new(options.clone());
            writer.string(Some(data))
                .expect("Writing string with escapes");

            let (output, _) = writer.done()
                .expect("Finalizing data");
            File::create(format!("/tmp/test-string-with-escapes-{}.binjs", suffix)).unwrap()
                .write_all(&output).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let escapes_string = reader.string()
                .expect("Reading string with escapes")
                .expect("Non-null string");
            assert_eq!(&escapes_string, data);
        }

        println!("Testing untagged tuple I/O");

        {
            let mut writer = TreeTokenWriter::new(options.clone());
            writer.untagged_tuple(&[])
                .expect("Writing empty untagged tuple");

            let (output, _) = writer.done()
                .expect("Finalizing data");
            File::create(format!("/tmp/test-empty-untagged-tuple-{}.binjs", suffix)).unwrap()
                .write_all(&output).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let guard = reader.untagged_tuple()
                .expect("Reading empty untagged tuple");

            guard.done()
                .expect("Empty untagged tuple read properly");

        }

        {
            let mut writer = TreeTokenWriter::new(options.clone());
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.string(Some("bar")).unwrap();
            writer.untagged_tuple(&[item_0, item_1])
                .expect("Writing trivial untagged tuple");

            let (output, _) = writer.done()
                .expect("Finalizing data");
            File::create(format!("/tmp/test-trivial-untagged-tuple-{}.binjs", suffix)).unwrap()
                .write_all(&output).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let guard = reader.untagged_tuple()
                .expect("Reading trivial untagged tuple");
            let simple_string = reader.string()
                .expect("Reading trivial tuple[0]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "foo");
            let simple_string = reader.string()
                .expect("Reading trivial tuple[1]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "bar");

            guard.done()
                .expect("Untagged tuple read properly");
        }

        println!("Testing tagged tuple I/O");

        {
            let mut writer = TreeTokenWriter::new(options.clone());
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.string(Some("bar")).unwrap();
            let item_2 = writer.float(Some(3.1415)).unwrap();
            writer.tagged_tuple("some tuple", &[
                ("abc", item_0),
                ("def", item_1),
                ("value", item_2)
            ])
                .expect("Writing trivial tagged tuple");

            let (output, _) = writer.done()
                .expect("Finalizing data");
            File::create(format!("/tmp/test-simple-tagged-tuple-{}.binjs", suffix)).unwrap()
                .write_all(&output).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let (name, fields, guard) = reader.tagged_tuple()
                .expect("Reading trivial tagged tuple");
            assert_eq!(name, "some tuple".to_string());

            assert_eq!(fields, None);
            let simple_string_1 = reader.string()
                .expect("Reading trivial tagged tuple[0]")
                .expect("Reading a non-null string");
            let simple_string_2 = reader.string()
                .expect("Reading trivial tagged tuple[1]")
                .expect("Reading a non-null string");
            let simple_float = reader.float()
                .expect("Reading trivial tagged tuple[2]")
                .expect("Reading a non-null float");

            guard.done()
                .expect("Trivial tagged tuple read properly");

            assert_eq!(&simple_string_1, "foo");
            assert_eq!(&simple_string_2, "bar");
            assert_eq!(simple_float, 3.1415);

        }

        println!("Testing list I/O");

        {
            let mut writer = TreeTokenWriter::new(options.clone());
            writer.list(vec![])
                .expect("Writing empty list");

            let (output, _) = writer.done()
                .expect("Finalizing data");
            File::create(format!("/tmp/test-empty-list-{}.binjs", suffix)).unwrap()
                    .write_all(&output).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let (len, guard) = reader.list()
                .expect("Reading empty list");
            assert_eq!(len, 0);

            guard.done()
                .expect("Empty list read properly");
        }

        {
            let mut writer = TreeTokenWriter::new(options.clone());
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.string(Some("bar")).unwrap();
            writer.list(vec![item_0, item_1])
                .expect("Writing trivial list");

            let (output, _) = writer.done()
                .expect("Finalizing data");
            File::create(format!("/tmp/test-trivial-list-{}.binjs", suffix)).unwrap()
                .write_all(&output).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let (len, guard) = reader.list()
                .expect("Reading trivial list");
            assert_eq!(len, 2);

            let simple_string = reader.string()
                .expect("Reading trivial list[0]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "foo");
            let simple_string = reader.string()
                .expect("Reading trivial list[1]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "bar");

            guard.done()
                .expect("Trivial list read properly");
        }

        {
            let mut writer = TreeTokenWriter::new(options.clone());
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.string(Some("bar")).unwrap();
            let list = writer.list(vec![item_0, item_1])
                .expect("Writing inner list");
            writer.list(vec![list])
                .expect("Writing outer list");

            let (output, _) = writer.done()
                .expect("Finalizing data");
            File::create(format!("/tmp/test-nested-lists-{}.binjs", suffix)).unwrap()
                .write_all(&output).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(&output)).unwrap();
            let (len, guard) = reader.list()
                .expect("Reading outer list");
            assert_eq!(len, 1);

            let (len, inner_guard) = reader.list()
                .expect("Reading inner list");
            assert_eq!(len, 2);

            let simple_string = reader.string()
                .expect("Reading trivial list[0]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "foo");
            let simple_string = reader.string()
                .expect("Reading trivial list[1]")
                .expect("Non-null string");
            assert_eq!(&simple_string, "bar");

            inner_guard.done()
                .expect("Inner list read properly");
            guard.done()
                .expect("Inner list read properly");

        }
    }
}

