//! A multipart format, in which each part can be compressed independently.
//!
//! # Format
//!
//! The entire file is formatted as:
//!
//! - the characters `"BINJS"`;
//! - a container version number (varnum, currently 0);
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
//! - the number of compressed bytes (varnum);
//! - compressed in the format identified by `prefix`;
//!    - the number of entries (varnum);
//!    - for each entry,
//!      - byte length of entry (varnum);
//!    - for each entry,
//!      - byte length of node name (varnum);
//!      - number of fields (varnum);
//!      - for each field
//!        - byte length of field name (varnum);
//!        - field name (utf-8 encoded string).
//!
//! ## Strings table
//!
//! The grammar table serves to map tagged tuple indices to strings.
//!
//! - the characters `"[STRINGS]"`;
//! - a `prefix` identifying the compression format used for the grammar (one of "identity;", "br;", "gzip;", "compress;", "deflate;").
//! - the number of compressed bytes (varnum);
//! - compressed in the format identified by `prefix`;
//!    - the number of entries (varnum);
//!    - for each entry,
//!      - byte length of string (varnum);
//!      - one of
//!        - the invalid strings [255, 0] (representing the null string, only valid if byte length is 2);
//!        - a utf-8 encoded string of byte length bytes (utf-8 encoded).
//!
//! ## The tree
//!
//! This contains the actual tree for a specific grammar. The file does not contain all the information
//! to determine the nature of next token. Rather, this must be led by the grammar.
//!
//! - the characters `"[TREE]"`;
//! - a `prefix` identifying the compression format used for the grammar (one of "identity;", "br;", "gzip;", "compress;", "deflate;").
//! - the number of compressed bytes (varnum);
//! - one tree token.
//!
//! ### Tree token
//!
//!  A tree token is defined as one of
//!   - a null float, represented as a low-endian IEEE764 64-bit floating point value signalling NaN (8 bytes),
//!   - a non-null float, represented as a low-endian IEEE764 64-bit floating point value non-signalling NaN (8 bytes),
//!   - a null boolean, represented as a single byte with value 2 (one byte);
//!   - a non-null boolean, represented as a single byte with value 0 (false) or 1 (true) (one byte);
//!   - a string, representing an entry in the table of strings (varnum);
//!   - a list, represented as
//!     - byte length (varnum);
//!     - number of items (varnum);
//!     - for each item
//!        - the token;
//!   - a untagged tuple, represented as
//!     - for each item
//!       - the token;
//!   - a tagged tuple, represented as
//!     - an entry in the grammar table (varnum);
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
pub use self::write::{ TreeTokenWriter, WriteOptions };


#[test]
fn test_multipart_io() {
    println!("Multipart (de)tokenizer test starting");
    use ast::annotation::*;
    use ast::grammar::*;
    use token::io::*;

    use std::fs::*;

    use serde_json;
    use serde_json::Value as JSON;

    use std::io::{ Cursor, Write };

    type Object = serde_json::Map<String, JSON>;

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

    debug!("Setting up syntax");
    let mut builder = SyntaxBuilder::new();
    let null = builder.node_name("Null");
    let null_kind = builder.kind_name("Null");
    builder.add_kinded_interface(&null).unwrap();

    let kinded = builder.node_name("Pattern");
    let field_string = Field::new(builder.field_name("id"), Type::string());
    let field_number = Field::new(builder.field_name("value"), Type::number());

    builder.add_kinded_interface(&kinded).unwrap()
        .with_own_field(field_string.clone())
        .with_own_field(field_number.clone());

    struct FakeAnnotator;
    impl Annotator for FakeAnnotator {
        fn name(&self) -> String {
            unimplemented!()
        }
        fn process_references(&self, _: &Annotator, _: &mut Context<RefContents>, _: &mut Object) -> Result<(), ASTError> {
            unimplemented!()
        }
        fn process_declarations(&self, _: &Annotator, _: &mut Context<DeclContents>, _: &mut Object) -> Result<(), ASTError> {
            unimplemented!()
        }
    }

    let syntax = builder.into_syntax(SyntaxOptions {
        root: &kinded,
        null: &null_kind,
        annotator: Box::new(FakeAnnotator)
    });

    for options in all_options {
        println!("Options {:?}", options);
        let suffix = format!("{:?}-{:?}-{:?}", options.grammar_table, options.strings_table, options.tree);

        debug!("Testing string I/O");
        {

            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            writer.string(Some("simple string"))
                .expect("Writing simple string");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-simple-string-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let simple_string = reader.string()
                .expect("Reading simple string")
                .expect("Non-null string");
            assert_eq!(&simple_string, "simple string");
        }

        {
            let string = "string with escapes \u{0}\u{1}\u{0}";
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            writer.string(Some(string))
                .expect("Writing string with escapes");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-string-with-escapes-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let escapes_string = reader.string()
                .expect("Reading string with escapes")
                .expect("Non-null string");
            assert_eq!(escapes_string, string);
        }

        debug!("Testing untagged tuple I/O");

        {
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            writer.untagged_tuple(&[])
                .expect("Writing empty untagged tuple");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-empty-untagged-tuple-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let guard = reader.untagged_tuple()
                .expect("Reading empty untagged tuple");

            guard.done()
                .expect("Empty untagged tuple read properly");

        }

        {
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.string(Some("bar")).unwrap();
            writer.untagged_tuple(&[item_0, item_1])
                .expect("Writing trivial untagged tuple");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-trivial-untagged-tuple-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
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

        debug!("Testing tagged tuple I/O");

        {
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.float(Some(3.1415)).unwrap();
            writer.tagged_tuple(kinded.to_str(), &[(&field_string, item_0), (&field_number, item_1)])
                .expect("Writing trivial tagged tuple");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-simple-tagged-tuple-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let (name, fields, guard) = reader.tagged_tuple()
                .expect("Reading trivial tagged tuple");
            assert_eq!(name, "Pattern".to_string());

            // Order of fields is not deterministic
            if fields[0].name().to_string() == &"id".to_string() {
                assert_eq!(fields[0].name().to_string(), &"id".to_string());
                assert_eq!(*fields[0].type_(), Type::string());
                assert_eq!(fields[1].name().to_string(), &"value".to_string());
                assert_eq!(*fields[1].type_(), Type::number());
                let simple_string = reader.string()
                    .expect("Reading trivial tagged tuple[0]")
                    .expect("Reading a non-null string");
                let simple_float = reader.float()
                    .expect("Reading trivial tagged tuple[1]")
                    .expect("Reading a non-null float");
                assert_eq!(&simple_string, "foo");
                assert_eq!(simple_float, 3.1415);
            } else {
                assert_eq!(fields[1].name().to_string(), &"id".to_string());
                assert_eq!(*fields[1].type_(), Type::string());
                assert_eq!(fields[0].name().to_string(), &"value".to_string());
                assert_eq!(*fields[0].type_(), Type::number());
                let simple_float = reader.float()
                    .expect("Reading trivial tagged tuple[1]")
                    .expect("Reading a non-null float");
                let simple_string = reader.string()
                    .expect("Reading trivial tagged tuple[0]")
                    .expect("Reading a non-null string");
                assert_eq!(&simple_string, "foo");
                assert_eq!(simple_float, 3.1415);
            }

            guard.done()
                .expect("Trivial tagged tuple read properly");
        }

        debug!("Testing list I/O");

        {
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            writer.list(vec![])
                .expect("Writing empty list");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-empty-list-{}.binjs", suffix)).unwrap()
                    .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
            let (len, guard) = reader.list()
                .expect("Reading empty list");
            assert_eq!(len, 0);

            guard.done()
                .expect("Empty list read properly");
        }

        {
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.string(Some("bar")).unwrap();
            writer.list(vec![item_0, item_1])
                .expect("Writing trivial list");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-trivial-list-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
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
            let mut writer = TreeTokenWriter::new(options.clone(), &syntax);
            let item_0 = writer.string(Some("foo")).unwrap();
            let item_1 = writer.string(Some("bar")).unwrap();
            let list = writer.list(vec![item_0, item_1])
                .expect("Writing inner list");
            writer.list(vec![list])
                .expect("Writing outer list");

            let data = writer.done()
                .expect("Extracting data");

            File::create(format!("/tmp/test-nested-lists-{}.binjs", suffix)).unwrap()
                .write_all(&data).unwrap();

            let mut reader = TreeTokenReader::new(Cursor::new(data), &syntax)
                .expect("Opening data");
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

