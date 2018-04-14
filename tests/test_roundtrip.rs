//! Encode a BinJS, then decode it, ensure that we obtain the same AST.

extern crate binjs;
extern crate env_logger;
extern crate glob;
#[macro_use]
extern crate log;

use binjs::generic::*;
use binjs::io::bytes::compress::*;
use binjs::io::multipart::*;
use binjs::io::*;
use binjs::source::*;
use binjs::specialized::es6::ast::Walker;

use std::io::*;

const PATHS : [&'static str; 1] = [/* "tests/data/facebook/single/**/*.js", */ "tests/data/spidermonkey/ecma_2/**/*.js"];

fn progress() {
    // Make sure that we see progress in the logs, without spamming these logs.
    eprint!(".");
}

#[test]
fn test_roundtrip() {

    env_logger::init();

    let parser = Shift::new();

    // All combinations of options for compression.
    let all_options = {
        use self::Compression::*;
        let mut vec = vec![];
        let compressions = [Identity, Gzip, /*Deflate seems broken upstream,*/ Brotli, /*Lzw doesn't work yet*/];
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

    print!("\nTesting roundtrip with laziness");

    for path_suffix in &PATHS {
        let path = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), path_suffix);
        debug!(target: "test_roundtrip", "Starting laziness test_roundtrip from {}", path);

        for entry in glob::glob(&path)
            .expect("Invalid glob pattern")
        {
            let entry = entry.expect("Invalid entry");

            // Parse and preprocess file.

            print!("\nPreparing laziness test for {:?}.", entry);
            let mut ast    = parser.parse_file(entry.clone())
                .expect("Could not parse source");

            let mut ast = binjs::specialized::es6::ast::Script::import(&ast)
                .expect("Could not import AST");
            binjs::specialized::es6::scopes::AnnotationVisitor::new()
                .annotate_script(&mut ast);

            // Keep immutable.
            let reference_ast = ast;
            for level in &[0, 1, 2, 3, 4, 5] {
                let mut ast = reference_ast.clone();
                print!("\nLaziness level: {}", level);
                debug!(target: "test_roundtrip", "Laziness level {}", level);

                let mut path = binjs::specialized::es6::ast::Path::new();
                let mut visitor = binjs::specialized::es6::skip::LazifierVisitor::new(*level);
                ast.walk(&mut path, &mut visitor)
                    .expect("Could not introduce laziness");
                progress();

                let options = WriteOptions {
                    grammar_table: Compression::Identity,
                    strings_table: Compression::Identity,
                    tree: Compression::Identity,
                };
                debug!(target: "test_roundtrip", "Encoding.");
                let writer  = binjs::io::multipart::TreeTokenWriter::new(options.clone());
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, _) = serializer.done()
                    .expect("Could not finalize AST encoding");
                progress();

                debug!(target: "test_roundtrip", "Decoding.");
                let source = Cursor::new(data);
                let reader = binjs::io::multipart::TreeTokenReader::new(source)
                    .expect("Could not decode AST container");
                let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);

                let decoded = deserializer.deserialize()
                    .expect("Could not decode");
                progress();

                debug!(target: "test_roundtrip", "Checking.");
                assert_eq!(ast, decoded);
            }
        }
    }

    for path_suffix in &PATHS {
        let path = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), path_suffix);
        debug!(target: "test_roundtrip", "Starting test_roundtrip from {}", path);

        for entry in glob::glob(&path)
            .expect("Invalid glob pattern")
        {
            let entry = entry.expect("Invalid entry");

            // Parse and preprocess file.

            print!("\nParsing {:?}.", entry);
            let mut ast    = parser.parse_file(entry.clone())
                .expect("Could not parse source");
            debug!(target: "test_roundtrip", "Source: {}", ast.pretty(2));
            debug!(target: "test_roundtrip", "Annotating {:?}.", entry);

            let mut ast = binjs::specialized::es6::ast::Script::import(&ast)
                .expect("Could not import AST");
            binjs::specialized::es6::scopes::AnnotationVisitor::new()
                .annotate_script(&mut ast);

            {
                progress();
                debug!(target: "test_roundtrip", "Starting simple round trip for {:?}", entry);

                // Roundtrip `simple`
                debug!(target: "test_roundtrip", "Encoding");
                let mut writer = binjs::io::simple::TreeTokenWriter::new();
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, _) = serializer.done()
                    .expect("Could not finalize AST encoding");

                progress();
                debug!(target: "test_roundtrip", "Decoding.");
                let source = Cursor::new(data);
                let reader = binjs::io::simple::TreeTokenReader::new(source);
                let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);

                let decoded = deserializer.deserialize()
                    .expect("Could not decode");
                progress();

                debug!(target: "test_roundtrip", "Checking.");
                assert_eq!(ast, decoded);

                debug!(target: "test_roundtrip", "Completed simple round trip for {:?}", entry);
            }

            // Roundtrip `multipart`

            for options in &all_options {
                progress();
                debug!(target: "test_roundtrip", "Starting multipart round trip for {:?} with options {:?}", entry, options);
                debug!(target: "test_roundtrip", "Encoding.");
                let writer  = binjs::io::multipart::TreeTokenWriter::new(options.clone());
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, _) = serializer.done()
                    .expect("Could not finalize AST encoding");

                progress();

                debug!(target: "test_roundtrip", "Decoding.");
                let source = Cursor::new(data);
                let reader = binjs::io::multipart::TreeTokenReader::new(source)
                    .expect("Could not decode AST container");
                let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);

                let decoded = deserializer.deserialize()
                    .expect("Could not decode");
                progress();

                debug!(target: "test_roundtrip", "Checking.");
                assert_eq!(ast, decoded);

                debug!(target: "test_roundtrip", "Completed multipart round trip for {:?} with options {:?}", entry, options);
                progress();
            }
        }
    }
}

