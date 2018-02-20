//! Encode a BinJS, then decode it, ensure that we obtain the same AST.

extern crate binjs;
extern crate binjs_io;
extern crate env_logger;
extern crate glob;
#[macro_use]
extern crate log;

use binjs::generic::io::encode::*;
use binjs::generic::syntax::*;
use binjs::io::bytes::compress::*;
use binjs::io::multipart::*;
use binjs::meta::spec::*;
use binjs::source::*;

use std::io::*;

#[test]
fn test_roundtrip() {

    env_logger::init();

    let parser = Shift::new();
    let mut spec_builder = SpecBuilder::new();
    let library = binjs::generic::es6::Library::new(&mut spec_builder);
    let spec = spec_builder.into_spec(SpecOptions {
        null: &library.null,
        root: &library.program,
    });

    // All combinations of options for compression.
    let all_options = {
        use self::Compression::*;
        let mut vec = vec![];
        let compressions = [Identity, Gzip, Deflate, Brotli, /*Lzw doesn't work yet*/];
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

    let path = format!("{}/tests/data/spidermonkey/ecma_2/**/*.js", env!("CARGO_MANIFEST_DIR"));
    println!("Starting test_roundtrip from {}", path);

    for entry in glob::glob(&path)
        .expect("Invalid glob pattern")
    {
        let entry = entry.expect("Invalid entry");

        // Parse and preprocess file.

        println!("Parsing {:?}.", entry);
        let mut ast    = parser.parse_file(entry.clone())
            .expect("Could not parse source");
        debug!(target: "test_roundtrip", "Source: {}", ast.pretty(2));
        println!("Annotating {:?}.", entry);
        library.annotate(&mut ast);

        {
            println!("Starting simple round trip for {:?}", entry);

            // Roundtrip `simple`
            println!("Encoding");
            let writer  = binjs::io::simple::TreeTokenWriter::new();
            let encoder = binjs::generic::io::encode::Encoder::new(&spec, writer);

            encoder.encode(&ast)
                .expect("Could not encode AST");
            let (data, _) = encoder.done()
                .expect("Could not finalize AST encoding");

            println!("Decoding.");
            let source = Cursor::new(data.as_ref().clone());
            let reader = binjs::io::simple::TreeTokenReader::new(source);
            let mut decoder = binjs::generic::io::decode::Decoder::new(&spec, reader);

            let decoded = decoder.decode()
                .expect("Could not decode");

            println!("Checking.");
            let equal = Comparator::compare(&spec, &ast, &decoded)
                .expect("Could not compare ASTs");
            assert!(equal);

            println!("Completed simple round trip for {:?}", entry);
        }

        // Roundtrip `multipart`

        for options in &all_options {
            println!("Starting multipart round trip for {:?} with options {:?}", entry, options);
            println!("Encoding.");
            let writer  = binjs::io::multipart::TreeTokenWriter::new(options.clone());
            let encoder = binjs::generic::io::encode::Encoder::new(&spec, writer);

            encoder.encode(&ast)
                .expect("Could not encode AST");
            let (data, _) = encoder.done()
                .expect("Could not finalize AST encoding");

            println!("Decoding.");
            let source = Cursor::new(data.as_ref().clone());
            let reader = binjs::io::multipart::TreeTokenReader::new(source)
                .expect("Could not decode AST container");
            let mut decoder = binjs::generic::io::decode::Decoder::new(&spec, reader);

            let decoded = decoder.decode()
                .expect("Could not decode");

            println!("Checking.");
            let equal = Comparator::compare(&spec, &ast, &decoded)
                .expect("Could not compare ASTs");
            assert!(equal);
            println!("Completed multipart round trip for {:?} with options {:?}", entry, options);
        }
    }
}

