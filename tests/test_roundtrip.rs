//! Encode a BinJS, then decode it, ensure that we obtain the same AST.

extern crate binjs;
extern crate glob;
extern crate env_logger;

use binjs::bytes::compress::*;
use binjs::source::*;
use binjs::token::encode::*;
use binjs::token::multipart::*;

use std::io::*;

#[test]
fn test_roundtrip() {

    env_logger::init().unwrap();

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::Latest);

    // All combinations of options for compression.
    let all_options = {
        use self::Compression::*;
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

        println!("Annotating {:?}.", entry);
        grammar.annotate(&mut ast)
            .expect("Could not infer annotations");

        {
            println!("Starting simple round trip for {:?}", entry);

            // Roundtrip `simple`
            println!("Encoding");
            let writer  = binjs::token::simple::TreeTokenWriter::new();
            let encoder = binjs::token::encode::Encoder::new(&grammar, writer);

            encoder.encode(&ast)
                .expect("Could not encode AST");
            let data = encoder.done()
                .expect("Could not finalize AST encoding");

            println!("Decoding.");
            let source = Cursor::new(data.as_ref().clone());
            let reader = binjs::token::simple::TreeTokenReader::new(source, &grammar);
            let mut decoder = binjs::token::decode::Decoder::new(&grammar, reader);

            let decoded = decoder.decode()
                .expect("Could not decode");

            println!("Checking.");
            let equal = grammar.compare(&ast, &decoded)
                .expect("Could not compare ASTs");
            assert!(equal);

            println!("Completed simple round trip for {:?}", entry);
        }

        // Roundtrip `multipart`

        for options in &all_options {
            println!("Starting multipart round trip for {:?} with options {:?}", entry, options);
            println!("Encoding.");
            let writer  = binjs::token::multipart::TreeTokenWriter::new(options.clone(), &grammar);
            let encoder = binjs::token::encode::Encoder::new(&grammar, writer);

            encoder.encode(&ast)
                .expect("Could not encode AST");
            let data = encoder.done()
                .expect("Could not finalize AST encoding");

            println!("Decoding.");
            let source = Cursor::new(data.as_ref().clone());
            let reader = binjs::token::multipart::TreeTokenReader::new(source, &grammar)
                .expect("Could not decode AST container");
            let mut decoder = binjs::token::decode::Decoder::new(&grammar, reader);

            let decoded = decoder.decode()
                .expect("Could not decode");

            println!("Checking.");
            let equal = grammar.compare(&ast, &decoded)
                .expect("Could not compare ASTs");
            assert!(equal);
            println!("Completed multipart round trip for {:?} with options {:?}", entry, options);
        }
    }
}