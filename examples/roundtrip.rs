//! Encode a BinJS, then decode it, ensure that we obtain the same AST.

extern crate binjs;
extern crate clap;
extern crate env_logger;

use clap::*;

use binjs::bytes::compress::*;
use binjs::source::*;
use binjs::token::encode::*;

use std::io::*;


fn parse_compression(name: Option<&str>) -> Compression {
    match name {
        None | Some("identity") => Compression::Identity,
        Some("lzw") => Compression::Lzw,
        Some("br") => Compression::Brotli,
        Some("gzip") => Compression::Gzip,
        Some("deflate") => Compression::Deflate,
        Some(x) => panic!("Unexpected compression name {}", x)
    }
}

fn main() {
    env_logger::init().unwrap();

    let matches = App::new("BinJS roundtrip tester")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Check that encoding + decoding a file yields the same AST.")
        .args(&[
            Arg::with_name("INPUT")
                .required(true)
                .help("Input file to use. Must be a JS source file."),
            Arg::with_name("format")
                .long("format")
                .takes_value(true)
                .possible_values(&["simple", "multipart"])
                .help("Format to use for writing to OUTPUT. Defaults to simple unless --strings, --grammar or --tree is specified."),
            Arg::with_name("strings")
                .long("strings")
                .takes_value(true)
                .possible_values(&["identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for strings. Defaults to identity."),
            Arg::with_name("grammar")
                .long("grammar")
                .takes_value(true)
                .possible_values(&["identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for the grammar table. Defaults to identity."),
            Arg::with_name("tree")
                .long("tree")
                .takes_value(true)
                .possible_values(&["identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for the tree. Defaults to identity."),
            Arg::with_name("statistics")
                .long("stat")
                .help("Show statistics."),
        ])
        .group(ArgGroup::with_name("multipart")
            .args(&["strings", "grammar", "tree"])
            .multiple(true)
        )
        .get_matches();

    let source_path = matches.value_of("INPUT")
        .expect("Expected input file");

    let compression = {
        let mut is_compressed = false;
        if matches.value_of("strings").is_some()
        || matches.value_of("grammar").is_some()
        || matches.value_of("tree").is_some() {
            match matches.value_of("format") {
                None | Some("multipart") => {
                    is_compressed = true;
                }
                _ => {
                    println!("Error: Cannot specify `strings`, `grammar` or `tree` with this format.\n{}", matches.usage());
                    std::process::exit(-1);
                }
            }
        }
        if let Some("multipart") = matches.value_of("format") {
            is_compressed = true;
        }
        if is_compressed {
            let strings = parse_compression(matches.value_of("strings"));
            let grammar = parse_compression(matches.value_of("grammar"));
            let tree = parse_compression(matches.value_of("tree"));

            println!("Format: multipart\n\tstrings table: {:?}\n\tgrammar table: {:?}\n\ttree: {:?}", strings, grammar, tree);
            Some(binjs::token::multipart::WriteOptions {
                strings_table: strings,
                grammar_table: grammar,
                tree
            })
        } else {
            println!("Format: simple");
            None
        }
    };

    let show_stats = matches.is_present("statistics");

    println!("Applying roundtrip to {}", source_path);

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::Latest);

    println!("Parsing.");
    let mut ast    = parser.parse_file(source_path)
        .expect("Could not parse source");

    println!("Annotating.");
    grammar.annotate(&mut ast)
        .expect("Could not infer annotations");

    let decoded = match compression {
        None => {
            println!("Encoding.");
            let writer  = binjs::token::simple::TreeTokenWriter::new();
            let encoder = binjs::token::encode::Encoder::new(&grammar, writer);

            encoder.encode(&ast)
                .expect("Could not encode AST");
            let (data, _) = encoder.done()
                .expect("Could not finalize AST encoding");

            println!("Decoding.");
            let source = Cursor::new(data.as_ref().clone());
            let reader = binjs::token::simple::TreeTokenReader::new(source, &grammar);
            let mut decoder = binjs::token::decode::Decoder::new(&grammar, reader);

            decoder.decode()
                .expect("Could not decode")
        }
        Some(options) => {
            println!("Encoding.");
            let writer  = binjs::token::multipart::TreeTokenWriter::new(options, &grammar);
            let encoder = binjs::token::encode::Encoder::new(&grammar, writer);

            encoder.encode(&ast)
                .expect("Could not encode AST");
            let (data, stats) = encoder.done()
                .expect("Could not finalize AST encoding");

            if show_stats {
                println!("Statistics: {}", stats);
            }

            println!("Decoding.");
            let source = Cursor::new(data.as_ref().clone());
            let reader = binjs::token::multipart::TreeTokenReader::new(source, &grammar)
                .expect("Could not decode AST container");
            let mut decoder = binjs::token::decode::Decoder::new(&grammar, reader);

            decoder.decode()
                .expect("Could not decode")
        }
    };

    println!("Checking.");
    let equal = grammar.compare(&ast, &decoded)
        .expect("Could not compare ASTs");
    assert!(equal);

    println!("Roundtrip success!");
}