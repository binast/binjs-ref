//! Encode a BinJS, then decode it, ensure that we obtain the same AST.

extern crate binjs;
extern crate clap;
extern crate env_logger;

use clap::*;

use binjs::io::bytes::compress::*;
use binjs::io::multipart::SectionOption;
use binjs::meta::spec::*;
use binjs::source::*;
use binjs::generic::io::encode::*;

use std::default::Default;
use std::io::*;


fn parse_compression(name: Option<&str>) -> SectionOption {
    match name {
        None | Some("identity") => SectionOption::Compression(Compression::Identity),
        Some("lzw") => SectionOption::Compression(Compression::Lzw),
        Some("br") => SectionOption::Compression(Compression::Brotli),
        Some("gzip") => SectionOption::Compression(Compression::Gzip),
        Some("deflate") => SectionOption::Compression(Compression::Deflate),
        Some(x) => panic!("Unexpected compression name {}", x)
    }
}

fn main() {
    env_logger::init();

    let matches = App::new("BinJS roundtrip tester")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Check that encoding + decoding a file yields the same AST.")
        .args(&[
            Arg::with_name("in")
                .long("in")
                .multiple(true)
                .takes_value(true)
                .help("Input files to use. Must be JS source file."),
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
            Some(binjs::io::multipart::WriteOptions {
                strings_table: strings,
                grammar_table: grammar,
                tree
            })
        } else {
            println!("Format: simple");
            None
        }
    };

    let files : Vec<_> = matches.values_of("in")
        .unwrap()
        .collect();
    println!("List of files: {:?}", files);

    let show_stats = matches.is_present("statistics");

    let mut multipart_stats = binjs::io::multipart::Statistics::default()
        .with_source_bytes(0);
    let mut simple_stats = binjs::io::simple::Statistics::default();

    let parser = Shift::new();
    let mut spec_builder = SpecBuilder::new();
    let library = binjs::generic::es6::Library::new(&mut spec_builder);
    let spec = spec_builder.into_spec(SpecOptions {
        null: &library.null,
        root: &library.program,
    });

    for source_path in files {
        println!("Parsing {}.", source_path);
        let bytes = std::fs::metadata(source_path)
            .expect("Could not find source path")
            .len() as usize;

        let mut ast = parser.parse_file(source_path)
            .expect("Could not parse source");

        println!("Annotating.");
        library.annotate(&mut ast);

        let decoded = match compression {
            None => {
                println!("Encoding.");
                let writer  = binjs::io::simple::TreeTokenWriter::new();
                let encoder = binjs::generic::io::encode::Encoder::new(&spec, writer);

                encoder.encode(&ast)
                    .expect("Could not encode AST");
                let (data, stats) = encoder.done()
                    .expect("Could not finalize AST encoding");

                simple_stats = simple_stats + stats;

                println!("Decoding.");
                let source = Cursor::new(data);
                let reader = binjs::io::simple::TreeTokenReader::new(source);
                let mut decoder = binjs::generic::io::decode::Decoder::new(&spec, reader);

                decoder.decode()
                    .expect("Could not decode")
            }
            Some(ref options) => {
                println!("Encoding.");
                let writer  = binjs::io::multipart::TreeTokenWriter::new(options.clone());
                let encoder = binjs::generic::io::encode::Encoder::new(&spec, writer);

                encoder.encode(&ast)
                    .expect("Could not encode AST");
                let (data, stats) = encoder.done()
                    .expect("Could not finalize AST encoding");

                multipart_stats = multipart_stats + stats.with_source_bytes(bytes);

                println!("Decoding.");
                let source = Cursor::new(data.as_ref().clone());
                let reader = binjs::io::multipart::TreeTokenReader::new(source)
                    .expect("Could not decode AST container");
                let mut decoder = binjs::generic::io::decode::Decoder::new(&spec, reader);

                decoder.decode()
                    .expect("Could not decode")
            }
        };

        println!("Checking.");
        let equal = binjs::generic::syntax::Comparator::compare(&spec, &ast, &decoded)
            .expect("Could not compare ASTs");
        assert!(equal);

        println!("Roundtrip success!");
    }

    if show_stats {
        if compression.is_none() {
            println!("Statistics: {}", simple_stats);
        } else {
            println!("Statistics: {}", multipart_stats);
        }
    }
}
