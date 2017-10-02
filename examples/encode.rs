//! Encode a text source to a BinJS.

extern crate binjs;
extern crate clap;
extern crate env_logger;

use binjs::bytes::compress::*;
use binjs::token::encode::*;
use binjs::source::*;

use std::fmt::Display;
use std::fs::*;
use std::io::*;

use clap::*;


fn main() {
    env_logger::init().unwrap();

    let matches = App::new("BinJS encoder")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Encode a JavaScript text source to a JavaScript binary source in the BinJS format.")
        .args(&[
            Arg::with_name("INPUT")
                .required(true)
                .help("Input file to use. Must be a JS source file."),
            Arg::with_name("OUTPUT")
                .help("Output file to use. Will be overwritten."),
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
                .help("Show statistics.")
        ])
        .group(ArgGroup::with_name("multipart")
            .args(&["strings", "grammar", "tree"])
            .multiple(true)
        )
        .get_matches();

    let source_path = matches.value_of("INPUT")
        .expect("Expected input file");
    let dest_path = matches.value_of("OUTPUT");
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
            let strings = Compression::parse(matches.value_of("strings"))
                .expect("Could not parse string compression format");
            let grammar = Compression::parse(matches.value_of("grammar"))
                .expect("Could not parse grammar compression format");
            let tree = Compression::parse(matches.value_of("tree"))
                .expect("Could not parse tree compression format");
            println!("Format: multipart\n\tstrings table: {:?}, grammar table: {:?}, tree: {:?}", strings, grammar, tree);
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

    // Setup.
    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::Latest);

    let source_len = std::fs::metadata(source_path)
        .expect("Could not open source")
        .len();

    println!("Parsing.");
    let mut ast    = parser.parse_file(source_path)
        .expect("Could not parse source");

    println!("Annotating.");
    grammar.annotate(&mut ast)
        .expect("Could not infer annotations");

    println!("Encoding.");
    let result = {
        match compression {
            None => {
                let writer = binjs::token::simple::TreeTokenWriter::new();
                let encoder = binjs::token::encode::Encoder::new(&grammar, writer);
                encoder
                    .encode(&ast)
                    .expect("Could not encode AST");
                encoder.done()
                    .map(|(data, stats)| (Box::new(data) as Box<AsRef<[u8]>>, Box::new(stats) as Box<Display>))
            }
            Some(options) => {
                let writer = binjs::token::multipart::TreeTokenWriter::new(options, &grammar);
                let encoder = binjs::token::encode::Encoder::new(&grammar, writer);
                encoder
                    .encode(&ast)
                    .expect("Could not encode AST");
                encoder.done()
                    .map(|(data, stats)| (Box::new(data) as Box<AsRef<[u8]>>, Box::new(stats) as Box<Display>))
            }
        }
    };

    let (data, stats) = result
        .expect("Could not finalize AST encoding");

    let dest_len = data.as_ref().as_ref().len();

    if let Some(ref dest_path) = dest_path {
        println!("Writing.");
        let mut dest = File::create(dest_path)
            .expect("Could not create destination file");
        dest.write((*data).as_ref())
            .expect("Could not write destination file");
    } else {
        println!("Skipping write.");
    }

    println!("Successfully compressed {} bytes => {} bytes", source_len, dest_len);

    if show_stats {
        println!("Statistics: {}", stats);
    }
}