//! Encode a text source to a BinJS.

extern crate binjs;
extern crate clap;
extern crate env_logger;

use binjs::bytes::compress::*;
use binjs::token::encode::*;
use binjs::source::*;

use std::fs::*;
use std::io::*;
use std::path::{ Path, PathBuf };

use clap::*;


fn main() {
    env_logger::init().unwrap();

    let matches = App::new("BinJS encoder")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Encode a JavaScript text source to a JavaScript binary source in the BinJS format.")
        .args(&[
            Arg::with_name("in")
                .long("in")
                .short("i")
                .multiple(true)
                .takes_value(true)
                .help("Input files to use. Must be JS source file."),
            Arg::with_name("dir")
                .long("dir")
                .takes_value(true)
                .help("Output directory to use. Files in this directory may be overwritten. Required if `in` is specified more than once."),
            Arg::with_name("out")
                .long("out")
                .short("o")
                .takes_value(true)
                .help("Output file to use. Will be overwritten. Not possible if `in` is specified more than once."),
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
            Arg::with_name("sections")
                .long("sections")
                .takes_value(true)
                .possible_values(&["identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for all sections. Defaults to identity."),
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
                .long("show-stats")
                .help("Show statistics.")
        ])
        .group(ArgGroup::with_name("multipart")
            .args(&["strings", "grammar", "tree"])
            .multiple(true)
        )
        .get_matches();

    let sources : Vec<_> = matches.values_of("in")
        .expect("Missing `in`")
        .collect();

    let dest_path =
        if sources.len() > 1 {
            if matches.value_of("out").is_some() {
                panic!("Cannot specify a single destination path with several sources");
            }
            None
        } else {
            matches.value_of("out")
        };

    let dest_dir = matches.value_of("dir")
        .or_else(|| {
            if sources.len() > 1 || dest_path.is_none() {
                Some(".")
            } else {
                None
            }
        });


    let compression = {
        let mut is_compressed = false;
        if matches.values_of("sections").is_some()
        || matches.value_of("strings").is_some()
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
            if let Some(ref compression) = matches.value_of("sections") {
                let compression = Compression::parse(Some(compression))
                    .expect("Could not parse sections compression format");
                Some(binjs::token::multipart::WriteOptions {
                    strings_table: compression.clone(),
                    grammar_table: compression.clone(),
                    tree: compression,
                })
            } else {
                let strings = Compression::parse(matches.value_of("strings"))
                    .expect("Could not parse string compression format");
                let grammar = Compression::parse(matches.value_of("grammar"))
                    .expect("Could not parse grammar compression format");
                let tree = Compression::parse(matches.value_of("tree"))
                    .expect("Could not parse tree compression format");
                Some(binjs::token::multipart::WriteOptions {
                    strings_table: strings,
                    grammar_table: grammar,
                    tree
                })
            }
        } else {
            println!("Format: simple");
            None
        }
    };
    let show_stats = matches.is_present("statistics");

    // Setup.
    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::Latest);

    let mut multipart_stats = binjs::token::multipart::Statistics::default()
        .with_source_bytes(0);
    let mut simple_stats = binjs::token::simple::Statistics::default();

    for source_path in sources {
        println!("Treating {}", source_path);
        let dest_path = match dest_path {
            Some(ref x) => Some(x.to_string()),
            None => match dest_dir {
                None => None, // Do not write
                Some(ref d) => {
                    let source = Path::new(source_path);
                    let file_name = source.file_stem()
                        .expect("Could not extract file name");
                    let mut path = PathBuf::new();
                    path.push(d);
                    path.push(file_name);
                    path.set_extension("binjs");
                    Some(path.to_str()
                        .expect("Could not convert path to string")
                        .to_string())
                }
            }
        };

        if let Some(ref dest) = dest_path {
            println!("Output: {}", dest);
        } else {
            println!("Compressing to memory");
        }

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
        let data: Box<AsRef<[u8]>> = {
            match compression {
                None => {
                    let writer = binjs::token::simple::TreeTokenWriter::new();
                    let encoder = binjs::token::encode::Encoder::new(&grammar, writer);
                    encoder
                        .encode(&ast)
                        .expect("Could not encode AST");
                    let (data, stats) = encoder.done()
                        .expect("Could not finalize AST encoding");

                    simple_stats = simple_stats + stats;
                    Box::new(data)
                }
                Some(ref options) => {
                    let writer = binjs::token::multipart::TreeTokenWriter::new(options.clone(), &grammar);
                    let encoder = binjs::token::encode::Encoder::new(&grammar, writer);
                    encoder
                        .encode(&ast)
                        .expect("Could not encode AST");
                    let (data, stats) = encoder.done()
                        .expect("Could not finalize AST encoding");

                    multipart_stats = multipart_stats + stats.with_source_bytes(source_len as usize);
                    Box::new(data)
                }
            }
        };

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
    }

    if show_stats {
        if compression.is_none() {
            println!("Statistics: {}", simple_stats);
        } else {
            println!("Statistics: {}", multipart_stats);
        }
    }
}