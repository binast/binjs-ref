//! Generate a believed-to-be-correct JS file and its encoding.
//!
//! Note that the JS file is only correct insofar as the AST matches the grammar.


extern crate binjs;

extern crate clap;
extern crate env_logger;
extern crate rand;

const DEFAULT_TREE_SIZE: isize = 5;


use binjs::source::Shift;
use binjs::generic::io::encode::Encode;
use binjs::generic::pick::{ Pick, Picker };

use clap::*;
use rand::Rand;

use std::io::Write;

fn main() {
    env_logger::init();

    let matches = App::new("BinJS source file generator")
        .author("David Teller <dteller@mozilla.com>")
        .about(
r#"Generate large numbers of believed-to-be-correct-files, both in text source code and binjs.
Note that this tool does not attempt to make sure that the files are entirely correct, only that they match BinJS's AST."#)
        .args(&[
            Arg::with_name("PREFIX")
                .required(true)
                .help("A prefix for generated files."),
            Arg::with_name("format")
                .long("format")
                .takes_value(true)
                .possible_values(&["simple", "multipart"])
                .help("Format to use for writing the files. If multipart, the choice of internal compression algorithms is randomized. If unspecified, defaults to simple."),
            Arg::with_name("number")
                .long("number")
                .short("n")
                .takes_value(true)
                .required(true)
                .help("Number of files to generate."),
            Arg::with_name("size")
                .long("size")
                .takes_value(true)
                .help("Expected file size (in AST depth). Default: 5."),
            Arg::with_name("random-ast-metadata")
                .long("random-metadata")
                .help("If specified, generate random ast metadata (declared variables, etc.).")
        ])
        .get_matches();
    let is_multipart =
        if let Some("multipart") = matches.value_of("format") {
            true
        } else {
            false
        };

    let prefix = matches.value_of("PREFIX")
        .expect("Missing argument `PREFIX`");

    let number = matches.value_of("number")
        .expect("Missing argument `number`");
    let number : usize = number.parse()
        .expect("Invalid number");

    let size : isize = match matches.value_of("size") {
        None => DEFAULT_TREE_SIZE,
        Some(size) => size.parse()
            .expect("Invalid size")
    };

    let mut builder = binjs::meta::spec::SpecBuilder::new();
    let library = binjs::generic::es6::Library::new(&mut builder);
    let spec = builder.into_spec(binjs::meta::spec::SpecOptions {
        root: &library.program,
        null: &library.null
    });

    let random_metadata = matches.is_present("random-metadata");

    let mut rng = rand::thread_rng();
    let parser = Shift::new();

    let mut i = 0;
    loop {
        if i >= number {
            break;
        }
        let mut ast = Picker.random(&spec, &mut rng, size);

        if !random_metadata {
            // Overwrite random annotations.
            library.annotate(&mut ast);
        }

        if let Ok(source) = parser.to_source(&spec, &ast) {
            i += 1;

            println!("Generated sample {}/{}", i, number);
            {
                let mut source_file = std::fs::File::create(format!("{}-{}.js", prefix, i))
                    .expect("Could not create js file");
                source_file.write_all(source.as_bytes())
                    .expect("Could not write js file");
            }


            let encoded =
                if is_multipart {
                    let options = binjs::io::multipart::WriteOptions::rand(&mut rng);
                    let writer = binjs::io::multipart::TreeTokenWriter::new(options);
                    let encoder = binjs::generic::io::encode::Encoder::new(&spec, writer);
                    encoder
                        .encode(&ast)
                        .expect("Could not encode AST");
                    encoder.done()
                        .map(|(data, _)| Box::new(data) as Box<AsRef<[u8]>>)
                } else {
                    let writer = binjs::io::simple::TreeTokenWriter::new();
                    let encoder = binjs::generic::io::encode::Encoder::new(&spec, writer);
                    encoder
                        .encode(&ast)
                        .expect("Could not encode AST");
                    encoder.done()
                        .map(|(data, _)| Box::new(data) as Box<AsRef<[u8]>>)
                };

            let encoded = encoded
                .expect("Could not finalize encoding");
            {
                let mut encoded_file = std::fs::File::create(format!("{}-{}.binjs", prefix, i))
                    .expect("Could not create binjs file");
                encoded_file.write_all(encoded.as_ref().as_ref())
                    .expect("Could not write binjs file");
            }
        } else {
            println!("...could not pretty print this sample");
            continue;
        }
    }
}
