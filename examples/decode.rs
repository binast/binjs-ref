//! Decode a BinJS to a text source.

extern crate binjs;
extern crate binjs_es6;
extern crate binjs_io;
extern crate binjs_generic;
extern crate binjs_meta;
extern crate clap;
extern crate env_logger;


use binjs::source::Shift;
use binjs_meta::spec::{ SpecBuilder, SpecOptions };

use std::fs::*;
use std::io::*;

use clap::*;

fn main() {
    env_logger::init();

    let matches = App::new("BinJS decoder")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Decode a JavaScript BinJS source to a JavaScript text source.")
        .args(&[
            Arg::with_name("INPUT")
                .required(true)
                .help("Input file to use. Must be a BinJS source file."),
            Arg::with_name("OUTPUT")
                .required(true)
                .help("Output file to use. Will be overwritten."),
            Arg::with_name("dump")
                .long("dump")
                .takes_value(false)
                .help("If specified, dump a JSON version of the AST.")
        ])
    .get_matches();

    let source_path = matches.value_of("INPUT")
        .expect("Expected input file");
    let dest_path = matches.value_of("OUTPUT")
        .expect("Expected output file");

    // Setup.
    let parser = Shift::new();
    let mut spec_builder = SpecBuilder::new();
    let library = binjs_generic::es6::Library::new(&mut spec_builder);
    let spec = spec_builder.into_spec(SpecOptions {
        null: &library.null,
        root: &library.program,
    });

    println!("Reading.");
    let file = File::open(source_path)
        .expect("Could not open source");
    let stream = BufReader::new(file);

    println!("Attempting to decode as multipart.");
    let tree = if let Ok(reader) = binjs_io::multipart::TreeTokenReader::new(stream) {
        let mut decoder = binjs_generic::io::decode::Decoder::new(&spec, reader);
        decoder.decode()
            .expect("Could not decode")
    } else {
        println!("... falling back to simple format.");

        let file = File::open(source_path)
            .expect("Could not open source");
        let stream = BufReader::new(file);

        let reader = binjs_io::simple::TreeTokenReader::new(stream);
        let mut decoder = binjs_generic::io::decode::Decoder::new(&spec, reader);
        decoder.decode()
            .expect("Could not decode")
    };

    if matches.is_present("print-json") {
        println!("Printing to screen...");
        let pretty = tree.pretty(2);
        println!("{}", pretty);
    }

    println!("Pretty-printing");
    let source = parser.to_source(&spec, &tree)
        .expect("Could not pretty-print");

    println!("Writing.");
    let mut dest = File::create(dest_path)
        .expect("Could not create destination file");
    dest.write(source.as_bytes())
        .expect("Could not write destination file");
}
