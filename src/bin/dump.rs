//! Dump a BinJS file structure to stdout.

extern crate binjs;
extern crate clap;
extern crate env_logger;

use binjs::io::Deserialization;
use binjs::io::FileStructurePrinter;

use std::fs::*;
use std::io::*;

use clap::*;

fn main() {
    env_logger::init();

    let matches = App::new("BinJS dumper")
        .author("Tooru Fujisawa [:arai], <arai@mozilla.com>,\n\
                 David Teller, <dteller@mozilla.com>")
        .about("Dump a JavaScript BinJS file structure to stdout.")
        .args(&[
            Arg::with_name("INPUT")
                .required(true)
                .help("Input file to use. Must be a BinJS source file.")
        ])
    .get_matches();

    let source_path = matches.value_of("INPUT")
        .expect("Expected input file");

    println!("Reading.");
    let file = File::open(source_path)
        .expect("Could not open source");
    let stream = BufReader::new(file);

    println!("Attempting to decode as multipart.");
    if let Ok(mut reader) = binjs::io::multipart::TreeTokenReader::new(stream) {
        reader.enable_file_structure_print();
        let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);
        let _tree : binjs::specialized::es6::ast::Script = deserializer.deserialize(&mut binjs::specialized::es6::ast::IOPath::new())
            .expect("Could not decode");
    } else {
        println!("not supported format.");
    };
}
