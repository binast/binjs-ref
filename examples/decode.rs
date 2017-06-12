extern crate binjs;
#[macro_use]
extern crate clap;
extern crate env_logger;

use binjs::ast::grammar::*;
use binjs::source::*;

use std::fs::*;
use std::io::*;

fn main() {
    env_logger::init().unwrap();

    let matches = clap_app!(myapp =>
        (author: "David Teller <dteller@mozilla.com>")
        (about: "Decode a JavaScript BinJS source to a JavaScript AST.")
        (@arg INPUT: +required "Input file to use. Must be a BinJS source file.")
        (@arg OUTPUT: +required "Output file to use. Will be overwritten.")
    ).get_matches();

    let source_path = matches.value_of("INPUT")
        .expect("Expected input file");
    let dest_path = matches.value_of("OUTPUT")
        .expect("Expected output file");

    // Setup.
    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::Latest);

    println!("Reading.");
    let mut file = File::open(source_path)
        .expect("Could not open source");
    let stream = BufReader::new(file);

    println!("Decoding.");
    let reader = binjs::token::simple::TreeTokenReader::new(stream, &grammar);
    let mut decoder = binjs::token::decode::Decoder::new(&grammar, reader);

    let tree = decoder.decode()
        .expect("Could not decode");

    println!("Pretty-printing");
    let source = parser.to_source(&tree)
        .expect("Could not pretty-print");

    println!("Writing.");
    let mut dest = File::create(dest_path)
        .expect("Could not create destination file");
    dest.write(source.as_bytes())
        .expect("Could not write destination file");
}