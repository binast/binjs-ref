//! Encode a BinJS, then decode it, ensure that we obtain the same AST.

extern crate binjs;
#[macro_use]
extern crate clap;
extern crate env_logger;

use binjs::source::*;

use std::io::*;

fn main() {
    env_logger::init().unwrap();

    let matches = clap_app!(myapp =>
        (author: "David Teller <dteller@mozilla.com>")
        (about: "Check that encoding + decoding a file yields the same AST.")
        (@arg INPUT: +required "Input file to use. Must be a BinJS source file.")
    ).get_matches();

    let source_path = matches.value_of("INPUT")
        .expect("Expected input file");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::Latest);

    println!("Parsing.");
    let ast    = parser.parse_file(source_path)
        .expect("Could not parse source");

    println!("Encoding.");
    let writer  = binjs::token::simple::TreeTokenWriter::new();
    let encoder = binjs::token::encode::Encoder::new(&grammar, writer);

    encoder.encode(&ast)
        .expect("Could not encode AST");
    let writer = encoder.done();

    println!("Decoding.");
    let source = Cursor::new(writer.data());
    let reader = binjs::token::simple::TreeTokenReader::new(source, &grammar);
    let mut decoder = binjs::token::decode::Decoder::new(&grammar, reader);

    let decoded = decoder.decode()
        .expect("Could not decode");

    println!("Checking.");
    let equal = grammar.compare(&ast, &decoded)
        .expect("Could not compare ASTs");
    assert!(equal);

    println!("Roundtrip success!");
}