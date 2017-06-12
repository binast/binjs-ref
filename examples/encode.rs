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
        (about: "Encode a JavaScript text source to a JavaScript binary source in the BinJS format.")
        (@arg INPUT: +required "Input file to use. Must be a JS source file.")
        (@arg OUTPUT: +required "Output file to use. Will be overwritten.")
    ).get_matches();

    let source_path = matches.value_of("INPUT")
        .expect("Expected input file");
    let dest_path = matches.value_of("OUTPUT")
        .expect("Expected output file");

    // Setup.
    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let program_kind = grammar.get_kind("Program")
        .expect("This grammar does not have a kind `Program`");
    let program_interface = grammar.get_interface_by_kind(&program_kind)
        .expect("This grammar does not have an interface `Program`");
    let program_type = Type::Interfaces {
        names: vec![program_interface.name().clone()],
        or_null: false
    };

    println!("Parsing.");
    let ast    = parser.parse_file(source_path)
        .expect("Could not parse source");


    println!("Encoding.");
    let writer  = binjs::token::simple::TreeTokenWriter::new();
    let encoder = binjs::token::encode::Encoder::new(&grammar, writer);

    encoder.encode(&ast, &program_type)
        .expect("Could not encode AST");
    let writer = encoder.done();

    println!("Writing.");
    let mut dest = File::create(dest_path)
        .expect("Could not create destination file");
    dest.write(writer.data())
        .expect("Could not write destination file");
}