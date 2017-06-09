extern crate binjs;
extern crate env_logger;

#[macro_use]
extern crate test_logger;

use binjs::ast::grammar::*;
use binjs::source::*;

use std::io::*;

test!(test_simple_tokenization, {
    println!("Preparing test.");


    let source = "function foo() {}";
    let parser = Babel::new();
    let ast    = parser.parse_str(source)
        .expect("Could not parse source.");

    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);
    let program_kind = grammar.get_kind("Program")
        .expect("This grammar does not have a kind `Program`.");
    let program_interface = grammar.get_interface_by_kind(&program_kind)
        .expect("This grammar does not have an interface `Program`");
    let program_type = Type::Interfaces(vec![program_interface.name().clone()]);


    println!("Encoding sample.");
    let writer  = binjs::token::simple::TreeTokenWriter::new();
    let mut encoder = binjs::token::encode::Encoder::new(&grammar, writer);

    encoder.encode(&ast, &program_type)
        .expect("Could not encode AST.");
    let writer = encoder.done();

    println!("Decoding sample.");
    let reader = binjs::token::simple::TreeTokenReader::new(Cursor::new(writer.data()),
        &grammar);
    let mut decoder = binjs::token::decode::Decoder::new(&grammar, reader);

    let decoded = decoder.decode(&program_type)
        .expect("Could not decode AST.");
});