extern crate binjs;
extern crate env_logger;

#[macro_use]
extern crate test_logger;

use binjs::ast::grammar::*;
use binjs::source::*;

use std::io::*;

test!(test_simple_tokenization, {
    println!("Preparing test.");

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

    for source in [
        "function foo() {}",
        "(function foo() {})",
        "var i;",
        "for (i = 0; i < 100; ++i) {}",
        "console.log(i);",
        "console.log(i); console.log(j);",
        "var i; var j;",
        "var i; for (i = 0; i < 100; ++i) {}",
        "var i; for (i = 0; i < 100; ++i) { console.log(i); }",
        "function foo(x, y) { var i; for (i = 0; i < 100; ++i) { console.log(x, y + i, x + y + i, x + y + i + 1); } }",
    ].iter() {
        println!("Attempting to (de)tokenize {}", source);

        let ast    = parser.parse_str(source)
            .expect("Could not parse source");


        println!("Encoding sample");
        let writer  = binjs::token::simple::TreeTokenWriter::new();
        let encoder = binjs::token::encode::Encoder::new(&grammar, writer);

        encoder.encode(&ast, &program_type)
            .expect("Could not encode AST");
        let writer = encoder.done();

        println!("Decoding sample");
        let reader = binjs::token::simple::TreeTokenReader::new(Cursor::new(writer.data()),
            &grammar);
        let mut decoder = binjs::token::decode::Decoder::new(&grammar, reader);

        let _decoded = decoder.decode(&program_type)
            .expect("Could not decode AST");
    }

/*
    FIXME: We first need to strip `ast` to the bits accepted by the grammar.
    println!("Comparing ASTs.");
    binjs::util::strip(&mut ast);
    assert_eq!(decoded, ast);
*/
});