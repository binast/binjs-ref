extern crate binjs;
extern crate serde_json;


#[macro_use]
extern crate test_logger;

use binjs::source::*;
use binjs::token::encode::*;

use std::io::*;

test!(test_simple_tokenization, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

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

        println!("Parsing");
        let mut ast  = parser.parse_str(source)
            .expect("Could not parse source");

        println!("Annotating");
        grammar.annotate(&mut ast)
            .expect("Could not annotate AST");

        println!("Encoding sample {}", serde_json::to_string_pretty(&ast).unwrap());
        let writer  = binjs::token::simple::TreeTokenWriter::new();
        let encoder = binjs::token::encode::Encoder::new(&grammar, writer);

        encoder.encode(&ast)
            .expect("Could not encode AST");
        let data = encoder.done()
            .expect("Could not finalize encoding");

        println!("Decoding sample");
        let reader = binjs::token::simple::TreeTokenReader::new(Cursor::new(data),
            &grammar);
        let mut decoder = binjs::token::decode::Decoder::new(&grammar, reader);

        let decoded = decoder.decode()
            .expect("Could not decode AST");

        let pretty = parser.to_source(&decoded)
            .expect("Could not pretty-print");

        println!("Decoded: {}", pretty);

        let equal = grammar.compare(&ast, &decoded)
            .expect("Could not compare ASTs");
        assert!(equal)
    }
});