extern crate binjs;

#[macro_use]
extern crate test_logger;

use binjs::generic::io::encode::*;
use binjs::generic::syntax::*;
use binjs::meta::spec::*;
use binjs::source::*;

use std::io::*;

test!(test_simple_tokenization, {
    println!("Preparing test.");

    let parser = Shift::new();
    let mut spec_builder = SpecBuilder::new();
    let library = binjs::generic::es6::Library::new(&mut spec_builder);
    let spec = spec_builder.into_spec(SpecOptions {
        null: &library.null,
        root: &library.program,
    });

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
        "let a = b => c;"
    ].iter() {
        println!("Attempting to (de)tokenize {}", source);

        println!("Parsing");
        let mut ast  = parser.parse_str(source)
            .expect("Could not parse source");

        println!("Annotating");
        let mut visitor = binjs::specialized::es6::scopes::AnnotationVisitor::new();
        visitor.annotate(&mut ast);

        println!("Encoding sample {}", ast.pretty(2));
        let writer  = binjs::io::TokenWriterTreeAdapter::new(binjs::io::simple::TreeTokenWriter::new());
        let encoder = binjs::generic::io::encode::Encoder::new(&spec, writer);

        encoder.encode(&ast)
            .expect("Could not encode AST");
        let (data, _) = encoder.done()
            .expect("Could not finalize encoding");

        println!("Decoding sample");
        let reader = binjs::io::simple::TreeTokenReader::new(Cursor::new(data));
        let mut decoder = binjs::generic::io::decode::Decoder::new(&spec, reader);

        let decoded = decoder.decode()
            .expect("Could not decode AST");

        let pretty = parser.to_source(&spec, &decoded)
            .expect("Could not pretty-print");

        println!("Decoded: {}", pretty);

        let equal = Comparator::compare(&spec, &ast, &decoded)
            .expect("Could not compare ASTs");
        assert!(equal)
    }
});

