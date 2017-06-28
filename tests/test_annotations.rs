extern crate binjs;
extern crate env_logger;
#[macro_use]
extern crate serde_json;
#[macro_use]
extern crate test_logger;

use binjs::source::*;
use serde_json::Value as JSON;

test!(test_annotations_scopes, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = "function h() { { let x; { var y; } } }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    // Toplevel
    assert_eq!(ast["BINJS:CapturedNames"], json!([]));
    assert_eq!(ast["BINJS:HasDirectEval"], json!(false));
    assert_eq!(ast["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:VarDeclaredNames"], json!(["h"]));

    // Function body
    let body = &ast["body"][0]["body"];
    assert_eq!(body["BINJS:CapturedNames"], json!([]));
    assert_eq!(body["BINJS:HasDirectEval"], json!(false));
    assert_eq!(body["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:VarDeclaredNames"], json!(["y"]));

    // Block `{ let x; { var y; } }`
    let body = &body["body"][0];
    assert_eq!(body["BINJS:CapturedNames"], json!([]));
    assert_eq!(body["BINJS:HasDirectEval"], json!(false));
    assert_eq!(body["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:LetDeclaredNames"], json!(["x"]));
    assert_eq!(body["BINJS:VarDeclaredNames"], json!(["y"]));

    // Block `{var y; }`
    let body = &body["body"][1];
    assert_eq!(body["BINJS:CapturedNames"], json!([]));
    assert_eq!(body["BINJS:HasDirectEval"], json!(false));
    assert_eq!(body["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:VarDeclaredNames"], json!(["y"]));
});

test!(test_annotations_scopes_2, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = "function f() { function g() { var x; } for (var i = 0; i < 10; i++) ; }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    // Toplevel
    assert_eq!(ast["BINJS:CapturedNames"], json!([]));
    assert_eq!(ast["BINJS:HasDirectEval"], json!(false));
    assert_eq!(ast["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:VarDeclaredNames"], json!(["f"]));

    // Function body
    let body = &ast["body"][0]["body"];
    println!("{}", serde_json::to_string_pretty(body).unwrap());
    assert_eq!(body["BINJS:CapturedNames"], json!([]));
    assert_eq!(body["BINJS:HasDirectEval"], json!(false));
    assert_eq!(body["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:VarDeclaredNames"], json!(["g", "i"]));

    // Function `g` body
    let g = &body["body"][0]["body"];
    assert_eq!(g["BINJS:CapturedNames"], json!([]));
    assert_eq!(g["BINJS:HasDirectEval"], json!(false));
    assert_eq!(g["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(g["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(g["BINJS:VarDeclaredNames"], json!(["x"]));

    // Loop
    let for_loop = &body["body"][1];
    println!("{}", serde_json::to_string_pretty(for_loop).unwrap());
    assert_eq!(for_loop["BINJS:CapturedNames"], json!([]));
    assert_eq!(for_loop["BINJS:HasDirectEval"], json!(false));
    assert_eq!(for_loop["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(for_loop["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(for_loop["BINJS:VarDeclaredNames"], json!(["i"]));
});

test!(test_annotations_capture, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = "function f(a, b) { function g() { var c = f; var d = b; } }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", serde_json::to_string_pretty(&ast).unwrap());

    // Toplevel
    assert_eq!(ast["BINJS:CapturedNames"], json!([]));
    assert_eq!(ast["BINJS:HasDirectEval"], json!(false));
    assert_eq!(ast["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:VarDeclaredNames"], json!(["f"]));

    // Function scope
    let scope = &ast["body"][0]["BINJS:Scope"];
    assert_eq!(scope["BINJS:CapturedNames"], json!(["b", "f"]));
    assert_eq!(scope["BINJS:HasDirectEval"], json!(false));
    assert_eq!(scope["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(scope["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(scope["BINJS:VarDeclaredNames"], json!([]));

    // Function body
    let body = &ast["body"][0]["body"];
    assert_eq!(body["BINJS:CapturedNames"], json!([]));
    assert_eq!(body["BINJS:HasDirectEval"], json!(false));
    assert_eq!(body["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:VarDeclaredNames"], json!(["g"]));

    // Function `g` body
    let g = &body["body"][0]["body"];
    assert_eq!(g["BINJS:CapturedNames"], json!([]));
    assert_eq!(g["BINJS:HasDirectEval"], json!(false));
    assert_eq!(g["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(g["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(g["BINJS:VarDeclaredNames"], json!(["c", "d"]));
});


test!(test_annotations_capture_2, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = "function f(a, b) { function g(b, f) { var c = f; var d = b; } }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", serde_json::to_string_pretty(&ast).unwrap());

    // Toplevel
    assert_eq!(ast["BINJS:CapturedNames"], json!([]));
    assert_eq!(ast["BINJS:HasDirectEval"], json!(false));
    assert_eq!(ast["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:VarDeclaredNames"], json!(["f"]));

    // Function scope
    let scope = &ast["body"][0]["BINJS:Scope"];
    assert_eq!(scope["BINJS:CapturedNames"], JSON::Null);
    assert_eq!(scope["BINJS:HasDirectEval"], JSON::Null);
    assert_eq!(scope["BINJS:ConstDeclaredNames"], JSON::Null);
    assert_eq!(scope["BINJS:LetDeclaredNames"], JSON::Null);
    assert_eq!(scope["BINJS:VarDeclaredNames"], JSON::Null);

    // Function body
    let body = &ast["body"][0]["body"];
    assert_eq!(body["BINJS:CapturedNames"], json!([]));
    assert_eq!(body["BINJS:HasDirectEval"], json!(false));
    assert_eq!(body["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:VarDeclaredNames"], json!(["g"]));

    // Function `g` body
    let g = &body["body"][0]["body"];
    assert_eq!(g["BINJS:CapturedNames"], json!([]));
    assert_eq!(g["BINJS:HasDirectEval"], json!(false));
    assert_eq!(g["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(g["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(g["BINJS:VarDeclaredNames"], json!(["c", "d"]));
});

test!(test_annotations_capture_3, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = "function f() { } function g() { var a = f; }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", serde_json::to_string_pretty(&ast).unwrap());

    // Toplevel
    assert_eq!(ast["BINJS:CapturedNames"], json!(["f"]));
    assert_eq!(ast["BINJS:HasDirectEval"], json!(false));
    assert_eq!(ast["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:VarDeclaredNames"], json!(["f", "g"]));
});

test!(test_annotations_scopes_3, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = "function f() { let x; {/*1*/ const y = 5; function g() {} } }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

//    println!("{}", serde_json::to_string_pretty(&ast).unwrap());

    // Toplevel
    assert_eq!(ast["BINJS:CapturedNames"], json!([]));
    assert_eq!(ast["BINJS:HasDirectEval"], json!(false));
    assert_eq!(ast["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:VarDeclaredNames"], json!(["f"]));

    // Function scope
    assert_eq!(&ast["body"][0]["BINJS:Scope"], &JSON::Null);

    // Function body
    let body = &ast["body"][0]["body"];
    println!("{}", serde_json::to_string_pretty(&body).unwrap());
    assert_eq!(body["BINJS:CapturedNames"], json!([]));
    assert_eq!(body["BINJS:HasDirectEval"], json!(false));
    assert_eq!(body["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:LetDeclaredNames"], json!(["x"]));
    assert_eq!(body["BINJS:VarDeclaredNames"], json!([]));

    // Block 1
    let block = &body["body"][1];
    assert_eq!(block["BINJS:CapturedNames"], json!([]));
    assert_eq!(block["BINJS:HasDirectEval"], json!(false));
    assert_eq!(block["BINJS:ConstDeclaredNames"], json!(["y"]));
    assert_eq!(block["BINJS:LetDeclaredNames"], json!(["g"]));
    assert_eq!(block["BINJS:VarDeclaredNames"], json!([]));
});

test!(test_annotations_capture_4, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = "function f() { var x; function g() { print(x); } }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", serde_json::to_string_pretty(&ast).unwrap());

    // Toplevel
    assert_eq!(ast["BINJS:CapturedNames"], json!([]));
    assert_eq!(ast["BINJS:HasDirectEval"], json!(false));
    assert_eq!(ast["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(ast["BINJS:VarDeclaredNames"], json!(["f"]));

    // Function scope
    assert_eq!(&ast["body"][0]["BINJS:Scope"], &JSON::Null);

    // Function body
    let body = &ast["body"][0]["body"];
    println!("{}", serde_json::to_string_pretty(&body).unwrap());
    assert_eq!(body["BINJS:CapturedNames"], json!(["x"]));
    assert_eq!(body["BINJS:HasDirectEval"], json!(false));
    assert_eq!(body["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(body["BINJS:VarDeclaredNames"], json!(["g", "x"]));
});

/*
        r#"
        function foo() {
            eval("abc");
        }"#,
        r#"
        function foo() {
            function eval() {}
            eval("abc");
        }"#,
    ].iter() {
*/
