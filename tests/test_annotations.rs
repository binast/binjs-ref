fn main() {
    unimplemented!()
}
/* FIXME: Refactoring


extern crate binjs;
#[macro_use]
extern crate json;
#[macro_use]
extern crate test_logger;

use binjs::source::*;
use binjs::util::JSONAs;
use json::JsonValue as JSON;

test!(test_annotations_scopes_1, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = "function h() { { let x; { var y; } } }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["h"]
    });

    // Function body
    let body = &ast["body"][0]["body"];
    assert_eq!(body["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["y"]
    });

    // Block `{ let x; { var y; } }`
    let body = &body["body"][0];
    assert_eq!(body["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => array!["x"],
        "BINJS:VarDeclaredNames" => array!["y"]
    });

    // Block `{var y; }`
    let body = &body["body"][1];
    assert_eq!(body["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["y"]
    });
});

test!(test_annotations_scopes_2, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = "function f() { function g() { var x; } for (var i = 0; i < 10; i++) ; }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["f"]
    });

    // Function body
    let body = &ast["body"][0]["body"];
    println!("{}", body.pretty(2));
    assert_eq!(body["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["g", "i"]
    });

    // Function `g` body
    let g = &body["body"][0]["body"];
    assert_eq!(g["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["x"]
    });

    // Loop
    let for_loop = &body["body"][1];
    println!("{}", for_loop.pretty(2));
    assert_eq!(for_loop["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["i"]
    });
});

test!(test_annotations_capture_1, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = "function f(a, b) { function g() { var c = f; var d = b; } }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", ast.pretty(2));

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["f"]
    });

    // Function scope
    let scope = &ast["body"][0]["BINJS:Scope"];
    assert_eq!(scope["BINJS:CapturedNames"], array!["b", "f"]);
    assert_eq!(scope["BINJS:HasDirectEval"], false);
    assert_eq!(scope["BINJS:ConstDeclaredNames"], JSON::new_array());
    assert_eq!(scope["BINJS:LetDeclaredNames"], JSON::new_array());
    assert_eq!(scope["BINJS:VarDeclaredNames"], array!["g"]); // FIXME: Is this the right place for g?

    // Function body
    let body = &ast["body"][0]["body"];
    assert_eq!(body["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["g"]
    });

    // Function `g` body
    let g = &body["body"][0]["body"];
    assert_eq!(g["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["c", "d"]
    });
});


test!(test_annotations_capture_2, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = "function f(a, b) { function g(b, f) { var c = f; var d = b; } }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", ast.pretty(2));

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["f"]
    });

    // Function scope
    let scope = &ast["body"][0]["BINJS:Scope"];
    assert_eq!(scope, &object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["g"]
    });

    // Function body
    let body = &ast["body"][0]["body"];
    assert_eq!(body["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["g"]
    });

    // Function `g` body
    let g = &body["body"][0]["body"];
    assert_eq!(g["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["c", "d"]
    });
});

test!(test_annotations_capture_3, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = "function f() { } function g() { var a = f; }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", ast.pretty(2));

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => array!["f"],
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["f", "g"]
    });
});

test!(test_annotations_scopes_3, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = "function f() { let x; {/*1*/ const y = 5; function g() {} } }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

//    println!("{}", ast.pretty(2));

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["f"]
    });

    // Function body
    let body = &ast["body"][0]["body"];
    println!("{}", body.pretty(2));
    assert_eq!(body["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => array!["x"],
        "BINJS:VarDeclaredNames" => JSON::new_array()
    });

    // Block 1
    let block = &body["body"][1];
    assert_eq!(block["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => array!["y"],
        "BINJS:LetDeclaredNames" => array!["g"],
        "BINJS:VarDeclaredNames" => JSON::new_array()
    });
});

test!(test_annotations_capture_4, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = "function f() { var x; function g() { print(x); } }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", ast.pretty(2));

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => array!["print"],
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["f", "print"]
    });

    // Function body
    let body = &ast["body"][0]["body"];
    println!("{}", body.pretty(2));
    assert_eq!(body["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => array!["x"], // FIXME: Is this right? Should `print` be considered captured?
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["g", "x"]
    });
});

test!(test_annotations_eval_1, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = "function foo() { eval('abc'); } { /* 1 */ }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", ast.pretty(2));

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => true,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["foo"]
    });

    // Function scope
    let scope = &ast["body"][0];
    assert_eq!(scope["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => true,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => JSON::new_array()
    });

    // Function body
    let body = &ast["body"][0]["body"];
    println!("{}", body.pretty(2));
    assert_eq!(body["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => true,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => JSON::new_array()
    });

    // Block 1
    let scope = &ast["body"][1];
    assert_eq!(scope["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => JSON::new_array()
    });
});

test!(test_annotations_eval_2, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = "function foo() { eval('abc'); var eval; } { /* 1 */ }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", ast.pretty(2));

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["foo"]
    });

    // Function body
    let body = &ast["body"][0]["body"];
    assert_eq!(body["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => array!["eval"]
    });

    // Block 1
    let scope = &ast["body"][1];
    assert_eq!(scope["BINJS:Scope"], object!{
        "type" => "BINJS:Scope",
        "BINJS:CapturedNames" => JSON::new_array(),
        "BINJS:HasDirectEval" => false,
        "BINJS:ConstDeclaredNames" => JSON::new_array(),
        "BINJS:LetDeclaredNames" => JSON::new_array(),
        "BINJS:VarDeclaredNames" => JSON::new_array()
    });
});

test!(test_directives_1, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = "function foo() { function bar() {} }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", ast.pretty(2));

    assert_eq!(ast["directives"].as_array("").unwrap().len(), 0);

    let ref foo = ast["body"][0];
    assert_eq!(foo["body"]["directives"].as_array("").unwrap().len(), 0);

    let ref bar = foo["body"]["body"][0];
    assert_eq!(bar["body"]["directives"].as_array("").unwrap().len(), 0);
});

test!(test_directives_2, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = r#"function foo() { function bar() { "use strict"; } }"#;

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", ast.pretty(2));

    assert_eq!(ast["directives"].as_array("").unwrap().len(), 0);

    let ref foo = ast["body"][0];
    assert_eq!(foo["body"]["directives"].as_array("").unwrap().len(), 0);

    let ref bar = foo["body"]["body"][0];
    let array = bar["directives"].as_array("").unwrap();
    assert_eq!(array.len(), 1);
    assert_eq!(array[0].as_str().unwrap(), "use strict");
});


test!(test_directives_3, {
    println!("Preparing test.");

    let parser = Shift::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES6);

    let source = r#"function foo() { "use strict"; function bar() { "something different"; } }"#;

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", ast.pretty(2));

    assert_eq!(ast["directives"].as_array("").unwrap().len(), 0);

    let ref foo = ast["body"][0];
    let array = foo["directives"].as_array("").unwrap();
    assert_eq!(array.len(), 1);
    assert_eq!(array[0].as_str(), Some("use strict"));

    let ref bar = foo["body"]["body"][0];
    let array = bar["directives"].as_array("").unwrap();
    assert_eq!(array.len(), 1);
    assert_eq!(array[0].as_str(), Some("something different"));
});

*/
