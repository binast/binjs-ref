extern crate binjs;
#[macro_use]
extern crate serde_json;
#[macro_use]
extern crate test_logger;

use binjs::source::*;

test!(test_annotations_scopes_1, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = "function h() { { let x; { var y; } } }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["h"]
    }));

    // Function body
    let body = &ast["body"][0]["body"];
    assert_eq!(body["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["y"]
    }));

    // Block `{ let x; { var y; } }`
    let body = &body["body"][0];
    assert_eq!(body["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": ["x"],
        "BINJS:VarDeclaredNames": ["y"]
    }));

    // Block `{var y; }`
    let body = &body["body"][1];
    assert_eq!(body["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["y"]
    }));
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
    assert_eq!(ast["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["f"]
    }));

    // Function body
    let body = &ast["body"][0]["body"];
    println!("{}", serde_json::to_string_pretty(body).unwrap());
    assert_eq!(body["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["g", "i"]
    }));

    // Function `g` body
    let g = &body["body"][0]["body"];
    assert_eq!(g["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["x"]
    }));

    // Loop
    let for_loop = &body["body"][1];
    println!("{}", serde_json::to_string_pretty(for_loop).unwrap());
    assert_eq!(for_loop["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["i"]
    }));
});

test!(test_annotations_capture_1, {
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
    assert_eq!(ast["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["f"]
    }));

    // Function scope
    let scope = &ast["body"][0]["BINJS:Scope"];
    assert_eq!(scope["BINJS:CapturedNames"], json!(["b", "f"]));
    assert_eq!(scope["BINJS:HasDirectEval"], json!(false));
    assert_eq!(scope["BINJS:ConstDeclaredNames"], json!([]));
    assert_eq!(scope["BINJS:LetDeclaredNames"], json!([]));
    assert_eq!(scope["BINJS:VarDeclaredNames"], json!(["g"])); // FIXME: Is this the right place for g?

    // Function body
    let body = &ast["body"][0]["body"];
    assert_eq!(body["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["g"]
    }));

    // Function `g` body
    let g = &body["body"][0]["body"];
    assert_eq!(g["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["c", "d"]
    }));
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
    assert_eq!(ast["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["f"]
    }));

    // Function scope
    let scope = &ast["body"][0]["BINJS:Scope"];
    assert_eq!(scope, &json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["g"]
    }));

    // Function body
    let body = &ast["body"][0]["body"];
    assert_eq!(body["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["g"]
    }));

    // Function `g` body
    let g = &body["body"][0]["body"];
    assert_eq!(g["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["c", "d"]
    }));
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
    assert_eq!(ast["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": ["f"],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["f", "g"]
    }));
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
    assert_eq!(ast["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["f"]
    }));

    // Function body
    let body = &ast["body"][0]["body"];
    println!("{}", serde_json::to_string_pretty(&body).unwrap());
    assert_eq!(body["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": ["x"],
        "BINJS:VarDeclaredNames": []
    }));

    // Block 1
    let block = &body["body"][1];
    assert_eq!(block["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": ["y"],
        "BINJS:LetDeclaredNames": ["g"],
        "BINJS:VarDeclaredNames": []
    }));
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
    assert_eq!(ast["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": ["print"],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["f", "print"]
    }));

    // Function body
    let body = &ast["body"][0]["body"];
    println!("{}", serde_json::to_string_pretty(&body).unwrap());
    assert_eq!(body["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": ["x"], // FIXME: Is this right? Should `print` be considered captured?
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["g", "x"]
    }));
});

test!(test_annotations_eval_1, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = "function foo() { eval('abc'); } { /* 1 */ }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", serde_json::to_string_pretty(&ast).unwrap());

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": true,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["foo"]
    }));

    // Function scope
    let scope = &ast["body"][0];
    assert_eq!(scope["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": true,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": []
    }));

    // Function body
    let body = &ast["body"][0]["body"];
    println!("{}", serde_json::to_string_pretty(&body).unwrap());
    assert_eq!(body["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": true,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": []
    }));

    // Block 1
    let scope = &ast["body"][1];
    assert_eq!(scope["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": []
    }));
});

test!(test_annotations_eval_2, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = "function foo() { eval('abc'); var eval; } { /* 1 */ }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", serde_json::to_string_pretty(&ast).unwrap());

    // Toplevel
    assert_eq!(ast["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["foo"]
    }));

    // Function body
    let body = &ast["body"][0]["body"];
    assert_eq!(body["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": ["eval"]
    }));

    // Block 1
    let scope = &ast["body"][1];
    assert_eq!(scope["BINJS:Scope"], json!({
        "type": "BINJS:Scope",
        "BINJS:CapturedNames": [],
        "BINJS:HasDirectEval": false,
        "BINJS:ConstDeclaredNames": [],
        "BINJS:LetDeclaredNames": [],
        "BINJS:VarDeclaredNames": []
    }));
});

test!(test_directives_1, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = "function foo() { function bar() {} }";

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", serde_json::to_string_pretty(&ast).unwrap());

    assert_eq!(ast["directives"].as_array().unwrap().len(), 0);

    let ref foo = ast["body"][0];
    assert_eq!(foo["body"]["directives"].as_array().unwrap().len(), 0);

    let ref bar = foo["body"]["body"][0];
    assert_eq!(bar["body"]["directives"].as_array().unwrap().len(), 0);
});

test!(test_directives_2, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = r#"function foo() { function bar() { "use strict"; } }"#;

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", serde_json::to_string_pretty(&ast).unwrap());

    assert_eq!(ast["directives"].as_array().unwrap().len(), 0);

    let ref foo = ast["body"][0];
    assert_eq!(foo["body"]["directives"].as_array().unwrap().len(), 0);

    let ref bar = foo["body"]["body"][0];
    let array = bar["body"]["directives"].as_array().unwrap();
    assert_eq!(array.len(), 1);
    assert_eq!(array[0], json!({
        "type": "Directive",
        "value": {
            "type": "DirectiveLiteral",
            "value": "use strict"
        }
    }));
});


test!(test_directives_3, {
    println!("Preparing test.");

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::ES5);

    let source = r#"function foo() { "use strict"; function bar() { "something different"; } }"#;

    let mut ast  = parser.parse_str(source)
        .expect("Could not parse source");
    grammar.annotate(&mut ast)
        .expect("Could not annotate AST");

    println!("{}", serde_json::to_string_pretty(&ast).unwrap());

    assert_eq!(ast["directives"].as_array().unwrap().len(), 0);

    let ref foo = ast["body"][0];
    let array = foo["body"]["directives"].as_array().unwrap();
    assert_eq!(array.len(), 1);
    assert_eq!(array[0]["type"], json!("Directive"));
    assert_eq!(array[0]["value"]["type"], json!("DirectiveLiteral"));
    assert_eq!(array[0]["value"]["value"], json!("use strict"));

    let ref bar = foo["body"]["body"][0];
    let array = bar["body"]["directives"].as_array().unwrap();
    assert_eq!(array.len(), 1);
    assert_eq!(array[0]["type"], json!("Directive"));
    assert_eq!(array[0]["value"]["type"], json!("DirectiveLiteral"));
    assert_eq!(array[0]["value"]["value"], json!("use strict"));
});