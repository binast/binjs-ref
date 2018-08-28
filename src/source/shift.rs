//! Read the data through a call to the Shift parser

use json;
use json::object::Object;
use json::JsonValue as JSON;

use std;
use std::env;
use std::io::{ Write };
use std::path::*;
use std::process::*;

use binjs_meta::spec::{ Interface, NodeName, Spec };
use binjs_generic::syntax::{ASTError, MutASTVisitor, MutASTWalker, WalkPath };
use binjs_meta::spec::ToStr;

use source::parser::SourceParser;

#[derive(Debug)]
pub enum Error {
    CouldNotLaunch(std::io::Error),
    CouldNotReadFile(std::io::Error),
    ExecutionError(std::io::Error),
    CouldNotCreateFile(std::io::Error),
    ReturnedError(ExitStatus),
    JsonError(json::JsonError),
    InvalidPath(PathBuf),
    InvalidUTF8(std::string::FromUtf8Error),
    InvalidAST(ASTError),
}

/// Using a Node + Shift binary to parse an AST.
pub struct Shift {
    bin_path: PathBuf
}

impl Shift {
    pub fn new() -> Self {
        Shift::with_path("node")
    }

    pub fn with_path<P: AsRef<Path>>(bin_path: P) -> Self {
        Shift {
            bin_path: bin_path.as_ref().to_path_buf()
        }
    }

    fn parse_script_output(&self, script: &str) -> Result<String, Error> {
        debug!(target: "Shift", "Preparing script {}", script);

        let script = format!(r##"
        var result;
        try {{
            result = (function() {{
                {}
            }})();
        }} catch (ex) {{
            console.warn(ex);
            /* rethrow */ throw ex;
        }};
        var process = require('process');
        process.stdout.write(result);
        console.warn(result);
        "##,
        script);

        debug!(target: "Shift", "Launching script {}", script);

        let node_memory = match env::var("NODE_MAX_OLD_SPACE_SIZE") {
            Err(_) => String::from("--max_old_space_size=2048"),
            Ok(v) => format!("--max_old_space_size={}", v)
        };

        let mut child = Command::new(&*self.bin_path)
            .arg(node_memory)
            .env("NODE_PATH", "node_modules")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(Error::CouldNotLaunch)?;

        if let Some(ref mut stdin) = child.stdin {
            stdin.write(script.as_bytes())
                .map_err(Error::ExecutionError)?;
        }

        let output = child.wait_with_output()
            .map_err(Error::ExecutionError)?;

        debug!(target: "Shift", "Output {:?}", output);
        if !output.status.success() {
            error!(target: "Shift", "Script error {}", String::from_utf8(output.stderr).unwrap());
            return Err(Error::ReturnedError(output.status));
        }

        let result = String::from_utf8(output.stdout)
            .map_err(Error::InvalidUTF8)?;
        Ok(result)
    }

    fn parse_script_json_output(&self, script: &str) -> Result<JSON, Error> {
        let stdout = self.parse_script_output(script)?;

        // Now attempt to parse JSON
        json::parse(&stdout)
            .map_err(Error::JsonError)
    }

    pub fn to_source(&self, syntax: &Spec, ast: &JSON) -> Result<String, Error> {
        let mut ast = ast.clone();

        debug!(target: "Shift", "Preparing source\n{:#}", ast);
        let mut walker = MutASTWalker::new(syntax, ToShift);
        walker.walk(&mut ast)
            .map_err(Error::InvalidAST)?;
        debug!(target: "Shift", "Prepared source\n{:#}", ast);


        // Escape `"`.
        let data = ast.dump()
            .replace("\\", "\\\\")
            .replace("\"", "\\\"");


        // A script to parse a string, write it to stdout as JSON.
        let script = format!(
            r##"
            var codegen = require('shift-codegen').default;
            var ast     = JSON.parse("{}");
            return codegen(ast);
            "##,
            data);
        self.parse_script_output(&script)
            .map_err(|err| {
                warn!("Could not pretty-print {}", ast.pretty(2));
                err
            })
    }
}

impl SourceParser for Shift {
    type Error = Error;
    fn parse_str(&self, data: &str) -> Result<JSON, Error> {
        // Escape `"`.
        let data = data.replace("\"", "\\\"");

        // A script to parse a string, write it to stdout as JSON.
        let script = format!(
            r##"
            var parseScript = require('shift-parser').parseScript;
            var data = "{}";

            var parsed = parseScript(data, {{ earlyErrors: false }});

            return JSON.stringify(parsed);
            "##,
            data);

        let mut ast = self.parse_script_json_output(&script)?;
        FromShift.convert(&mut ast);
        Ok(ast)
    }

    /// Parse a text source file, using Shift.
    fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<JSON, Error> {
        let path = path.as_ref().to_str()
            .ok_or_else(||Error::InvalidPath(path.as_ref().to_path_buf()))?;

        // A script to parse a source file, write it to stdout as JSON.
        let script = format!(
            r##"
            var parseScript = require('shift-parser').parseScript;
            var fs      = require('fs');

            var source  = fs.readFileSync('{}', {{encoding: "utf-8"}});
            return JSON.stringify(parseScript(source));
            "##,
            path);
        let mut ast = self.parse_script_json_output(&script)?;
        FromShift.convert(&mut ast);
        Ok(ast)
    }
}

#[derive(PartialEq)]
enum FunctionKind {
    FunctionDeclaration,
    Method,
    FunctionExpression,
    ArrowExpression,
    ArrowExpressionWithFunctionBody,
    ArrowExpressionWithExpression,
    Getter,
    Setter,
}

/// A data structure designed to convert from Shift AST to BinJS AST.
struct FromShift;
impl FromShift {
    fn convert(&self, value: &mut JSON) {
        use json::JsonValue::*;
        match *value {
            Array(ref mut array) => {
                for value in array {
                    self.convert(value);
                }
            }
            Object(ref mut object) => {
                for (_, value) in object.iter_mut() {
                    self.convert(value);
                }
                self.convert_object(object)
            }
            _ => {}
        }
    }

    fn make_eager(&self, object: &mut json::object::Object) {
        let kind = format!("Eager{}", object["type"].as_str().unwrap());
        object.insert("type", json::from(kind));
    }

    fn dummy_declared_scope(&self, name: &str) -> json::JsonValue {
        let mut scope = Object::new();
        scope["type"] = json::from(name);
        scope["declaredNames"] = array![];
        scope["hasDirectEval"] = JSON::Boolean(false);
        JSON::Object(scope)
    }

    fn dummy_parameter_scope<'a, I>(&self, params: I) -> json::JsonValue
    where I: IntoIterator<Item=&'a json::JsonValue> {
        let mut scope = Object::new();
        scope["type"] = json::from("AssertedParameterScope");
        scope["boundNames"] = array![];
        scope["hasDirectEval"] = JSON::Boolean(false);

        let mut is_simple_parameter_list = true;
        for item in params {
            match item["type"].as_str() {
                Some("BindingIdentifier") => {
                }
                _ => {
                    is_simple_parameter_list = false;
                    break;
                }
            }
        }

        scope["isSimpleParameterList"] = JSON::Boolean(is_simple_parameter_list);
        JSON::Object(scope)
    }

    fn dummy_bound_names_scope(&self) -> json::JsonValue {
        let mut scope = Object::new();
        scope["type"] = json::from("AssertedBoundNamesScope");
        scope["boundNames"] = array![];
        scope["hasDirectEval"] = JSON::Boolean(true);
        JSON::Object(scope)
    }

    fn create_function_contents(&self, object: &mut json::object::Object, kind: FunctionKind) {
        assert!(kind != FunctionKind::ArrowExpressionWithFunctionBody);
        assert!(kind != FunctionKind::ArrowExpressionWithExpression);
        if kind != FunctionKind::Getter && kind != FunctionKind::Setter {
            // `isAsync` is not supported by the parser yet.
            if let None = object.get("isAsync") {
                object["isAsync"] = JSON::Boolean(false)
            }
        }

        let directives = if object["body"].has_key("directives") {
            object["body"].remove("directives")
        } else {
            json::JsonValue::new_array()
        };

        let is_expression_body;
        let mut body = object.remove("body").unwrap();
        match body["type"].as_str() {
            Some("FunctionBody") => {
                body = body.remove("statements");
                is_expression_body = false;
            }
            _ => {
                is_expression_body = true;
            }
        }

        let mut contents = Object::new();
        match kind {
            FunctionKind::FunctionDeclaration | FunctionKind::Method => {
                contents["type"] = json::from("FunctionOrMethodContents");
                contents["isThisCaptured"] = JSON::Boolean(false);
            }
            FunctionKind::FunctionExpression => {
                contents["type"] = json::from("FunctionExpressionContents");
                contents["isFunctionNameCaptured"] = JSON::Boolean(false);
                contents["isThisCaptured"] = JSON::Boolean(false);
            }
            FunctionKind::ArrowExpression => {
                if is_expression_body {
                    object["type"] = json::from("ArrowExpressionWithExpression");
                    contents["type"] = json::from("ArrowExpressionContentsWithExpression");
                } else {
                    object["type"] = json::from("ArrowExpressionWithFunctionBody");
                    contents["type"] = json::from("ArrowExpressionContentsWithFunctionBody");
                }
            }
            FunctionKind::Getter => {
                contents["type"] = json::from("GetterContents");
                contents["isThisCaptured"] = JSON::Boolean(false);
            }
            FunctionKind::Setter => {
                contents["type"] = json::from("SetterContents");
                contents["isThisCaptured"] = JSON::Boolean(false);
            }
            _ => {
                panic!("unexpected FunctionKind");
            }
        }
        if kind != FunctionKind::Getter {
            if kind == FunctionKind::Setter {
                contents["param"] = object.remove("param").unwrap();
                contents["parameterScope"] = self.dummy_parameter_scope(vec![&contents["param"]]);
            } else {
                contents["params"] = object.remove("params").unwrap();
                assert!(contents["params"]["items"].is_array());
                contents["parameterScope"] = self.dummy_parameter_scope(contents["params"]["items"].members());
            }
        }
        contents["bodyScope"] = self.dummy_declared_scope("AssertedVarScope");
        contents["body"] = body;
        object.insert("contents", JSON::Object(contents));
        object.insert("directives", directives);
        self.make_eager(object);
    }

    fn convert_object(&self, object: &mut json::object::Object) {
        // By alphabetical order
        match object["type"].as_str() {
            Some("Block") => {
                object.insert("scope", self.dummy_declared_scope("AssertedBlockScope"));
            }
            Some("BlockStatement") => {
                // Rewrite
                //
                // BlockStatement {
                //    block: Block {
                //       ...foo
                //    }
                // }
                //
                // into
                //
                // Block {
                //    ...foo
                // }
                let mut remove = match object.remove("block") {
                    Some(JSON::Object(remove)) => remove,
                    _ => panic!("No field `block` in a `BlockStatement`")
                };
                std::mem::swap(object, &mut remove);
                // At this stage
                // - `remove` is the `BlockStatement`
                // - `object` is the `Block`
            }
            Some("ForInStatement") | Some("ForOfStatement") => {
                // In Shift, `left` is a `VariableDeclaration or AssignmentTarget`.
                // In BinJS, `left` is a `ForInOfBinding or AssignmentTarget`.
                if let Some("VariableDeclaration") = object["left"]["type"].as_str() {
                    object["left"]["type"] = json::from("ForInOfBinding");
                    let binding = object["left"]["declarators"][0].remove("binding");
                    object["left"]["binding"] = binding;
                    object["left"].remove("declarators");
                }
            }
            Some("FunctionDeclaration") => {
                self.create_function_contents(object, FunctionKind::FunctionDeclaration);
            }
            Some("Method") => {
                self.create_function_contents(object, FunctionKind::Method);
            }
            Some("FunctionExpression") => {
                self.create_function_contents(object, FunctionKind::FunctionExpression);
            }
            Some("ArrowExpression") => {
                self.create_function_contents(object, FunctionKind::ArrowExpression);
            }
            Some("Getter") => {
                self.create_function_contents(object, FunctionKind::Getter);
            }
            Some("Setter") => {
                self.create_function_contents(object, FunctionKind::Setter);
            }
            Some("LabeledStatement") => {
                // Rewrite type
                object["type"] = json::from("LabelledStatement");
            }
            Some("LiteralRegExpExpression") => {
                let mut flags = String::new();
                if let JSON::Boolean(true) = object["global"] {
                    flags.push('g');
                }
                if let JSON::Boolean(true) = object["ignoreCase"] {
                    flags.push('i');
                }
                if let JSON::Boolean(true) = object["multiLine"] {
                    flags.push('m');
                }
                if let JSON::Boolean(true) = object["sticky"] {
                    flags.push('y');
                }
                if let JSON::Boolean(true) = object["unicode"] {
                    flags.push('u');
                }
                object.insert("flags", json::from(flags));
            }
            Some("Script") => {
                object.insert("scope", self.dummy_declared_scope("AssertedScriptGlobalScope"));
            }
            Some("CatchClause") => {
                object.insert("bindingScope", self.dummy_bound_names_scope());
            }
            Some("StaticPropertyName") => {
                // Change type.
                object["type"] = json::from("LiteralPropertyName");
            }
            Some("VariableDeclarationStatement") => {
                // Rewrite
                //
                // VariableDeclarationStatement {
                //    declaration: VariableDeclaration {
                //       ...foo
                //    }
                // }
                //
                // into
                //
                // VariableDeclaration {
                //    ...foo
                // }
                let mut remove = match object.remove("declaration") {
                    Some(JSON::Object(remove)) => remove,
                    _ => panic!("No field `declaration` in a `VariableDeclarationStatement`")
                };
                std::mem::swap(object, &mut remove);
                // At this stage
                // - `remove` is the `VariableDeclarationStatement`
                // - `object` is the `VariableDeclaration`
            }
            _ => { /* No change */ }
        }
    }
}

/// A data structure designed to convert from BinJS AST to Shift AST.

struct ToShift;
impl ToShift {
    fn remove_eager_or_lazy(&self, obj: &mut json::object::Object) {
        let kind = { obj["type"].as_str().unwrap().to_string()  };
        const EAGER : &'static str = "Eager";
        const LAZY : &'static str = "Lazy";

        if kind.starts_with(EAGER) {
            obj["type"] = json::from(&kind[EAGER.len()..])
        } else if kind.starts_with(LAZY) {
            obj["type"] = json::from(&kind[LAZY.len()..])
        } else {
            panic!()
        }
    }
    fn remove_with_suffix(&self, obj: &mut json::object::Object) {
        let kind = { obj["type"].as_str().unwrap().to_string()  };
        const WITHFUNCTIONBODY : &'static str = "WithFunctionBody";
        const WITHEXPRESSION : &'static str = "WithExpression";

        if kind.ends_with(WITHFUNCTIONBODY) {
            obj["type"] = json::from(&kind[..(kind.len() - WITHFUNCTIONBODY.len())])
        } else if kind.ends_with(WITHEXPRESSION) {
            obj["type"] = json::from(&kind[..(kind.len() - WITHEXPRESSION.len())])
        }
    }
    fn remove_function_contents(&self, obj: &mut json::object::Object, kind: FunctionKind) {
        self.remove_eager_or_lazy(obj);
        self.remove_with_suffix(obj);

        // Remove unused fields.
        obj.remove("isAsync");
        obj.remove("scope");
        obj.remove("contents_skip");

        // Move some fields back from *Contents.
        let mut contents = obj.remove("contents").unwrap();
        obj["params"] = contents.remove("params");

        if kind == FunctionKind::ArrowExpressionWithExpression {
            obj["body"] = contents.remove("body");
        } else {
            let mut body = Object::new();
            body["type"] = json::from("FunctionBody");
            body["directives"] = obj.remove("directives").unwrap();
            body["statements"] = contents.remove("body");
            obj["body"] = JSON::Object(body);
        }
    }
}
impl MutASTVisitor for ToShift {
    fn exit_interface(&mut self, _path: &WalkPath, value: &mut JSON, interface: &Interface, name: &NodeName) -> Result<(), ASTError> {
        debug!(target: "Shift", "Should I rewrite {:?} at {:?}", interface.name(), name);
        match (name.to_str(), interface.name().to_str(), value) {
            ("Statement", "Block", &mut JSON::Object(ref mut object)) => {
                debug!(target: "Shift", "Yes I should: from {}", object.dump());
                // Rewrite
                //
                // Block { // Used as Statement
                //    ...foo
                // }
                //
                // into
                //
                // BlockStatement {
                //    block: Block {
                //       ...foo
                //    }
                // }
                let mut insert = Object::new();
                insert["type"] = json::from("BlockStatement");
                std::mem::swap(&mut insert, object);
                // From here,
                // - `object` is `BlockStatement`.
                // - `insert` is `Block`.
                object["block"] = JSON::Object(insert);
                debug!(target: "Shift", "into {}", object.dump());
            }
            ("Statement", "VariableDeclaration", &mut JSON::Object(ref mut object)) => {
                // Rewrite
                //
                // VariableDeclaration { // Used as Statement
                //    ...foo
                // }
                //
                // into
                //
                // VariableDeclarationStatement {
                //    declaration: VariableDeclaration {
                //       ...foo
                //    }
                // }
                let mut insert = Object::new();
                insert["type"] = json::from("VariableDeclarationStatement");
                std::mem::swap(&mut insert, object);
                // From here,
                // - `object` is `VariableDeclarationStatement`.
                // - `insert` is `VariableDeclaration`.
                object["declaration"] = JSON::Object(insert);
            }
            (_, "Script", &mut JSON::Object(ref mut object)) => {
                // Remove unused field.
                object.remove("scope");
            }
            (_, "CatchClause", &mut JSON::Object(ref mut object)) => {
                // Remove unused field.
                object.remove("bindingScope");
            }
            (_, "LabelledStatement", &mut JSON::Object(ref mut object)) => {
                // Change type.
                object["type"] = json::from("LabeledStatement");
            }
            (_, "LiteralPropertyName", &mut JSON::Object(ref mut object)) => {
                // Change type.
                object["type"] = json::from("StaticPropertyName");
            }
            (_, "LiteralRegExpExpression", &mut JSON::Object(ref mut object)) => {
                let mut global = false;
                let mut ignore_case = false;
                let mut multi_line = false;
                let mut sticky = false;
                let mut unicode = false;
                if let Some(flags) = object["flags"].as_str() {
                    for c in flags.chars() {
                        match c {
                            'g' => { global = true; }
                            'i' => { ignore_case = true; }
                            'm' => { multi_line = true; }
                            'y' => { sticky = true; }
                            'u' => { unicode = true; }
                            _ => { /* ignore */ }
                        }
                    }
                }
                object.insert("global", json::from(global));
                object.insert("ignoreCase", json::from(ignore_case));
                object.insert("multiLine", json::from(multi_line));
                object.insert("sticky", json::from(sticky));
                object.insert("unicode", json::from(unicode));
            }
            (_, "ForInOfBinding", &mut JSON::Object(ref mut object)) => {
                // Rewrite
                //
                // ForInOfBinding {
                //    kind,
                //    binding
                // }
                //
                // into
                //
                // VariableDeclaration {
                //    kind,
                //    declarators: [{
                //      VariableDeclarator {
                //        init: null,
                //        binding
                //      }
                //    }]
                // }
                object["type"] = json::from("VariableDeclaration");
                let binding = object.remove("binding");
                object["declarators"] = array![
                    object!{
                        "type" => "VariableDeclarator",
                        "init" => json::Null,
                        "binding" => binding
                    }
                ];
            }
            (_, "EagerFunctionExpression", &mut JSON::Object(ref mut object))
            | (_, "LazyFunctionExpression", &mut JSON::Object(ref mut object))=> {
                self.remove_function_contents(object, FunctionKind::FunctionExpression);
            }
            (_, "EagerFunctionDeclaration", &mut JSON::Object(ref mut object))
            | (_, "LazyFunctionDeclaration", &mut JSON::Object(ref mut object)) => {
                self.remove_function_contents(object, FunctionKind::FunctionDeclaration);
            }
            (_, "EagerMethod", &mut JSON::Object(ref mut object))
            | (_, "LazyMethod", &mut JSON::Object(ref mut object)) => {
                self.remove_function_contents(object, FunctionKind::Method);
            }
            (_, "EagerGetter", &mut JSON::Object(ref mut object))
            | (_, "LazyGetter", &mut JSON::Object(ref mut object)) => {
                self.remove_function_contents(object, FunctionKind::Getter);
            }
            (_, "EagerSetter", &mut JSON::Object(ref mut object))
            | (_, "LazySetter", &mut JSON::Object(ref mut object)) => {
                self.remove_function_contents(object, FunctionKind::Setter);
            }
            (_, "EagerArrowExpressionWithFunctionBody", &mut JSON::Object(ref mut object))
            | (_, "LazyArrowExpressionWithFunctionBody", &mut JSON::Object(ref mut object)) => {
                self.remove_function_contents(object, FunctionKind::ArrowExpressionWithFunctionBody);
            }
            (_, "EagerArrowExpressionWithExpression", &mut JSON::Object(ref mut object))
            | (_, "LazyArrowExpressionWithExpression", &mut JSON::Object(ref mut object)) => {
                self.remove_function_contents(object, FunctionKind::ArrowExpressionWithExpression);
            }
            _ => {
                // Nothing to do.
            }
        }
        Ok(())
    }
}

#[test]
fn test_shift_basic() {
    use env_logger;
    env_logger::init();

    let shift = Shift::new();
    let parsed = shift.parse_str("function foo() {}")
        .expect("Error in parse_str");
    let expected = object!{
        "type" => "Script",
        "directives" => array![],
        "scope" => object!{
            "type" => "AssertedScriptGlobalScope",
            "declaredNames" => array![],
            "hasDirectEval" => false,
        },
        "statements" => array![
            object!{
                "type" => "EagerFunctionDeclaration",
                "isGenerator" => false,
                "isAsync" => false,
                "name" => object!{
                    "type" => "BindingIdentifier",
                    "name" => "foo"
                },
                "directives" => array![],
                "contents" => object!{
                    "type" => "FunctionOrMethodContents",
                    "isThisCaptured" => false,
                    "parameterScope" => object!{
                        "type" => "AssertedParameterScope",
                        "boundNames" => array![],
                        "hasDirectEval" => false,
                        "isSimpleParameterList" => true,
                    },
                    "params" => object!{
                        "type" => "FormalParameters",
                        "items" => array![],
                        "rest" => json::Null,
                    },
                    "bodyScope" => object!{
                        "type" => "AssertedVarScope",
                        "declaredNames" => array![],
                        "hasDirectEval" => false,
                    },
                    "body" => array![],
                }
            }
        ]
    };


    println!("Comparing\n{}\n{}",
        parsed.pretty(2),
        expected.pretty(2)
    );

    assert_eq!(parsed, expected);
}
