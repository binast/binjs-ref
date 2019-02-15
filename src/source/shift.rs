//! Read the data through a call to the Shift parser

use json;
use json::object::Object;
use json::JsonValue as JSON;

use std::env;
use std::ffi::OsString;
use std::io::{BufRead, BufReader, LineWriter, Lines, Write};
use std::path::*;
use std::process::*;
use std::sync::Mutex;

use binjs_generic::syntax::{ASTError, MutASTVisitor, MutASTWalker, WalkPath};
use binjs_meta::spec::{Interface, NodeName, Spec};

use source::parser::SourceParser;

use which::which;

#[derive(Debug)]
pub enum Error {
    CouldNotLaunch(std::io::Error),
    IOError(std::io::Error),
    JsonError(json::Error),
    InvalidPath(PathBuf),
    InvalidAST(ASTError),
    NodeNotFound(which::Error),
    ParsingError(String),
}

pub struct NodeConfig {
    /// Paths to the Node executable.
    bin_path: PathBuf,
    /// Path to a directory with our Node.js scripts (same as this Rust file).
    scripts_path: &'static Path,
    /// Node.js flag to configure memory usage.
    memory: OsString,
}

impl NodeConfig {
    pub fn try_new() -> Result<Self, Error> {
        Ok(Self {
            bin_path: which("node").map_err(Error::NodeNotFound)?,

            scripts_path: Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/src/source")),

            memory: match env::var("NODE_MAX_OLD_SPACE_SIZE") {
                Err(_) => String::from("--max_old_space_size=2048"),
                Ok(v) => format!("--max_old_space_size={}", v),
            }
            .into(),
        })
    }

    pub fn create_cmd(&self, name: &str) -> Command {
        let mut cmd = Command::new(&self.bin_path);
        cmd.env("NODE_PATH", "node_modules");
        cmd.arg(&self.memory);
        cmd.arg(self.scripts_path.join(name));
        cmd
    }
}

struct ScriptIO {
    input: LineWriter<ChildStdin>,
    output: Lines<BufReader<ChildStdout>>,
}

pub struct Script(Mutex<ScriptIO>);

impl Script {
    pub fn try_new(node_config: &NodeConfig, name: &str) -> Result<Self, Error> {
        let child = node_config
            .create_cmd(name)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .map_err(Error::CouldNotLaunch)?;

        Ok(Script(Mutex::new(ScriptIO {
            input: LineWriter::new(child.stdin.unwrap()),
            output: BufReader::new(child.stdout.unwrap()).lines(),
        })))
    }

    /// This function is responsible for piping JSON inputs to the script
    /// in a newline-delimited format and parsing JSON outputs back.
    ///
    /// Each output can be either `{"type":"Ok","value":...}` if operation was
    /// successful or `{"type":"Err","value":...}` if corresponding JS callback
    /// has failed with an error, and these are converted into Rust `Result`.
    ///
    /// See start-json-stream.js for some more details.
    pub fn transform(&self, input: &JSON) -> Result<JSON, Error> {
        let output = (move || {
            let mut io = self.0.lock().unwrap();
            input.write(&mut io.input)?;
            writeln!(io.input)?;
            io.output.next().unwrap()
        })()
        .map_err(Error::IOError)?;

        let result = json::parse(&output).map_err(Error::JsonError)?;
        if let JSON::Object(mut obj) = result {
            if let (Some(mut value), Some(ty)) = (
                obj.remove("value"),
                obj.get("type").and_then(|ty| ty.as_str()),
            ) {
                match ty {
                    "Ok" => return Ok(value),
                    "Err" => {
                        if let Some(msg) = value.take_string() {
                            return Err(Error::ParsingError(msg));
                        }
                    }
                    _ => {}
                }
            }
        }
        Err(Error::JsonError(json::Error::wrong_type(
            "Result-like JSON object",
        )))
    }
}

/// Options for parsing or pretty printing.
struct Options {
    /// If `true`, rewrite `foo["bar"]` into `foo.bar` whenever possible.
    ///
    /// Default value: `true`.
    cleanup_bracket_expressions: bool,
}
impl Options {
    fn new() -> Self {
        Options {
            cleanup_bracket_expressions: true,
        }
    }
}

/// Using a Node + Shift binary to parse an AST.
pub struct Shift {
    parse_str: Script,
    parse_file: Script,
    codegen: Script,
    options: Options,
}

impl Shift {
    pub fn try_new() -> Result<Self, Error> {
        let node = NodeConfig::try_new()?;

        Ok(Self {
            parse_str: Script::try_new(&node, "parse_str.js")?,
            parse_file: Script::try_new(&node, "parse_file.js")?,
            codegen: Script::try_new(&node, "codegen.js")?,
            options: Options::new(),
        })
    }

    /// Determine whether bracket expressions should be cleaned up during parsing.
    ///
    /// If `true` or unset, rewrite `foo["bar"]` into `foo.bar` whenever possible.
    pub fn with_cleanup_bracket_expressions(&mut self, value: bool) -> &mut Self {
        self.options.cleanup_bracket_expressions = value;
        self
    }

    // We need to mutate the AST to adjust it to the Shift format, so we take
    // it by ownership rather than by reference.
    //
    // If the caller needs the original AST after this call, it's their
    // responsibility to clone it and pass to this function.
    pub fn to_source(&self, syntax: &Spec, mut ast: JSON) -> Result<String, Error> {
        debug!(target: "Shift", "Preparing source\n{:#}", ast);
        let mut walker = MutASTWalker::new(syntax, ToShift);
        walker.walk(&mut ast).map_err(Error::InvalidAST)?;
        debug!(target: "Shift", "Prepared source\n{:#}", ast);

        self.codegen
            .transform(&ast)
            .and_then(|mut res| {
                res.take_string()
                    .ok_or_else(|| Error::JsonError(json::Error::wrong_type("string")))
            })
            .map_err(|err| {
                warn!("Could not pretty-print {}", ast.pretty(2));
                err
            })
    }
}

impl SourceParser for Shift {
    type Error = Error;

    fn parse_str(&self, data: &str) -> Result<JSON, Error> {
        let mut ast = self.parse_str.transform(&data.into())?;
        let converter = FromShift::new(&self.options);
        converter.convert(&mut ast);
        Ok(ast)
    }

    /// Parse a text source file, using Shift.
    fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<JSON, Error> {
        let path = path
            .as_ref()
            .to_str()
            .ok_or_else(|| Error::InvalidPath(path.as_ref().to_path_buf()))?;

        // A script to parse a source file, write it to stdout as JSON.
        let mut ast = self.parse_file.transform(&path.into())?;
        let converter = FromShift::new(&self.options);
        converter.convert(&mut ast);
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

struct ParameterScopeAndFunctionLength {
    scope: json::JsonValue,
    length: usize,
}

/// A data structure designed to convert from Shift AST to BinJS AST.
struct FromShift<'a> {
    options: &'a Options,
}
impl<'a> FromShift<'a> {
    fn new(options: &'a Options) -> Self {
        Self { options }
    }
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

    fn parameter_scope_and_length<'b, I>(&self, params: I) -> ParameterScopeAndFunctionLength
    where
        I: IntoIterator<Item = &'b json::JsonValue>,
    {
        let mut scope = Object::new();
        let mut length = 0;
        scope["type"] = json::from("AssertedParameterScope");
        scope["paramNames"] = array![];
        scope["hasDirectEval"] = JSON::Boolean(false);

        let mut is_simple_parameter_list = true;
        for item in params {
            match item["type"].as_str() {
                Some("BindingIdentifier") => {
                    length += 1;
                }
                Some("ObjectBinding") | Some("ArrayBinding") => {
                    is_simple_parameter_list = false;
                    length += 1;
                }
                _ => {
                    is_simple_parameter_list = false;
                    break;
                }
            }
        }

        scope["isSimpleParameterList"] = JSON::Boolean(is_simple_parameter_list);

        ParameterScopeAndFunctionLength {
            scope: JSON::Object(scope),
            length,
        }
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
                let scope_and_length = self.parameter_scope_and_length(vec![&contents["param"]]);
                contents["parameterScope"] = scope_and_length.scope;
                object["length"] = json::from(scope_and_length.length);
            } else {
                contents["params"] = object.remove("params").unwrap();
                assert!(contents["params"]["items"].is_array());
                let scope_and_length =
                    self.parameter_scope_and_length(contents["params"]["items"].members());
                contents["parameterScope"] = scope_and_length.scope;
                object["length"] = json::from(scope_and_length.length);
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
                    _ => panic!("No field `block` in a `BlockStatement`"),
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
                object.insert(
                    "scope",
                    self.dummy_declared_scope("AssertedScriptGlobalScope"),
                );
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
                    _ => panic!("No field `declaration` in a `VariableDeclarationStatement`"),
                };
                std::mem::swap(object, &mut remove);
                // At this stage
                // - `remove` is the `VariableDeclarationStatement`
                // - `object` is the `VariableDeclaration`
            }
            Some("IdentifierExpression") => {
                debug!(target: "Shift", "FromShift IdentifierExpression {:?}", object);
                // No change.
            }
            Some("ComputedMemberExpression") if self.options.cleanup_bracket_expressions => {
                // Rewrite
                //
                // ComputedMemberExpression {
                //     _object: o
                //     expression: LiteralStringExpression {
                //        value: "foo"
                //     }
                // }
                //
                // into
                //
                // StaticMemberExpression {
                //    _object: o
                //    property: "foo"
                // }
                if let Some("LiteralStringExpression") = object["expression"]["type"].as_str() {
                    let value = object["expression"].remove("value");
                    object["type"] = "StaticMemberExpression".into();
                    object["property"] = value;
                    object.remove("expression");
                }
            }
            Some("ComputedMemberAssignmentTarget") if self.options.cleanup_bracket_expressions => {
                // Rewrite
                //
                // ComputedMemberAssignmentTarget {
                //     _object: o
                //     expression: LiteralStringExpression {
                //        value: "foo"
                //     }
                // }
                //
                // into
                //
                // ComputedMemberAssignmentTarget {
                //    _object: o
                //    property: "foo"
                // }
                if let Some("LiteralStringExpression") = object["expression"]["type"].as_str() {
                    let value = object["expression"].remove("value");
                    object["type"] = "StaticMemberExpression".into();
                    object["property"] = value;
                    object.remove("expression");
                }
            }
            _ => { /* No change */ }
        }
    }
}

/// A data structure designed to convert from BinJS AST to Shift AST.

struct ToShift;
impl ToShift {
    fn remove_eager_or_lazy(&self, obj: &mut json::object::Object) {
        let kind = { obj["type"].as_str().unwrap().to_string() };
        const EAGER: &'static str = "Eager";
        const LAZY: &'static str = "Lazy";

        if kind.starts_with(EAGER) {
            obj["type"] = json::from(&kind[EAGER.len()..])
        } else if kind.starts_with(LAZY) {
            obj["type"] = json::from(&kind[LAZY.len()..])
        } else {
            panic!()
        }
    }
    fn remove_with_suffix(&self, obj: &mut json::object::Object) {
        let kind = { obj["type"].as_str().unwrap().to_string() };
        const WITHFUNCTIONBODY: &'static str = "WithFunctionBody";
        const WITHEXPRESSION: &'static str = "WithExpression";

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
        obj.remove("length");

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
    fn exit_interface(
        &mut self,
        _path: &WalkPath,
        value: &mut JSON,
        interface: &Interface,
        name: &NodeName,
    ) -> Result<(), ASTError> {
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
                            'g' => {
                                global = true;
                            }
                            'i' => {
                                ignore_case = true;
                            }
                            'm' => {
                                multi_line = true;
                            }
                            'y' => {
                                sticky = true;
                            }
                            'u' => {
                                unicode = true;
                            }
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
                object["declarators"] = array![object! {
                    "type" => "VariableDeclarator",
                    "init" => json::Null,
                    "binding" => binding
                }];
            }
            (_, "EagerFunctionExpression", &mut JSON::Object(ref mut object))
            | (_, "LazyFunctionExpression", &mut JSON::Object(ref mut object)) => {
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
                self.remove_function_contents(
                    object,
                    FunctionKind::ArrowExpressionWithFunctionBody,
                );
            }
            (_, "EagerArrowExpressionWithExpression", &mut JSON::Object(ref mut object))
            | (_, "LazyArrowExpressionWithExpression", &mut JSON::Object(ref mut object)) => {
                self.remove_function_contents(object, FunctionKind::ArrowExpressionWithExpression);
            }
            (_, "IdentifierExpression", &mut JSON::Object(ref mut object)) => {
                debug!(target: "Shift", "IdentifierExpression {:?}", object);
                // FIXME: We probably need to rewrite the IdentifierDefinition.
            }
            _ => {
                // Nothing to do.
            }
        }
        Ok(())
    }
}

#[test]
fn test_shift() {
    use env_logger;
    env_logger::init();

    let mut shift = Shift::try_new().expect("Could not launch Shift");

    // Parse a simple function.

    let parsed = shift
        .parse_str("function foo() {}")
        .expect("Error in parse_str");
    let expected = object! {
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
                "length" => 0,
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
                        "paramNames" => array![],
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

    println!("Comparing\n{}\n{}", parsed.pretty(2), expected.pretty(2));
    assert_eq!(parsed, expected);

    // Parse a statement with cleanup.
    let source = "foo['bar']"; // Will be rewritten `foo.bar`.
    let parsed = shift
        .with_cleanup_bracket_expressions(true)
        .parse_str(source)
        .expect("Error in parse_str");
    let expected = object! {
        "type" => "Script",
        "directives" => array![],
        "statements" => array![
            object!{
                "type" => "ExpressionStatement",
                "expression" => object!{
                    "type" => "StaticMemberExpression",
                    "object" => object!{
                        "type" => "IdentifierExpression",
                        "name" => "foo"
                    },
                    "property" => "bar"
                }
            }
        ],
        "scope" => object!{
            "type" => "AssertedScriptGlobalScope",
            "declaredNames" => array![],
            "hasDirectEval" => false,
        },
    };
    println!("Comparing\n{}\n{}", parsed.pretty(2), expected.pretty(2));
    assert_eq!(parsed, expected);

    // Parse the same statement without cleanup.
    let parsed = shift
        .with_cleanup_bracket_expressions(false)
        .parse_str(source)
        .expect("Error in parse_str");
    let expected = object! {
        "type" => "Script",
        "directives" => array![],
        "statements" => array![
            object!{
                "type" => "ExpressionStatement",
                "expression" => object!{
                    "type" => "ComputedMemberExpression",
                    "object" => object!{
                        "type" => "IdentifierExpression",
                        "name" => "foo"
                    },
                    "expression" => object!{
                        "type" => "LiteralStringExpression",
                        "value" => "bar"
                    }
                }
            }
        ],
        "scope" => object!{
            "type" => "AssertedScriptGlobalScope",
            "declaredNames" => array![],
            "hasDirectEval" => false,
        },
    };
    println!("Comparing\n{}\n{}", parsed.pretty(2), expected.pretty(2));
    assert_eq!(parsed, expected);
}
