use json;
use json::JsonValue as JSON;
use json::object::Object as Object;

use std;
use std::io::{ Read, Write };
use std::path::*;
use std::process::*;

use source::parser::SourceParser;
use util::get_temporary_file;

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
    InvalidAST,
}

/// Using a Node + Babel binary to parse an AST.
pub struct Babel {
    bin_path: PathBuf
}

impl Babel {
    pub fn new() -> Self {
        Babel::with_path("node")
    }

    pub fn with_path<P: AsRef<Path>>(bin_path: P) -> Self {
        Babel {
            bin_path: bin_path.as_ref().to_path_buf()
        }
    }

    fn parse_script_output(&self, script: &str) -> Result<String, Error> {
        debug!(target: "Babel", "Writing script {}", script);
        let (path, mut channel) = get_temporary_file("js")
            .map_err(Error::CouldNotCreateFile)?;

        let script = format!(r##"
        var result  = (function() {{
            {}
        }})();
        var fs     = require('fs');
        var mktemp = require('mktemp');
        var out    = mktemp.createFileSync("/tmp/binjs-XXXXXXXX.out");
        fs.writeFileSync(out, result);
        console.log(out);
        "##,
        script);

        channel.write(script.as_bytes())
            .map_err(Error::CouldNotCreateFile)?;

        debug!(target: "Babel", "Launching script {}", script);
        let output = Command::new(&*self.bin_path)
            .arg(path)
            .env("NODE_PATH", "node_modules")
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .map_err(Error::CouldNotLaunch)?;

        debug!(target: "Babel", "Output {:?}", output);
        if !output.status.success() {
            error!(target: "Babel", "Script error {}", String::from_utf8(output.stderr).unwrap());
            return Err(Error::ReturnedError(output.status));
        }

        let return_path = String::from_utf8(output.stdout.clone())
            .map_err(Error::InvalidUTF8)?;
        let return_path = return_path
            .trim_right();

        let mut file = std::fs::File::open(return_path)
            .map_err(Error::CouldNotReadFile)?;

        let mut result = String::new();
        file.read_to_string(&mut result)
            .map_err(Error::CouldNotReadFile)?;
        Ok(result)
    }

    fn parse_script_json_output(&self, script: &str) -> Result<JSON, Error> {
        let stdout = self.parse_script_output(script)?;

        // Now attempt to parse JSON
        json::parse(&stdout)
            .map_err(Error::JsonError)
    }

    pub fn to_source(&self, ast: &JSON) -> Result<String, Error> {
        let mut ast = ast.clone();
        ToBabel.convert(&mut ast);

        // Escape `"`.
        let data = ast.dump()
            .replace("\\", "\\\\")
            .replace("\"", "\\\"");


        // A script to parse a string, write it to stdout as JSON.
        let script = format!(
            r##"
            var babel   = require('babel-core');
            var ast     = JSON.parse("{}");
            var parsed  = babel.transformFromAst(ast);
            return parsed.code;
            "##,
            data);
        self.parse_script_output(&script)
    }
}

impl SourceParser for Babel {
    type Error = Error;
    fn parse_str(&self, data: &str) -> Result<JSON, Error> {
        // Escape `"`.
        let data = data.replace("\"", "\\\"");

        // A script to parse a string, write it to stdout as JSON.
        let script = format!(
            r##"
            var babylon = require('babylon');
            var data = "{}";

            var parsed;
            try {{
                parsed = babylon.parse(data);
            }} catch (ex) {{
                if (ex instanceof SyntaxError) {{
                    parsed = babylon.parseExpression(data);
                }} else {{
                    throw ex;
                }}
            }}
            return JSON.stringify(parsed.program);
            "##,
            data);

        let mut ast = self.parse_script_json_output(&script)?;
        FromBabel.convert(&mut ast);
        Ok(ast)
    }

    /// Parse a text source file, using Babel.
    fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<JSON, Error> {
        let path = path.as_ref().to_str()
            .ok_or_else(||Error::InvalidPath(path.as_ref().to_path_buf()))?;

        // A script to parse a source file, write it to stdout as JSON.
        let script = format!(
            r##"
            var babylon = require('babylon');
            var fs      = require('fs');

            var source  = fs.readFileSync('{}', {{encoding: "utf-8"}});
            try {{
                return JSON.stringify(babylon.parse(source).program);
            }} catch (ex) {{
                if (ex instanceof SyntaxError) {{
                    // Pass off a JSON as a program.
                    var parsed = babylon.parseExpression(source);
                    var wrapper = ({{
                        type: "Program",
                        body: [{{
                            type: "ExpressionStatement",
                            expression: parsed,
                        }}],
                        scope: null,
                        directives: []
                    }});
                    return JSON.stringify(wrapper);
                }} else {{
                    throw ex;
                }}
            }}
            "##,
            path);
        let mut ast = self.parse_script_json_output(&script)?;
        FromBabel.convert(&mut ast);
        Ok(ast)
    }
}

/// A data structure designed to convert from Babel AST to BinJS AST.
struct FromBabel;
impl FromBabel {
    /// Since the Babylon grammar and our grammar have a few differences, we need
    /// to apply fixes when parsing an AST.
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
                match object["type"].as_str() {
                    Some("ArrayExpression") => self.convert_array_expression(object),
                    Some("ObjectMethod") => self.convert_object_method(object),
                    Some("MemberExpression") => self.convert_member_expression(object),
                    _ => { /* No change */ }
                }
            }
            _ => {}
        }
    }

    fn convert_member_expression(&self, object: &mut Object) {
        match object["computed"] {
            JSON::Boolean(true) => { object["type"] = json::from("BracketExpression"); }
            JSON::Boolean(false) => { object["type"] = json::from("DotExpression"); }
            _ => { /* No change */ }
        }
    }

    fn convert_object_method(&self, object: &mut Object) {
        // In Babylon, `ObjectMethod::kind` is `"get" | "set" | "init"`.
        // In BinJS, there are three interfaces `ObjectMethod`, `ObjectGetter`, `ObjectSetter`.
        match object["kind"].as_str() {
            Some("get") => { object["type"] = json::from("ObjectGetter"); }
            Some("set") => { object["type"] = json::from("ObjectSetter"); }
            _ => { /* No change */ }
        }
    }

    fn convert_array_expression(&self, object: &mut Object) {
        // In Babylon, `ArrayExpression::elements` has type `[Expression | null]`.
        // In BinJS, it has type `[Expression | Elision]`.
        if let JSON::Array(ref mut elements) = object["elements"] {
            for item in elements.iter_mut() {
                if item.is_null() {
                    *item = object!{
                        "type" => "Elision"
                    }
                }
            }
        }
    }
}
/// A data structure designed to convert from BinJS AST to Babel AST.
struct ToBabel;
impl ToBabel {
    /// Since the Babylon grammar and our grammar have a few differences, we need
    /// to apply fixes when parsing an AST.
    fn convert(&self, value: &mut JSON) {
        use json::JsonValue::*;
        match *value {
            Array(ref mut array) => {
                // Propagate to children.
                for value in array {
                    self.convert(value);
                }
            }
            Object(ref mut object) => {
                // Propagate to children.
                for (_, value) in object.iter_mut() {
                    self.convert(value);
                }
                match object["type"].as_str() {
                    Some("ArrayExpression") => self.convert_array_expression(object),
                    Some("ObjectMethod")
                    | Some("ObjectGetter")
                    | Some("ObjectSetter") => self.convert_object_method(object),
                    Some("DotExpression")
                    | Some("BracketExpression") => self.convert_member_expression(object),
                    _ => { /* No change */ }
                }
            }
            _ => {
                // Nothing to do.
            }
        }
    }

    fn convert_member_expression(&self, object: &mut Object) {
        match object["type"].as_str() {
            Some("BracketExpression") => {
                object["type"] = json::from("MemberExpression");
                object["computed"] = JSON::Boolean(true);
            }
            Some("DotExpression") => {
                object["type"] = json::from("MemberExpression");
                object["computed"] = JSON::Boolean(false);
            }
            _ => { panic!("Invalid member expression kind"); }
        }
    }

    fn convert_object_method(&self, object: &mut Object) {
        // In Babylon, `ObjectMethod::kind` is `"get" | "set" | "init"`.
        // In BinJS, there are three interfaces `ObjectMethod`, `ObjectGetter`, `ObjectSetter`.
        match object["type"].as_str() {
            Some("ObjectMethod") => {
                object["kind"] = json::from("init");
            }
            Some("ObjectGetter") => {
                object["type"] = json::from("ObjectMethod");
                object["kind"] = json::from("get");
            }
            Some("ObjectSetter") => {
                object["type"] = json::from("ObjectMethod");
                object["kind"] = json::from("set");
            }
            _ => { panic!("Invalid member expression kind"); }
        }
    }

    // In Babylon, `ArrayExpression::elements` has type `[Expression | null]`.
    // In BinJS, it has type `[Expression | Elision]`.
    fn convert_array_expression(&self, object: &mut Object) {
        if let JSON::Array(ref mut elements) = object["elements"] {
            for item in elements.iter_mut() {
                if let Some("Elision") = item["type"].as_str() {
                    *item = JSON::Null
                }
            }
        }
    }
}


#[test]
fn test_babel_basic() {
    use env_logger;
    env_logger::init().unwrap();

    use util::strip;

    use json::JsonValue as JSON;

    let babel = Babel::new();
    let mut parsed = babel.parse_str("function foo() {}")
        .unwrap();
    let expected = object!{
        "body" => array![object!{
            "type" => "FunctionDeclaration",
            "body" => object!{
                "type" => "BlockStatement",
                "directives" => JSON::new_array(),
                "body" => JSON::new_array()
            },
            "async" =>      false,
            "expression" => false,
            "generator" =>  false,
            "id" => object!{
                "type" => "Identifier",
                "name" => "foo"
            },
            "params" => JSON::new_array()
        }],
        "directives" => JSON::new_array(),
        "sourceType" => "script",
        "type" =>       "Program"
    };


    strip(&mut parsed);
    println!("Comparing\n{}\n{}",
        parsed.pretty(2),
        expected.pretty(2)
    );

    assert_eq!(parsed, expected);
}