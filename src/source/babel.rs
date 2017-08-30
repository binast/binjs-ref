use serde;
use serde_json;
use serde_json::Value as JSON;

use std;
use std::io::{ Cursor, Read, Write };
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
    JsonError(serde_json::Error),
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
        let data = Cursor::new(stdout);
        let mut deserializer = serde_json::de::Deserializer::from_reader(data)
            .with_recursion_limit(255);
        serde::de::Deserialize::deserialize(&mut deserializer)
            .map_err(Error::JsonError)
    }

    pub fn to_source(&self, ast: &JSON) -> Result<String, Error> {
        // Escape `"`.
        let data = serde_json::to_string(ast)
            .map_err(Error::JsonError)?
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

            var parsed  = babylon.parse("{}");
            return JSON.stringify(parsed.program);
            "##,
            data);
        self.parse_script_json_output(&script)
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
            var parsed  = babylon.parse(source);
            return JSON.stringify(parsed.program);
            "##,
            path);
        self.parse_script_json_output(&script)
    }
}


#[test]
fn test_babel_basic() {
    use env_logger;
    env_logger::init().unwrap();

    use util::strip;
    use serde_json;



    let babel = Babel::new();
    let mut parsed = babel.parse_str("function foo() {}")
        .unwrap();
    let expected = json!({
        "body": [{
            "type":"FunctionDeclaration",
            "body": {
                "type":"BlockStatement",
                "directives": [],
                "body":[],
            },
            "async":      false,
            "expression": false,
            "generator":  false,
            "id":{
                "type":"Identifier",
                "name":"foo",
            },
            "params":[],
        }],
        "directives": [],
        "sourceType": "script",
        "type":       "Program"
    });


    strip(&mut parsed);
    println!("Comparing\n{}\n{}",
        serde_json::to_string_pretty(&parsed).unwrap(),
        serde_json::to_string_pretty(&expected).unwrap(),
    );

    assert_eq!(parsed, expected);
}