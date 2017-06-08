use serde_json;
use serde_json::Value as JSON;

use std;
use std::io::Cursor;
use std::path::*;
use std::process::*;

use source::parser::SourceParser;

#[derive(Debug)]
pub enum Error {
    CouldNotLaunch(std::io::Error),
    ExecutionError(std::io::Error),
    ReturnedError(ExitStatus),
    JsonError(serde_json::Error),
    InvalidPath(PathBuf),
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

    fn parse_script_output(&self, script: &str) -> Result<JSON, Error> {
        info!(target: "Babel", "Launching script {}", script);
        let output = Command::new(&*self.bin_path)
            .arg("-e")
            .arg(script)
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

        debug!(target: "Babel", "JSON {:?}", String::from_utf8(output.stdout.clone()).unwrap());

        // Now attempt to parse JSON
        let data = Cursor::new(output.stdout);
        let result: JSON = serde_json::from_reader(data)
            .map_err(Error::JsonError)?;

        // Extract meaningful subset
        if let JSON::Object(mut obj) = result {
            let subset = obj.remove("program")
                .ok_or(Error::InvalidAST)?;
            return Ok(subset);
        }

        // Result should be a valid AST.
        Err(Error::InvalidAST)
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
            var json    = JSON.stringify(parsed);
            process.stdout.write(json)
            "##,
            data);
        self.parse_script_output(&script)
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

            var source  = fs.readFileSync({}, {{encoding: "utf-8"}});
            var parsed  = babylon.parse(source);
            var json    = JSON.stringify(parsed);
            process.stdout.write(json)
            "##,
            path);
        self.parse_script_output(&script)
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