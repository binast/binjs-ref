use serde_json;
use serde_json::Value as JSON;

use std;
use std::io::{ Cursor, Write };
use std::path::*;
use std::process::*;

#[derive(Debug)]
pub enum Error {
    CouldNotLaunch(std::io::Error),
    CouldNotSendData(std::io::Error),
    ExecutionError(std::io::Error),
    ReturnedError(ExitStatus),
    JsonError(serde_json::Error),
    InvalidPath(PathBuf),
}

/// Using Node + Esprima to parse an AST.
pub struct Esprima {
    /// Path to `node`.
    bin_path: PathBuf
}

impl Esprima {
    pub fn new() -> Self {
        Esprima::with_path("esparse")
    }
    pub fn with_path<P: AsRef<Path>>(bin_path: P) -> Self {
        Esprima {
            bin_path: bin_path.as_ref().to_path_buf()
        }
    }

    pub fn parse_str(&self, data: &str) -> Result<JSON, Error> {
        info!(target: "Esprima", "Parsing data {}", data);
        let mut child = Command::new(&*self.bin_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(Error::CouldNotLaunch)?;

        let bytes : Vec<_> = data.bytes().collect();
        {
            let ref mut stdin = child.stdin
                .as_mut()
                .expect("Child should have a stdin.");

            stdin
                .write(&bytes)
                .map_err(Error::CouldNotSendData)?;

            stdin
                .flush()
                .map_err(Error::CouldNotSendData)?;
        }

        let output = child.wait_with_output()
            .map_err(Error::ExecutionError)?;

        debug!(target: "Esprima", "Output {:?}", output);
        if !output.status.success() {
            error!(target: "Esprima", "Script error {}", String::from_utf8(output.stderr).unwrap());
            return Err(Error::ReturnedError(output.status));
        }

        debug!(target: "Esprima", "JSON {:?}", String::from_utf8(output.stdout.clone()).unwrap());
        // Now attempt to parse JSON
        let data = Cursor::new(output.stdout);
        let result: JSON = serde_json::from_reader(data)
            .map_err(Error::JsonError)?;

        // Result should be a valid AST.
        Ok(result)
    }

    /// Parse a text source file, using Esprima.
    pub fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<JSON, Error> {
        let path = path.as_ref().to_str()
            .ok_or_else(||Error::InvalidPath(path.as_ref().to_path_buf()))?;

        info!(target: "Esprima", "Reading path {}", path);
        let output = Command::new(&*self.bin_path)
            .arg(path)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .map_err(Error::CouldNotLaunch)?;

        debug!(target: "Esprima", "Output {:?}", output);
        if !output.status.success() {
            error!(target: "Esprima", "Script error {}", String::from_utf8(output.stderr).unwrap());
            return Err(Error::ReturnedError(output.status));
        }

        debug!(target: "Esprima", "JSON {:?}", String::from_utf8(output.stdout.clone()).unwrap());
        // Now attempt to parse JSON
        let data = Cursor::new(output.stdout);
        let result: JSON = serde_json::from_reader(data)
            .map_err(Error::JsonError)?;

        // Result should be a valid AST.
        Ok(result)
    }
}


#[test]
fn test_esprima_basic() {
    let esprima = Esprima::new();
    let parsed = esprima.parse_str("function foo() {}")
        .unwrap();
    let expected = json!({
        "body": [{
            "body": {
                "body":[],
                "type":"BlockStatement"
            },
            "expression":false,
            "generator":false,
            "id":{
                "name":"foo",
                "type":"Identifier"
            },
            "params":[],
            "type":"FunctionDeclaration"
        }],
        "sourceType":"script",
        "type":"Program"
    });
    assert_eq!(parsed, expected);
}