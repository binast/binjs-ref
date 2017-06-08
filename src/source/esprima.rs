use serde_json;
use serde_json::Value as JSON;

use std;
use std::io::Cursor;
use std::path::*;
use std::process::*;

#[derive(Debug)]
pub enum Error {
    CouldNotLaunch(std::io::Error),
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
    pub fn new<P: AsRef<Path>>(bin_path: P) -> Self {
        Esprima {
            bin_path: bin_path.as_ref().to_path_buf()
        }
    }

    pub fn parse_str(&self, data: &str) -> Result<JSON, Error> {
        // Escape `"`.
        let data = data.replace("\"", "\\\"");

        // A script to parse a string, write it to stdout as JSON.
        let script = format!(
            r##"
            var esprima = require('esprima');

            var parsed  = esprima.parse("{}");
            var json    = JSON.stringify(parsed);
            console.log(json)
            "##,
            data);
        self.parse_script_output(&script)
    }

    /// Parse a text source file, using Esprima.
    pub fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<JSON, Error> {
        let path = path.as_ref().to_str()
            .ok_or_else(||Error::InvalidPath(path.as_ref().to_path_buf()))?;

        // A script to parse a source file, write it to stdout as JSON.
        let script = format!(
            r##"
            var esprima = require('esprima');
            var fs      = require('fs');

            var source  = fs.readFileSync("{}");
            var parsed  = esprima.parse(source);
            var json    = JSON.stringify(parsed);
            console.log(json)
            "##,
            path);
        self.parse_script_output(&script)
    }

    fn parse_script_output(&self, script: &str) -> Result<JSON, Error> {
        info!(target: "Esprima", "Launching script {}", script);
        let output = Command::new(&*self.bin_path)
            .arg("-e")
            .arg(script)
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
