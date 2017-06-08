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

/// Using a SpiderMonkey binary to parse an AST.
pub struct SpiderMonkey {
    bin_path: PathBuf
}

impl SpiderMonkey {
    pub fn new() -> Self {
        SpiderMonkey::with_path("js")
    }

    pub fn with_path<P: AsRef<Path>>(bin_path: P) -> Self {
        SpiderMonkey {
            bin_path: bin_path.as_ref().to_path_buf()
        }
    }

    pub fn parse_str(&self, data: &str) -> Result<JSON, Error> {
        // Escape `"`.
        let data = data.replace("\"", "\\\"");

        // A script to parse a string, write it to stdout as JSON.
        let script = format!(
                    r##"
                    let parsed  = Reflect.parse("{}");
                    let json    = JSON.stringify(parsed);
                    print(json)
                    "##,
                    data);
        self.parse_script_output(&script)
    }

    /// Parse a text source file, using SpiderMonkey.
    pub fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<JSON, Error> {
        let path = path.as_ref().to_str()
            .ok_or_else(||Error::InvalidPath(path.as_ref().to_path_buf()))?;

        // A script to parse a source file, write it to stdout as JSON.
        let script = format!(
                    r##"
                    let source  = os.file.readFile("{}");
                    let parsed  = Reflect.parse(source);
                    let json    = JSON.stringify(parsed);
                    print(json)
                    "##,
                    path);
        self.parse_script_output(&script)
    }

    fn parse_script_output(&self, script: &str) -> Result<JSON, Error> {
        info!(target: "SpiderMonkey", "Launching script {}", script);
        let output = Command::new(&*self.bin_path)
            .arg("-e")
            .arg(script)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .map_err(Error::CouldNotLaunch)?;

        debug!(target: "SpiderMonkey", "Output {:?}", output);
        if !output.status.success() {
            error!(target: "SpiderMonkey", "Script error {}", String::from_utf8(output.stderr).unwrap());
            return Err(Error::ReturnedError(output.status));
        }

        debug!(target: "SpiderMonkey", "JSON {:?}", String::from_utf8(output.stdout.clone()).unwrap());
        // Now attempt to parse JSON
        let data = Cursor::new(output.stdout);
        let result: JSON = serde_json::from_reader(data)
            .map_err(Error::JsonError)?;

        // Result should be a valid AST.
        Ok(result)
    }
}
