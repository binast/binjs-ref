//! Read the data through a call to the Shift parser

use json::JsonValue as JSON;

use std::env;
use std::ffi::OsString;
use std::io::{BufRead, BufReader, LineWriter, Lines, Write};
use std::path::*;
use std::process::*;
use std::sync::Mutex;

use binjs_io::escaped_wtf8;

use source::parser::SourceParser;

use which::which;

#[derive(Debug)]
pub enum Error {
    CouldNotLaunch(std::io::Error),
    IOError(std::io::Error),
    JSONError(json::Error),
    InvalidPath(PathBuf),
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

        let result = json::parse(&output).map_err(Error::JSONError)?;
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
        Err(Error::JSONError(json::Error::wrong_type(
            "Result-like JSON object",
        )))
    }
}

/// Using a Node + Shift binary to parse an AST.
pub struct Shift {
    parse_str: Script,
    parse_file: Script,
    codegen: Script,
}

impl Shift {
    pub fn try_new() -> Result<Self, Error> {
        let node = NodeConfig::try_new()?;

        Ok(Self {
            parse_str: Script::try_new(&node, "parse_str.js")?,
            parse_file: Script::try_new(&node, "parse_file.js")?,
            codegen: Script::try_new(&node, "codegen.js")?,
        })
    }

    // We need to mutate the AST to adjust it to the Shift format, so we take
    // it by ownership rather than by reference.
    //
    // If the caller needs the original AST after this call, it's their
    // responsibility to clone it and pass to this function.
    pub fn to_source(&self, ast: &JSON) -> Result<String, Error> {
        self.codegen
            .transform(&ast)
            .and_then(|mut res| {
                res.take_string()
                    .map(escaped_wtf8::to_unicode_escape)
                    .ok_or_else(|| Error::JSONError(json::Error::wrong_type("string")))
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
        self.parse_str.transform(&data.into())
    }

    /// Parse a text source file, using Shift.
    fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<JSON, Error> {
        let path = path
            .as_ref()
            .to_str()
            .ok_or_else(|| Error::InvalidPath(path.as_ref().to_path_buf()))?;

        // A script to parse a source file, write it to stdout as JSON.
        self.parse_file.transform(&path.into())
    }
}

#[test]
fn test_shift_basic() {
    use env_logger;
    env_logger::init();

    let shift = Shift::try_new().expect("Could not launch Shift");
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

    assert!(
        parsed == expected,
        "{} != {}",
        parsed.pretty(2),
        expected.pretty(2)
    );
}
