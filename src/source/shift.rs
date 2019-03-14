//! Read the data through a call to the Shift parser

use serde::Deserialize;

use std::env;
use std::ffi::OsString;
use std::io::{BufRead, BufReader, LineWriter, Lines, Write};
use std::path::*;
use std::process::*;
use std::sync::Mutex;

use binjs_es6::ast::Script as AST;
use binjs_io::escaped_wtf8;

use source::parser::SourceParser;

use which::which;

#[derive(Debug)]
pub enum Error {
    CouldNotLaunch(std::io::Error),
    IOError(std::io::Error),
    JSONError(serde_json::Error),
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
    pub fn transform<I, O>(&self, input: &I) -> Result<O, Error>
    where
        I: ?Sized + serde::Serialize,
        O: serde::de::DeserializeOwned,
    {
        let output = {
            let mut io = self.0.lock().unwrap();

            serde_json::to_writer(&mut io.input, input).map_err(Error::JSONError)?;
            writeln!(io.input).map_err(Error::IOError)?;

            io.output.next().unwrap().map_err(Error::IOError)?
        };

        #[derive(Deserialize)]
        #[serde(tag = "type", content = "value")]
        #[serde(remote = "std::result::Result")]
        enum Result<T, E> {
            Ok(T),
            Err(E),
        }

        let mut deserializer = serde_json::Deserializer::from_str(&output);

        deserializer.disable_recursion_limit();

        Result::deserialize(&mut deserializer)
            .map_err(Error::JSONError)?
            .map_err(Error::ParsingError)
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

    pub fn to_source(&self, ast: &AST) -> Result<String, Error> {
        self.codegen
            .transform(ast)
            .map(escaped_wtf8::to_unicode_escape)
    }
}

impl SourceParser<AST> for Shift {
    type Error = Error;

    fn parse_str(&self, data: &str) -> Result<AST, Error> {
        self.parse_str.transform(data)
    }

    /// Parse a text source file, using Shift.
    fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<AST, Error> {
        let path = path
            .as_ref()
            .to_str()
            .ok_or_else(|| Error::InvalidPath(path.as_ref().to_path_buf()))?;

        // A script to parse a source file, write it to stdout as JSON.
        self.parse_file.transform(path)
    }
}

#[test]
fn test_shift_basic() {
    use env_logger;
    env_logger::init();

    use binjs_es6::ast::*;
    use binjs_shared::IdentifierName;

    let shift = Shift::try_new().expect("Could not launch Shift");

    let parsed = shift
        .parse_str("function foo() {}")
        .expect("Error in parse_str");

    let expected = Script {
        statements: vec![Statement::EagerFunctionDeclaration(Box::new(
            EagerFunctionDeclaration {
                name: BindingIdentifier {
                    name: IdentifierName::from_str("foo"),
                },
                contents: FunctionOrMethodContents {
                    parameter_scope: AssertedParameterScope {
                        is_simple_parameter_list: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                ..Default::default()
            },
        ))],
        ..Default::default()
    };

    assert!(parsed == expected, "{:#?} != {:#?}", parsed, expected);
}
