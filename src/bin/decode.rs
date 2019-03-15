//! Decode a BinJS to a text source.

extern crate binjs;
extern crate clap;
extern crate env_logger;

use binjs::generic::ToJSON;
use binjs::source::Shift;
use binjs::specialized::es6::io::Decoder;

use std::fs::{self, File};
use std::io::*;
use std::thread;

use clap::*;

macro_rules! progress {
    ($quiet:expr, $($args:tt)*) => {
        if !$quiet {
            println!($($args)*);
        }
    }
}

// Command line options.
struct Options<'a> {
    /// True if --print-json is specified.
    print_json: bool,

    /// The OUTPUT path, or None if not specified.
    dest_path: Option<&'a str>,

    /// The format used to decode.
    ///
    /// The decoder will not attempt to sniff the format used.
    format: binjs::io::Format,
}

fn main() {
    thread::Builder::new()
        .name("large stack dedicated thread".to_string())
        .stack_size(20 * 1024 * 1024)
        .spawn(|| {
            main_aux();
        })
        .expect("Could not launch dedicated thread")
        .join()
        .expect("Error in dedicated thread");
}

fn main_aux() {
    env_logger::init();

    let matches = App::new("BinJS decoder")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Decode a JavaScript BinJS source to a JavaScript text source.")
        .args(&[
            Arg::with_name("INPUT").help(
                "Input file to use. Must be a BinJS source file. If not specified, stdin is used",
            ),
            Arg::with_name("OUTPUT")
                .help("Output file to use. Will be overwritten. If not specified, stdout is used"),
            Arg::with_name("dump")
                .long("dump")
                .takes_value(false)
                .help("If specified, dump a JSON version of the AST."),
            Arg::with_name("quiet")
                .long("quiet")
                .short("q")
                .help("Do not print progress"),
            Arg::with_name("print-json")
                .long("print-json")
                .help("Print JSON of parse tree"),
        ])
        .subcommand(binjs::io::Format::subcommand())
        .get_matches();

    // Prepare grammar (used for entropy).
    let spec = binjs::generic::es6::Library::spec();

    // Common options.
    let source_path = matches.value_of("INPUT");
    let dest_path = matches.value_of("OUTPUT");
    let quiet = matches.is_present("quiet") || dest_path.is_none();

    // Format options.
    let format =
        binjs::io::Format::from_matches(&spec, &matches).expect("Could not parse encoding format");
    progress!(quiet, "Using format: {}", format.name());

    // Setup.
    let mut options = Options {
        print_json: matches.is_present("print-json"),
        dest_path,
        format,
    };

    progress!(quiet, "Reading.");
    let tree: binjs::specialized::es6::ast::Script = match source_path {
        Some(path) => parse_tree(
            &|| BufReader::new(File::open(path).expect("Could not open source")),
            &mut options,
        ),
        None => {
            let mut buffer = Vec::new();
            stdin()
                .read_to_end(&mut buffer)
                .expect("Failed to read from stdin");

            parse_tree(&|| Cursor::new(&buffer), &mut options)
        }
    };

    if options.print_json {
        progress!(quiet, "Printing to screen...");
        println!("{:#}", tree.export());
    }

    progress!(quiet, "Pretty-printing");
    let printer = Shift::try_new().expect("Could not launch Shift");
    let source = printer.to_source(&tree).expect("Could not pretty-print");

    progress!(quiet, "Writing.");
    match options.dest_path {
        Some(path) => {
            fs::write(path, source).expect("Could not write destination file");
        }
        None => {
            stdout()
                .write(source.as_bytes())
                .expect("Could not write destination file");
        }
    }
}

fn parse_tree<R: Read + Seek>(
    get_stream: &Fn() -> R,
    options: &mut Options,
) -> binjs::specialized::es6::ast::Script {
    let decoder = Decoder::new();
    decoder
        .decode(&mut options.format, get_stream())
        .expect("Could not decode")
}
