extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate webidl;

use binjs::ast::webidl::Importer;

use std::fs::*;
use std::io::*;

use clap::*;

fn main() {
    env_logger::init();

    let matches = App::new("BinJS import from webidl")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Import a webidl defining the syntax of JavaScript.")
        .args(&[
            Arg::with_name("INPUT")
                .required(true)
                .help("Input file to use. Must be a webidl source file."),
            Arg::with_name("OUTPUT")
                .required(false)
                .help("Output file to use. Will be overwritten. If none, print to stdout."),
        ])
    .get_matches();

    let source_path = matches.value_of("INPUT")
        .expect("Expected INPUT");

    let mut file = File::open(source_path)
        .expect("Could not open source");
    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Could not read source");

    println!("...parsing");
    let parser = webidl::Parser::new();
    let ast = parser.parse_string(&source)
        .expect("Could not parse source");

    println!("...importing");
    let mut importer = Importer::new();
    importer.import_ast(&ast);

    println!("...exporting");
    let source = importer.builder().into_rust_source();
    println!("{}", source);
}