//! Decode a BinJS to a text source.

extern crate binjs;
extern crate clap;
extern crate env_logger;

use binjs::io::Deserialization;
use binjs::generic::ToJSON;
use binjs::source::Shift;

use std::fs::*;
use std::io::*;

use clap::*;

fn main() {
    env_logger::init();

    let matches = App::new("BinJS decoder")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Decode a JavaScript BinJS source to a JavaScript text source.")
        .args(&[
            Arg::with_name("INPUT")
                .required(true)
                .help("Input file to use. Must be a BinJS source file."),
            Arg::with_name("OUTPUT")
                .required(true)
                .help("Output file to use. Will be overwritten."),
            Arg::with_name("dump")
                .long("dump")
                .takes_value(false)
                .help("If specified, dump a JSON version of the AST.")
        ])
    .get_matches();

    let source_path = matches.value_of("INPUT")
        .expect("Expected input file");
    let dest_path = matches.value_of("OUTPUT")
        .expect("Expected output file");

    // Setup.
    let printer = Shift::new();

    println!("Reading.");
    let file = File::open(source_path)
        .expect("Could not open source");
    let stream = BufReader::new(file);

    println!("Attempting to decode as multipart.");
    let tree : binjs::specialized::es6::ast::Script = if let Ok(reader) = binjs::io::multipart::TreeTokenReader::new(stream) {
        let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);
        deserializer.deserialize()
            .expect("Could not decode")
    } else {
        println!("... falling back to simple format.");

        let file = File::open(source_path)
            .expect("Could not open source");
        let stream = BufReader::new(file);

        let reader = binjs::io::simple::TreeTokenReader::new(stream);
        let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);
        deserializer.deserialize()
            .expect("Could not decode")
    };

    let json = tree.export();
    if matches.is_present("print-json") {
        println!("Printing to screen...");
        let pretty = json.pretty(2);
        println!("{}", pretty);
    }

    println!("Pretty-printing");
    let mut builder = binjs::meta::spec::SpecBuilder::new();
    let _ = binjs::generic::es6::Library::new(&mut builder);
    let spec_options = binjs::meta::spec::SpecOptions {
        null: &builder.node_name(""),
        root: &builder.node_name("Script"),
    };
    let spec = builder.into_spec(spec_options);
    let source = printer.to_source(&spec, &json)
        .expect("Could not pretty-print");

    println!("Writing.");
    let mut dest = File::create(dest_path)
        .expect("Could not create destination file");
    dest.write(source.as_bytes())
        .expect("Could not write destination file");
}
