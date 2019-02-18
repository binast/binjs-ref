extern crate binjs_generate_library;
extern crate binjs_meta;
extern crate clap;
extern crate env_logger;

use binjs_generate_library::*;
use binjs_meta::import::Importer;
use binjs_meta::spec::SpecOptions;

use std::fs::*;
use std::io::*;

use clap::*;

fn main() {
    env_logger::init();

    let matches = App::new("BinJS import from webidl")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Import a webidl defining the spec of JavaScript, export it as a strongly-typed Rust data structure.")
        .args(&[
            Arg::with_name("INPUT.webidl")
                .required(true)
                .help("Input webidl file to use. Must be a webidl source file."),
            Arg::with_name("OUTPUT")
                .required(true)
                .help("Prefix of output files to use. OUTPUT-strong.rs and OUTPUT-generic.rs will be produced."),
        ])
    .get_matches();

    let source_path = matches
        .value_of("INPUT.webidl")
        .expect("Expected INPUT.webidl");
    let dest_path = matches.value_of("OUTPUT").expect("Expected OUTPUT");

    let mut file = File::open(source_path).expect("Could not open source");
    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Could not read source");

    println!("...importing webidl");
    let mut builder =
        Importer::import(vec![source.as_str()].into_iter()).expect("Could not parse source");
    let fake_root = builder.node_name("@@ROOT@@"); // Ignored.
    let null = builder.node_name(""); // Used.
    builder.add_interface(&null).unwrap();
    let spec = builder.into_spec(SpecOptions {
        root: &fake_root,
        null: &null,
    });

    println!("...generating source code");
    let exporter = RustExporter::new(spec);
    let code = exporter.to_rust_source();

    let dest_name = format!("{}-generic.rs", dest_path);
    println!("...exporting generic code to {}", dest_name);
    let mut dest = File::create(dest_name).expect("Could not create rust generic source output");
    dest.write_all(code.generic.as_bytes())
        .expect("Could not write rust generic source output");

    let dest_name = format!("{}-strong.rs", dest_path);
    println!("...exporting strongly-typed code to {}", dest_name);
    let mut dest =
        File::create(dest_name).expect("Could not create rust strongly-typed source output");
    dest.write_all(code.typed.as_bytes())
        .expect("Could not write rust strongly-typed source output");

    println!("...done");
}
