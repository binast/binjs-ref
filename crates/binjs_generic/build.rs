extern crate binjs_generate_library;
extern crate binjs_meta;

use binjs_generate_library::*;
use binjs_meta::import::Importer;
use binjs_meta::spec::SpecOptions;

use std::env;
use std::fs::*;
use std::io::*;

/// The webidl source files.
///
/// Order is important: first the main file, then any extension.
const PATH_SOURCES: [&'static str; 2] =
    ["../../spec/es6.webidl", "../../spec/es6-extension.webidl"];

fn main() {
    for source in &PATH_SOURCES {
        println!("cargo:rerun-if-changed={}", source);
    }

    // Load webidl.

    let sources: Vec<_> = PATH_SOURCES
        .iter()
        .map(|path| {
            read_to_string(path)
                .unwrap_or_else(|e| panic!("Could not open grammar file {}: {}", path, e))
        })
        .collect();
    let sources = sources.iter().map(String::as_str);

    let mut builder = Importer::import(sources).expect("Could not parse webidl sources");
    let fake_root = builder.node_name("Program");
    let null = builder.node_name("");
    builder.add_interface(&null).unwrap();
    let spec = builder.into_spec(SpecOptions {
        root: &fake_root,
        null: &null,
    });

    // Generate source code.
    let exporter = RustExporter::new(spec);
    let code = exporter.to_rust_source();

    // Export generic source.
    let dest_dir = env::var("OUT_DIR").expect("OUT_DIR is not set");
    let dest_name = format!("{}/es6.rs", dest_dir);
    let mut dest = File::create(dest_name).expect("Could not create rust generic source output");
    dest.write_all(code.generic.as_bytes())
        .expect("Could not write rust generic source output");

    println!("...done");
}
