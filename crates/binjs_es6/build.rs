extern crate binjs_generate_library;
extern crate binjs_meta;

use binjs_generate_library::*;
use binjs_meta::import::Importer;
use binjs_meta::spec::SpecOptions;

use std::env;
use std::fs;

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
            fs::read_to_string(path)
                .unwrap_or_else(|e| panic!("Could not open grammar file {}: {}", path, e))
        })
        .collect();
    let sources = sources.iter().map(String::as_str);

    // Check spec. We don't really need fake_root
    // for this operation. It may change in the future,
    // we'll see then.

    let mut builder = Importer::import(sources).expect("Could not parse webidl sources");
    let fake_root = builder.node_name("@@ROOT@@"); // Ignored.
    let null = builder.node_name(""); // Actually used
    builder.add_interface(&null).unwrap();
    let spec = builder.into_spec(SpecOptions {
        root: &fake_root,
        null: &null,
    });

    // Generate source code.
    let exporter = RustExporter::new(spec);
    let code = exporter.to_rust_source();

    // Export strongly-typed source.
    let dest_dir = env::var("OUT_DIR").expect("OUT_DIR is not set");
    let dest_name = format!("{}/ast.rs", dest_dir);
    fs::write(dest_name, code.typed).expect("Could not write rust strongly-typed source output");

    println!("...done");
}
