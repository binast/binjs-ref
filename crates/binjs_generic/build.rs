extern crate binjs_generate_library;
extern crate binjs_meta;

use binjs_generate_library::*;
use binjs_meta::import::Importer;
use binjs_meta::spec::SpecOptions;

use std::env;
use std::fs::*;
use std::io::*;

const PATH_GRAMMAR_ES6: &'static str = "../../spec/es6.webidl";

fn main() {
    println!("cargo:rerun-if-changed={}", PATH_GRAMMAR_ES6);

    // Load webidl.

    let mut file = File::open(PATH_GRAMMAR_ES6).expect("Could not open source");
    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Could not read source");

    // Check spec. We don't really need null/fake_root
    // for this operation. It may change in the future,
    // we'll see then.

    let mut builder = Importer::import(&source).expect("Could not parse source");
    let fake_root = builder.node_name(""); // Ignored.
    let null = builder.node_name("_Null"); // Ignored.
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
