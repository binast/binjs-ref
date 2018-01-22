extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate webidl;

use binjs::ast::annotation::Annotator;
use binjs::ast::grammar::SyntaxOptions;
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
                .required(true)
                .help("Prefix of output files to use. Will be overwritten."),
        ])
    .get_matches();

    let source_path = matches.value_of("INPUT")
        .expect("Expected INPUT");
    let dest_path = matches.value_of("OUTPUT")
        .expect("Expected OUTPUT");

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
    let mut builder = Importer::import(&ast);
    let fake_root = builder.node_name("");
    struct FakeAnnotator;
    impl Annotator for FakeAnnotator {
        fn name(&self) -> String {
            "FakeAnnotator".to_string()
        }
    }
    let fake_annotator = Box::new(FakeAnnotator);
    let syntax = builder.into_syntax(SyntaxOptions {
            root: &fake_root,
            annotator: fake_annotator
        });

    println!("...exporting Rust code");
    let mut dest = File::create(format!("{}.rs", dest_path))
        .expect("Could not create Rust source output");
    dest.write_all(syntax.to_rust_source().as_bytes())
        .expect("Could not write Rust source output");

    println!("...exporting C++ header code");
    let mut dest = File::create(format!("{}.h", dest_path))
        .expect("Could not create C++ header source output");
    dest.write_all(syntax.to_spidermonkey_hpp().as_bytes())
        .expect("Could not write C++ header source output");

    println!("...exporting C++ implementation code");
    let mut dest = File::create(format!("{}.cpp", dest_path))
        .expect("Could not create C++ implementation source output");
    dest.write_all(syntax.to_spidermonkey_cpp().as_bytes())
        .expect("Could not write C++ implementation source output");

    println!("...done");
}