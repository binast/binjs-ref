extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate webidl;
extern crate yaml_rust;

use binjs::ast::annotation::Annotator;
use binjs::ast::grammar::SyntaxOptions;
use binjs::ast::webidl::Importer;
use binjs::ast::export_utils::{ FieldParsingRules, InterfaceParsingRules };

use std::collections::HashMap;
use std::fs::*;
use std::io::*;

use clap::*;

fn main() {
    env_logger::init();

    let matches = App::new("BinJS import from webidl")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Import a webidl defining the syntax of JavaScript.")
        .args(&[
            Arg::with_name("INPUT.webidl")
                .required(true)
                .help("Input webidl file to use. Must be a webidl source file."),
            Arg::with_name("INPUT.yaml")
                .required(true)
                .help("Input rules file to use. Must be a yaml source file."),
            Arg::with_name("OUTPUT")
                .required(true)
                .help("Prefix of output files to use. Will be overwritten."),
        ])
    .get_matches();

    let source_path = matches.value_of("INPUT.webidl")
        .expect("Expected INPUT.webidl");
    let dest_path = matches.value_of("OUTPUT")
        .expect("Expected OUTPUT");

    let mut file = File::open(source_path)
        .expect("Could not open source");
    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Could not read source");

    println!("...parsing webidl");
    let parser = webidl::Parser::new();
    let ast = parser.parse_string(&source)
        .expect("Could not parse source");

    println!("...verifying grammar");
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

    let mut computed_rules = HashMap::new();
    if let Some(rules_source_path) = matches.value_of("INPUT.yaml") {
        println!("...generating rules");
        let mut file = File::open(rules_source_path)
            .expect("Could not open rules");
        let mut data = String::new();
        file.read_to_string(&mut data)
            .expect("Could not read rules");

        let yaml = yaml_rust::YamlLoader::load_from_str(&data)
            .expect("Could not parse rules");
        assert_eq!(yaml.len(), 1);
        let rules = yaml[0].as_hash()
            .expect("Rules are not a dictionary");
        for (node_key, node_entries) in rules.iter() {
            let node_key = node_key.as_str()
                .expect("Could not convert node_key to string");
            let node_name = syntax.get_node_name(&node_key)
                .unwrap_or_else(|| panic!("Unknown node name {}", node_key));

            let hash = node_entries.as_hash()
                .unwrap_or_else(|| panic!("Node {} isn't a dictionary"));

            let mut node_rule = InterfaceParsingRules::default();
            for subkey in hash.keys() {
                if let Some(ref as_string) = subkey.as_str() {
                    match *as_string {
                        "init" => {
                            let string = node_entries["init"].as_str()
                                .unwrap_or_else(|| panic!("Rule {}.init must be a string", node_key))
                                .to_string();
                            node_rule.start = Some(string);                            
                        }
                        "build" => {
                            let string = node_entries["build"].as_str()
                                .unwrap_or_else(|| panic!("Rule {}.build must be a string", node_key))
                                .to_string();
                            node_rule.build_result = Some(string);                            
                        }
                        _ if as_string.starts_with("field-") => {
                            let field_key = as_string["fields-".len() - 1..].to_string();
                            let field_name = syntax.get_field_name(&field_key)
                                .unwrap_or_else(|| panic!("In rule {}, can't find field {}",
                                    node_key,
                                    field_key));
                            let ref field_entries = node_entries[*as_string];

                            let mut field_rule = FieldParsingRules::default();

                            let ref block_entries = field_entries["block"];
                            if !block_entries.is_badvalue() {
                                let ref declare = block_entries["declare"];
                                let as_str = declare.as_str()
                                    .unwrap_or_else(|| panic!("Rule {}.field-{}.block.declare must be a string", node_key, field_key))
                                    .to_string();
                                field_rule.declare = Some(as_str);

                                let ref block_before_field = block_entries["before"];
                                if !block_before_field.is_badvalue() {
                                    eprintln!("Found rule {}.field-{}.block.before, inserting in {:?}", node_key, field_key, field_name);
                                    let as_str = block_before_field.as_str()
                                        .unwrap_or_else(|| panic!("Rule {}.field-{}.block.before must be a string", node_key, field_key))
                                        .to_string();
                                    field_rule.block_before_field = Some(as_str);
                                }

                                node_rule.by_field.insert(field_name.clone(), field_rule);
                            }
                        }
                        _ => panic!("Unexpected subkey {}.{}", node_key, as_string)
                    }
                } else {
                    panic!("Unexpected subkey in {}", node_key);
                }
            }

            computed_rules.insert(node_name.clone(), node_rule);
            // FIXME: Check that rules are only for interfaces.
        }
    } else {
        println!("...skipping rules");
    }


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
    dest.write_all(syntax.to_spidermonkey_cpp(computed_rules).as_bytes())
        .expect("Could not write C++ implementation source output");

    println!("...done");
}