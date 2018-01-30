extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate itertools;
extern crate json;
#[macro_use] extern crate log;
extern crate webidl;

use binjs::ast::annotation::Annotator;
use binjs::ast::grammar::*;
use binjs::ast::webidl::Importer;
use binjs::util::{ ToCases };

use std::collections::{ HashSet };
use std::fs::*;
use std::io::*;

use clap::*;
use itertools::Itertools;

/// Generate Rust source
struct RustExporter<'a> {
    syntax: &'a Syntax
}
impl<'a> RustExporter<'a> {
    fn new(syntax: &'a Syntax) -> Self {
        RustExporter {
            syntax
        }
    }

    pub fn type_(type_: &Type, prefix: &str) -> String {
        let spec = Self::type_spec(type_.spec(), prefix);
        let ref spec = spec.trim_left();
        format!("{prefix}{type_}{close}",
            type_ = spec,
            prefix = prefix,
            close =
                if type_.is_optional() {
                    ".optional()"
                } else {
                    ".required()"
                }
            )
    }
    pub fn type_spec(spec: &TypeSpec, prefix: &str) -> String {
        match *spec {
            TypeSpec::Array { ref contents, supports_empty: false } => {
                format!("{prefix}{contents}.non_empty_array()",
                    contents = Self::type_(contents, prefix),
                    prefix = prefix)
            }
            TypeSpec::Array { ref contents, supports_empty: true } => {
                format!("{prefix}{contents}.array()",
                    contents = Self::type_(contents, prefix),
                    prefix = prefix)
            }
            TypeSpec::Boolean =>
                format!("{prefix}Type::bool()",
                    prefix = prefix),
            TypeSpec::String =>
                format!("{prefix}Type::string()",
                    prefix = prefix),
            TypeSpec::Number =>
                format!("{prefix}Type::number()",
                    prefix = prefix),
            TypeSpec::NamedType(ref name) =>
                format!("{prefix}Type::named(&{name})",
                    name = name.to_rust_identifier_case(),
                    prefix = prefix),
            TypeSpec::TypeSum(ref types) => {
                let indent = format!("{prefix}    ",
                    prefix = prefix);
                format!("{prefix}Type::sum(&[\n{sum}\n{prefix}])",
                    prefix = prefix,
                    sum = types.types()
                        .iter()
                        .map(|t| Self::type_spec(t, &indent))
                        .format(",\n")
                )
            }
            TypeSpec::Void => "void".to_string()
        }
    }


    pub fn to_rust_source(&self) -> String {
        let mut buffer = String::new();

        fn print_names<'a, T>(buffer: &mut String, source: T) where T: Iterator<Item = &'a NodeName> {
            let mut names : Vec<_> = source.map(|x| x.to_string())
                .collect();
            names.sort();
            for name in names {
                let source = format!("let {snake} = syntax.node_name(\"{original}\");\n",
                    snake = name.to_rust_identifier_case(),
                    original = name);
                buffer.push_str(&source);
            }
        }
        buffer.push_str("// String enum names (by lexicographical order)\n");
        print_names(&mut buffer, self.syntax.string_enums_by_name().keys());

        buffer.push_str("\n\n// Typedef names (by lexicographical order)\n");
        print_names(&mut buffer, self.syntax.typedefs_by_name().keys());

        buffer.push_str("\n\n// Interface names (by lexicographical order)\n");
        print_names(&mut buffer, self.syntax.interfaces_by_name().keys());

        buffer.push_str("\n\n\n// Field names (by lexicographical order)\n");
        let mut fields = HashSet::new();
        for interface in self.syntax.interfaces_by_name().values() {
            for field in interface.contents().fields() {
                fields.insert(field.name().to_string().clone());
            }
        }
        let mut fields : Vec<_> = fields.drain().collect();
        fields.sort();
        for name in fields {
            let source = format!("let field_{snake} = syntax.field_name(\"{original}\");\n",
                snake = name.to_rust_identifier_case(),
                original = name);
            buffer.push_str(&source);
        }

        buffer.push_str("\n\n\n// Enumerations\n");
        for (name, def) in self.syntax.string_enums_by_name() {
            let strings = format!("{strings}",
                strings = def.strings()
                    .iter()
                    .map(|s| format!("        \"{s}\"", s=s))
                    .format(",\n"));
            let source = format!("syntax.add_string_enum(&{name}).unwrap()
    .with_strings(&[\n{strings}\n    ]);\n\n",
                name = name.to_rust_identifier_case(),
                strings = strings);
            buffer.push_str(&source);
        }
        for (name, def) in self.syntax.typedefs_by_name() {
            let source = format!("syntax.add_typedef(&{name}).unwrap()
    .with_type(\n{spec});\n\n",
                name = name.to_rust_identifier_case(),
                spec = Self::type_(def, "        "));
            buffer.push_str(&source);
        }
        for (name, def) in self.syntax.interfaces_by_name() {
            let fields = format!("{fields}",
                fields = def.contents()
                    .fields()
                    .iter()
                    .map(|field| format!("    .with_field(\n         &field_{name},\n{type_}\n    )",
                        name = field.name().to_rust_identifier_case(),
                        type_= Self::type_(field.type_(), "         ")))
                    .format("\n"));
            let source = format!("syntax.add_interface(&{name}).unwrap()\n{fields};\n\n",
                name = name.to_rust_identifier_case(),
                fields = fields);
            buffer.push_str(&source);
        }
        buffer
    }
}


fn main() {
    env_logger::init();

    let matches = App::new("BinJS import from webidl")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Import a webidl defining the syntax of JavaScript.")
        .args(&[
            Arg::with_name("INPUT.webidl")
                .required(true)
                .help("Input webidl file to use. Must be a webidl source file."),
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
    let null = builder.node_name("_Null");
    builder.add_interface(&null)
        .unwrap();
    struct FakeAnnotator;
    impl Annotator for FakeAnnotator {
        fn name(&self) -> String {
            "FakeAnnotator".to_string()
        }
    }
    let fake_annotator = Box::new(FakeAnnotator);
    let syntax = builder.into_syntax(SyntaxOptions {
            root: &fake_root,
            null: &null,
            annotator: fake_annotator
        });

    let exporter = RustExporter::new(&syntax);

    println!("...exporting Rust library code");
    let mut dest = File::create(format!("{}-library.rs", dest_path))
        .expect("Could not create rust library source output");
    dest.write_all(exporter.to_rust_source().as_bytes())
        .expect("Could not write rust library source output");

    println!("...done");
}