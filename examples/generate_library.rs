extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate itertools;
extern crate json;
extern crate log;
extern crate yaml_rust;
extern crate webidl;

use binjs::ast::annotation::Annotator;
use binjs::ast::export_utils::{ TypeDeanonymizer, TypeName };
use binjs::ast::grammar::*;
use binjs::ast::webidl::Importer;
use binjs::util::{ ToCases };

use std::collections::{ HashMap, HashSet };
use std::rc::Rc;
use std::fs::*;
use std::io::*;

use clap::*;
use itertools::Itertools;

struct FakeAnnotator;
impl Annotator for FakeAnnotator {
    fn name(&self) -> String {
        "FakeAnnotator".to_string()
    }
}

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
                format!("{prefix}Type::named(&names.{name})",
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
        let fake_annotator = Box::new(FakeAnnotator);
        let deanonymized = TypeDeanonymizer::new(&self.syntax)
            .into_syntax(SyntaxOptions {
                root: self.syntax.get_root_name(),
                null: self.syntax.get_null_name(),
                annotator: fake_annotator
            });

        let mut ast_buffer = String::new();
        ast_buffer.push_str("pub mod ast {\n    use ::util::{ FromJSON, FromJSONError, ToJSON };\n    use std;\n    use json;\n    use json::JsonValue as JSON;\n\n\n");

        let mut struct_buffer = String::new();
        struct_buffer.push_str("pub struct Library {\n");

        let mut impl_buffer = String::new();
        impl_buffer.push_str("impl Library {\n    pub fn new(builder: &mut SyntaxBuilder) -> Self {\n        let names = Library {\n");


        // Export the name definitions.
        fn print_struct_names<'a, T>(buffer: &mut String, source: T) where T: Iterator<Item = &'a NodeName> {
            let mut names : Vec<_> = source.map(|x| x.to_string())
                .collect();
            names.sort();
            for name in names {
                let source = format!("    pub {snake}: NodeName,\n",
                    snake = name.to_rust_identifier_case());
                buffer.push_str(&source);
            }
        }
        fn print_impl_names<'a, T>(buffer: &mut String, source: T) where T: Iterator<Item = &'a NodeName> {
            let mut names : Vec<_> = source.map(|x| x.to_string())
                .collect();
            names.sort();
            for name in names {
                let source = format!("            {snake}: builder.node_name(\"{original}\"),\n",
                    snake = name.to_rust_identifier_case(),
                    original = name);
                buffer.push_str(&source);
            }
        }
        fn print_ast_string_enums(buffer: &mut String, source: &HashMap<NodeName, Rc<StringEnum>>) {
            let mut names : Vec<_> = source.keys()
                .collect();
            names.sort();
            for name in names.drain(..) {
                let string_enum = source.get(&name).unwrap();
                let name = name.to_class_cases();
                let definition = format!("    #[derive(PartialEq, Debug, Clone)]\n    pub enum {name} {{\n{values}\n    }}\n",
                    name = name,
                    values = string_enum.strings()
                        .iter()
                        .map(|s| format!("         {}", ToCases::to_cpp_enum_case(s)))
                        .format(",\n"));
                let to_json = format!("
    impl ToJSON for {name} {{
        fn export(&self) -> JSON {{
            json::from(match *self {{
{cases}
            }})
        }}
    }}\n\n",
                    cases = string_enum.strings()
                        .iter()
                        .map(|s| format!("               {name}::{typed} => \"{string}\"",
                            name = name,
                            typed = s.to_cpp_enum_case(),
                            string = s,
                        ))
                        .format(",\n"),
                    name = name);

                let from_json = format!("
    impl FromJSON for {name} {{
        fn import(source: &JSON) -> Result<Self, FromJSONError > {{
            match source.as_str() {{
{cases},
                _ => Err(FromJSONError {{
                    expected: \"Instance of {name}\".to_string(),
                    got: source.dump(),
                }})
            }}
        }}
    }}\n\n",
                    cases = string_enum.strings()
                        .iter()
                        .map(|s| format!("                Some(\"{string}\") => Ok({name}::{typed})",
                            name = name,
                            typed = s.to_cpp_enum_case(),
                            string = s,
                        ))
                        .format(",\n"),
                    name = name);

                let walker = format!("
    impl Walker for {name} {{
        fn walk<V, E>(&mut self, _: &mut ASTPath, _: &V) -> Result<(), E> where V: Visitor<E> {{
            Ok(())
        }}
    }}\n",
                    name = name);

                buffer.push_str(&definition);
                buffer.push_str(&from_json);
                buffer.push_str(&to_json);
                buffer.push_str(&walker);
                buffer.push_str("\n\n\n");
            }
        }
        fn print_ast_typedefs(buffer: &mut String, source: &HashMap<NodeName, Rc<Type>>) {
            let mut enums = vec![];
            let mut options = vec![];
            let mut lists = vec![];
            let mut primitives = vec![];
            let mut names : Vec<_> = source.keys()
                .collect();
            names.sort();
            for name in names.drain(..) {
                // Since the source is deanonymized, all type definitions are just one layer deep.
                let typedef = source.get(name).unwrap();
                if typedef.is_optional() {
                    options.push(name);
                } else {
                    match *typedef.spec() {
                        TypeSpec::TypeSum(_) => { enums.push(name); }
                        TypeSpec::Array{ .. } => { lists.push(name); }
                        TypeSpec::Boolean | TypeSpec::Number | TypeSpec::String | TypeSpec::Void => { primitives.push(name); }
                        _ => { buffer.push_str(&format!("    // UNIMPLEMENTED: {}\n", name)); }
                    }
                }
            }
            buffer.push_str("\n\n    // Type sums (by lexicographical order)\n");
            for name in enums.drain(..) {
                let typedef = source.get(name).unwrap();
                let name = name.to_class_cases();
                if let TypeSpec::TypeSum(ref sum) = *typedef.spec() {
                    let definition = format!("    #[derive(PartialEq, Debug, Clone)]\n    pub enum {name} {{\n{contents}\n    }}\n",
                        name = name,
                        contents = sum.types()
                            .iter()
                            .map(|t| {
                                if let TypeSpec::NamedType(ref case) = *t {
                                    format!("        {name}(Box<{name}>)",
                                        name = case.to_class_cases())
                                } else {
                                    panic!();
                                }
                            })
                            .format(",\n"));

                            let from_json = format!("
    impl FromJSON for {name} {{
        fn import(value: &JSON) -> Result<Self, FromJSONError> {{
            match value[\"type\"].as_str() {{
{cases},
                _ => Err(FromJSONError {{
                    expected: \"Instance of {kind}\".to_string(),
                    got: value.dump()
                }})
            }}
        }}
    }}\n\n",
                                name = name,
                                kind = name,
                                cases = sum.types()
                                    .iter()
                                    .map(|t| {
                                        if let TypeSpec::NamedType(ref case) = *t {
                                            format!("               Some(\"{case}\") => Ok({name}::{constructor}(Box::new(FromJSON::import(value)?)))",
                                                name = name,
                                                case = case,
                                                constructor = case.to_class_cases())
                                        } else {
                                            panic!();
                                        }
                                    })
                                    .format(",\n")
                                );



                            let to_json = format!("
    impl ToJSON for {name} {{
        fn export(&self) -> JSON {{
            match *self {{
{cases}
            }}
        }}
    }}\n\n",
                                name = name,
                                cases = sum.types()
                                    .iter()
                                    .map(|t| {
                                        if let TypeSpec::NamedType(ref case) = *t {
                                            format!("               {name}::{constructor}(box ref value) => value.export()",
                                                name = name,
                                                constructor = case.to_class_cases())
                                        } else {
                                            panic!();
                                        }
                                    })
                                    .format(",\n")
                                );

                    let walk = format!("
    impl Walker for {name} {{
        fn walk<V, E>(&mut self, path: &mut ASTPath, visitor: &V) -> Result<(), E> where V: Visitor<E> {{
            match *self {{
{cases}
            }}
        }}
    }}
",
                        name = name,
                        cases = sum.types()
                            .iter()
                            .map(|t| {
                                if let TypeSpec::NamedType(ref case) = *t {
                                    format!("               {name}::{constructor}(box ref mut value) => value.walk(path, visitor)",
                                        name = name,
                                        constructor = case.to_class_cases())
                                } else {
                                    panic!();
                                }
                            })
                            .format(",\n")
                        );

                    buffer.push_str(&definition);
                    buffer.push_str(&from_json);
                    buffer.push_str(&to_json);
                    buffer.push_str(&walk);
                    buffer.push_str("\n\n");
                } else {
                    panic!()
                }
            }

            buffer.push_str("\n\n    // Aliases to primitive types (by lexicographical order)\n");
            // FromJSON/ToJSON are already implemented in `binjs::utils`
            for name in primitives.drain(..) {
                let typedef = source.get(name).unwrap();
                let source = format!("     pub type {name} = {contents};\n",
                    name = name.to_class_cases(),
                    contents = match *typedef.spec() {
                        TypeSpec::Boolean => "bool",
                        TypeSpec::Number => "f64",
                        TypeSpec::String => "std::string::String",
                        TypeSpec::Void => "()",
                        _ => panic!("Unexpected type in alias to a primitive type: {name}",
                            name = name)
                    });
                buffer.push_str(&source);
            }
            buffer.push_str("\n\n    // Aliases to list types (by lexicographical order)\n");
            // FromJSON/ToJSON are already implemented in `binjs::utils`
            for name in lists.drain(..) {
                let typedef = source.get(name).unwrap();
                if let TypeSpec::Array { ref contents, ref supports_empty } = *typedef.spec() {
                    if let TypeSpec::NamedType(ref contents) = *contents.spec() {
                        let source = format!("{empty_check}    pub type {name} = Vec<{contents}>;\n",
                            empty_check = if *supports_empty { "" } else { "    // FIXME: Should discard empty vectors.\n" },
                            name = name.to_class_cases(),
                            contents = contents.to_class_cases());
                        buffer.push_str(&source);
                        continue;
                    }
                }
                panic!("Could not implement alias to list type {name}: {contents:?}",
                    contents = typedef.spec(),
                    name = name);
            }
            buffer.push_str("\n\n    // Aliases to optional types (by lexicographical order)\n");
            // FromJSON/ToJSON are already implemented in `binjs::utils`
            for name in options.drain(..) {
                let typedef = source.get(name).unwrap();
                if let TypeSpec::NamedType(ref contents) = *typedef.spec() {
                    let source = format!("    pub type {name} = Option<{contents}>;\n",
                        name = name.to_class_cases(),
                        contents = contents.to_class_cases());
                    buffer.push_str(&source);
                } else {
                    panic!();
                }
            }
        }
        fn print_ast_interfaces(buffer: &mut String, source: &HashMap<NodeName, Rc<Interface>>) {
            let mut names : Vec<_> = source.keys()
                .collect();
            names.sort();
            for name in &names {
                let interface = source.get(name).unwrap();
                let name = name.to_class_cases();
                let definition = format!("    #[derive(PartialEq, Debug, Clone)]\n    pub struct {name} {{\n{fields}\n    }}\n",
                    fields = interface.contents().fields()
                        .iter()
                        .map(|field| {
                            let spec =
                                if field.type_().is_optional() {
                                    TypeName::type_(field.type_())
                                } else {
                                    match *field.type_().spec() {
                                        TypeSpec::NamedType(ref contents) => contents.to_class_cases(),
                                        TypeSpec::Boolean => "bool".to_string(),
                                        TypeSpec::Number => "f64".to_string(),
                                        TypeSpec::String => "String".to_string(),
                                        TypeSpec::Void => "()".to_string(),
                                        _ => TypeName::type_(field.type_())
                                    }
                                };
                            format!("        {name}: {contents}",
                                name = field.name().to_rust_identifier_case(),
                                contents = spec)
                        })
                        .format(",\n"),
                    name = name);

                let from_json = format!("
    impl FromJSON for {name} {{
        fn import(value: &JSON) -> Result<Self, FromJSONError> {{
            match value[\"type\"].as_str() {{
                Some(\"{kind}\") => {{ /* Good */ }},
                _ => return Err(FromJSONError {{
                    expected: \"Instance of {kind}\".to_string(),
                    got: value.dump()
                }})
            }}
            Ok({name} {{ {fields} }})
        }}
    }}\n\n",
                    kind = name,
                    name = name,
                    fields = interface.contents()
                        .fields()
                        .iter()
                        .map(|field| format!("{name}: FromJSON::import(&value[\"{key}\"])?",
                            key = field.name().to_str(),
                            name = field.name().to_rust_identifier_case()))
                        .format(", ")
                    );

                let to_json = format!("
    impl ToJSON for {name} {{
        fn export(&self) -> JSON {{
            object!{{
                \"type\" => json::from(\"{kind}\"),
{fields}
            }}
        }}
    }}\n\n",
                    kind = name,
                    name = name,
                    fields = interface.contents()
                        .fields()
                        .iter()
                        .map(|field| format!("                 \"{key}\" => self.{name}.export()",
                            key = field.name().to_str(),
                            name = field.name().to_rust_identifier_case()))
                        .format(",\n")
                    );

                let walk = format!("
    impl Walker for {name} {{
        fn walk<V, E>(&mut self, path: &mut ASTPath, visitor: &V) -> Result<(), E> where V: Visitor<E> {{
            path.enter_interface(ASTNode::{name});
            if let Some(replacement) = visitor.enter_{snake}(path, self)? {{
                *self = replacement;
            }}
{fields}
            if let Some(replacement) = visitor.exit_{snake}(path, self)? {{
                *self = replacement;
            }}
            path.exit_interface(ASTNode::{name});
            Ok(())
        }}
    }}
",
                    name = name,
                    snake = name.to_rust_identifier_case(),
                    fields = interface.contents()
                        .fields()
                        .iter()
                        .map(|field| {
                            format!("            path.enter_field(ASTField::{variant});
            self.{name}.walk(path, visitor)?;
            path.exit_field(ASTField::{variant});",
                                name = field.name().to_rust_identifier_case(),
                                variant = field.name().to_class_cases())
                        })
                        .format("\n")
                    );
                buffer.push_str(&definition);
                buffer.push_str(&from_json);
                buffer.push_str(&to_json);
                buffer.push_str(&walk);
                buffer.push_str("\n\n\n");
            }

            let interfaces_enum = format!("    #[derive(Clone, Copy, Debug, PartialEq, Eq)]\n     pub enum ASTNode {{
{interfaces}
    }}\n\n\n",
                interfaces = names.iter()
                    .map(|name| format!("        {}", name.to_class_cases()))
                    .format(",\n")
            );
            // Now generate the interface visitors
            let path = "
    pub struct ASTPathItem {
        pub interface: ASTNode,
        pub field: ASTField,
    }
    pub struct ASTPath {
        /// Some(foo) if we have entered interface foo but no field yet.
        /// Otherwise, None.
        interface: Option<ASTNode>,
        items: Vec<ASTPathItem>,
    }
    impl ASTPath {
        fn enter_interface(&mut self, node: ASTNode) {
            debug_assert!(self.interface.is_none());
            self.interface = Some(node);
        }
        fn exit_interface(&mut self, node: ASTNode) {
            let interface = self.interface.take()
                .expect(\"Could not exit_interface if we're not in an interface\");
            debug_assert!(node == interface);
        }
        fn enter_field(&mut self, field: ASTField) {
            let interface = self.interface.take()
                .unwrap();
            self.items.push(ASTPathItem {
                interface,
                field,
            });
        }
        fn exit_field(&mut self, field: ASTField) {
            debug_assert!(self.interface.is_none());
            let ASTPathItem {
                interface,
                field: prev
            } = self.items.pop()
                .expect(\"Could not exit_field from an empty ASTath\");
            debug_assert!(prev == field);
            self.interface = Some(interface);
        }
        fn len(&self) -> usize {
            self.items.len()
        }

        pub fn get(&self, index: usize) -> Option<&ASTPathItem> {
            if index >= self.len() {
                return None;
            }
            Some(&self.items[self.len() - index - 1])
        }
    }
";
            let visitor = format!("
    pub trait Visitor<E> {{
{interfaces}
    }}\n
    pub trait Walker {{
        fn walk<V, E>(&mut self, path: &mut ASTPath, visitor: &V) -> Result<(), E> where V: Visitor<E>;
    }}\n
    impl Walker for String {{
        fn walk<V, E>(&mut self, _: &mut ASTPath, _: &V) -> Result<(), E> where V: Visitor<E> {{
            Ok(())
        }}
    }}
    impl Walker for bool {{
        fn walk<V, E>(&mut self, _: &mut ASTPath, _: &V) -> Result<(), E> where V: Visitor<E> {{
            Ok(())
        }}
    }}
    impl Walker for f64 {{
        fn walk<V, E>(&mut self, _: &mut ASTPath, _: &V) -> Result<(), E> where V: Visitor<E> {{
            Ok(())
        }}
    }}
    impl<T> Walker for Option<T> where T: Walker {{
        fn walk<V, E>(&mut self, path: &mut ASTPath, visitor: &V) -> Result<(), E> where V: Visitor<E> {{
            if let Some(ref mut contents) = *self {{
                contents.walk(path, visitor)?;
            }}
            Ok(())
        }}        
    }}
    impl<T> Walker for Vec<T> where T: Walker {{
        fn walk<V, E>(&mut self, path: &mut ASTPath, visitor: &V) -> Result<(), E> where V: Visitor<E> {{
            for iter in self.iter_mut() {{
                iter.walk(path, visitor)?;
            }}
            Ok(())
        }}
    }}
    \n\n\n",
                interfaces = names.iter()
                    .map(|name| {
                        let interface = source.get(name).unwrap();
                        let name = name.to_rust_identifier_case();
                        format!("
        fn enter_{name}(&self, _path: &ASTPath, _node: &{node_name}) -> Result<Option<{node_name}>, E> {{
            Ok(None)
        }}
        fn exit_{name}(&self, _path: &ASTPath, _node: &{node_name}) -> Result<Option<{node_name}>, E> {{
            Ok(None)
        }}
",
                            name = name,
                            node_name = interface.name().to_class_cases())
                    })
                    .format("\n")
                );
            buffer.push_str(&interfaces_enum);
            buffer.push_str(&path);
            buffer.push_str(&visitor);
        }
        struct_buffer.push_str("    // String enum names (by lexicographical order)\n");
        impl_buffer.push_str("            // String enum names (by lexicographical order)\n");
        ast_buffer.push_str("     // String enums (by lexicographical order)\n");
        print_struct_names(&mut struct_buffer, self.syntax.string_enums_by_name().keys());
        print_impl_names(&mut impl_buffer, self.syntax.string_enums_by_name().keys());
        print_ast_string_enums(&mut ast_buffer, deanonymized.string_enums_by_name());

        struct_buffer.push_str("\n\n    // Typedef names (by lexicographical order)\n");
        impl_buffer.push_str("\n\n            // Typedef names (by lexicographical order)\n");
        print_struct_names(&mut struct_buffer, self.syntax.typedefs_by_name().keys());
        print_impl_names(&mut impl_buffer, self.syntax.typedefs_by_name().keys());
        print_ast_typedefs(&mut ast_buffer, deanonymized.typedefs_by_name());

        struct_buffer.push_str("\n\n    // Interface names (by lexicographical order)\n");
        impl_buffer.push_str("\n\n            // Interface names (by lexicographical order)\n");
        ast_buffer.push_str("\n\n     // Interfaces and interface names (by lexicographical order)\n");
        print_struct_names(&mut struct_buffer, self.syntax.interfaces_by_name().keys());
        print_impl_names(&mut impl_buffer, self.syntax.interfaces_by_name().keys());
        print_ast_interfaces(&mut ast_buffer, deanonymized.interfaces_by_name());

        struct_buffer.push_str("\n\n\n    // Field names (by lexicographical order)\n");
        impl_buffer.push_str("\n\n\n            // Field names (by lexicographical order)\n");
        ast_buffer.push_str("\n\n\n     // Field names (by lexicographical order)\n     #[derive(Clone, Copy, PartialEq, Eq, Debug)]\n    pub enum ASTField {\n");
        let mut fields = HashSet::new();
        for interface in self.syntax.interfaces_by_name().values() {
            for field in interface.contents().fields() {
                fields.insert(field.name().to_string().clone());
            }
        }
        let mut fields : Vec<_> = fields.drain().collect();
        fields.sort();
        for name in fields {
            let snake = name.to_rust_identifier_case();
            let struct_source = format!("    pub field_{snake}: FieldName,\n",
                snake = snake);
            struct_buffer.push_str(&struct_source);

            let impl_source = format!("            field_{snake}: builder.field_name(\"{original}\"),\n",
                snake = snake,
                original = name);
            impl_buffer.push_str(&impl_source);

            let ast_source = format!("        {variant},\n",
                variant = name.to_class_cases());
            ast_buffer.push_str(&ast_source);
        }


        impl_buffer.push_str("        };\n");
        struct_buffer.push_str("}");
        ast_buffer.push_str("    }\n}\n");


        impl_buffer.push_str("\n\n\n        // Enumerations\n");
        for (name, def) in self.syntax.string_enums_by_name() {
            let strings = format!("{strings}",
                strings = def.strings()
                    .iter()
                    .map(|s| format!("                \"{s}\"", s=s))
                    .format(",\n"));
            let impl_source = format!("        builder.add_string_enum(&names.{name}).unwrap()
            .with_strings(&[\n{strings}\n           ]);\n\n",
                name = name.to_rust_identifier_case(),
                strings = strings);
            impl_buffer.push_str(&impl_source);
        }
        for (name, def) in self.syntax.typedefs_by_name() {
            let impl_source = format!("        builder.add_typedef(&names.{name}).unwrap()
            .with_type(\n{spec});\n\n",
                name = name.to_rust_identifier_case(),
                spec = Self::type_(def, "                    "));
            impl_buffer.push_str(&impl_source);
        }
        for (name, def) in self.syntax.interfaces_by_name() {
            let fields = format!("{fields}",
                fields = def.contents()
                    .fields()
                    .iter()
                    .map(|field| format!("            .with_field(\n                 &names.field_{name},\n{type_}\n            )",
                        name = field.name().to_rust_identifier_case(),
                        type_= Self::type_(field.type_(), "                 ")))
                    .format("\n"));
            let impl_source = format!("        builder.add_interface(&names.{name}).unwrap()\n{fields};\n\n",
                name = name.to_rust_identifier_case(),
                fields = fields);
            impl_buffer.push_str(&impl_source);
        }

        impl_buffer.push_str("        names\n    }\n}\n");
        format!("// This file is autogenerated.\nuse ast::grammar::*;\n\n\n{ast_}\n{struct_}\n{impl_}",
            ast_ = ast_buffer,
            struct_ = struct_buffer,
            impl_ = impl_buffer)
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