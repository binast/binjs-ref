use ast::grammar::*;

use util::to_rust_case;

use json::JsonValue as JSON;

use std::collections::HashSet;

impl TypeSpec {
    pub fn to_rust_source(&self) -> String {
        match *self {
            TypeSpec::Array { ref contents, supports_empty: false } => {
                format!("{}.non_empty_array()", contents.to_rust_source())
            }
            TypeSpec::Array { ref contents, supports_empty: true } => {
                format!("{}.array()", contents.to_rust_source())
            }
            TypeSpec::Boolean => "Type::bool()".to_string(),
            TypeSpec::String => "Type::string()".to_string(),
            TypeSpec::Number => "Type::number()".to_string(),
            TypeSpec::NamedType(ref name) => format!("Type::named(&{})", to_rust_case(name.to_str())),
            TypeSpec::TypeSum(ref types) => {
                let mut source = String::new();
                let mut first = true;
                for type_ in types.types() {
                    if first {
                        first = false;
                    } else {
                        source.push_str(",");
                    }
                    source.push_str("\n\t");
                    source.push_str(&type_.to_rust_source())
                }
                format!("Type::sum(&[{}\n])", source)
            }
            TypeSpec::Void => "void".to_string()
        }
    }
}

impl Type {
    pub fn to_rust_source(&self) -> String {
        let pretty_type = self.spec.to_rust_source();
        match self.defaults_to {
            None => format!("{}.close()", pretty_type),
            Some(JSON::Null) => {
                format!("{}.defaults_to(JSON::Null)",
                    pretty_type)
            }
            _ => unimplemented!()
        }
    }
}

impl Syntax {
    pub fn to_rust_source(&self) -> String {
        let mut buffer = String::new();

        fn print_names<'a, T>(buffer: &mut String, source: T) where T: Iterator<Item = &'a NodeName> {
            use inflector;
            let mut names : Vec<_> = source.map(|x| x.to_string())
                .collect();
            names.sort();
            for name in names {
                let source = format!("let {snake} = syntax.node_name(\"{original}\"); // C name {cname}\n",
                    snake = to_rust_case(name),
                    original = name,
                    cname = inflector::cases::camelcase::to_camel_case(name));
                buffer.push_str(&source);
            }
        }
        buffer.push_str("// String enum names (by lexicographical order)\n");
        print_names(&mut buffer, self.string_enums_by_name().keys());

        buffer.push_str("\n\n// Typedef names (by lexicographical order)\n");
        print_names(&mut buffer, self.typedefs_by_name().keys());

        buffer.push_str("\n\n// Resolving sums of interfaces (by lexicographical order)\n");
        let mut sums_of_interfaces : Vec<_> = self.get_sums_of_interfaces()
            .collect();
        sums_of_interfaces.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));

        for (name, nodes) in sums_of_interfaces.drain(..) {
            let mut nodes : Vec<_> = nodes.iter()
                .collect();
            nodes.sort_by(|a, b| str::cmp(a.to_str(), b.to_str()));
            buffer.push_str("/*\n ");
            buffer.push_str(name.to_str());
            buffer.push_str(" ::= ");
            let mut first = true;
            for node in nodes {
                if first {
                    first = false;
                } else {
                    buffer.push_str("     ");
                }
                buffer.push_str(node.to_str());
                buffer.push_str("\n");
            }
            buffer.push_str("*/\n");
        }

        buffer.push_str("\n\n// Interface names (by lexicographical order)\n");
        print_names(&mut buffer, self.interfaces_by_name().keys());

        buffer.push_str("\n\n\n// Field names (by lexicographical order)\n");
        let mut fields = HashSet::new();
        for interface in self.interfaces_by_name().values() {
            for field in interface.contents().fields() {
                fields.insert(field.name().to_string().clone());
            }
        }
        let mut fields : Vec<_> = fields.drain().collect();
        fields.sort();
        for name in fields {
            let source = format!("let field_{snake} = syntax.field_name(\"{original}\");\n",
                snake = to_rust_case(&name),
                original = name);
            buffer.push_str(&source);
        }

        buffer.push_str("\n\n\n// Enumerations\n");
        for (name, def) in self.string_enums_by_name() {
            let mut strings = String::new();
            let mut first = true;
            for string in def.strings() {
                if first {
                    first = false;
                } else {
                    strings.push_str(",\n\t");
                }
                strings.push_str("\"");
                strings.push_str(&*string);
                strings.push_str("\"");
            }
            let source = format!("syntax.add_string_enum(&{name}).unwrap()
                .with_strings(&[
                    {strings}
                ]);\n\n",
                name = to_rust_case(name.to_str()),
                strings = strings);
            buffer.push_str(&source);
        }
        for (name, def) in self.typedefs_by_name() {
            let source = format!("syntax.add_typedef(&{name}).unwrap()
                .with_type({spec});\n\n",
                name = to_rust_case(name.to_str()),
                spec = def.to_rust_source());
            buffer.push_str(&source);
        }
        for (name, def) in self.interfaces_by_name() {
            let mut fields = String::new();
            for field in def.contents().fields() {
                let source = format!("\n  .with_field(&field_{name}, {spec})",
                    name = to_rust_case(field.name().to_str()),
                    spec = field.type_().to_rust_source());
                fields.push_str(&source);
            }
            let source = format!("syntax.add_interface(&{name}).unwrap(){fields};\n\n",
                name = to_rust_case(name.to_str()),
                fields = fields);
            buffer.push_str(&source);
        }
        buffer
    }
}