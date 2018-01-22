use ast::grammar::*;
use inflector;

use json::JsonValue as JSON;

use std::collections::HashSet;

fn to_rust_case(value: &str) -> String {
    use inflector;
    let snake = inflector::cases::snakecase::to_snake_case(value);
    match &snake as &str {
        "super" => "super_".to_string(),
        "type" => "type_".to_string(),
        _ => snake
    }
}

fn to_cpp_case(value: &str) -> String {
    use inflector;
    let snake = inflector::cases::snakecase::to_snake_case(value);
    match &snake as &str {
        "class" => "class_".to_string(),
        "operator" => "operator_".to_string(),
        _ => snake
    }
}

impl StringEnum {
    pub fn to_webidl(&self, prefix: &str, indent: &str) -> String {
        let mut result = format!("{prefix}enum {name} {{\n",
            prefix = prefix,
            name = self.name().to_str());
        {
            let prefix = format!("{prefix}{indent}", prefix=prefix, indent=indent);
            for string in self.strings() {
                result.push_str(&format!("{prefix}\"{string}\",\n", prefix=prefix, string=string));
            }
        }
        result.push_str(prefix);
        result.push_str("}\n");
        result
    }
}
impl Type {
    pub fn to_webidl(&self, prefix: &str, indent: &str) -> String {
        let pretty_type = self.spec.to_webidl(prefix, indent);
        let pretty_default = match self.defaults_to {
            None => String::new(),
            Some(ref default) =>
                format!("? /* = {} */", default.dump())
        };
        format!("{}{}", pretty_type, pretty_default)
    }
}

impl TypeSpec {
    pub fn to_name(&self) -> String {
        match *self {
            TypeSpec::Array { ref contents, supports_empty: false } =>
                format!("{}NonEmptyList", contents.to_name()),
            TypeSpec::Array { ref contents, supports_empty: true } =>
                format!("{}List", contents.to_name()),
            TypeSpec::NamedType(ref name) =>
                name.to_string().clone(),
            TypeSpec::Boolean =>
                "_Bool".to_string(),
            TypeSpec::Number =>
                "_Number".to_string(),
            TypeSpec::String =>
                "_String".to_string(),
            TypeSpec::Void =>
                "_Void".to_string(),
            TypeSpec::TypeSum(ref sum) => {
                let mut result = String::new();
                let mut first = true;
                for item in sum.types() {
                    if first {
                        first = false;
                    } else {
                        result.push_str("Or");
                    }
                    result.push_str(&item.to_name());
                }
                result
            }
        }
    }

    pub fn to_webidl(&self, prefix: &str, indent: &str) -> String {
        match *self {
            TypeSpec::Array { ref contents, supports_empty: false } =>
                format!("FrozenArray<{}> /* Non-empty */", contents.to_webidl(prefix, indent)),
            TypeSpec::Array { ref contents, supports_empty: true } =>
                format!("FrozenArray<{}>", contents.to_webidl(prefix, indent)),
            TypeSpec::Boolean =>
                "bool".to_string(),
            TypeSpec::String =>
                "string".to_string(),
            TypeSpec::Number =>
                "number".to_string(),
            TypeSpec::NamedType(ref name) =>
                name.to_str().to_string(),
            TypeSpec::TypeSum(ref types) => {
                let mut result = String::new();
                result.push('(');
                let mut first = true;
                for typ in types.types() {
                    if first {
                        first = false;
                    } else {
                        result.push_str(" or ");
                    }
                    result.push_str(&typ.to_webidl("", indent));
                }
                result.push(')');
                result
            }
            TypeSpec::Void => "void".to_string()
        }
    }


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

impl Interface {
    pub fn to_webidl(&self, prefix: &str, indent: &str) -> String {
        let mut result = format!("{prefix} interface {name} : Node {{\n", prefix=prefix, name=self.name().to_str());
        {
            let prefix = format!("{prefix}{indent}",
                prefix=prefix,
                indent=indent);
            for field in self.contents().fields() {
                if let Some(ref doc) = field.doc() {
                    result.push_str(&format!("{prefix}// {doc}\n", prefix = prefix, doc = doc));
                }
                result.push_str(&format!("{prefix}{description} {name};\n",
                    prefix = prefix,
                    name = field.name().to_str(),
                    description = field.type_().to_webidl(&prefix, indent)
                ));
                if field.doc().is_some() {
                    result.push_str("\n");
                }
            }
        }
        result.push_str(&format!("{prefix} }}\n", prefix=prefix));
        result
    }
}

impl Type {
    pub fn to_name(&self) -> String {
        let spec_name = self.spec.to_name();
        if self.defaults_to.is_some() {
            format!("{}OrNull", spec_name)
        } else {
            spec_name
        }
    }

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
    /// Generate C++ code for SpiderMonkey
    pub fn to_spidermonkey_cpp(&self) -> String {
        let mut buffer = String::new();

        // 1. Typesums
        buffer.push_str("\n\n// ----- Sums of interfaces (by lexicographical order)\n");

        let mut sums_of_interfaces : Vec<_> = self.get_sums_of_interfaces()
            .collect();
        sums_of_interfaces.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));

        for (name, nodes) in sums_of_interfaces.drain(..) {
            // Generate comments (FIXME: We should use the actual webidl, not the resolved sum)
            let mut nodes : Vec<_> = nodes.iter()
                .collect();
            nodes.sort_by(|a, b| str::cmp(a.to_str(), b.to_str()));
            buffer.push_str("/*\n ");
            buffer.push_str(name.to_str());
            buffer.push_str(" ::= ");
            let mut first = true;
            for node in &nodes {
                if first {
                    first = false;
                } else {
                    buffer.push_str("     ");
                }
                buffer.push_str(node.to_str());
                buffer.push_str("\n");
            }
            buffer.push_str("*/\n");

            // Generate code
            let mut buffer_cases = String::new();
            for node in nodes {
                buffer_cases.push_str(&format!("
          case BinKind::{kind}:
            MOZ_TRY_VAR(result, parse{kind}Aux(kind, fields));
            break;",
                kind = inflector::cases::classcase::to_class_case(node.to_str())));
            }
            buffer.push_str(&format!("JS::Result<ParseNode*>
BinASTParser::parse{kind}()
{{
    BinKind kind;
    BinFields fields(cx_);
    AutoTaggedTuple guard(*tokenizer_);

    TRY(tokenizer_->enterTaggedTuple(kind, fields, guard));
    ParseNode* result(nullptr);
    switch(kind) {{ {cases}
      default:
        return raiseInvalidKind(\"{kind}\", kind);
    }}

    TRY(guard.done());
    return result;
}}

",
    kind = inflector::cases::classcase::to_class_case(name.to_str()),
    cases = buffer_cases));
        }

        // 2. Single interfaces
        // FIXME: This will generate lots of dead code.
        buffer.push_str("\n\n// ----- Interfaces (by lexicographical order)\n");

        for (name, interface) in self.interfaces_by_name() {
            // Generate comments
            let comment = format!("\n/*\n{}*/\n", interface.to_webidl("", "    "));
            buffer.push_str(&comment);

            // Generate full method
            let kind = inflector::cases::classcase::to_class_case(name.to_str());
            buffer.push_str(&format!("JS::Result<ParseNode*>
BinASTParser::parse{kind}()
{{
    BinKind kind;
    BinFields fields(cx_);
    AutoTaggedTuple guard(*tokenizer_);

    TRY(tokenizer_->enterTaggedTuple(kind, fields, guard));
    TRY_DECL(result, parse{kind}Aux(kind, field));
    TRY(guard.done());

    return result;
}}

",
                kind = kind
            ));

            // Generate aux method
            let mut fields_list = String::new();
            fields_list.push_str("{ ");
            let mut first = true;
            for field in interface.contents().fields() {
                if first {
                    first = false;
                } else {
                    fields_list.push_str(", ");
                }
                fields_list.push_str("BinFields::");
                fields_list.push_str(to_cpp_case(field.name().to_str()));
            }
            fields_list.push_str(" }");

            let mut fields_implem = String::new();
            for field in interface.contents().fields() {
                let typename = field.type_().to_name();
                let single_field_implem = format!("    TRY_DECL({name}, parse{typename}());\n",
                    name = to_cpp_case(field.name().to_str()),
                    typename = typename);
                fields_implem.push_str(&single_field_implem);
            }

            buffer.push_str(&format!("JS::Result<ParseNode*>
BinASTParser::parse{kind}Aux(const BinKind kind, const BinFields& fields)
{{
    MOZ_ASSERT(kind == BinKind::{kind});
    MOZ_TRY(checkFields(kind, fields, {fields_list});

{fields_implem}
    // FIXME: Unimplemented
}}

",
                kind = kind,
                fields_list = fields_list,
                fields_implem = fields_implem
            ));
        }

        // 3. String Enums
        buffer
    }
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