use ast::grammar::*;
use inflector;
use itertools::Itertools;

use json::JsonValue as JSON;

use std::collections::{ HashMap, HashSet };

#[derive(Default)]
pub struct FieldParsingRules {
    pub declare: Option<String>,
    pub before_field: Option<String>,
    pub after_field: Option<String>,
    pub block_before_field: Option<String>,
    pub block_after_field: Option<String>,

}
#[derive(Default)]
pub struct InterfaceParsingRules {
    pub start: Option<String>,
    pub by_field: HashMap<FieldName, FieldParsingRules>,
    pub build_result: Option<String>,
}


fn to_rust_case(string: &str) -> String {
    use inflector;
    let snake = inflector::cases::snakecase::to_snake_case(string);
    match &snake as &str {
        "super" => "super_".to_string(),
        "type" => "type_".to_string(),
        _ => snake
    }
}


fn to_cpp_case(string: &str) -> String {
    use inflector;
    let snake = inflector::cases::snakecase::to_snake_case(string);
    match &snake as &str {
        "class" => "class_".to_string(),
        "operator" => "operator_".to_string(),
        "const" => "const_".to_string(),
        "void" => "void_".to_string(),
        "delete" => "delete_".to_string(),
        "in" => "in_".to_string(),
        "result" => "result_".to_string(),
        "" => unimplemented!(),
        _ => snake
    }
}

fn to_cpp_enum_case(string: &str) -> String {
    match string {
        "+=" => "PlusAssign".to_string(),
        "-=" => "MinusAssign".to_string(),
        "*=" => "MulAssign".to_string(),
        "/=" => "DivAssign".to_string(),
        "%=" => "ModAssign".to_string(),
        "**=" => "PowAssign".to_string(),
        "<<=" => "LshAssign".to_string(),
        ">>=" => "RshAssign".to_string(),
        ">>>=" => "UrshAssign".to_string(),
        "|=" => "BitOrAssign".to_string(),
        "^=" => "BitXorAssign".to_string(),
        "&=" => "BitAndAssign".to_string(),
        "," => "Comma".to_string(),
        "||" => "LogicalOr".to_string(),
        "&&" => "LogicalAnd".to_string(),
        "|" => "BitOr".to_string(),
        "^" => "BitXor".to_string(),
        "&" => "BitAnd".to_string(),
        "==" => "Eq".to_string(),
        "!=" => "Neq".to_string(),
        "===" => "StrictEq".to_string(),
        "!==" => "StrictNeq".to_string(),
        "<" => "LessThan".to_string(),
        "<=" => "LeqThan".to_string(),
        ">" => "GreaterThan".to_string(),
        ">=" => "GeqThan".to_string(),
        "<<" => "Lsh".to_string(),
        ">>" => "Rsh".to_string(),
        ">>>" => "Ursh".to_string(),
        "+" => "Plus".to_string(),
        "-" => "Minus".to_string(),
        "~" => "Opposite".to_string(),
        "*" => "Mul".to_string(),
        "/" => "Div".to_string(),
        "%" => "Mod".to_string(),
        "**" => "Pow".to_string(),
        "!" => "Not".to_string(),
        "++" => "Incr".to_string(),
        "--" => "Decr".to_string(),
        _ => {
            let class_cased = inflector::cases::classcase::to_class_case(string);
            assert!(&class_cased != "");
            class_cased
        }
    }
}

fn reindent(prefix: &str, snippet: &Option<String>) -> String {
    match *snippet {
        None => "".to_string(),
        Some(ref string) => {
            // Determine the number of whitespace chars on the first line.
            // Trim that many whitespace chars on the following lines.
            if let Some(first_line) = string.lines().next() {
                let remove_indent = first_line.chars()
                    .take_while(|c| char::is_whitespace(*c))
                    .count();
                let mut lines = vec![];
                for line in string.lines() {
                    lines.push(format!("{prefix}{text}\n",
                        prefix = prefix,
                        text = line[remove_indent..].to_string()
                    ));
                }
                format!("\n{}", lines.iter().format(""))
            } else {
                "".to_string()
            }
        }
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
                format!("{}", sum.types()
                    .iter()
                    .map(TypeSpec::to_name)
                    .format("Or"))
            }
        }
    }

    pub fn to_cpp_arg_name(&self, syntax: &Syntax) -> Option<String> {
        let result = match *self {
            TypeSpec::Void => {
                return None;
            }
            TypeSpec::String => "HandleAtom".to_string(),
            TypeSpec::Number => "double".to_string(),
            TypeSpec::Boolean => "bool".to_string(),
            TypeSpec::TypeSum(_) => "ParseNode*".to_string(),
            TypeSpec::NamedType(ref key) => {
                match syntax.get_type_by_name(key).unwrap() {
                    NamedType::Interface(_) => "ParseNode*".to_string(),
                    NamedType::StringEnum(_) => inflector::cases::classcase::to_class_case(key.to_str()),
                    NamedType::Typedef(ref type_) => {
                        return type_.to_cpp_arg_name(syntax)
                    }
                }
            }
            _ => "ParseNode*".to_string()
        };
        Some(result)
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
            TypeSpec::TypeSum(ref sum) => {
                format!("({})", sum.types()
                    .iter()
                    .map(|x| x.to_webidl("", indent))
                    .format(" or "))
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

    pub fn to_cpp_arg_name(&self, syntax: &Syntax) -> Option<String> {
        self.spec.to_cpp_arg_name(syntax)
    }
}

impl Syntax {
    /// Generate C++ code for SpiderMonkey
    pub fn to_spidermonkey_hpp(&self) -> String {
        let mut buffer = String::new();

        // 1. String Enums
        buffer.push_str("\n\n// ----- Declaring string enums (by lexicographical order)\n");
        let mut string_enums_by_name : Vec<_> = self.string_enums_by_name()
            .iter()
            .collect();
        string_enums_by_name.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        for (name, enum_) in string_enums_by_name {
            let rendered_cases = enum_.strings()
                .iter()
                .map(|str| to_cpp_enum_case(*&str))
                .format(",\n    ");
            let rendered = format!("enum class {name} {{\n    {cases}\n}};\n\n",
                cases = rendered_cases,
                name = inflector::cases::classcase::to_class_case(name.to_str()));
            buffer.push_str(&rendered);
        }

        // 2. Class

        buffer.push_str("\n\n\n /*insert me into */class BinASTParser {\n");

        // 2.1 Sums of interfaces
        let mut sums_of_interfaces : Vec<_> = self.get_sums_of_interfaces()
            .collect();
        sums_of_interfaces.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        buffer.push_str("\n\n    // ----- Sums of interfaces (by lexicographical order)\n");
        buffer.push_str("    // Implementations are autogenerated\n");
        buffer.push_str("    // `ParseNode*` may never be nullptr\n");
        for (name, _) in sums_of_interfaces {
            let rendered = format!("    JS::Result<ParseNode*> parse{kind}();\n",
                kind = inflector::cases::classcase::to_class_case(name.to_str()));
            buffer.push_str(&rendered);
        }

        // 2.b Interfaces
        buffer.push_str("\n\n    // ----- Interfaces (by lexicographical order)\n");
        buffer.push_str("    // Implementations are autogenerated\n");
        buffer.push_str("    // `ParseNode*` may never be nullptr\n");
        let mut interfaces_by_name : Vec<_> = self.interfaces_by_name()
            .iter()
            .collect();
        interfaces_by_name.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        for &(name, _) in &interfaces_by_name {
            let rendered = format!("    JS::Result<ParseNode*> parse{kind}();\n    JS::Result<ParseNode*> parseInterface{kind}(const size_t start, const BinKind kind, const BinFields& fields);\n",
                kind = inflector::cases::classcase::to_class_case(name.to_str()));
            buffer.push_str(&rendered);
        }

        // 2.d Sums of interfaces
        buffer.push_str("\n\n    // ----- String enums (by lexicographical order)\n");
        buffer.push_str("    // Implementations are autogenerated\n");
        let mut string_enums_by_name : Vec<_> = self.string_enums_by_name()
            .iter()
            .collect();
        string_enums_by_name.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        for (name, _) in string_enums_by_name {
            let rendered = format!("    JS::Result<{kind}> parse{kind}();\n",
                kind = inflector::cases::classcase::to_class_case(name.to_str()));
            buffer.push_str(&rendered);
        }
        buffer.push_str("\n};\n");

        buffer
    }

    /// Generate C++ code for SpiderMonkey
    pub fn to_spidermonkey_cpp(&self, rules: HashMap<NodeName, InterfaceParsingRules>) -> String {
        let mut buffer = String::new();

        // 1. Typesums
        buffer.push_str("\n\n// ----- Sums of interfaces (autogenerated, by lexicographical order)\n");

        let mut sums_of_interfaces : Vec<_> = self.get_sums_of_interfaces()
            .collect();
        sums_of_interfaces.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));

        for (name, nodes) in sums_of_interfaces.drain(..) {
            // Generate comments (FIXME: We should use the actual webidl, not the resolved sum)
            let mut nodes : Vec<_> = nodes.iter()
                .collect();
            nodes.sort_by(|a, b| str::cmp(a.to_str(), b.to_str()));
            let rendered_bnf = format!("/*\n{name} ::= {nodes} \n*/",
                nodes = nodes.iter()
                    .map(|x| NodeName::to_str(x))
                    .format("\n    "),
                name = name.to_str());

            // Generate code
            let mut buffer_cases = String::new();
            for node in nodes {
                buffer_cases.push_str(&format!("
      case BinKind::{kind}:
        MOZ_TRY_VAR(result, parse{kind}(start, kind, fields));
        break;",
                kind = inflector::cases::classcase::to_class_case(node.to_str())));
            }
            buffer.push_str(&format!("{bnf}\nJS::Result<ParseNode*>
BinASTParser::parse{kind}()
{{
    BinKind kind;
    BinFields fields(cx_);
    AutoTaggedTuple guard(*tokenizer_);

    TRY(tokenizer_->enterTaggedTuple(kind, fields, guard));

    const auto start = tokenizer_->offset();
    ParseNode* result(nullptr);
    switch(kind) {{ {cases}
      default:
        return raiseInvalidKind(\"{kind}\", kind);
    }}

    TRY(guard.done());
    return result;
}}

",
    bnf= rendered_bnf,
    kind = inflector::cases::classcase::to_class_case(name.to_str()),
    cases = buffer_cases));
        }

        // 2. Single interfaces
        buffer.push_str("\n\n// ----- Interfaces (autogenerated, by lexicographical order)\n");
        let mut interfaces_by_name : Vec<_> = self.interfaces_by_name()
            .iter()
            .collect();
        interfaces_by_name.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));

        // FIXME: This will generate lots of dead code.

        for (name, interface) in interfaces_by_name {
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
    const auto start = tokenizer_->offset();

    TRY_DECL(result, parseInterface{kind}(start, kind, field));
    TRY(guard.done());

    return result;
}}

",
                kind = kind
            ));

            // Generate aux method
            let fields_type_list = format!("{{ {} }}", interface.contents()
                .fields()
                .iter()
                .map(|field| format!("BinFields::{}", to_cpp_enum_case(field.name().to_str())))
                .format(", "));

            let rules_for_this_interface = rules.get(name);
            let mut fields_implem = String::new();
            for field in interface.contents().fields() {
                let rules_for_this_field = match rules_for_this_interface {
                    None => None,
                    Some(ref rule) => rule.by_field.get(field.name())
                };
                let needs_block = if let Some(ref rule) = rules_for_this_field {
                    rule.block_before_field.is_some() || rule.block_after_field.is_some()
                } else {
                    false
                };
                let var_name = to_cpp_case(field.name().to_str());
                let (decl_var, parse_var) = match *field.type_().spec() {
                    TypeSpec::String => {
                        (Some(format!("RootedAtom {var_name}(cx);", var_name = var_name)),
                         Some(format!("TRY(readString({var_name}));", var_name = var_name)))
                    }
                    TypeSpec::Number => {
                        (Some(format!("double {var_name};", var_name = var_name)),
                         Some(format!("TRY(readNumber({var_name}));", var_name = var_name)))
                    }
                    TypeSpec::Boolean => {
                        (Some(format!("bool {var_name};", var_name = var_name)),
                         Some(format!("TRY(readBool({var_name}));", var_name = var_name)))
                    }
                    TypeSpec::Void => {
                        (Some(format!("// Skipping void field {}", field.name().to_str())),
                         None)
                    }
                    _ => {
                        let typename = field.type_().to_name();
                        if needs_block {
                            (Some(format!("{typename} {var_name};",
                                var_name = var_name,
                                typename = typename)),
                             Some(format!("TRY_VAR({var_name}, parse{typename}());",
                                var_name = var_name,
                                typename = typename)
                            ))
                        } else {
                            (None,
                             Some(format!("TRY_DECL({var_name}, parse{typename}());",
                                var_name = var_name,
                                typename = typename)))
                        }
                    }
                };

                let rendered = if let Some(ref rule) = rules_for_this_field {
                    let before_field = reindent("    ", &rule.before_field);
                    let after_field = reindent("    ", &rule.after_field);
                    let decl_var = if rule.declare.is_some() {
                        reindent("    ", &rule.declare)
                     } else {
                        reindent("    ", &decl_var)
                    };
                    if needs_block {
                        let parse_var = reindent("        ", &parse_var);
                        format!("{before_field} {decl_var}    {{
    {block_before_field}{parse_var}{block_after_field}    }} {after_field}",
                            before_field = before_field,
                            decl_var = decl_var,
                            parse_var = parse_var,
                            after_field = after_field,
                            block_before_field = reindent("        ", &rule.block_before_field),
                            block_after_field = reindent("        ", &rule.block_after_field))
                    } else {
                        let parse_var = reindent("    ", &parse_var);
                        format!("{before_field}{decl_var}{parse_var}{after_field}",
                            before_field = before_field,
                            decl_var = decl_var,
                            parse_var = parse_var,
                            after_field = after_field)
                    }
                } else {
                    format!("{decl_var}{parse_var}",
                        decl_var = reindent("    ", &decl_var),
                        parse_var = reindent("    ", &parse_var))
                };
                fields_implem.push_str(&rendered);
            }

            let (start, build_result) = if let Some(ref rule) = rules_for_this_interface {
                (reindent("    ", &rule.start),
                 reindent("    ", &rule.build_result))
            } else {
                ("".to_string(), "".to_string())
            };

            buffer.push_str(&format!("JS::Result<ParseNode*>
BinASTParser::parseInterface{kind}(const size_t start, const BinKind kind, const BinFields& fields)
{{
    MOZ_ASSERT(kind == BinKind::{kind});
    MOZ_TRY(checkFields(kind, fields, {fields_type_list});
{pre}
{fields_implem}
{post}
    return result;
}}

",
                kind = kind,
                fields_type_list = fields_type_list,
                fields_implem = fields_implem,
                pre = start,
                post = build_result
            ));
        }


        // 3. String Enums
        buffer.push_str("\n\n// ----- String enums (autogenerated, by lexicographical order)\n");
        let mut string_enums_by_name : Vec<_> = self.string_enums_by_name()
            .iter()
            .collect();
        string_enums_by_name.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        for (name, enum_) in string_enums_by_name {
            let kind = inflector::cases::classcase::to_class_case(name.to_str());
            let mut cases = String::new();
            for string in enum_.strings() {
                cases.push_str(&format!("if (*chars == \"{string}\")
        return {kind}::{variant};\n    else ",
                    string = string,
                    kind = kind,
                    variant = to_cpp_enum_case(&string)
                ));
            }
            cases.push_str(&format!("\n        return raiseInvalidEnum(\"{kind}\", *chars);",
                kind = kind));

            buffer.push_str(&format!("JS::Result<{kind}>
BinASTParser::parse{kind}()
{{
    // Unoptimized implementation.
    Chars chars;
    MOZ_TRY(readString(chars));

    {cases}
}}

",
                kind = kind,
                cases = cases,
            ));
        }
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