extern crate binjs_meta;
extern crate clap;
extern crate env_logger;
extern crate itertools;
#[macro_use] extern crate log;
extern crate webidl;
extern crate yaml_rust;

use binjs_meta::export::{ TypeDeanonymizer, TypeName };
use binjs_meta::import::Importer;
use binjs_meta::spec::*;
use binjs_meta::util::*;

use std::collections::{ HashMap, HashSet };
use std::fs::*;
use std::io::*;

use clap::*;

use itertools::Itertools;

#[derive(Clone, Default)]
pub struct FieldParsingRules {
    pub declare: Option<String>,
    /// Replace the declaration and assignation.
    pub replace: Option<String>,
    pub before_field: Option<String>,
    pub after_field: Option<String>,
    pub block_before_field: Option<String>,
    pub block_after_field: Option<String>,

}
#[derive(Clone, Default)]
pub struct NodeParsingRules {
    /// This node inherits from another node.
    pub inherits: Option<NodeName>,

    /// Override the result type for the method.
    pub type_ok: Option<String>,

    pub start: Option<String>,

    /// Append to a list. Used only for lists.
    pub append: Option<String>,

    /// Custom per-field treatment. Used only for interfaces.
    pub by_field: HashMap<FieldName, FieldParsingRules>,
    pub build_result: Option<String>,
}
impl GenerationRules {
    fn get(&self, name: &NodeName) -> NodeParsingRules {
        let mut rules = self.per_node.get(name)
            .cloned()
            .unwrap_or_default();
        let inherits = rules.inherits.clone();
        if let Some(ref parent) = inherits {
            let NodeParsingRules {
                inherits,
                type_ok,
                start,
                append,
                by_field,
                build_result,
            } = self.get(parent);
            if rules.inherits.is_none() {
                rules.inherits = inherits;
            }
            if rules.type_ok.is_none() {
                rules.type_ok = type_ok;
            }
            if rules.start.is_none() {
                rules.start = start;
            }
            if rules.append.is_none() {
                rules.append = append;
            }
            if rules.build_result.is_none() {
                rules.build_result = build_result;
            }
            for (key, value) in by_field {
                rules.by_field.entry(key)
                    .or_insert(value);
            }
        }
        rules
    }
}
#[derive(Default)]
pub struct GenerationRules {
    cpp_header: Option<String>,
    cpp_footer: Option<String>,
    hpp_class_header: Option<String>,
    hpp_tokens_header: Option<String>,
    hpp_tokens_footer: Option<String>,
    hpp_tokens_kind_doc: Option<String>,
    hpp_tokens_field_doc: Option<String>,
    per_node: HashMap<NodeName, NodeParsingRules>,
}

struct ToWebidl;
impl ToWebidl {
    pub fn spec(spec: &TypeSpec, prefix: &str, indent: &str) -> String {
        match *spec {
            TypeSpec::Array { ref contents, supports_empty: false } =>
                format!("[NonEmpty] FrozenArray<{}>", Self::type_(&*contents, prefix, indent)),
            TypeSpec::Array { ref contents, supports_empty: true } =>
                format!("FrozenArray<{}>", Self::type_(&*contents, prefix, indent)),
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
                    .map(|x| Self::spec(x, "", indent))
                    .format(" or "))
            }
            TypeSpec::Void => "void".to_string()
        }
    }

    pub fn type_(type_: &Type, prefix: &str, indent: &str) -> String {
        let pretty_type = Self::spec(type_.spec(), prefix, indent);
        format!("{}{}", pretty_type, if type_.is_optional() { "?" } else { "" })
    }

    pub fn interface(interface: &Interface, prefix: &str, indent: &str) -> String {
        let mut result = format!("{prefix} interface {name} : Node {{\n", prefix=prefix, name=interface.name().to_str());
        {
            let prefix = format!("{prefix}{indent}",
                prefix=prefix,
                indent=indent);
            for field in interface.contents().fields() {
                if let Some(ref doc) = field.doc() {
                    result.push_str(&format!("{prefix}// {doc}\n", prefix = prefix, doc = doc));
                }
                result.push_str(&format!("{prefix}{description} {name};\n",
                    prefix = prefix,
                    name = field.name().to_str(),
                    description = Self::type_(field.type_(), &prefix, indent)
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


pub struct CPPExporter {
    syntax: Spec,
    rules: GenerationRules,
    list_parsers_to_generate: Vec<(NodeName, (/* supports_empty */ bool, NodeName))>,
    option_parsers_to_generate: Vec<(NodeName, NodeName)>,
    webidl_file_name: String,
    yaml_file_name: String,
}

impl CPPExporter {
    pub fn new(deanonymizer: TypeDeanonymizer, webidl_file_name: String, yaml_file_name: String, options: SpecOptions) -> Self {
        let syntax = deanonymizer.into_spec(options);

        let mut list_parsers_to_generate = vec![];
        let mut option_parsers_to_generate = vec![];
        for (parser_node_name, typedef) in syntax.typedefs_by_name() {
            if typedef.is_optional() {
                let content_name = TypeName::type_(typedef); // FIXME: Wait, do we have an implementation of type names in two places?
                let content_node_name = syntax.get_node_name(&content_name)
                    .unwrap_or_else(|| panic!("While generating an option parser, could not find node name {}", content_name))
                    .clone();
                option_parsers_to_generate.push((parser_node_name.clone(), content_node_name));
            } else if let TypeSpec::Array { ref contents, ref supports_empty } = *typedef.spec() {
                let content_name = TypeName::type_(&**contents); // FIXME: Wait, do we have an implementation of type names in two places?
                let content_node_name = syntax.get_node_name(&content_name)
                    .unwrap_or_else(|| panic!("While generating an array parser, could not find node name {}", content_name))
                    .clone();
                list_parsers_to_generate.push((parser_node_name.clone(), (*supports_empty, content_node_name)));
            }
        }
        list_parsers_to_generate.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        option_parsers_to_generate.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));

        CPPExporter {
            syntax,
            rules: GenerationRules::default(),
            list_parsers_to_generate,
            option_parsers_to_generate,
            webidl_file_name,
            yaml_file_name,
        }
    }

    fn set_export_rules(&mut self, rules: GenerationRules) {
        self.rules = rules;
    }
}

// ----- Generating the header
impl CPPExporter {
    fn get_type_ok(&self, name: &NodeName, default: &str) -> String {
        let rules_for_this_interface = self.rules.get(name);
        // If the override is provided, use it.
        if let Some(ref type_ok) = rules_for_this_interface.type_ok {
            return type_ok.to_string()
        }
        default.to_string()
    }

    fn get_method_signature(&self, name: &NodeName, default_type_ok: &str, prefix: &str, args: &str) -> String {
        let type_ok = self.get_type_ok(name, default_type_ok);
        let kind = name.to_class_cases();
        format!("    JS::Result<{type_ok}> parse{prefix}{kind}({args});",
            prefix = prefix,
            type_ok = type_ok,
            kind = kind,
            args = args,
        )
    }

    fn get_method_definition_start(&self, name: &NodeName, default_type_ok: &str, prefix: &str, args: &str) -> String {
        let type_ok = self.get_type_ok(name, default_type_ok);
        let kind = name.to_class_cases();
        format!("JS::Result<{type_ok}>\nBinASTParser::parse{prefix}{kind}({args})",
            prefix = prefix,
            type_ok = type_ok,
            kind = kind,
            args = args,
        )
    }


    /// Declaring enums for kinds and fields.
    fn export_declare_kinds_and_fields_enums(&self, buffer: &mut String) {
        buffer.push_str(&self.rules.hpp_tokens_header.reindent(""));

        buffer.push_str("\n\n");
        if self.rules.hpp_tokens_kind_doc.is_some() {
            buffer.push_str(&self.rules.hpp_tokens_kind_doc.reindent(""));
        }

        let mut node_names : Vec<_> = self.syntax.node_names().keys().collect();
        node_names.sort();
        buffer.push_str(&format!("\n#define FOR_EACH_BIN_KIND(F) \\\n{nodes}",
            nodes = node_names.drain(..)
                .map(|name| format!("    F({name}, {name})",
                    name = name))
                .format(" \\\n")));
        buffer.push_str("


enum class BinKind {
#define EMIT_ENUM(name, _) name,
    FOR_EACH_BIN_KIND(EMIT_ENUM)
#undef EMIT_ENUM
};
");

        buffer.push_str(&format!("\n// The number of distinct values of BinKind.\nconst size_t BINKIND_LIMIT = {};\n", self.syntax.node_names().len()));
        buffer.push_str("\n\n");
        if self.rules.hpp_tokens_field_doc.is_some() {
            buffer.push_str(&self.rules.hpp_tokens_field_doc.reindent(""));
        }

        let mut field_names : Vec<_> = self.syntax.field_names().keys().collect();
        field_names.sort();
        buffer.push_str(&format!("\n#define FOR_EACH_BIN_FIELD(F) \\\n{nodes}",
            nodes = field_names.drain(..)
                .map(|name| format!("    F({enum_name}, {spec_name})",
                    spec_name = name,
                    enum_name = name.to_cpp_enum_case()))
                .format(" \\\n")));
        buffer.push_str("


enum class BinField {
#define EMIT_ENUM(name, _) name,
    FOR_EACH_BIN_FIELD(EMIT_ENUM)
#undef EMIT_ENUM
};
");
        buffer.push_str(&format!("\n// The number of distinct values of BinField.\nconst size_t BINFIELD_LIMIT = {};\n", self.syntax.field_names().len()));

        buffer.push_str(&self.rules.hpp_tokens_footer.reindent(""));
        buffer.push_str("\n");
    }

    /// Declare string enums
    fn export_declare_string_enums_classes(&self, buffer: &mut String) {
        buffer.push_str("\n\n// ----- Declaring string enums (by lexicographical order)\n");
        let mut string_enums_by_name : Vec<_> = self.syntax.string_enums_by_name()
            .iter()
            .collect();
        string_enums_by_name.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        for (name, enum_) in string_enums_by_name {
            let rendered_cases = enum_.strings()
                .iter()
                .map(|str| format!("{case}      /* \"{original}\" */",
                    case = str.to_cpp_enum_case(),
                    original = str))
                .format(",\n    ");
            let rendered = format!("enum class {name} {{\n    {cases}\n}};\n\n",
                cases = rendered_cases,
                name = name.to_class_cases());
            buffer.push_str(&rendered);
        }
    }

    fn export_declare_sums_of_interface_methods(&self, buffer: &mut String) {
        let mut sums_of_interfaces : Vec<_> = self.syntax.resolved_sums_of_interfaces_by_name()
            .iter()
            .collect();
        sums_of_interfaces.sort_by(|a, b| a.0.cmp(&b.0));
        buffer.push_str("\n\n// ----- Sums of interfaces (by lexicographical order)\n");
        buffer.push_str("// Implementations are autogenerated\n");
        buffer.push_str("// `ParseNode*` may never be nullptr\n");
        for &(ref name, _) in &sums_of_interfaces {
            let rendered = self.get_method_signature(name, "ParseNode*", "", "");
            buffer.push_str(&rendered.reindent(""));
            buffer.push_str("\n");
        }
        for (name, _) in sums_of_interfaces {
            let rendered = self.get_method_signature(name, "ParseNode*", "Sum", "const size_t start, const BinKind kind, const BinFields& fields");
            buffer.push_str(&rendered.reindent(""));
            buffer.push_str("\n");
        }
    }

    fn export_declare_single_interface_methods(&self, buffer: &mut String) {
        buffer.push_str("\n\n// ----- Interfaces (by lexicographical order)\n");
        buffer.push_str("// Implementations are autogenerated\n");
        buffer.push_str("// `ParseNode*` may never be nullptr\n");
        let mut interfaces_by_name : Vec<_> = self.syntax.interfaces_by_name()
            .iter()
            .collect();
        interfaces_by_name.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));

        let mut outer_parsers = Vec::with_capacity(interfaces_by_name.len());
        let mut inner_parsers = Vec::with_capacity(interfaces_by_name.len());
        
        for &(name, _) in &interfaces_by_name {
            let outer = self.get_method_signature(name, "ParseNode*", "", "");
            let inner = self.get_method_signature(name, "ParseNode*", "Interface", "const size_t start, const BinKind kind, const BinFields& fields");
            outer_parsers.push(outer.reindent(""));
            inner_parsers.push(inner.reindent(""));
        }

        for parser in outer_parsers.drain(..) {
            buffer.push_str(&parser);
            buffer.push_str("\n");
        }

        for parser in inner_parsers.drain(..) {
            buffer.push_str(&parser);
            buffer.push_str("\n");
        }
    }

    fn export_declare_string_enums_methods(&self, buffer: &mut String) {
        buffer.push_str("\n\n// ----- String enums (by lexicographical order)\n");
        buffer.push_str("// Implementations are autogenerated\n");
        let mut string_enums_by_name : Vec<_> = self.syntax.string_enums_by_name()
            .iter()
            .collect();
        string_enums_by_name.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        for (kind, _) in string_enums_by_name {
            let type_ok = format!("BinASTParser::{kind}", kind = kind.to_class_cases());
            let rendered = self.get_method_signature(kind, &type_ok, "", "");
            buffer.push_str(&rendered.reindent(""));
            buffer.push_str("\n");
        }
    }

    fn export_declare_list_methods(&self, buffer: &mut String) {
        buffer.push_str("\n\n// ----- Lists (by lexicographical order)\n");
        buffer.push_str("// Implementations are autogenerated\n");
        for &(ref kind, _) in &self.list_parsers_to_generate {
            let rendered = self.get_method_signature(kind, "ParseNode*", "", "");
            buffer.push_str(&rendered.reindent(""));
            buffer.push_str("\n");
        }
    }

    fn export_declare_option_methods(&self, buffer: &mut String) {
        buffer.push_str("\n\n// ----- Default values (by lexicographical order)\n");
        buffer.push_str("// Implementations are autogenerated\n");
        for &(ref kind, _) in &self.option_parsers_to_generate {
            let rendered = self.get_method_signature(kind, "ParseNode*", "", "");
            buffer.push_str(&rendered.reindent(""));
            buffer.push_str("\n");
        }
    }

    fn generate_autogenerated_warning(&self) -> String {
        format!("// This file was autogenerated by generate_spidermonkey, please do not modify by hand.
//   Source specification: {webidl}
//   Source annotations: {yaml}

",
            webidl = self.webidl_file_name,
            yaml = self.yaml_file_name
        )
    }

    /// Generate C++ headers for SpiderMonkey
    pub fn to_spidermonkey_token_hpp(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&self.generate_autogenerated_warning());

        self.export_declare_kinds_and_fields_enums(&mut buffer);

        buffer.push_str("\n");
        buffer
    }
    pub fn to_spidermonkey_class_hpp(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&self.generate_autogenerated_warning());

        buffer.push_str(&self.rules.hpp_class_header.reindent(""));
        buffer.push_str("\n");

        self.export_declare_string_enums_classes(&mut buffer);
        self.export_declare_sums_of_interface_methods(&mut buffer);
        self.export_declare_single_interface_methods(&mut buffer);
        self.export_declare_string_enums_methods(&mut buffer);
        self.export_declare_list_methods(&mut buffer);
        self.export_declare_option_methods(&mut buffer);

        buffer.push_str("\n");
        buffer
    }
}

impl CPPExporter {
    /// Generate implementation of a single typesum.
    fn generate_implement_sum(&self, buffer: &mut String, name: &NodeName, nodes: &HashSet<NodeName>) {
        // Generate comments (FIXME: We should use the actual webidl, not the resolved sum)
        let mut nodes : Vec<_> = nodes.iter()
            .collect();
        nodes.sort();
        let kind = name.to_class_cases();
        let rendered_bnf = format!("/*\n{name} ::= {nodes} \n*/",
            nodes = nodes.iter()
                .format("\n    "),
            name = name.to_str());

        // Generate outer method
        buffer.push_str(&format!("{bnf}
{first_line}
{{
    BinKind kind;
    BinFields fields((cx_));
    AutoTaggedTuple guard(*tokenizer_);
    const auto start = tokenizer_->offset();

    TRY(tokenizer_->enterTaggedTuple(kind, fields, guard));

    MOZ_TRY_DECL(result, parseSum{kind}(start, kind, fields));

    TRY(guard.done());
    return result;
}}\n",
                bnf = rendered_bnf,
                kind = kind,
                first_line = self.get_method_definition_start(name, "ParseNode*", "", "")
        ));

        // Generate inner method
        let mut buffer_cases = String::new();
        for node in nodes {
            buffer_cases.push_str(&format!("
      case BinKind::{kind}:
        MOZ_TRY_VAR(result, parseInterface{kind}(start, kind, fields));
        break;",
                kind = node.to_class_cases()));
            }
            buffer.push_str(&format!("\n{first_line}
{{
    {type_ok} result;
    switch(kind) {{ {cases}
      default:
        return raiseInvalidKind(\"{kind}\", kind);
    }}
    return result;
}}

",
        kind = kind,
        cases = buffer_cases,
        first_line = self.get_method_definition_start(name, "ParseNode*", "Sum", "const size_t start, const BinKind kind, const BinFields& fields"),
        type_ok = self.get_type_ok(name, "ParseNode*")
    ));
    }

    /// Generate the implementation of a single list parser
    fn generate_implement_list(&self, buffer: &mut String, name: &NodeName, supports_empty: bool, contents: &NodeName) {
        let rules_for_this_list = self.rules.get(name);
        let kind = name.to_class_cases();
        let first_line = self.get_method_definition_start(name, "ParseNode*", "", "");

        let init = match rules_for_this_list.start {
            Some(str) => str.reindent("    "),
            None => {
                // We cannot generate the method if we don't know how to initialize the list.
                let rendered = format!("
{first_line}
{{
    return raiseError(\"FIXME: Not implemented yet ({kind})\");
}}\n",
                    first_line = first_line,
                    kind = kind,
                );
                buffer.push_str(&rendered);
                return;
            }
        };
        let append = match rules_for_this_list.append {
            Some(str) => format!("{}\n", str.reindent("    ")),
            None => "        result->appendWithoutOrderAssumption(item);".to_string()
        };


        let rendered = format!("\n{first_line}
{{
    uint32_t length;
    AutoList guard(*tokenizer_);

    const auto start = tokenizer_->offset();
    TRY(tokenizer_->enterList(length, guard));{empty_check}
{init}

    for (uint32_t i = 0; i < length; ++i) {{
        MOZ_TRY_DECL(item, parse{inner}());
{append}
    }}

    TRY(guard.done());
    return result;
}}\n",
            first_line = first_line,
            empty_check =
                if supports_empty {
                    "".to_string()
                } else {
                    format!("\n    if (length == 0)\n         return raiseEmpty(\"{kind}\");\n",
                        kind = kind)
                },
            inner = contents.to_class_cases(),
            init = init,
            append = append);
        buffer.push_str(&rendered);
    }

    fn generate_implement_option(&self, buffer: &mut String, name: &NodeName, contents: &NodeName) {
        debug!(target: "export_utils", "Implementing optional value {} backed by {}",
            name.to_str(), contents.to_str());

        let rules_for_this_node = self.rules.get(name);

        let type_ok = self.get_type_ok(name, "ParseNode*");
        let default_value = 
            if type_ok == "Ok" {
                "Ok()"
            } else {
                "nullptr"
            }.to_string();

        // At this stage, thanks to deanonymization, `contents`
        // is something like `OptionalFooBar`.
        let named_implementation = 
            if let Some(NamedType::Typedef(ref typedef)) = self.syntax.get_type_by_name(&contents) {
                assert!(typedef.is_optional());
                if let TypeSpec::NamedType(ref named) = *typedef.spec() {
                    self.syntax.get_type_by_name(named)
                        .unwrap()
                } else {
                    panic!()
                }
            } else {
                panic!()
            };
        match named_implementation {
            NamedType::Interface(_) => {
                buffer.push_str(&format!("{first_line}
{{
    BinKind kind;
    BinFields fields((cx_));
    AutoTaggedTuple guard(*tokenizer_);

    TRY(tokenizer_->enterTaggedTuple(kind, fields, guard));
    {type_ok} result;
    if (kind == BinKind::{null}) {{
        result = {default_value};
    }} else {{
        const auto start = tokenizer_->offset();
        MOZ_TRY_VAR(result, parseInterface{contents}(start, kind, fields));
    }}
    TRY(guard.done());

    return result;
}}

",
                    first_line = self.get_method_definition_start(name, "ParseNode*", "", ""),
                    null = self.syntax.get_null_name().to_str(),
                    contents = contents.to_class_cases(),
                    type_ok = type_ok,
                    default_value = default_value,
                ));
            }
            NamedType::Typedef(ref type_) => {
                match type_.spec() {
                    &TypeSpec::TypeSum(_) => {
                buffer.push_str(&format!("{first_line}
{{
    BinKind kind;
    BinFields fields((cx_));
    AutoTaggedTuple guard(*tokenizer_);

    TRY(tokenizer_->enterTaggedTuple(kind, fields, guard));
    {type_ok} result;
    if (kind == BinKind::_Null) {{
        result = {default_value};
    }} else {{
        const auto start = tokenizer_->offset();
        MOZ_TRY_VAR(result, parseSum{contents}(start, kind, fields));
    }}
    TRY(guard.done());

    return result;
}}

",
                            first_line = self.get_method_definition_start(name, "ParseNode*", "", ""),
                            contents = contents.to_class_cases(),
                            type_ok = type_ok,
                            default_value = default_value,
                        ));
                    }
                    &TypeSpec::String => {
                        let build_result = rules_for_this_node.start.reindent("    ");

                        buffer.push_str(&format!("{first_line}
{{
    RootedAtom string((cx_));
    MOZ_TRY(readMaybeString(&string));

    {build}

    {return_}
}}

",
                            first_line = self.get_method_definition_start(name, "ParseNode*", "", ""),
                            build = build_result,
                            return_ = if build_result.len() == 0 {
                                format!("return raiseError(\"FIXME: Not implemented yet ({kind})\");\n",
                                    kind = name.to_str())
                            } else {
                                "return result;".to_string()
                            }
                        ));
                    }
                    _else => unimplemented!("{:?}", _else)
                }
            }
            NamedType::StringEnum(_) => {
                unimplemented!()
            }
        }
    }

    fn generate_implement_interface(&self, buffer: &mut String, name: &NodeName, interface: &Interface) {
        let rules_for_this_interface = self.rules.get(name);

        // Generate comments
        let comment = format!("\n/*\n{}*/\n", ToWebidl::interface(interface, "", "    "));
        buffer.push_str(&comment);

        // Generate public method
        let kind = name.to_class_cases();
        buffer.push_str(&format!("{first_line}
{{
    BinKind kind;
    BinFields fields((cx_));
    AutoTaggedTuple guard(*tokenizer_);

    TRY(tokenizer_->enterTaggedTuple(kind, fields, guard));
    const auto start = tokenizer_->offset();

    MOZ_TRY_DECL(result, parseInterface{kind}(start, kind, fields));
    TRY(guard.done());

    return result;
}}

",
            first_line = self.get_method_definition_start(name, "ParseNode*", "", ""),
            kind = kind
        ));

        // Generate aux method
        let number_of_fields = interface.contents().fields().len();
        let first_line = self.get_method_definition_start(name, "ParseNode*", "Interface", "const size_t start, const BinKind kind, const BinFields& fields");

        let fields_type_list = format!("{{ {} }}", interface.contents()
            .fields()
            .iter()
            .map(|field| format!("BinField::{}", field.name().to_cpp_enum_case()))
            .format(", "));

        let mut fields_implem = String::new();
        for field in interface.contents().fields() {
            let rules_for_this_field = rules_for_this_interface.by_field.get(field.name());
            let needs_block = if let Some(ref rule) = rules_for_this_field {
                rule.block_before_field.is_some() || rule.block_after_field.is_some()
            } else {
                false
            };

            let var_name = field.name().to_cpp_field_case();
            let (decl_var, parse_var) = match field.type_().get_primitive(&self.syntax) {
                Some(IsNullable { is_nullable: false, content: Primitive::Number }) => {
                    if needs_block {
                        (Some(format!("double {var_name};", var_name = var_name)),
                            Some(format!("MOZ_TRY_VAR({var_name}, readNumber());", var_name = var_name)))
                    } else {
                        (None,
                            Some(format!("MOZ_TRY_DECL({var_name}, readNumber());", var_name = var_name)))
                    }
                }
                Some(IsNullable { is_nullable: false, content: Primitive::Boolean }) => {
                    if needs_block {
                        (Some(format!("bool {var_name};", var_name = var_name)),
                        Some(format!("MOZ_TRY_VAR({var_name}, readBool());", var_name = var_name)))
                    } else {
                        (None,
                        Some(format!("MOZ_TRY_DECL({var_name}, readBool());", var_name = var_name)))
                    }
                }
                Some(IsNullable { content: Primitive::Void, .. }) => {
                    (Some(format!("// Skipping void field {}", field.name().to_str())),
                        None)
                }
                Some(IsNullable { is_nullable: false, content: Primitive::String }) => {
                    (Some(format!("RootedAtom {var_name}((cx_));", var_name = var_name)),
                        Some(format!("MOZ_TRY(readString(&{var_name}));", var_name = var_name)))
                }
                Some(IsNullable { content: Primitive::Interface(ref interface), ..})
                    if &self.get_type_ok(interface.name(), "?") == "Ok" =>
                {
                    // Special case: `Ok` means that we shouldn't bind the return value.
                    let typename = TypeName::type_(field.type_());
                    (None,
                        Some(format!("MOZ_TRY(parse{typename}());",
                            typename = typename)))
                }
                _else => {
                    let typename = TypeName::type_(field.type_());
                    if needs_block {
                        (Some(format!("{typename} {var_name};",
                            var_name = var_name,
                            typename = typename)),
                            Some(format!("MOZ_TRY_VAR({var_name}, parse{typename}());",
                            var_name = var_name,
                            typename = typename)
                        ))
                    } else {
                        (None,
                            Some(format!("MOZ_TRY_DECL({var_name}, parse{typename}());",
                            var_name = var_name,
                            typename = typename)))
                    }
                }
            };

            let rendered = if let Some(ref rule) = rules_for_this_field {
                if rule.replace.is_some() {
                    rule.replace.reindent("    ")
                        .newline()
                } else {
                    let before_field = rule.before_field.reindent("    ");
                    let after_field = rule.after_field.reindent("    ");
                    let decl_var = if rule.declare.is_some() {
                        rule.declare.reindent("    ")
                    } else {
                        decl_var.reindent("    ")
                    };
                    if needs_block {
                        let parse_var = parse_var.reindent("        ");
                        format!("{before_field}
{decl_var}
    {{
{block_before_field}
{parse_var}
{block_after_field}
    }}
{after_field}",
                            before_field = before_field.reindent("    "),
                            decl_var = decl_var.reindent("    "),
                            parse_var = parse_var.reindent("        "),
                            after_field = after_field.reindent("    "),
                            block_before_field = rule.block_before_field.reindent("        "),
                            block_after_field = rule.block_after_field.reindent("        "))
                    } else {
                        format!("
{before_field}
{decl_var}
{parse_var}
{after_field}
",
                            before_field = before_field.reindent("    "),
                            decl_var = decl_var.reindent("    "),
                            parse_var = parse_var.reindent("    "),
                            after_field = after_field.reindent("    "))
                    }
                }
            } else {
                format!("
{decl_var}
{parse_var}
",
                    decl_var = decl_var.reindent("    "),
                    parse_var = parse_var.reindent("    "))
            };
            fields_implem.push_str(&rendered);
        }

        let (start, build_result) =
            (rules_for_this_interface.start.reindent("    "),
             rules_for_this_interface.build_result.reindent("    "));

        if build_result == "" {
            buffer.push_str(&format!("{first_line}
{{
    return raiseError(\"FIXME: Not implemented yet ({})\");
}}

",
                kind = kind.to_str(),
                first_line = first_line,
            ));
        } else {
            let check_fields = if number_of_fields == 0 {
                format!("MOZ_TRY(checkFields0(kind, fields));")
            } else {
                format!("MOZ_TRY(checkFields(kind, fields, {fields_type_list}));",
                    fields_type_list = fields_type_list)
            };
            buffer.push_str(&format!("{first_line}
{{
    MOZ_ASSERT(kind == BinKind::{kind});
    {check_fields}
{pre}
{fields_implem}
{post}
    return result;
}}

",
                check_fields = check_fields,
                fields_implem = fields_implem,
                pre = start,
                post = build_result,
                kind = kind,
                first_line = first_line,
            ));
        }
    }

    /// Generate C++ code for SpiderMonkey
    pub fn to_spidermonkey_cpp(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&self.generate_autogenerated_warning());

        // 0. Header
        buffer.push_str(&self.rules.cpp_header.reindent(""));
        buffer.push_str("\n");

        // 1. Typesums
        buffer.push_str("\n\n// ----- Sums of interfaces (autogenerated, by lexicographical order)\n");
        buffer.push_str("// Sums of sums are flattened.\n");

        let mut sums_of_interfaces : Vec<_> = self.syntax.resolved_sums_of_interfaces_by_name()
            .iter()
            .collect();
        sums_of_interfaces.sort_by(|a, b| a.0.cmp(&b.0));

        for (name, nodes) in sums_of_interfaces.drain(..) {
            self.generate_implement_sum(&mut buffer, name, nodes);
        }

        // 2. Single interfaces
        buffer.push_str("\n\n// ----- Interfaces (autogenerated, by lexicographical order)\n");
        buffer.push_str("// When fields have a non-trivial type, implementation is deanonymized and delegated to another parser.\n");
        let mut interfaces_by_name : Vec<_> = self.syntax.interfaces_by_name()
            .iter()
            .collect();
        interfaces_by_name.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));

        for (name, interface) in interfaces_by_name {
            self.generate_implement_interface(&mut buffer, name, interface);
        }

        // 3. String Enums
        buffer.push_str("\n\n// ----- String enums (autogenerated, by lexicographical order)\n");
        let mut string_enums_by_name : Vec<_> = self.syntax.string_enums_by_name()
            .iter()
            .collect();
        string_enums_by_name.sort_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        for (name, enum_) in string_enums_by_name {
            let kind = name.to_class_cases();
            let mut cases = String::new();
            for string in enum_.strings() {
                cases.push_str(&format!("if (chars == \"{string}\")
        return {kind}::{variant};\n    else ",
                    string = string,
                    kind = kind,
                    variant = string.to_cpp_enum_case()
                ));
            }
            cases.push_str(&format!("\n        return raiseInvalidEnum(\"{kind}\", chars);",
                kind = kind));

            let rendered_doc = format!("/*\nenum {kind} {{\n{cases}\n}};\n*/\n",
                kind = kind,
                cases = enum_.strings()
                        .iter()
                        .map(|s| format!("    \"{}\"", s))
                        .format(",\n")
            );
            buffer.push_str(&format!("{rendered_doc}{first_line}
{{
    // Unoptimized implementation.
    Chars chars((cx_));
    MOZ_TRY(readString(chars));

    {cases}
}}

",
                rendered_doc = rendered_doc,
                cases = cases,
                first_line = self.get_method_definition_start(name, &format!("BinASTParser::{kind}", kind = kind), "", "")
            ));
        }

        // 4. Lists
        buffer.push_str("\n\n// ----- Lists (autogenerated, by lexicographical order)\n");
        for &(ref name, (supports_empty, ref contents)) in &self.list_parsers_to_generate {
            self.generate_implement_list(&mut buffer, name, supports_empty, contents);
        }

        // 5. Optional values
        buffer.push_str("\n\n    // ----- Default values (by lexicographical order)\n");
        for &(ref name, ref contents) in &self.option_parsers_to_generate {
            self.generate_implement_option(&mut buffer, name, contents);
        }

        buffer.push_str("\n");
        buffer.push_str(&self.rules.cpp_footer.reindent(""));
        buffer.push_str("\n");

        buffer
    }
}

fn update_rule(rule: &mut Option<String>, entry: &yaml_rust::Yaml) -> Option<Option<()>> {
    if entry.is_badvalue() {
        return Some(None)
    } else if let Some(as_str) = entry.as_str() {
        *rule = Some(as_str.to_string());
        Some(Some(()))
    } else {
        None
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
            Arg::with_name("INPUT.yaml")
                .required(true)
                .help("Input rules file to use. Must be a yaml source file."),
            Arg::with_name("OUT_HEADER_CLASS_FILE")
                .long("out-class")
                .required(true)
                .takes_value(true)
                .help("Output header file (.h, designed to be included from within the class file)"),
            Arg::with_name("OUT_TOKEN_FILE")
                .long("out-token")
                .required(true)
                .takes_value(true)
                .help("Output token file (.h)"),
            Arg::with_name("OUT_IMPL_FILE")
                .long("out-impl")
                .required(true)
                .takes_value(true)
                .help("Output implementation file (.cpp)"),
            Arg::with_name("OUT_WEBIDL_FILE")
                .long("out-webidl")
                .required(false)
                .takes_value(true)
                .help("Path to copy the webidl source"),
            Arg::with_name("OUT_YAML_FILE")
                .long("out-yaml")
                .required(false)
                .takes_value(true)
                .help("Path to copy the yaml source"),
        ])
    .get_matches();

    let source_path = matches.value_of("INPUT.webidl")
        .expect("Expected INPUT.webidl");

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
    let syntax = builder.into_spec(SpecOptions {
        root: &fake_root,
        null: &null,
    });

    let syntax_options = SpecOptions {
        root: &fake_root,
        null: &null,
    };

    let deanonymizer = TypeDeanonymizer::new(&syntax);

    let mut generation_rules = GenerationRules::default();
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

            match node_key {
                "cpp" => {
                    update_rule(&mut generation_rules.cpp_header, &node_entries["header"])
                        .unwrap_or_else(|| panic!("Rule cpp.header must be a string"));
                    update_rule(&mut generation_rules.cpp_footer, &node_entries["footer"])
                        .unwrap_or_else(|| panic!("Rule cpp.footer must be a string"));
                    continue;
                }
                "hpp" => {
                    update_rule(&mut generation_rules.hpp_class_header, &node_entries["class"]["header"])
                        .unwrap_or_else(|| panic!("Rule hpp.class.header must be a string"));
                    update_rule(&mut generation_rules.hpp_tokens_header, &node_entries["tokens"]["header"])
                        .unwrap_or_else(|| panic!("Rule hpp.tokens.header must be a string"));
                    update_rule(&mut generation_rules.hpp_tokens_footer, &node_entries["tokens"]["footer"])
                        .unwrap_or_else(|| panic!("Rule hpp.tokens.footer must be a string"));
                    update_rule(&mut generation_rules.hpp_tokens_kind_doc, &node_entries["tokens"]["kind"]["doc"])
                        .unwrap_or_else(|| panic!("Rule hpp.tokens.kind.doc must be a string"));
                    update_rule(&mut generation_rules.hpp_tokens_field_doc, &node_entries["tokens"]["field"]["doc"])
                        .unwrap_or_else(|| panic!("Rule hpp.tokens.field.doc must be a string"));
                    continue;
                }
                _ => {}
            }


            let node_name = deanonymizer.get_node_name(&node_key)
                .unwrap_or_else(|| panic!("Unknown node name {}", node_key));

            let hash = node_entries.as_hash()
                .unwrap_or_else(|| panic!("Node {} isn't a dictionary"));

            let mut node_rule = NodeParsingRules::default();
            for (node_item_key, node_item_entry) in hash {
                let as_string = node_item_key.as_str()
                    .unwrap_or_else(|| panic!("Keys for rule {} must be strings", node_key));
                match as_string {
                    "inherits" => {
                        let name = node_item_entry.as_str()
                            .unwrap_or_else(|| panic!("Rule {}.{} must be a string", node_key, as_string));
                        let inherits = deanonymizer.get_node_name(name)
                            .unwrap_or_else(|| panic!("Unknown node name {}", node_key));
                        node_rule.inherits = Some(inherits);
                    }
                    "init" => {
                        update_rule(&mut node_rule.start, node_item_entry)
                            .unwrap_or_else(|| panic!("Rule {}.{} must be a string", node_key, as_string));
                    }
                    "build" => {
                        update_rule(&mut node_rule.build_result, node_item_entry)
                            .unwrap_or_else(|| panic!("Rule {}.{} must be a string", node_key, as_string));
                    }
                    "append" => {
                        update_rule(&mut node_rule.append, node_item_entry)
                            .unwrap_or_else(|| panic!("Rule {}.{} must be a string", node_key, as_string));
                    }
                    "type-ok" => {
                        update_rule(&mut node_rule.type_ok, node_item_entry)
                            .unwrap_or_else(|| panic!("Rule {}.{} must be a string", node_key, as_string));
                    }
                    "fields" => {
                        let fields = node_item_entry.as_hash()
                            .unwrap_or_else(|| panic!("Rule {}.fields must be a hash, got {:?}", node_key, node_entries["fields"]));
                        for (field_key, field_entry) in fields {
                            let field_key = field_key.as_str()
                                .unwrap_or_else(|| panic!("In rule {}, field entries must be field names",
                                    node_key))
                                .to_string();
                            let field_name = syntax.get_field_name(&field_key)
                                .unwrap_or_else(|| panic!("In rule {}, can't find field {}",
                                    node_key,
                                    field_key));

                            let mut field_rule = FieldParsingRules::default();
                            for (field_config_key, field_config_entry) in field_entry.as_hash()
                                .unwrap_or_else(|| panic!("Rule {}.fields.{} must be a hash", node_key, field_key))
                            {
                                let field_config_key = field_config_key.as_str()
                                    .expect("Expected a string as a key");
                                match field_config_key
                                {
                                    "block" => {
                                        update_rule(&mut field_rule.declare, &field_config_entry["declare"])
                                            .unwrap_or_else(|| panic!("Rule {}.fields.{}.{}.{} must be a string", node_key, field_key, field_config_key, "declare"));

                                        update_rule(&mut field_rule.replace, &field_config_entry["replace"])
                                            .unwrap_or_else(|| panic!("Rule {}.fields.{}.{}.{} must be a string", node_key, field_key, field_config_key, "replace"));

                                        update_rule(&mut field_rule.block_before_field, &field_config_entry["before"])
                                            .unwrap_or_else(|| panic!("Rule {}.fields.{}.{}.{} must be a string", node_key, field_key, field_config_key, "before"));

                                        update_rule(&mut field_rule.block_after_field, &field_config_entry["after"])
                                            .unwrap_or_else(|| panic!("Rule {}.fields.{}.{}.{} must be a string", node_key, field_key, field_config_key, "after"));
                                    }
                                    "before" => {
                                        update_rule(&mut field_rule.before_field, &field_config_entry)
                                            .unwrap_or_else(|| panic!("Rule {}.fields.{}.{} must be a string", node_key, field_key, field_config_key));
                                    }
                                    "after" => {
                                        update_rule(&mut field_rule.after_field, &field_config_entry)
                                            .unwrap_or_else(|| panic!("Rule {}.fields.{}.{} must be a string", node_key, field_key, field_config_key));
                                    }
                                    _ => {
                                        panic!("Unexpected {}.fields.{}.{}", node_key, field_key, field_config_key)
                                    }
                                }
                            }
                            node_rule.by_field.insert(field_name.clone(), field_rule);
                        }
                    }
                    _ => panic!("Unexpected node_item_key {}.{}", node_key, as_string)
                }
            }

            generation_rules.per_node.insert(node_name.clone(), node_rule);
            // FIXME: Check that rules are only for interfaces.
        }
    } else {
        println!("...skipping rules");
    }

    fn apparent_file_name(default: &str, candidate: Option<&str>) -> String {
        if let Some(path) = candidate {
            if let Some(str) = std::path::Path::new(path).file_name() {
                return str.to_str()
                    .expect("Encoding error")
                    .to_string()
            }
        }
        std::path::Path::new(default).file_name()
            .expect("Source is not a proper file name")
            .to_str()
            .expect("Encoding error")
            .to_string()
    }
    let apparent_webidl_file_name = apparent_file_name(source_path, matches.value_of("OUT_WEBIDL_FILE"));
    let apparent_yaml_file_name = apparent_file_name(matches.value_of("INPUT.yaml").unwrap(), matches.value_of("OUT_YAML_FILE"));

    let mut exporter = CPPExporter::new(deanonymizer,
        apparent_webidl_file_name,
        apparent_yaml_file_name,
        syntax_options);
    exporter.set_export_rules(generation_rules);

    let dest_path = matches.value_of("OUT_HEADER_CLASS_FILE")
        .unwrap();
    println!("...exporting C++ class header code: {}", dest_path);
    let mut dest = File::create(format!("{}", dest_path))
        .expect("Could not create C++ header source output");
    dest.write_all(exporter.to_spidermonkey_class_hpp().as_bytes())
        .expect("Could not write C++ header source output");

    let dest_path = matches.value_of("OUT_TOKEN_FILE")
        .unwrap();
    println!("...exporting C++ token header code: {}", dest_path);
    let mut dest = File::create(format!("{}", dest_path))
        .expect("Could not create C++ header source output");
    dest.write_all(exporter.to_spidermonkey_token_hpp().as_bytes())
        .expect("Could not write C++ header source output");

    let dest_path = matches.value_of("OUT_IMPL_FILE")
        .unwrap();
    println!("...exporting C++ implementation code: {}", dest_path);
    let mut dest = File::create(format!("{}", dest_path))
        .expect("Could not create C++ implementation source output");
    dest.write_all(exporter.to_spidermonkey_cpp().as_bytes())
        .expect("Could not write C++ implementation source output");

    if let Some(dest_path) = matches.value_of("OUT_WEBIDL_FILE") {
        println!("...exporting webidl source: {}", dest_path);
        std::fs::copy(source_path, dest_path)
            .expect("Could not copy source webidl");
    }
    if let Some(dest_path) = matches.value_of("OUT_YAML_FILE") {
        println!("...exporting yaml source: {}", dest_path);
        let source_path = matches.value_of("INPUT.yaml")
            .expect("Could not find source yaml");
        std::fs::copy(source_path, dest_path)
            .expect("Could not copy source yaml");
    }

    println!("...done");
}