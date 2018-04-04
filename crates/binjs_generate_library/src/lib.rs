extern crate binjs_meta;
extern crate env_logger;
extern crate itertools;
extern crate log;

use binjs_meta::export::{ TypeDeanonymizer, TypeName };
use binjs_meta::spec::*;
use binjs_meta::util::*;

use std::collections::{ HashMap, HashSet };
use std::rc::Rc;

use itertools::Itertools;

/// Source code produced by exporting a spec to Rust.
pub struct ExportedSource {
    /// Source code for a strongly-typed data structure implementing the specification.
    pub typed: String,

    /// Source code for a weakly-typed (JSON-based) data structure implementing the specification.
    pub generic: String,
}

/// Generate Rust source
pub struct RustExporter {
    /// The original specifications, without deanonymization.
    ///
    /// Used to generate code that will create dynamically
    /// an instance of `Spec`.
    spec: Spec
}
impl RustExporter {
    /// Create a Rust exporter from the original specifications.
    pub fn new(spec: Spec) -> Self {
        RustExporter {
            spec
        }
    }

    /// Generate the dynamic version of a `Type`.
    fn type_(type_: &Type, prefix: &str) -> String {
        let spec = Self::type_spec(type_.spec(), prefix);
        let ref spec = spec.trim_left();
        format!("{prefix}{type_}{close}",
            type_ = spec,
            prefix = prefix,
            close =
                if type_.is_optional() {
                    ".optional().unwrap()"
                } else {
                    ".required()"
                }
            )
    }

    /// Generate the dynamic version of a `TypeSpec`.
    fn type_spec(spec: &TypeSpec, prefix: &str) -> String {
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
            TypeSpec::Offset =>
                format!("{prefix}Type::offset()",
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


    pub fn to_rust_source(&self) -> ExportedSource {
        let deanonymized = TypeDeanonymizer::new(&self.spec)
            .into_spec(SpecOptions {
                root: self.spec.get_root_name(),
                null: self.spec.get_null_name(),
            });

        // Buffer used to generate the strongly-typed data structure.
        let mut ast_buffer = String::new();
        ast_buffer.push_str("
use binjs_shared;
use binjs_shared::{ FromJSON, FromJSONError, Offset, ToJSON };
use binjs_io::{ Deserialization, Guard, InnerDeserialization, Serialization, TokenReader, TokenReaderError, TokenWriter };

use io::*;

use std;
use std::convert::From;

use json;
use json::JsonValue as JSON;

");

        // Buffer used to generate the generic data structure (struct declaration).
        let mut struct_buffer = String::new();
        struct_buffer.push_str("pub struct Library {\n");

        // Buffer used to generate the generic data structure (impl declaration).
        let mut impl_buffer = String::new();
        impl_buffer.push_str("impl Library {\n    pub fn new(builder: &mut SpecBuilder) -> Self {\n        let names = Library {\n");


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
                let definition = format!("#[derive(PartialEq, Debug, Clone)]\npub enum {name} {{\n{values}\n    }}\n",
                    name = name,
                    values = string_enum.strings()
                        .iter()
                        .map(|s| format!("     {}", ToCases::to_cpp_enum_case(s)))
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
                        .map(|s| format!("           {name}::{typed} => \"{string}\"",
                            name = name,
                            typed = s.to_cpp_enum_case(),
                            string = s,
                        ))
                        .format(",\n"),
                    name = name);

                let from_reader = format!("
impl<R> Deserializer<R> where R: TokenReader {{
    fn deserialize_variant_{lowercase_name}_aux(&mut self) -> Result<{name}, R::Error> where R: TokenReader {{
        let key = self.reader.string()?;
        match key {{
            None => Err(From::from(TokenReaderError::EmptyVariant)),
{variants}
            _ => Err(From::from(TokenReaderError::InvalidValue)),
        }}
    }}
    fn deserialize_variant_{lowercase_name}(&mut self) -> Result<{name}, R::Error> where R: TokenReader {{
        let result = self.deserialize_variant_{lowercase_name}_aux();
        if result.is_err() {{
            self.reader.poison();
        }}
        result
    }}
}}

impl<R> Deserialization<R, {name}> for Deserializer<R> where R: TokenReader {{
    fn deserialize(&mut self) -> Result<{name}, R::Error> {{
        debug!(target: \"deserialize_es6\", \"Deserializing variant {name}\");
        self.deserialize_variant_{lowercase_name}()
    }}
}}
",
                    name = name,
                    lowercase_name = name.to_rust_identifier_case(),
                    variants = string_enum.strings()
                        .iter()
                        .map(|s| format!("            Some(ref s) if s == \"{string}\" => Ok({name}::{typed}),",
                            name = name,
                            typed = s.to_cpp_enum_case(),
                            string = s))
                        .format("\n")
                    );

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
                        .map(|s| format!("           Some(\"{string}\") => Ok({name}::{typed})",
                            name = name,
                            typed = s.to_cpp_enum_case(),
                            string = s,
                        ))
                        .format(",\n"),
                    name = name);

                let to_writer = format!("
impl<'a, W> Serialization<W, &'a {name}> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, value: &'a {name}) -> Result<W::Tree, W::Error> {{
        debug!(target: \"serialize_es6\", \"Serializing string enum {name}\");
        let str = match *value {{
{variants}
        }};
        (self as &mut Serialization<W, &'a str>).serialize(str)
    }}
}}
",
                    name = name,
                    variants = string_enum.strings()
                        .iter()
                        .map(|s| format!("            {name}::{typed} => \"{string}\"",
                            name = name,
                            typed = s.to_cpp_enum_case(),
                            string = s,
                        ))
                        .format(",\n")
                );

                let walker = format!("
impl Walker for {name} {{
    fn walk<V, E>(&mut self, _: &mut Path, _: &mut V) -> Result<(), E> where V: Visitor<E> {{
        Ok(())
    }}
}}\n",
                    name = name);

                buffer.push_str(&definition);
                buffer.push_str(&from_json);
                buffer.push_str(&to_json);
                buffer.push_str(&from_reader);
                buffer.push_str(&to_writer);
                buffer.push_str(&walker);
                buffer.push_str("\n\n\n");
            }
        }
        fn print_ast_typedefs(buffer: &mut String, source: &HashMap<NodeName, Rc<Type>>, null_name: &str) {
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
                        _ => { buffer.push_str(&format!("// UNIMPLEMENTED: {}\n", name)); }
                    }
                }
            }
            buffer.push_str("\n\n// Type sums (by lexicographical order)\n");
            for name in enums.drain(..) {
                let typedef = source.get(name).unwrap();
                let name = name.to_class_cases();
                if let TypeSpec::TypeSum(ref sum) = *typedef.spec() {
                    let definition = format!("#[derive(PartialEq, Debug, Clone)]\npub enum {name} {{\n{contents}\n}}\n",
                        name = name,
                        contents = sum.types()
                            .iter()
                            .map(|t| {
                                if let TypeSpec::NamedType(ref case) = *t {
                                    format!("    {name}(Box<{name}>)",
                                        name = case.to_class_cases())
                                } else {
                                    panic!();
                                }
                            })
                            .format(",\n"));

                let from_reader = format!("
impl<R> Deserialization<R, {name}> for Deserializer<R> where R: TokenReader {{
    fn deserialize(&mut self) -> Result<{name}, R::Error> {{
        debug!(target: \"deserialize_es6\", \"Deserializing sum {name}\");
        let (kind, _, guard) = self.reader.tagged_tuple()?;
        debug!(target: \"deserialize_es6\", \"Deserializing sum {name}, found {{}}\", kind);
        let result = match kind.as_str() {{
{variants}
            _ => {{
                error!(target: \"deserialize_es6\", \"Deserializing sum {name}, found invalid {{}}\", kind);
                Err(From::from(TokenReaderError::BadEnumVariant))
            }}
        }};
        if result.is_err() {{
            self.reader.poison();
        }}
        guard.done()?;
        result
    }}
}}
impl<R> Deserialization<R, Option<{name}>> for Deserializer<R> where R: TokenReader {{
    fn deserialize(&mut self) -> Result<Option<{name}>, R::Error> {{
        debug!(target: \"deserialize_es6\", \"Deserializing optional sum {name}\");
        let (kind, _, guard) = self.reader.tagged_tuple()?;
        let result = match kind.as_str() {{
{variants_some}
            \"{null}\" => Ok(None),
            _ => {{
                error!(target: \"deserialize_es6\", \"Deserializing sum Option<{name}>, found invalid {{}}\", kind);
                Err(From::from(TokenReaderError::BadEnumVariant))
            }}
        }};
        if result.is_err() {{
            self.reader.poison();
        }}
        guard.done()?;
        result
    }}
}}
",
                                name = name,
                                variants = sum.types()
                                    .iter()
                                    .map(|t| {
                                        if let TypeSpec::NamedType(ref case) = *t {
                                            format!("           \"{case}\" => {{
                self.deserialize_inner()
                    .map(|r| {name}::{constructor}(Box::new(r)))
            }}",
                                                name = name,
                                                case = case,
                                                constructor = case.to_class_cases())
                                        } else {
                                            panic!("We should only have named types in sums at this stage");
                                        }
                                    })
                                    .format("\n"),
                                variants_some = sum.types()
                                    .iter()
                                    .map(|t| {
                                        if let TypeSpec::NamedType(ref case) = *t {
                                            format!("           \"{case}\" => {{
            self.deserialize_inner()
                .map(|r| Some({name}::{constructor}(Box::new(r))))
        }}",
                                                name = name,
                                                case = case,
                                                constructor = case.to_class_cases())
                                        } else {
                                            panic!("We should only have named types in sums at this stage");
                                        }
                                    })
                                    .format("\n"),
                                    null = null_name,
                                );

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
                                            format!("           Some(\"{case}\") => Ok({name}::{constructor}(Box::new(FromJSON::import(value)?)))",
                                                name = name,
                                                case = case,
                                                constructor = case.to_class_cases())
                                        } else {
                                            panic!("We should only have named types in sums at this stage");
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
                                            format!("           {name}::{constructor}(box ref value) => value.export()",
                                                name = name,
                                                constructor = case.to_class_cases())
                                        } else {
                                            panic!();
                                        }
                                    })
                                    .format(",\n")
                                );

                    let to_writer = format!("
impl<'a, W> Serialization<W, &'a Option<{name}>> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, value: &'a Option<{name}>) -> Result<W::Tree, W::Error> {{
        debug!(target: \"serialize_es6\", \"Serializing optional sum {name}\");
        match *value {{
            None => self.writer.tagged_tuple(\"{null}\", &[]),
            Some(ref sum) => (self as &mut Serialization<W, &'a {name}>).serialize(sum)
        }}
    }}
}}
impl<'a, W> Serialization<W, &'a {name}> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, value: &'a {name}) -> Result<W::Tree, W::Error> {{
        debug!(target: \"serialize_es6\", \"Serializing sum {name}\");
        match *value {{
{variants}
        }}
    }}
}}
",
                        null = null_name,
                        name = name,
                        variants = sum.types()
                            .iter()
                            .map(|t| {
                                if let TypeSpec::NamedType(ref case) = *t {
                                    format!("          {name}::{constructor}(box ref value) => (self as &mut Serialization<W, &'a {constructor}>).serialize(value)",
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
    fn walk<V, E>(&mut self, path: &mut Path, visitor: &mut V) -> Result<(), E> where V: Visitor<E> {{
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
                                    format!("          {name}::{constructor}(box ref mut value) => value.walk(path, visitor)",
                                        name = name,
                                        constructor = case.to_class_cases())
                                } else {
                                    panic!();
                                }
                            })
                            .format(",\n")
                        );

                    buffer.push_str(&definition);
                    buffer.push_str(&from_reader);
                    buffer.push_str(&to_writer);
                    buffer.push_str(&from_json);
                    buffer.push_str(&to_json);
                    buffer.push_str(&walk);
                    buffer.push_str("\n\n");
                } else {
                    panic!()
                }
            }

            buffer.push_str("\n\n// Aliases to primitive types (by lexicographical order)\n");
            // FromJSON/ToJSON are already implemented in `binjs::utils`
            for name in primitives.drain(..) {
                let typedef = source.get(name).unwrap();
                let source = format!("pub type {name} = {contents};\n",
                    name = name.to_class_cases(),
                    contents = match *typedef.spec() {
                        TypeSpec::Boolean => "bool",
                        TypeSpec::Number => "f64",
                        TypeSpec::String => "std::string::String",
                        TypeSpec::Offset => "Offset",
                        TypeSpec::Void => "()",
                        _ => panic!("Unexpected type in alias to a primitive type: {name}",
                            name = name)
                    });
                buffer.push_str(&source);
            }
            buffer.push_str("\n\n// Aliases to list types (by lexicographical order)\n");
            // FromJSON/ToJSON are already implemented in `binjs::utils`
            for name in lists.drain(..) {
                let typedef = source.get(name).unwrap();
                if let TypeSpec::Array { ref contents, ref supports_empty } = *typedef.spec() {
                    if let TypeSpec::NamedType(ref contents) = *contents.spec() {
                        let source = format!("{empty_check}pub type {name} = Vec<{contents}>;
impl<'a, W> Serialization<W, &'a {name}> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, value: &'a {name}) -> Result<W::Tree, W::Error> {{
        debug!(target: \"serialize_es6\", \"Serializing list {name}\");
        let mut children = Vec::with_capacity(value.len());
        for child in value {{
            children.push(self.serialize(child)?);
        }}
        self.writer.list(children)
    }}
}}
",
                            empty_check = if *supports_empty { "" } else { "// FIXME: Should discard empty vectors.\n" },
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
            buffer.push_str("\n\n// Aliases to optional types (by lexicographical order)\n");
            // FromJSON/ToJSON are already implemented in `binjs::utils`
            for name in options.drain(..) {
                let typedef = source.get(name).unwrap();
                if let TypeSpec::NamedType(ref contents) = *typedef.spec() {
                    let source = format!("pub type {name} = Option<{contents}>;\n",
                        name = name.to_class_cases(),
                        contents = contents.to_class_cases());
                    buffer.push_str(&source);
                } else {
                    panic!();
                }
            }
        }
        fn print_ast_interfaces(buffer: &mut String, source: &HashMap<NodeName, Rc<Interface>>, null_name: &str) {
            let mut names : Vec<_> = source.keys()
                .collect();
            names.sort();
            for name in &names {
                let interface = source.get(name).unwrap();
                let name = name.to_class_cases();
                let definition = format!("#[derive(PartialEq, Debug, Clone)]\npub struct {name} {{\n{fields}\n}}\n",
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
                                        TypeSpec::Offset => "Offset".to_string(),
                                        _ => TypeName::type_(field.type_())
                                    }
                                };
                            format!("    pub {name}: {contents}",
                                name = field.name().to_rust_identifier_case(),
                                contents = spec)
                        })
                        .format(",\n"),
                    name = name);

                let from_reader = format!("
impl<R> Deserializer<R> where R: TokenReader {{
    fn deserialize_tuple_{lowercase_name}(&mut self) -> Result<{name}, R::Error> where R: TokenReader {{
        let (kind, _, guard) = self.reader.tagged_tuple()?;
        let result =
            if let \"{name}\" = kind.as_str() {{
                debug!(target: \"deserialize_es6\", \"Deserializing tagged tuple {name}: present\");
                self.deserialize_inner()
            }} else {{
                debug!(target: \"deserialize_es6\", \"Deserializing tagged tuple {name}: found invalid {{}}\", kind.as_str());
                Err(From::from(TokenReaderError::BadEnumVariant))
            }};
        if result.is_err() {{
            self.reader.poison();
        }}
        guard.done()?;
        result
    }}
}}

impl<R> InnerDeserialization<R, {name}> for Deserializer<R> where R: TokenReader {{
    fn deserialize_inner(&mut self) -> Result<{name}, R::Error> where R: TokenReader {{
        Ok({name} {{
{fields}
        }})
    }}
}}

impl<R> Deserialization<R, {name}> for Deserializer<R> where R: TokenReader {{
    fn deserialize(&mut self) -> Result<{name}, R::Error> {{
        debug!(target: \"deserialize_es6\", \"Deserializing tagged tuple {name}\");
        self.deserialize_tuple_{lowercase_name}()
    }}
}}
impl<R> Deserialization<R, Option<{name}>> for Deserializer<R> where R: TokenReader {{
    fn deserialize(&mut self) -> Result<Option<{name}>, R::Error> {{
        debug!(target: \"deserialize_es6\", \"Deserializing optional tuple {name}\");
        let (kind, _, guard) = self.reader.tagged_tuple()?;
        let result = match kind.as_str() {{
            \"{name}\" => {{
                debug!(target: \"deserialize_es6\", \"Deserializing optional tuple {name}: present\");
                self.deserialize_inner().map(Some)
            }}
            \"{null}\" => {{
                debug!(target: \"deserialize_es6\", \"Deserializing optional tuple {name}: absent\");
                Ok(None)
            }},
            _ => {{
                error!(target: \"deserialize_es6\", \"Deserializing optional tuple {name}, found invalid {{}}\", kind.as_str());
                Err(From::from(TokenReaderError::BadEnumVariant))
            }}
        }};
        if result.is_err() {{
            self.reader.poison();
        }}
        guard.done()?;
        result
    }}
}}
",
                    name = name,
                    null = null_name,
                    lowercase_name = name.to_rust_identifier_case()
                        .trim_right_matches('_'),
                    fields = interface.contents()
                        .fields()
                        .iter()
                        .map(|field| format!("            {name}: (self.deserialize() as Result<_, R::Error>)?,",
                            name = field.name().to_rust_identifier_case()))
                        .format("\n")
                    );
                    let len = interface.contents()
                        .fields()
                        .len();
                    let to_writer = format!("
impl<'a, W> Serialization<W, &'a Option<{name}>> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, value: &'a Option<{name}>) -> Result<W::Tree, W::Error> {{
        debug!(target: \"serialize_es6\", \"Serializing optional tagged tuple {name}\");
        match *value {{
            None => self.writer.tagged_tuple(\"{null}\", &[]),
            Some(ref sum) => (self as &mut Serialization<W, &'a {name}>).serialize(sum)
        }}
    }}
}}
impl<'a, W> Serialization<W, &'a {name}> for Serializer<W> where W: TokenWriter {{
    fn serialize(&mut self, {value}: &'a {name}) -> Result<W::Tree, W::Error> {{
        debug!(target: \"serialize_es6\", \"Serializing tagged tuple {name}\");
        let {mut} children = Vec::with_capacity({len});
{fields}
        self.writer.tagged_tuple(\"{name}\", &children)
    }}
}}
",
                        mut = if len > 0 { "mut" } else { "" },
                        value = if len > 0 { "value" } else { "_" },
                        null = null_name,
                        name = name,
                        len = len,
                        fields = interface.contents()
                            .fields()
                            .iter()
                            .map(|field| format!("        children.push((\"{field_name}\", (self as &mut Serialization<W, &'a _>).serialize(&value.{rust_field_name})?));",
                                field_name = field.name().to_str(),
                                rust_field_name = field.name().to_rust_identifier_case()))
                            .format("\n")
                    );

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
        Ok({name} {{
{fields}
        }})
    }}
}}\n\n",
                    kind = name,
                    name = name,
                    fields = interface.contents()
                        .fields()
                        .iter()
                        .map(|field| format!("            {name}: FromJSON::import(&value[\"{key}\"])?,\n",
                            key = field.name().to_str(),
                            name = field.name().to_rust_identifier_case()))
                        .format("")
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
                        .map(|field| format!("             \"{key}\" => self.{name}.export()",
                            key = field.name().to_str(),
                            name = field.name().to_rust_identifier_case()))
                        .format(",\n")
                    );

                let walk = format!("
impl Walker for {name} {{
    fn walk<V, E>(&mut self, path: &mut Path, visitor: &mut V) -> Result<(), E> where V: Visitor<E> {{
        path.enter_interface(ASTNode::{name});
        visitor.enter_{snake}(path, self)?;
{fields}
        visitor.exit_{snake}(path, self)?;
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
                            format!("        path.enter_field(ASTField::{variant});
        self.{name}.walk(path, visitor)?;
        path.exit_field(ASTField::{variant});",
                                name = field.name().to_rust_identifier_case(),
                                variant = field.name().to_class_cases())
                        })
                        .format("\n")
                    );
                buffer.push_str(&definition);
                buffer.push_str(&from_reader);
                buffer.push_str(&to_writer);
                buffer.push_str(&from_json);
                buffer.push_str(&to_json);
                buffer.push_str(&walk);
                buffer.push_str("\n\n\n");
            }

            let interfaces_enum = format!("#[derive(Clone, Copy, Debug, PartialEq, Eq)]\npub enum ASTNode {{
{interfaces}
}}\n\n\n",
                interfaces = names.iter()
                    .map(|name| format!("    {}", name.to_class_cases()))
                    .format(",\n")
            );

            // Now generate the interface visitors
            let path = "
pub type PathItem = binjs_shared::ast::PathItem<ASTNode, ASTField>;
pub type Path = binjs_shared::ast::Path<ASTNode, ASTField>;
";
            let visitor = format!("
/// A set of callbacks used to inspect the contents of an AST in a strongly-typed
/// manner. For each node `Foo`, `enter_foo()` will be called before visiting the
/// children, giving the opportunity to alter the node, and `enter_foo()` will be
/// called after visiting the children, giving the opportunity to alter it further.
///
/// Each of the nodes of this AST implements `Walker` and may be visited recursively
/// using `Visitor`.
pub trait Visitor<E> {{
{interfaces}
}}\n
pub trait Walker {{
    fn walk<V, E>(&mut self, path: &mut Path, visitor: &mut V) -> Result<(), E> where V: Visitor<E>;
}}\n
impl Walker for String {{
    fn walk<V, E>(&mut self, _: &mut Path, _: &mut V) -> Result<(), E> where V: Visitor<E> {{
        Ok(())
    }}
}}
impl Walker for bool {{
    fn walk<V, E>(&mut self, _: &mut Path, _: &mut V) -> Result<(), E> where V: Visitor<E> {{
        // Do not inspect the contents of a bool.
        Ok(())
    }}
}}
impl Walker for f64 {{
    fn walk<V, E>(&mut self, _: &mut Path, _: &mut V) -> Result<(), E> where V: Visitor<E> {{
        // Do not inspect the contents of a f64.
        Ok(())
    }}
}}
impl Walker for u32 {{
    fn walk<V, E>(&mut self, _: &mut Path, _: &mut V) -> Result<(), E> where V: Visitor<E> {{
        // Do not inspect the contents of a u32.
        Ok(())
    }}
}}
impl Walker for Offset {{
    fn walk<V, E>(&mut self, _: &mut Path, _: &mut V) -> Result<(), E> where V: Visitor<E> {{
        // Do not inspect the contents of a Offset.
        Ok(())
    }}
}}
impl<T> Walker for Option<T> where T: Walker {{
    fn walk<V, E>(&mut self, path: &mut Path, visitor: &mut V) -> Result<(), E> where V: Visitor<E> {{
        // Do not callback on the `Option<>` itself, just on its contents.
        if let Some(ref mut contents) = *self {{
            contents.walk(path, visitor)?;
        }}
        Ok(())
    }}
}}
impl<T> Walker for Vec<T> where T: Walker {{
    fn walk<V, E>(&mut self, path: &mut Path, visitor: &mut V) -> Result<(), E> where V: Visitor<E> {{
        // Do not callback on the `Vec<>` itself, just on its contents.
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
    fn enter_{name}(&mut self, _path: &Path, _node: &mut {node_name}) -> Result<(), E> {{
        Ok(())
    }}
    fn exit_{name}(&mut self, _path: &Path, _node: &mut {node_name}) -> Result<(), E> {{
        Ok(())
    }}
",
                            name = name,
                            node_name = interface.name().to_class_cases())
                    })
                    .format("\n")
                );
            buffer.push_str(&interfaces_enum);
            buffer.push_str(&visitor);
            buffer.push_str(&path);
        }
        struct_buffer.push_str("    // String enum names (by lexicographical order)\n");
        impl_buffer.push_str("            // String enum names (by lexicographical order)\n");
        ast_buffer.push_str("// String enums (by lexicographical order)\n");
        print_struct_names(&mut struct_buffer, self.spec.string_enums_by_name().keys());
        print_impl_names(&mut impl_buffer, self.spec.string_enums_by_name().keys());
        print_ast_string_enums(&mut ast_buffer, deanonymized.string_enums_by_name());

        struct_buffer.push_str("\n\n    // Typedef names (by lexicographical order)\n");
        impl_buffer.push_str("\n\n            // Typedef names (by lexicographical order)\n");
        print_struct_names(&mut struct_buffer, self.spec.typedefs_by_name().keys());
        print_impl_names(&mut impl_buffer, self.spec.typedefs_by_name().keys());
        print_ast_typedefs(&mut ast_buffer, deanonymized.typedefs_by_name(), self.spec.get_null_name().to_str());

        struct_buffer.push_str("\n\n    // Interface names (by lexicographical order)\n");
        impl_buffer.push_str("\n\n            // Interface names (by lexicographical order)\n");
        ast_buffer.push_str("\n\n// Interfaces and interface names (by lexicographical order)\n");
        print_struct_names(&mut struct_buffer, self.spec.interfaces_by_name().keys());
        print_impl_names(&mut impl_buffer, self.spec.interfaces_by_name().keys());
        print_ast_interfaces(&mut ast_buffer, deanonymized.interfaces_by_name(), self.spec.get_null_name().to_str());

        struct_buffer.push_str("\n\n\n    // Field names (by lexicographical order)\n");
        impl_buffer.push_str("\n\n\n            // Field names (by lexicographical order)\n");
        ast_buffer.push_str("\n\n\n// Field names (by lexicographical order)\n#[derive(Clone, Copy, PartialEq, Eq, Debug)]\npub enum ASTField {\n");
        let mut fields = HashSet::new();
        for interface in self.spec.interfaces_by_name().values() {
            for field in interface.contents().fields() {
                fields.insert(field.name().to_string().clone());
            }
        }
        fields.insert("Offset".to_string()); // Hardcoded Offset field.
        let fields : Vec<_> = fields.drain().sorted();
        for name in fields {
            let snake = name.to_rust_identifier_case();
            let struct_source = format!("    pub field_{snake}: FieldName,\n",
                snake = snake);
            struct_buffer.push_str(&struct_source);

            let impl_source = format!("            field_{snake}: builder.field_name(\"{original}\"),\n",
                snake = snake,
                original = name);
            impl_buffer.push_str(&impl_source);

            let ast_source = format!("    {variant},\n",
                variant = name.to_class_cases());
            ast_buffer.push_str(&ast_source);
        }


        impl_buffer.push_str("        };\n");
        struct_buffer.push_str("}");
        ast_buffer.push_str("}\n");


        impl_buffer.push_str("\n\n\n        // Enumerations\n");
        for (name, def) in self.spec.string_enums_by_name() {
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
        for (name, def) in self.spec.typedefs_by_name() {
            let impl_source = format!("        builder.add_typedef(&names.{name}).unwrap()
            .with_type(\n{spec});\n\n",
                name = name.to_rust_identifier_case(),
                spec = Self::type_(def, "                    "));
            impl_buffer.push_str(&impl_source);
        }
        for (name, def) in self.spec.interfaces_by_name() {
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

        impl_buffer.push_str("        names\n    }\n");
        impl_buffer.push_str("
}

impl Library {
    /// Insert scope annotations in the scope.
    pub fn annotate(&self, ast: &mut JSON) {
        use binjs_es6;
        let mut visitor = binjs_es6::scopes::AnnotationVisitor::new();
        visitor.annotate(ast);
    }
}

impl Annotator for Library {
    fn annotate(&self, ast: &mut JSON) {
        Library::annotate(self, ast)
    }
}
");
        ExportedSource {
            typed: format!("// This file was generated by binjs_meta generate_library.\n{ast_}\n",
                ast_ = ast_buffer),
            generic: format!("// This file was generated by binjs_meta generate_library.\npub use annotate::Annotator;\nuse binjs_meta::spec::*;\nuse json::JsonValue as JSON;\n\n\n{struct_}\n{impl_}",
                struct_ = struct_buffer,
                impl_ = impl_buffer)
        }
    }
}

