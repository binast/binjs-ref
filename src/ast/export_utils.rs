use ast::grammar::*;

use std::collections::{ HashSet };

use itertools::Itertools;

pub struct TypeDeanonymizer {
    builder: SyntaxBuilder,
}
impl TypeDeanonymizer {
    pub fn new(syntax: &Syntax) -> Self {
        let mut result = TypeDeanonymizer {
            builder: SyntaxBuilder::new(),
        };
        // Copy names
        for (_, name) in syntax.field_names() {
            result.builder.import_field_name(name)
        }
        // Copy and deanonymize interfaces.
        for (name, interface) in syntax.interfaces_by_name() {
            result.builder.import_node_name(name);
            // Collect interfaces to copy them into the `builder`
            // and walk through their fields to deanonymize types.
            let mut fields = vec![];
            for field in interface.contents().fields() {
                result.import_type(syntax, field.type_(), None);
                fields.push(field.clone());
            }

            // Copy the declaration.
            let mut declaration = result.builder.add_interface(name)
                .unwrap();
            for field in fields.drain(..) {
                declaration.with_field(field.name(), field.type_().clone());
            }
        }
        // Copy and deanonymize typedefs
        for (name, definition) in syntax.typedefs_by_name() {
            result.builder.import_node_name(name);
            if result.builder.get_typedef(name).is_some() {
                // Already imported by following links.
                continue
            }
            result.import_type(syntax, &definition, Some(name.clone()));
        }
        // Copy and deanonymize string enums
        for (name, definition) in syntax.string_enums_by_name() {
            result.builder.import_node_name(name);
            let mut strings: Vec<_> = definition.strings()
                .iter()
                .collect();
            let mut declaration = result.builder.add_string_enum(name)
                .unwrap();
            for string in strings.drain(..) {
                declaration.with_string(&string);
            }
        }
        debug!(target: "export_utils", "Names: {:?}", result.builder.names().keys().format(", "));

        result
    }
    pub fn into_syntax(self, options: SyntaxOptions) -> Syntax {
        self.builder.into_syntax(options)
    }
    pub fn get_node_name(&self, name: &str) -> Option<NodeName> {
        self.builder.get_node_name(name)
    }
    /// Returns `(sum, name)` where `sum` is `Some(names)` iff this type can be resolved to a sum of interfaces.
    fn import_type(&mut self, syntax: &Syntax, type_: &Type, public_name: Option<NodeName>) -> (Option<HashSet<NodeName>>, NodeName) {
        debug!(target: "export_utils", "import_type {:?} => {:?}", public_name, type_);
        if type_.is_optional() {
            let (_, spec_name) = self.import_typespec(syntax, &type_.spec, None);
            let my_name = 
                match public_name {
                    None => self.builder.node_name(&format!("Optional{}", spec_name)),
                    Some(ref name) => name.clone()
                };
            let deanonymized = Type::named(&spec_name).optional();
            if let Some(ref mut typedef) = self.builder.add_typedef(&my_name) {
                typedef.with_type(deanonymized.clone());
            } else {
                debug!(target: "export_utils", "import_type: Attempting to redefine typedef {name}", name = my_name.to_str());
            }
            (None, my_name)
        } else {
            self.import_typespec(syntax, &type_.spec, public_name)
        }
    }
    fn import_typespec(&mut self, syntax: &Syntax, spec: &TypeSpec, public_name: Option<NodeName>) -> (Option<HashSet<NodeName>>, NodeName) {
        debug!(target: "export_utils", "import_typespec {:?} => {:?}", public_name, spec);
        match *spec {
            TypeSpec::Boolean |
            TypeSpec::Number |
            TypeSpec::String |
            TypeSpec::Void    => {
                if let Some(ref my_name) = public_name {
                    if let Some(ref mut typedef) = self.builder.add_typedef(&my_name) {
                        debug!(target: "export_utils", "import_typespec: Defining {name} (primitive)", name = my_name.to_str());
                        typedef.with_type(spec.clone().required());
                    } else {
                        debug!(target: "export_utils", "import_typespec: Attempting to redefine typedef {name}", name = my_name.to_str());
                    }
                }
                (None, self.builder.node_name("@@"))
            }
            TypeSpec::NamedType(ref link) => {
                let resolved = syntax.get_type_by_name(link)
                    .unwrap_or_else(|| panic!("While deanonymizing, could not find the definition of {} in the original syntax.", link.to_str()));
                let (sum, rewrite, primitive) = match resolved {
                    NamedType::StringEnum(_) => {
                        // - Can't use in a sum
                        // - No rewriting happened.
                        (None, None, None)
                    }
                    NamedType::Typedef(ref type_) => {
                        // - Might use in a sum.
                        // - Might be rewritten.
                        let (sum, name) = self.import_type(syntax, type_, Some(link.clone()));
                        (sum, Some(name), type_.get_primitive(syntax))
                    }
                    NamedType::Interface(_) => {
                        // - May use in a sum.
                        // - If a rewriting takes place, it didn't change the names.
                        let sum = [link.clone()].iter()
                            .cloned()
                            .collect();
                        (Some(sum), None, None)
                    }
                };
                debug!(target: "export_utils", "import_typespec dealing with named type {}, public name {:?} => {:?}",
                    link, public_name, rewrite);
                if let Some(ref my_name) = public_name {
                    // If we have a public name, alias it to `content`
                    if let Some(content) = rewrite {
                        let deanonymized = match primitive {
                            None |
                            Some(IsNullable { is_nullable: true, .. }) |
                            Some(IsNullable { content: Primitive::Interface(_), .. }) => Type::named(&content).required(),
                            Some(IsNullable { content: Primitive::String, .. }) => Type::string().required(),
                            Some(IsNullable { content: Primitive::Number, .. }) => Type::number().required(),
                            Some(IsNullable { content: Primitive::Boolean, .. }) => Type::bool().required(),
                            Some(IsNullable { content: Primitive::Void, .. }) => Type::void().required()
                        };
                        debug!(target: "export_utils", "import_typespec aliasing {:?} => {:?}",
                            my_name, deanonymized);
                        if let Some(ref mut typedef) = self.builder.add_typedef(&my_name) {
                            debug!(target: "export_utils", "import_typespec: Defining {name} (name to content)", name = my_name.to_str());
                            typedef.with_type(deanonymized.clone());
                        } else {
                            debug!(target: "export_utils", "import_typespec: Attempting to redefine typedef {name}", name = my_name.to_str());
                        }
                    }
                    // Also, don't forget to copy the typedef and alias `link`
                    let deanonymized = Type::named(link).required();
                    if let Some(ref mut typedef) = self.builder.add_typedef(&my_name) {
                        debug!(target: "export_utils", "import_typespec: Defining {name} (name to link)", name = my_name.to_str());
                        typedef.with_type(deanonymized.clone());
                    } else {
                        debug!(target: "export_utils", "import_typespec: Attempting to redefine typedef {name}", name = my_name.to_str());
                    }
                }
                (sum, link.clone())
            }
            TypeSpec::Array {
                ref contents,
                ref supports_empty
            } => {
                let (_, contents_name) = self.import_type(syntax, contents, None);
                let my_name = 
                    match public_name {
                        None => self.builder.node_name(&format!("{non_empty}ListOf{content}",
                            non_empty =
                                if *supports_empty {
                                    ""
                                } else {
                                    "NonEmpty"
                                },
                            content = contents_name.to_str())),
                        Some(ref name) => name.clone()
                    };
                let deanonymized =
                    if *supports_empty {
                        Type::named(&contents_name).array()
                    } else {
                        Type::named(&contents_name).non_empty_array()
                    };
                if let Some(ref mut typedef) = self.builder.add_typedef(&my_name) {
                    debug!(target: "export_utils", "import_typespec: Defining {name} (name to list)",
                        name = my_name.to_str());
                    typedef.with_type(deanonymized.clone());
                } else {
                    debug!(target: "export_utils", "import_typespec: Attempting to redefine typedef {name}", name = my_name.to_str());
                }
                (None, my_name)
            }
            TypeSpec::TypeSum(ref sum) => {
                let mut full_sum = HashSet::new();
                let mut names = vec![];
                for sub_type in sum.types() {
                    let (mut sub_sum, name) = self.import_typespec(syntax, sub_type, None);
                    let mut sub_sum = sub_sum.unwrap_or_else(
                        || panic!("While treating {:?}, attempting to create a sum containing {}, which isn't an interface or a sum of interfaces", spec, name)
                    );
                    names.push(name);
                    for item in sub_sum.drain() {
                        full_sum.insert(item);
                    }
                }
                let my_name =
                    match public_name {
                        None => self.builder.node_name(&format!("{}",
                            names.drain(..).format("Or"))),
                        Some(ref name) => name.clone()
                    };
                let sum : Vec<_> = full_sum.iter()
                    .map(Type::named)
                    .collect();
                let deanonymized = Type::sum(&sum).required();
                if let Some(ref mut typedef) = self.builder.add_typedef(&my_name) {
                    debug!(target: "export_utils", "import_typespec: Defining {name} (name to sum)", name = my_name.to_str());
                    typedef.with_type(deanonymized.clone());
                } else {
                    debug!(target: "export_utils", "import_type: Attempting to redefine typedef {name}", name = my_name.to_str());
                }
                (Some(full_sum), my_name)
            }
        }
    }
}


