use spec::{ self, SpecBuilder, TypeSum };

use webidl::ast::*;

pub struct Importer {
    builder: SpecBuilder,
}
impl Importer {
    pub fn import(ast: &AST) -> SpecBuilder {
        let mut importer = Importer {
            builder: SpecBuilder::new()
        };
        importer.import_ast(ast);
        importer.builder
    }
    fn import_ast(&mut self, ast: &AST) {
        for definition in ast {
            self.import_definition(&definition)
        }
    }
    fn import_definition(&mut self, def: &Definition) {
        match *def {
            Definition::Enum(ref enum_) => self.import_enum(enum_),
            Definition::Typedef(ref typedef) => self.import_typedef(typedef),
            Definition::Interface(ref interface) => self.import_interface(interface),
            _ => panic!("Not implemented: importing {:?}", def)
        }
    }
    fn import_enum(&mut self, enum_: &Enum) {
        let name = self.builder.node_name(&enum_.name);
        let mut node = self.builder.add_string_enum(&name)
            .expect("Name already present");
        for variant in &enum_.variants {
            node.with_string(variant);
        }
    }
    fn import_typedef(&mut self, typedef: &Typedef) {
        let name = self.builder.node_name(&typedef.name);
        let type_ = self.convert_type(&*typedef.type_);
        let mut node = self.builder.add_typedef(&name)
            .expect("Name already present");
        assert!(!type_.is_optional());
        node.with_spec(type_.spec);
    }
    fn import_interface(&mut self, interface: &Interface) {
        let interface = if let &Interface::NonPartial(ref interface) = interface {
            interface
        } else {
            panic!("Expected a non-partial interface, got {:?}", interface);
        };
        if interface.name == "Node" {
            return;
        }
        if let Some(ref parent) = interface.inherits {
            assert_eq!(parent, "Node");
        }
        let mut fields = Vec::new();
        for member in &interface.members {
            if let InterfaceMember::Attribute(Attribute::Regular(ref attribute)) = *member {
                let name = self.builder.field_name(&attribute.name);
                let type_ = self.convert_type(&*attribute.type_);
                fields.push((name, type_));
            } else {
                panic!("Expected an attribute, got {:?}", member);
            }
        }
        let name = self.builder.node_name(&interface.name);
        let mut node = self.builder.add_interface(&name)
            .expect("Name already present");
        for (field_name, field_type) in fields.drain(..) {
            node.with_field(&field_name, field_type);
        }
    }
    fn convert_type(&mut self, t: &Type) -> spec::Type {
        let spec = match t.kind {
            TypeKind::Boolean => spec::TypeSpec::Boolean,
            TypeKind::Identifier(ref id) => {
                let name = self.builder.node_name(id);
                spec::TypeSpec::NamedType(name.clone())
            }
            TypeKind::DOMString => spec::TypeSpec::String,
            TypeKind::Union(ref types) => {
                let mut dest = Vec::with_capacity(types.len());
                for typ in types {
                    dest.push(self.convert_type(&*typ).spec)
                }
                spec::TypeSpec::TypeSum(TypeSum::new(dest))
            }
            TypeKind::FrozenArray(ref type_) => {
                spec::TypeSpec::Array {
                    contents: Box::new(self.convert_type(&*type_)),
                    supports_empty: true
                }
            }
            TypeKind::RestrictedDouble =>
                spec::TypeSpec::Number,
            _ => {
                panic!("I don't know how to import {:?} yet", t);
            }
        };
        if t.nullable {
            spec.optional()
        } else {
            spec.required()
        }
    }
}