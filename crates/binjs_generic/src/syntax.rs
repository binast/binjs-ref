use util::type_of;

use binjs_meta::spec::*;
use binjs_shared;

use std;

use json::JsonValue as JSON;

pub type WalkPathItem = binjs_shared::ast::PathItem<NodeName, FieldName>;
pub type WalkPath = binjs_shared::ast::Path<NodeName, FieldName>;

macro_rules! make_ast_visitor {
    // $mutability is either `mut` or nothing.
    ($visitor_name:ident, $walker_name:ident, $($mutability: ident)*) => {
        pub trait $visitor_name {
            fn enter_type(&mut self, _path: &WalkPath, _value: & $($mutability)* JSON, _type_: &Type, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn exit_type(&mut self, _path: &WalkPath, _value: & $($mutability)* JSON, _type_: &Type, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn enter_typespec(&mut self, _path: &WalkPath, _value: & $($mutability)* JSON, _typespec_: &TypeSpec, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn exit_typespec(&mut self, _path: &WalkPath, _value: & $($mutability)* JSON, _typespec_: &TypeSpec, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn enter_string_enum(&mut self, _path: &WalkPath, _value: & $($mutability)* JSON, _enum_: &StringEnum, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn exit_string_enum(&mut self, _path: &WalkPath, _value: & $($mutability)* JSON, _enum_: &StringEnum, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn enter_interface(&mut self, _path: &WalkPath, _value: & $($mutability)* JSON, _interface: &Interface, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn exit_interface(&mut self, _path: &WalkPath, _value: & $($mutability)* JSON, _interface: &Interface, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
        }

        pub struct $walker_name<'a, V> where V: $visitor_name {
            syntax: &'a Spec,
            visitor: V,
            path: WalkPath,
        }

        impl<'a, V> $walker_name<'a, V> where V: $visitor_name {
            pub fn new(syntax: &'a Spec, visitor: V) -> Self {
                $walker_name {
                    syntax,
                    visitor,
                    path: WalkPath::new(),
                }
            }
            pub fn walk(&mut self, value: & $($mutability)* JSON) -> Result<(), ASTError> {
                let root = self.syntax.get_root();
                let name = self.syntax.get_root_name();
                assert_eq!(self.path.len(), 0);
                self.walk_named_type(value, &root, name)?;
                assert_eq!(self.path.len(), 0);
                Ok(())
            }
            pub fn walk_named_type(&mut self, value: & $($mutability)* JSON, named: &NamedType, name: &NodeName) -> Result<(), ASTError> {
                match *named {
                    NamedType::StringEnum(ref enum_) => self.walk_string_enum(value, enum_, name)?,
                    NamedType::Interface(ref interface_) => self.walk_interface(value, interface_, name)?,
                    NamedType::Typedef(ref typedef_) => self.walk_type(value, typedef_, name)?,
                }
                Ok(())
            }
            pub fn walk_string_enum(&mut self, value: & $($mutability)* JSON, enum_: &StringEnum, name: &NodeName) -> Result<(), ASTError> {
                self.visitor.enter_string_enum(&self.path, value, enum_, name)?;
                if let Some(ref s) = value.as_str() {
                    if enum_.strings().iter()
                        .find(|x| x == s)
                        .is_none()
                    {
                        return Err(ASTError::InvalidValue {
                            expected: format!("One of {:?}", enum_.strings()),
                            got: s.to_string()
                        })
                    }
                } else {
                    return Err(ASTError::InvalidValue {
                        expected: "string".to_string(),
                        got: format!("{:?}", *value)
                    })
                }
                self.visitor.exit_string_enum(&self.path, value, enum_, name)?;
                Ok(())
            }
            pub fn walk_type(&mut self, value: & $($mutability)* JSON, type_: &Type, name: &NodeName) -> Result<(), ASTError> {
                self.visitor.enter_type(&self.path, value, type_, name)?;
                if let JSON::Null = *value {
                    // Value was omitted.
                    if type_.is_optional() {
                        self.visitor.exit_type(&self.path, value, type_, name)?;
                        return Ok(())
                    }
                }
                self.walk_type_spec(value, type_.spec(), name)?;
                self.visitor.exit_type(&self.path, value, type_, name)?;
                Ok(())
            }
            pub fn walk_type_spec(&mut self, value: & $($mutability)* JSON, spec: &TypeSpec, name: &NodeName) -> Result<(), ASTError> {
                self.visitor.enter_typespec(&self.path, value, spec, name)?;
                self.walk_type_spec_aux(value, spec, name)?;
                self.visitor.exit_typespec(&self.path, value, spec, name)?;
                Ok(())
            }

            pub fn walk_interface(&mut self, value: & $($mutability)* JSON, interface: &Interface, name: &NodeName) -> Result<(), ASTError> {
                // Let the visitor rewrite the object if necessary.
                self.visitor.enter_interface(&self.path, value, interface, name)?;
                // Check type.
                let is_ok =
                    if let Some(name) = value["type"].as_str() {
                        name == interface.name().to_str()
                    } else {
                        false
                    };
                if !is_ok {
                    return Err(ASTError::invalid_value(value, &format!("Instance of {:?}", interface.name())))
                }
                if value.is_object() {
                    self.path.enter_interface(name.clone());
                    // Visit fields, ignoring excess fields and, if necessary, optional fields.
                    for field in interface.contents().fields() {
                        self.path.enter_field(field.name().clone());
                        let ref $($mutability)* value = value[field.name().to_str()];
                        self.walk_type(value, field.type_(), interface.name())?;
                        self.path.exit_field(field.name().clone());
                    }
                    self.path.exit_interface(name.clone());
                } else {
                    return Err(ASTError::InvalidValue {
                        expected: "object".to_string(),
                        got: type_of(&*value)
                    })
                }
                self.visitor.exit_interface(&self.path, value, interface, name)?;
                Ok(())
            }
            fn walk_type_spec_aux(&mut self, value: & $($mutability)* JSON, spec: &TypeSpec, name: &NodeName) -> Result<(), ASTError> {
                match spec {
                    &TypeSpec::Boolean => {
                        if let JSON::Boolean(_) = *value {
                            return Ok(())
                        }
                    }
                    &TypeSpec::Number => {
                        if let JSON::Number(_) = *value {
                            return Ok(())
                        }
                    }
                    &TypeSpec::UnsignedLong => {
                        if let JSON::Number(_) = *value {
                            return Ok(())
                        }
                    }
                    &TypeSpec::Offset => {
                        if let JSON::Number(_) = *value {
                            return Ok(())
                        }
                    }
                    &TypeSpec::String | &TypeSpec::IdentifierName | &TypeSpec::PropertyKey  => {
                        if value.as_str().is_some() {
                            return Ok(())
                        }
                    }
                    &TypeSpec::Void => {
                        if let JSON::Null = *value {
                            return Ok(())
                        }
                    }
                    &TypeSpec::NamedType(ref name) => {
                        let named = self.syntax.get_type_by_name(name)
                            .ok_or_else(|| ASTError::InvalidType(name.to_str().to_string()))?;
                        return self.walk_named_type(value, &named, name);
                    }
                    &TypeSpec::Array { ref contents, supports_empty } => {
                        if value.is_array() {
                            if value.len() == 0 {
                                if supports_empty {
                                    return Ok(())
                                } else {
                                    return Err(ASTError::InvalidValue {
                                        expected: "non-empty array".to_string(),
                                        got: "empty array".to_string()
                                    })
                                }
                            }
                            for i in 0..value.len() {
                                let ref $($mutability)* value = value[i];
                                self.walk_type(value, contents, name)?;
                            }
                            return Ok(())
                        }
                    }
                    &TypeSpec::TypeSum(ref sum) => {
                        // Find out to which element in the sum we belong.
                        let interface = {
                            let kind = value["type"].as_str()
                                .ok_or_else(|| ASTError::InvalidValue {
                                    expected: "object with a type".to_string(),
                                    got: value.dump()
                                })?;
                            let kind_name = self.syntax.get_node_name(kind)
                                .ok_or_else(|| ASTError::InvalidType(kind.to_string()))?;
                            let interface = sum.get_interface(self.syntax, kind_name)
                                .ok_or_else(|| ASTError::InvalidType(kind.to_string()))?;
                            interface.clone()
                        };
                        return self.walk_interface(value, &interface, name);
                    }
                }
                return Err(ASTError::InvalidValue {
                    expected: format!("{:?}", spec),
                    got: value.pretty(2)
                })
            }
        }
    }
}

/// Define mutable AST Visitor/walker
make_ast_visitor!(MutASTVisitor, MutASTWalker, mut);

#[derive(Debug)]
pub enum ASTError {
    InvalidField(String),
    Mismatch(Type),
    InvalidValue { got: String, expected: String },
    InvalidType(String),
    InvalidDescendent { got: String, valid: Vec<String> },
    MissingParent(String),
    MissingField(String),
    InvalidScope,
}
impl ASTError {
    pub fn invalid_field(name: &str) -> Self {
        ASTError::InvalidField(name.to_string())
    }

    pub fn invalid_value<T>(value: T, expected: &str) -> Self
    where
        T: std::ops::Deref<Target = JSON>,
    {
        ASTError::InvalidValue {
            got: value.dump(),
            expected: expected.to_string(),
        }
    }

    pub fn missing_field(name: &str) -> Self {
        ASTError::MissingField(name.to_string())
    }
}
