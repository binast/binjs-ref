use util::type_of;

use binjs_io::{ TokenWriter, TokenWriterError };
use binjs_meta::spec::*;
use binjs_shared::{ FieldName, InterfaceName, SharedString, self };

use std;
use std::cell::*;

use json::JsonValue as JSON;
use json::object::Object as Object;

type Path = binjs_shared::ast::Path<binjs_shared::InterfaceName, (usize, binjs_shared::FieldName)>;

#[derive(Debug)]
pub enum Error {
    Mismatch { expected: String, got: String },
    NoSuchInterface(String),
    NoSuchRefinement { expected: String, got: Vec<String> },
    NoSuchKind(String),
    NoSuchType(String),
    MissingField(String),
    NoSuchLiteral { strings: Vec<String> },
    TokenWriterError(TokenWriterError),
    NonNullableType(String),
}

impl From<TokenWriterError> for Error {
    fn from(value: TokenWriterError) -> Self {
        Error::TokenWriterError(value)
    }
}

impl Error {
    fn missing_field(field: &str, node: &NodeName) -> Self {
        Error::MissingField(format!("Field \"{}\" while handling {:?}", field, node))
    }
}

pub trait Encode {
    type Data;
    type Statistics;
    fn generic_encode(&self, value: &JSON) -> Result<(), std::io::Error> {
        self.encode(value)
    }
    fn encode(&self, value: &JSON) -> Result<(), std::io::Error>;
    fn done(self) -> Result<(Self::Data, Self::Statistics), std::io::Error>;
}

pub struct Encoder<'a, B> where B: TokenWriter {
    grammar: &'a Spec,
    builder: RefCell<B>,
}
impl<'a, B> Encoder<'a, B> where B: TokenWriter {
    pub fn new(syntax: &'a Spec, builder: B) -> Self {
        Encoder {
            grammar: syntax,
            builder: RefCell::new(builder),
        }
    }

    /// Encode a JSON into a Serialize() based on a grammar.
    /// This step doesn't perform any interesting check on the JSON.
    pub fn encode(&self, value: &JSON) -> Result<(), Error> {
        let root = self.grammar.get_root();
        let node = self.grammar.get_root_name();
        let mut path = Path::new();
        self.encode_from_named_type(&mut path, value, &root, &node, false)
    }
    pub fn encode_from_named_type(&self, path: &mut Path, value: &JSON, named: &NamedType, node: &NodeName, is_optional: bool) -> Result<(), Error> {
        match *named {
            NamedType::StringEnum(ref enum_) => {
                let string = value.as_str()
                    .ok_or_else(|| Error::Mismatch {
                        expected: format!("String (one of {:?}), while treating {:?}", enum_, node),
                        got: type_of(&value)
                    })?;
                if enum_.strings()
                    .iter()
                    .find(|c| c == &string)
                    .is_some()
                {
                    let string = SharedString::from_string(string.to_string());
                    return self.builder.borrow_mut()
                        .string_at(Some(&string), path)
                        .map_err(Error::TokenWriterError)
                }
                Err(Error::NoSuchLiteral {
                    strings: enum_.strings().iter().cloned().collect(),
                })
            }
            NamedType::Typedef(ref type_) =>
                return self.encode_from_type(path, value, type_, node, is_optional),
            NamedType::Interface(ref interface) => {
                debug!(target:"encode", "Attempting to encode value {:?} with interface {:?}", value, interface.name());
                let interface_name = InterfaceName::from_rc_string(interface.name()
                    .to_rc_string()
                    .clone());
                path.enter_interface(interface_name.clone());
                match *value {
                    JSON::Object(ref object) => {
                        debug!(target:"encode", "Attempting to encode object {:?} with interface {:?}", value["type"].as_str(), interface.name());
                        let type_field = object.get("type")
                            .ok_or_else(|| Error::missing_field("type", node))?
                            .as_str()
                            .ok_or_else(|| Error::missing_field("type", node))?;
                        if interface_name == type_field {
                            // Write the contents with the tag of the refined interface.
                            let fields = interface.contents()
                                .fields();
                            self.encode_structure(path, &interface_name, object, fields, interface.name())?;
                        } else {
                            return Err(Error::Mismatch {
                                expected: interface_name.as_str().to_string(),
                                got: type_field.to_string()
                            })
                        }
                    }
                    JSON::Null if is_optional => {
                        // Write the contents with the tag of the refined interface.
                        let null_name = InterfaceName::from_string(self.grammar.get_null_name().to_str().to_string());
                        {
                            self.builder
                                .borrow_mut()
                                .enter_tagged_tuple_at(&null_name, &[], path)
                                .map_err(Error::TokenWriterError)?;
                        }
                        // No fields.
                        {
                            self.builder
                                .borrow_mut()
                                .exit_tagged_tuple_at(&null_name, &[], path)
                                .map_err(Error::TokenWriterError)?;
                        }
                    }
                    _ => {
                        return Err(Error::Mismatch {
                            expected: node.to_string().clone(),
                            got: value.dump()
                        })
                    }
                }
                path.exit_interface(interface_name);
                Ok(())
            }
        }
    }
    pub fn encode_from_type(&self, path: &mut Path, value: &JSON, type_: &Type, node: &NodeName, is_optional: bool) -> Result<(), Error> {
        debug!(target:"encode", "value {:?} within node {:?}, type {:?}", value, node, type_);
        self.encode_from_type_spec(path, value, type_.spec(), node, is_optional || type_.is_optional())
    }
    fn encode_from_type_spec(&self, path: &mut Path, value: &JSON, spec: &TypeSpec, node: &NodeName, is_optional: bool) -> Result<(), Error> {
        use binjs_meta::spec::TypeSpec::*;
        match (spec, value) {
            (&Array { contents: ref kind, .. }, &JSON::Array(ref array)) => {
                {
                    self.builder.borrow_mut()
                        .enter_list_at(array.len(), path)
                        .map_err(Error::TokenWriterError)?;
                }
                for item in array {
                    self.encode_from_type(path, item, kind, node, false)?;
                }
                {
                    self.builder.borrow_mut()
                        .exit_list_at(path)
                        .map_err(Error::TokenWriterError)?;
                }
                return Ok(())
            }
            (&Boolean, &JSON::Boolean(b)) => {
                return self.builder
                    .borrow_mut()
                    .bool_at(Some(b), path)
                    .map_err(Error::TokenWriterError)
            }
            (&String, _) if value.is_string() => {
                let string = value.as_str()
                    .unwrap()
                    .to_string(); // Checked just above.
                return self.builder.borrow_mut()
                    .string_at(Some(&SharedString::from_string(string)), path)
                    .map_err(Error::TokenWriterError)
            }
            (&String, &JSON::Null) => {
                return self.builder
                    .borrow_mut()
                    .string_at(None, path)
                    .map_err(Error::TokenWriterError)
            }
            (&IdentifierName, _) if value.is_string() => {
                let string = value.as_str()
                    .unwrap()
                    .to_string(); // Checked just above.
                return self.builder.borrow_mut()
                    .identifier_name_at(Some(&binjs_shared::IdentifierName::from_string(string)), path)
                    .map_err(Error::TokenWriterError)
            }
            (&IdentifierName, &JSON::Null) => {
                return self.builder
                    .borrow_mut()
                    .identifier_name_at(None, path)
                    .map_err(Error::TokenWriterError)
            }
            (&PropertyKey, _) if value.is_string() => {
                let string = value.as_str()
                    .unwrap()
                    .to_string(); // Checked just above.
                return self.builder.borrow_mut()
                    .property_key_at(Some(&binjs_shared::PropertyKey::from_string(string)), path)
                    .map_err(Error::TokenWriterError)
            }
            (&PropertyKey, &JSON::Null) => {
                return self.builder
                    .borrow_mut()
                    .property_key_at(None, path)
                    .map_err(Error::TokenWriterError)
            }
            (&Number, &JSON::Number(x)) =>
                return self.builder
                    .borrow_mut()
                    .float_at(Some(x.into()), path)
                    .map_err(Error::TokenWriterError),
            (&UnsignedLong, &JSON::Number(x)) =>
                return self.builder
                    .borrow_mut()
                    .unsigned_long_at(x.into(), path)
                    .map_err(Error::TokenWriterError),
            (&NamedType(ref name), _) => {
                let named_type = self.grammar
                    .get_type_by_name(name)
                    .ok_or_else(|| Error::NoSuchType(name.to_string().clone()))?;
                return self.encode_from_named_type(path, value, &named_type, node, is_optional)
            }
            (&TypeSum(ref sum), &JSON::Object(ref object)) => {
                // Sums may only be used to encode objects or null.
                let kind = object["type"].as_str()
                    .ok_or_else(|| Error::MissingField("type".to_string()))?;
                let name = self.grammar.get_node_name(kind)
                    .ok_or_else(|| Error::NoSuchKind(kind.to_string()))?;
                sum.get_interface(self.grammar, name)
                    .ok_or_else(|| Error::NoSuchInterface(name.to_string().clone()))?;
                let named_type = self.grammar.get_type_by_name(name)
                    .ok_or_else(|| Error::NoSuchInterface(name.to_string().clone()))?;
                return self.encode_from_named_type(path, value, &named_type, node, is_optional)
            }
            (&TypeSum(_), &JSON::Null) if is_optional => {
                // The `Null` interface can be substituted.
                let null_name = self.grammar.get_null_name();
                let null_type = self.grammar.get_type_by_name(self.grammar.get_null_name())
                    .unwrap_or_else(|| panic!("Could not find type named {}", null_name.to_str()));
                return self.encode_from_named_type(path, value, &null_type, null_name, /* is_optional = */ true)
            }
            _ => {}
        }
        Err(Error::Mismatch {
            expected: format!("{spec:?}{is_optional}",
                spec = spec,
                is_optional = if is_optional { " (optional)"} else { " (required)" }),
            got: value.dump()
        })
    }
    fn encode_structure(&self, path: &mut Path, interface_name: &InterfaceName, object: &Object, fields: &[Field], node: &NodeName) -> Result<(), Error> {
        // FIXME: Once we unify FieldName between binjs_meta and everything else,
        // we should be able to avoid all these costly conversions. Not a priority
        // atm, as binjs_generic is used only for testing purposes.
        let field_names : Vec<_> = fields.iter()
            .map(|field| FieldName::from_rc_string(field
                .name()
                .to_rc_string()
                .clone()))
            .collect();
        let field_names_refs : Vec<_> = field_names.iter()
            .collect();
        {
            self.builder
                .borrow_mut()
                .enter_tagged_tuple_at(&interface_name, field_names_refs.as_ref(), path)?;
        }

        'fields: for (i, (field, field_name)) in fields
            .iter()
            .zip(field_names.iter())
            .enumerate()
        {
            path.enter_field((i, field_name.clone()));
            if let Some(source) = object.get(field.name().to_string()) {
                self.encode_from_type(path, source, field.type_(), node, false)?;
            } else if field.type_().is_optional() {
                self.encode_from_type(path, &JSON::Null, field.type_(), node, false)?;
            } else {
                debug!("Error in {:?}", object);
                return Err(Error::missing_field(field.name().to_string(), node));
            }
            path.exit_field((i, field_name.clone()));
        }

        {
            self.builder
                .borrow_mut()
                .exit_tagged_tuple_at(&interface_name, field_names_refs.as_ref(), path)?;
        }

        Ok(())
    }
}


impl<'a, B> Encode for Encoder<'a, B> where B: TokenWriter {
    type Data = B::Data;
    type Statistics = B::Statistics;
    fn encode(&self, value: &JSON) -> Result<(), std::io::Error> {
        (self as &Encoder<'a, B>).encode(value)
            .map(|_| ())
            .map_err(|err| {
                std::io::Error::new(std::io::ErrorKind::InvalidData, format!("{:?}", err))
            })
    }
    fn done(self) -> Result<(Self::Data, Self::Statistics), std::io::Error> {
        self.builder.into_inner().done()
            .map_err(|err| {
                std::io::Error::new(std::io::ErrorKind::InvalidData, format!("{:?}", err))
            })
    }
}

