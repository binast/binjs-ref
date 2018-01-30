use ast::grammar::*;
use token::io::*;
use util::type_of;

use std;
use std::cell::*;
use std::fmt::Debug;

use json::JsonValue as JSON;
use json::object::Object as Object;

#[derive(Debug)]
pub enum Error<E> {
    Mismatch { expected: String, got: String },
    NoSuchInterface(String),
    NoSuchRefinement { expected: String, got: Vec<String> },
    NoSuchKind(String),
    NoSuchType(String),
    MissingField(String),
    NoSuchLiteral { strings: Vec<String> },
    TokenWriterError(E),
    NonNullableType(String),
}

impl<E> From<E> for Error<E> {
    fn from(value: E) -> Self {
        Error::TokenWriterError(value)
    }
}

impl<E> Error<E> {
    fn missing_field(field: &str, node: &NodeName) -> Self {
        Error::MissingField(format!("Field \"{}\" while handling {:?}", field, node))
    }
}

pub trait Encode {
    type Data;
    type Statistics;
    fn encode(&self, value: &JSON) -> Result<(), std::io::Error>;
    fn done(self) -> Result<(Self::Data, Self::Statistics), std::io::Error>;
}

pub struct Encoder<'a, B, Tree, E> where B: TokenWriter<Tree=Tree, Error=E>, E: Debug {
    grammar: &'a Syntax,
    builder: RefCell<B>,
}
impl<'a, B, Tree, E> Encoder<'a, B, Tree, E> where B: TokenWriter<Tree=Tree, Error=E>, E: Debug {
    pub fn new(syntax: &'a Syntax, builder: B) -> Self {
        Encoder {
            grammar: syntax,
            builder: RefCell::new(builder),
        }
    }

    /// Encode a JSON into a SerializeTree based on a grammar.
    /// This step doesn't perform any interesting check on the JSON.
    pub fn encode(&self, value: &JSON) -> Result<Tree, Error<E>> {
        let root = self.grammar.get_root();
        let node = self.grammar.get_root_name();
        self.encode_from_named_type(value, &root, &node, false)
    }
    pub fn encode_from_named_type(&self, value: &JSON, named: &NamedType, node: &NodeName, is_optional: bool) -> Result<Tree, Error<E>> {
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
                    .is_some() {
                    return self.builder.borrow_mut().string(Some(&string))
                        .map_err(Error::TokenWriterError)
                }
                Err(Error::NoSuchLiteral {
                    strings: enum_.strings().iter().cloned().collect(),
                })
            }
            NamedType::Typedef(ref type_) =>
                return self.encode_from_type(value, type_, node),
            NamedType::Interface(ref interface) => {
                debug!(target:"encode", "Attempting to encode value {:?} with interface {:?}", value, interface.name());
                match *value {
                    JSON::Object(ref object) => {
                        debug!(target:"encode", "Attempting to encode object {:?} with interface {:?}", value["type"].as_str(), interface.name());
                        let interface_name = interface.name()
                            .to_string();
                        let type_field = object.get("type")
                            .ok_or_else(|| Error::missing_field("type", node))?
                            .as_str()
                            .ok_or_else(|| Error::missing_field("type", node))?;
                        if interface_name == type_field {
                            let fields = interface.contents().fields();
                            let contents = self.encode_structure(object, fields, interface.name())?;
                            // Write the contents with the tag of the refined interface.
                            let labelled = self.builder
                                .borrow_mut()
                                .tagged_tuple(&type_field, &contents)
                                .map_err(Error::TokenWriterError)?;
                            return Ok(labelled)
                        } else {
                            return Err(Error::Mismatch {
                                expected: interface_name.clone(),
                                got: type_field.to_string()
                            })
                        }
                    }
                    JSON::Null if is_optional => {
                        // Write the contents with the tag of the refined interface.
                        let labelled = self.builder
                            .borrow_mut()
                            .tagged_tuple(self.grammar.get_null_name().to_str(), &[])
                            .map_err(Error::TokenWriterError)?;
                        return Ok(labelled)
                    }
                    _ => {
                        return Err(Error::Mismatch {
                            expected: node.to_string().clone(),
                            got: value.dump()
                        })
                    }
                }
            }
        }
    }
    pub fn encode_from_type(&self, value: &JSON, type_: &Type, node: &NodeName) -> Result<Tree, Error<E>> {
        debug!(target:"encode", "value {:?} within node {:?}, type {:?}", value, node, type_);
        self.encode_from_type_spec(value, type_.spec(), node, type_.is_optional())
    }
    fn encode_from_type_spec(&self, value: &JSON, spec: &TypeSpec, node: &NodeName, is_optional: bool) -> Result<Tree, Error<E>> {
        use ast::grammar::TypeSpec::*;
        match (spec, value) {
            (&Array { contents: ref kind, .. }, &JSON::Array(ref array)) => {
                let mut encoded = Vec::new();
                for item in array {
                    let item = self.encode_from_type(item, kind, node)?;
                    encoded.push(item);
                }
                let result = self.builder.borrow_mut().list(encoded)
                    .map_err(Error::TokenWriterError)?;
                return Ok(result)
            }
            (&Boolean, &JSON::Boolean(b)) => {
                return self.builder.borrow_mut().bool(Some(b))
                    .map_err(Error::TokenWriterError)
            }
            (&String, _) if value.is_string() => {
                let string = value.as_str().unwrap(); // Checked just above.
                return self.builder.borrow_mut().string(Some(string))
                    .map_err(Error::TokenWriterError)
            }
            (&Number, &JSON::Number(x)) =>
                return self.builder.borrow_mut().float(Some(x.into()))
                    .map_err(Error::TokenWriterError),
            (&NamedType(ref name), _) => {
                let named_type = self.grammar.get_type_by_name(name)
                    .ok_or_else(|| Error::NoSuchType(name.to_string().clone()))?;
                return self.encode_from_named_type(value, &named_type, node, is_optional)
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
                return self.encode_from_named_type(value, &named_type, node, is_optional)
            }
            (&TypeSum(_), &JSON::Null) if is_optional => {
                // The `Null` interface can be substituted.
                let null_name = self.grammar.get_null_name();
                let null_type = self.grammar.get_type_by_name(self.grammar.get_null_name())
                    .unwrap_or_else(|| panic!("Could not find type named {}", null_name.to_str()));
                return self.encode_from_named_type(value, &null_type, null_name, /* is_optional = */ true)
            }
            _ => {}
        }
        Err(Error::Mismatch {
            expected: format!("{:?}{is_optional}", spec, is_optional = if is_optional { " (optional)"} else { " (required)" }),
            got: value.dump()
        })
    }
    fn encode_structure<'b>(&self, object: &'b Object, fields: &'b [Field], node: &NodeName) -> Result<Vec<(&'b Field, B::Tree)>, Error<E>> {
        let mut result = Vec::with_capacity(fields.len());
        'fields: for field in fields {
            if let Some(source) = object.get(field.name().to_string()) {
                let encoded = self.encode_from_type(source, field.type_(), node)?;
                result.push((field, encoded))
            } else {
                debug!("Error in {:?}", object);
                return Err(Error::missing_field(field.name().to_string(), node));
            }
        }
        Ok(result)
    }
}


impl<'a, B, Tree, E> Encode for Encoder<'a, B, Tree, E> where B: TokenWriter<Tree=Tree, Error=E>, E: Debug {
    type Data = B::Data;
    type Statistics = B::Statistics;
    fn encode(&self, value: &JSON) -> Result<(), std::io::Error> {
        (self as &Encoder<'a, B, Tree, E>).encode(value)
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
