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
    NoSuchEnum(String),
    NoSuchKind(String),
    MissingField(String),
    NoSuchLiteral { strings: Vec<String> },
    TokenWriterError(E),
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
        let start = self.grammar.get_root();
        let kind = Type::interfaces(&[start.name()]).close();
        self.encode_from_type(value, &kind, start.name())
    }
    pub fn encode_from_type(&self, value: &JSON, kind: &Type, node: &NodeName) -> Result<Tree, Error<E>> {
        debug!("encode: value {:?} with kind {:?}", value, kind);
        use ast::grammar::TypeSpec::*;
        match (kind.spec(), value) {
            (&Array(ref kind), &JSON::Array(ref array)) => {
                let mut encoded = Vec::new();
                for item in array {
                    let item = self.encode_from_type(item, kind, node)?;
                    encoded.push(item);
                }
                let result = self.builder.borrow_mut().list(encoded)
                    .map_err(Error::TokenWriterError)?;
                Ok(result)
            }
            (&Enum(ref name), _) => {
                let enum_ = self.grammar.get_enum_by_name(&name)
                    .ok_or_else(|| Error::NoSuchEnum(name.to_string().clone()))?;
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
           (&Interfaces(ref interfaces), &JSON::Object(ref object)) => {
               let type_field = object.get("type")
                   .ok_or_else(|| Error::missing_field("type", node))?;
               let kind_name = type_field
                   .as_str()
                   .ok_or_else(|| Error::Mismatch {
                       expected: "type name (as String)".to_string(),
                       got: type_of(type_field)
                   })?;
               let kind = self.grammar.get_kind(kind_name)
                   .ok_or_else(|| Error::NoSuchKind(kind_name.to_string().clone()))?;

               // We have a kind, so we know how to encode the data. We just need
               // to make sure that we expected this interface here.
               if let Some(interface) = self.grammar.get_interface_by_kind(&kind) {
                   if !self.grammar.has_ancestor_in(interface, interfaces) {
                       return Err(Error::NoSuchRefinement {
                           expected: kind.to_string().clone(),
                           got: interfaces.iter().map(NodeName::to_string).cloned().collect()
                       });
                   }
                   let fields = interface.contents().fields();
                   let contents = self.encode_structure(object, fields, interface.name())?;
                   // Write the contents with the tag of the refined interface.
                   let labelled = self.builder
                       .borrow_mut()
                       .tagged_tuple(&kind.to_string(), &contents)
                       .map_err(Error::TokenWriterError)?;
                   return Ok(labelled)
               }
               return Err(Error::NoSuchKind(kind.to_string().clone()));
           }
           (&Boolean, &JSON::Boolean(b)) => {
                self.builder.borrow_mut().bool(Some(b))
                    .map_err(Error::TokenWriterError)
           }
           (&String, _) => {
               match value.as_str() {
                   Some(s) =>self.builder.borrow_mut().string(Some(s))
                        .map_err(Error::TokenWriterError),
                   None =>
                       Err(Error::Mismatch {
                           expected: "string".to_string(),
                           got: type_of(&value)
                       })
               }
           }
           (&Number, &JSON::Number(x)) =>
                self.builder.borrow_mut().float(Some(x.into()))
                    .map_err(Error::TokenWriterError),
            _ => Err(Error::Mismatch {
                expected: format!("{:?}", kind.spec()),
                got: type_of(&value)
            })
        }
    }

    fn encode_structure<'b>(&self, object: &'b Object, fields: &'b [Field], node: &NodeName) -> Result<Vec<(&'b Field, B::Tree)>, Error<E>> {
        let mut result = Vec::with_capacity(fields.len());
        'fields: for field in fields {
            if let Some(source) = object.get(field.name().to_string()) {
                if let Some(default) = field.type_().default() {
                    if default == source {
                        // Nothing to do, this is the default value.
                        continue 'fields;
                    }
                }
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
