//! With the help of an underlying `Extractor`, decode a stream of bytes
//! to a JSON matching a specific grammar.

use estree::grammar::*;
use estree::io::*;

use serde_json::Value;

use std;

type Object = std::collections::BTreeMap<String, Value>;

pub enum Error {
    UnexpectedValue(String),
    ExtractorError, // FIXME: Improve this
    NoSuchInterface(String),
    NoSuchRefinement(String),
    NoSuchTag(String),
    NoSuchField(String),
    InvalidValue(String),
}

pub struct Decoder<'a, E> where E: Extractor {
    extractor: E,
    grammar: &'a Syntax,
}

impl<'a, E> Decoder<'a, E> where E: Extractor {
    pub fn new(extractor: E, grammar: &'a Syntax) -> Self {
        Decoder {
            extractor,
            grammar
        }
    }
    pub fn decode(&mut self, kind: &Type) -> Result<Value, Error> {
        use estree::grammar::Type::*;
        match *kind {
            Array(ref kind) => {
                let (len, mut extractor) = self.extractor.list()
                    .map_err(|_| Error::ExtractorError)?;
                let mut decoder = Decoder::new(extractor, self.grammar);
                let mut values = Vec::with_capacity(len as usize);
                for _ in 0..len {
                    values.push(decoder.decode(kind)?);
                }
                Ok(Value::Array(values))
            }
            Structure(ref structure) => {
                // At this stage, since there is no inheritance involved, use the built-in mapping.
                let mut object = Object::new();
                for field in structure.fields() {
                    let item = self.decode(field.type_())?;
                    object.insert(field.name().clone(), item);
                }
                Ok(Value::Object(object))
            }
            String => {
                let string = self.extractor.string()
                    .map_err(|_| Error::ExtractorError)?
                    .ok_or_else(|| Error::UnexpectedValue("(no string)".to_string()))?;
                Ok(Value::String(string))
            }
            Enum(ref enum_) => {
                use estree::grammar::Enum;
                let Enum {ref strings, or_null} = *enum_;
                let string = self.extractor.string()
                    .map_err(|_| Error::ExtractorError)?;
                match string {
                    None if or_null => Ok(Value::Null),
                    None => Err(Error::UnexpectedValue("(no string)".to_string())),
                    Some(s) => {
                        for candidate in strings {
                            if candidate == &s {
                                return Ok(Value::String(s))
                            }
                        }
                        Err(Error::UnexpectedValue(s))
                    }
                }
            }
            Interfaces(ref interfaces) => {
                let (kind, mapped_field_names) = self.extractor.tag()
                    .map_err(|_| Error::ExtractorError)?;
                let mut found = None;
                for name in interfaces {
                     let interface = self.grammar.get_interface(&name)
                         .ok_or_else(|| Error::NoSuchInterface(name.to_string().clone()))?;
                     if let Some(_) = interface.get_refinement(&kind) {
                         found = Some(name);
                     }
                }
                let found = found
                    .ok_or_else(|| Error::NoSuchTag(kind.to_string().clone()))?;
                let interface = self.grammar.get_interface(&found)
                    .unwrap();
                let refinement = interface
                    .get_refinement(&kind)
                    .unwrap();
                let structure = refinement.contents();

                // Read the fields **in the order** in which they appear in the stream.
                let mut object = Object::new();
                for field_name in mapped_field_names {
                    let field = structure.field(&field_name)
                        .ok_or_else(|| Error::NoSuchField(field_name.clone()))?;
                    let item = self.decode(field.type_())?;
                    object.insert(field_name, item);
                }
                Ok(Value::Object(object))
            }
            Boolean => {
                let value = self.extractor.bool()
                    .map_err(|_| Error::InvalidValue("bool".to_string()))?;
                Ok(Value::Bool(value))
            }
            Number => {
                let value = self.extractor.float()
                    .map_err(|_| Error::InvalidValue("float".to_string()))?;
                Ok(Value::F64(value))
            }
        }
    }
}