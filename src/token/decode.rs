//! With the help of an underlying `TokenReader`, decode a stream of bytes
//! to a JSON matching a specific grammar.

use ast::grammar::*;
use token::io::*;

use serde_json;
use serde_json::Value;

use std::collections::HashMap;
use std::fmt::Debug;

type Object = serde_json::Map<String, Value>;

#[derive(Debug)]
pub enum Error<E> where E: Debug {
    UnexpectedValue(String),
    TokenReaderError(E),
    NoSuchInterface(String),
    NoSuchRefinement(String),
    NoSuchKind(String),
    NoSuchField(String),
    NoSuchEnum(String),
    InvalidValue(String),
    MissingField {
        name: String,
        kind: String
    },
}

pub struct Decoder<'a, E> where E: TokenReader {
    extractor: E,
    grammar: &'a Syntax,

    /// Latest value decoded. Used for debugging/troubleshooting.
    latest: Value,
}

impl<'a, E> Decoder<'a, E> where E: TokenReader {
    pub fn new(grammar: &'a Syntax, extractor: E) -> Self {
        Decoder {
            extractor,
            grammar,
            latest: Value::Null
        }
    }
    fn register(&mut self, value: Value) -> Value {
        self.latest = value.clone();
        value
    }

    pub fn latest(&self) -> &Value {
        &self.latest
    }

    pub fn decode(&mut self) -> Result<Value, Error<E::Error>> {
        let start = self.grammar.get_root();
        let kind = Type::interfaces(&[start.name()]).close();
        self.decode_from_type(&kind)
    }
    pub fn decode_from_type(&mut self, kind: &Type) -> Result<Value, Error<E::Error>> {
        use ast::grammar::TypeSpec::*;
        debug!("decode: {:?}", kind);
        match *kind.spec() {
            Array(ref kind) => {
                let (len, guard) = self.extractor.list()
                    .map_err(Error::TokenReaderError)?;
                let mut values = Vec::with_capacity(len as usize);
                for _ in 0..len {
                    values.push(self.decode_from_type(kind)?);
                }
                guard.done()
                    .map_err(Error::TokenReaderError)?;
                Ok(self.register(Value::Array(values)))
            }
            Enum(ref name) => {
                // FIXME: Do we really need to check this here?
                let enum_ = self.grammar.get_enum_by_name(&name)
                    .ok_or_else(|| Error::NoSuchEnum(name.to_string().clone()))?;
                let string = self.extractor.string()
                    .map_err(Error::TokenReaderError)?
                    .ok_or_else(|| Error::UnexpectedValue("null string".to_owned()))?;
                for candidate in enum_.strings() {
                    if candidate == &string {
                        return Ok(self.register(Value::String(string)))
                    }
                }
                Err(Error::UnexpectedValue(string))
            }
            Interfaces(ref names) => {
                let (kind_name, mapped_field_names, guard) = self.extractor.tagged_tuple()
                    .map_err(Error::TokenReaderError)?;
                debug!("decoder: found kind {:?}", kind_name);

                // We have a kind, so we know how to parse the data. We just need
                // to make sure that we expected this interface here.
                let kind = self.grammar.get_kind(&kind_name)
                    .ok_or_else(|| {
                        self.extractor.poison();
                        Error::NoSuchKind(kind_name)
                    })?;

                if let Some(interface) = self.grammar.get_interface_by_kind(&kind) {
                    if !self.grammar.has_ancestor_in(interface, names) {
                        self.extractor.poison();
                        return Err(Error::NoSuchRefinement(kind.to_string().clone()))
                    }

                    // Determine all the fields that we were expecting.
                    let mut expected: HashMap<_,_> = interface.contents()
                        .fields()
                        .iter()
                        .map(|field| {
                            (field.name().clone(), field.type_())
                        })
                        .collect();

                    // Read the fields **in the order** in which they appear in the stream.
                    let mut object = Object::new();
                    for field in mapped_field_names.as_ref().iter() {
                        let item = self.decode_from_type(field.type_())?;
                        let name = field.name().to_string().clone();
                        if expected.remove(field.name()).is_none() {
                            return Err(Error::NoSuchField(name))
                        }
                        object.insert(name, item);
                    }

                    // Any field missing? Find out if there is a default value.
                    for (name, type_) in expected.drain() {
                        let name = name.to_string().clone();
                        match type_.default() {
                            None => return Err(Error::MissingField {
                                name,
                                kind: kind.to_string().clone()
                            }),
                            Some(default) => {
                                object.insert(name, default.clone());
                            }
                        }
                    }

                    // Don't forget `"type"`.
                    object.insert("type".to_owned(), Value::String(kind.to_string().clone()));
                    guard.done()
                        .map_err(Error::TokenReaderError)?;

                    Ok(self.register(Value::Object(object)))
                } else {
                    Err(Error::NoSuchKind(kind.to_string().clone()))
                }
            }
            String => {
                let extracted = self.extractor.string()
                    .map_err(Error::TokenReaderError)?;
                match extracted {
                    None =>
                        Err(Error::UnexpectedValue("null string".to_owned())),
                    Some(string) =>
                        Ok(self.register(Value::String(string)))
                }
            }
            Boolean => {
                let extracted = self.extractor.bool()
                    .map_err(Error::TokenReaderError)?;
                match extracted {
                    None =>
                        Err(Error::UnexpectedValue("null bool".to_owned())),
                    Some(b) =>
                        Ok(self.register(Value::Bool(b)))
                }
            }
            Number  => {
                let extracted = self.extractor.float()
                    .map_err(Error::TokenReaderError)?;
                match extracted {
                    None =>
                        Err(Error::UnexpectedValue("null float".to_owned())),
                    Some(f) =>
                        Ok(self.register(Value::from(f)))
                }
            }
        }
    }
}