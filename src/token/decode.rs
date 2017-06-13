//! With the help of an underlying `TokenReader`, decode a stream of bytes
//! to a JSON matching a specific grammar.

use ast::grammar::*;
use token::io::*;

use serde_json;
use serde_json::Value;

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
        let start = self.grammar.get_start();
        let kind = Type::interfaces(&[start.name()]);
        self.decode_from_type(&kind)
    }
    pub fn decode_from_type(&mut self, kind: &Type) -> Result<Value, Error<E::Error>> {
        use ast::grammar::Type::*;
        debug!("decode: {:?}", kind);
        match *kind {
            Array(ref kind) => {
                let (len, extractor) = self.extractor.list()
                    .map_err(Error::TokenReaderError)?;
                let mut decoder = Decoder::new(self.grammar, extractor);
                let mut values = Vec::with_capacity(len as usize);
                for _ in 0..len {
                    values.push(decoder.decode_from_type(kind)?);
                }
                Ok(self.register(Value::Array(values)))
            }
            String => {
                let string = self.extractor.string()
                    .map_err(Error::TokenReaderError)?;
                Ok(self.register(Value::String(string)))
            }
            Enum(ref name) => {
                // FIXME: Do we really need to check this here?
                let enum_ = self.grammar.get_enum_by_name(&name)
                    .ok_or_else(|| Error::NoSuchEnum(name.to_string().clone()))?;
                let string = self.extractor.string()
                    .map_err(Error::TokenReaderError)?;
                for candidate in enum_.strings() {
                    if candidate == &string {
                        return Ok(self.register(Value::String(string)))
                    }
                }
                Err(Error::UnexpectedValue(string))
            }
            Interfaces {
                names: ref interfaces,
                or_null
            } => {
                let (kind_name, mapped_field_names, extractor) = self.extractor.tagged_tuple()
                    .map_err(Error::TokenReaderError)?;
                debug!("decoder: found kind {:?}", kind_name);

                // Special case: `null`.
                if or_null && kind_name.to_string() == "Null" {
                    assert_eq!(mapped_field_names.len(), 0);
                    return Ok(self.register(Value::Null));
                }

                // We have a kind, so we know how to parse the data. We just need
                // to make sure that we expected this interface here.
                let kind = self.grammar.get_kind(&kind_name)
                    .ok_or_else(|| Error::NoSuchKind(kind_name))?;

                if let Some(interface) = self.grammar.get_interface_by_kind(&kind) {
                    if !self.grammar.has_ancestor_in(interface, interfaces) {
                        return Err(Error::NoSuchRefinement(kind.to_string().clone()))
                    }

                    // Read the fields **in the order** in which they appear in the stream.
                    let mut decoder = Decoder::new(self.grammar, extractor);
                    let mut object = Object::new();
                    for field in mapped_field_names.as_ref().iter() {
                        let item = decoder.decode_from_type(field.type_())?;
                        object.insert(field.name().to_string().clone(), item);
                    }

                    // Don't forget `"type"`.
                    object.insert("type".to_owned(), Value::String(kind.to_string().clone()));
                    Ok(self.register(Value::Object(object)))
                } else {
                    Err(Error::NoSuchKind(kind.to_string().clone()))
                }
            }
            Boolean => {
                let value = self.extractor.bool()
                    .map_err(|_| Error::InvalidValue("bool".to_string()))?;
                Ok(self.register(Value::Bool(value)))
            }
            Number => {
                let value = self.extractor.float()
                    .map_err(|_| Error::InvalidValue("float".to_string()))?;
                Ok(self.register(json!(value)))
            }
        }
    }
}