//! With the help of an underlying `TokenReader`, decode a stream of bytes
//! to a JSON matching a specific grammar.

use ast::grammar::*;
use token::io::*;

use json;
use json::JsonValue as JSON;
use json::object::Object as Object;

use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug)]
pub enum Error<E> where E: Debug {
    UnexpectedValue(String),
    TokenReaderError(E),
    NoSuchInterface(String),
    NoSuchRefinement(String),
    NoSuchKind(String),
    NoSuchField(String),
    NoSuchType(String),
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
    latest: JSON,
}

impl<'a, E> Decoder<'a, E> where E: TokenReader {
    pub fn new(grammar: &'a Syntax, extractor: E) -> Self {
        Decoder {
            extractor,
            grammar,
            latest: JSON::Null
        }
    }
    fn register(&mut self, value: JSON) -> JSON {
        self.latest = value.clone();
        value
    }
    fn raise_error(&mut self, error: Error<E::Error>) -> Error<E::Error> {
        self.extractor.poison();
        debug!(target: "decode", "Raising error {:?}", error);
        error
    }
    pub fn latest(&self) -> &JSON {
        &self.latest
    }

    pub fn decode(&mut self) -> Result<JSON, Error<E::Error>> {
        let start = self.grammar.get_root();
        self.decode_from_named_type(&start)
    }
    fn decode_from_named_type(&mut self, named: &NamedType) -> Result<JSON, Error<E::Error>> {
        match *named {
            NamedType::Typedef(ref type_) =>
                return self.decode_from_type(type_),
            NamedType::StringEnum(ref enum_) => {
                let string = self.extractor.string()
                    .map_err(Error::TokenReaderError)?
                    .ok_or_else(|| self.raise_error(Error::UnexpectedValue("null string".to_owned())))?;
                for candidate in enum_.strings() {
                    if candidate == &string {
                        return Ok(self.register(json::from(string)));
                    }
                }
                return Err(self.raise_error(Error::UnexpectedValue(format!("\"{}\"", string))))
            }
            NamedType::Interface(ref interface) => {
                // 1. Get the the interface.
                let (object_name, mapped_field_names, guard) = self.extractor.tagged_tuple()
                    .map_err(Error::TokenReaderError)?;
                debug!("decoder: found kind {:?}", object_name);

                // 2. If necessary, substitute null to any interface.
                // FIXME: Check above that `null` is acceptable.
                if object_name == self.grammar.get_null_name().to_str() {
                    guard.done()
                        .map_err(Error::TokenReaderError)?;
                    return Ok(self.register(JSON::Null))
                }

                // 3. Check that the object is appropriate here.
                if object_name != interface.name().to_str() {
                    return Err(self.raise_error(Error::UnexpectedValue(format!("Object named {} instead of {}",
                        object_name,
                        interface.name().to_str()))));
                }

                // 4. Parse within interface.
                self.decode_object_contents(interface, mapped_field_names, guard)
            }
        }
    }
    pub fn decode_object_contents(&mut self, interface: &Interface, mapped_field_names: Rc<Box<[Field]>>, guard: E::TaggedGuard) -> Result<JSON, Error<E::Error>> {
        debug!(target: "decode", "decode_object_contents: Interface {:?} ", interface.name());
        // Determine all the fields that we were expecting.
        let mut expected: HashMap<_,_> = interface.contents()
            .fields()
            .iter()
            .map(|field| {
                (field.name().clone(), field.type_())
            })
            .collect();
        debug!(target: "decode", "decode_object_contents: Expecting fields {:?} ", expected);

        // Read the fields **in the order** in which they appear in the stream.
        let mut object = Object::new();
        for field in mapped_field_names.as_ref().iter() {
            debug!(target: "decode", "decode_object_contents: Looking at field {:?} ", field.name().to_str());
            let item = self.decode_from_type(field.type_())?;
            let name = field.name().to_str();
            if expected.remove(field.name()).is_none() {
                debug!(target: "decode", "decode_object_contents: I didn't expect field {:?}.", field.name().to_str());
                self.extractor.poison();
                return Err(self.raise_error(Error::NoSuchField(name.to_string())))
            }
            object.insert(name, item);
        }
        debug!(target: "decode", "decode_object_contents: Remaining fields {:?} ", expected);

        // Any field missing? Find out if there is a default value.
        for (name, type_) in expected.drain() {
            let name = name.to_str();
            if type_.is_optional() {
                object.insert(name, JSON::Null);
            } else {
                self.extractor.poison();
                return Err(self.raise_error(Error::MissingField {
                    name: name.to_string(),
                    kind: interface.name().to_string().clone()
                }))
            }
        }

        // Don't forget `"type"`.
        debug!(target: "decode", "decode_object_contents: Adding type");
        object.insert("type", json::from(interface.name().to_str()));
        guard.done()
            .map_err(Error::TokenReaderError)?;

        Ok(self.register(JSON::Object(object)))
    }
    pub fn decode_from_type(&mut self, kind: &Type) -> Result<JSON, Error<E::Error>> {
        use ast::grammar::TypeSpec::*;
        debug!("decode: {:?}", kind);
        match *kind.spec() {
            Array { contents: ref kind, supports_empty } => {
                let (len, guard) = self.extractor.list()
                    .map_err(Error::TokenReaderError)?;
                if len == 0 && !supports_empty {
                    return Err(self.raise_error(Error::InvalidValue("Empty list".to_string())));
                }
                let mut values = Vec::with_capacity(len as usize);
                for _ in 0..len {
                    values.push(self.decode_from_type(kind)?);
                }
                guard.done()
                    .map_err(Error::TokenReaderError)?;
                Ok(self.register(JSON::Array(values)))
            }
            String => {
                let extracted = self.extractor.string()
                    .map_err(Error::TokenReaderError)?;
                match extracted {
                    None if kind.is_optional() =>
                        Ok(self.register(JSON::Null)),
                    None =>
                        Err(self.raise_error(Error::UnexpectedValue("null string".to_owned()))),
                    Some(string) =>
                        Ok(self.register(json::from(string))),
                }
            }
            Boolean => {
                let extracted = self.extractor.bool()
                    .map_err(Error::TokenReaderError)?;
                match extracted {
                    None =>
                        Err(self.raise_error(Error::UnexpectedValue("null bool".to_owned()))),
                    Some(b) =>
                        Ok(self.register(json::from(b)))
                }
            }
            Number  => {
                let extracted = self.extractor.float()
                    .map_err(Error::TokenReaderError)?;
                match extracted {
                    None =>
                        Err(self.raise_error(Error::UnexpectedValue("null float".to_owned()))),
                    Some(f) =>
                        Ok(self.register(json::from(f)))
                }
            }
            NamedType(ref name) => {
                let named_type = self.grammar.get_type_by_name(name)
                    .ok_or_else(|| self.raise_error(Error::NoSuchType(name.to_string().clone())))?;
                let result = self.decode_from_named_type(&named_type)?;
                if let JSON::Null = result {
                    if !kind.is_optional() {
                        return Err(self.raise_error(Error::UnexpectedValue("null".to_string())));
                    }
                }
                Ok(result)
            }
            TypeSum(ref sum) => {
                // The `sum` is necessarily a sum of interfaces, so this must be an object.
                // 1. Get the the interface.
                let (interface_name, mapped_field_names, guard) = self.extractor.tagged_tuple()
                    .map_err(Error::TokenReaderError)?;
                debug!("decoder: found kind {:?}", interface_name);
                let interface_node_name = self.grammar.get_node_name(&interface_name)
                    .ok_or_else(|| Error::NoSuchInterface(interface_name.to_string().clone()))?;

                if interface_node_name == self.grammar.get_null_name() {
                    if kind.is_optional() {
                        guard.done()
                            .map_err(Error::TokenReaderError)?;
                        return Ok(self.register(JSON::Null))
                    }
                }
                let interface = self.grammar.get_interface_by_name(&interface_node_name)
                    .ok_or_else(|| self.raise_error(Error::NoSuchInterface(interface_name.to_string().clone())))?;

                // 2. Check that the interface somehow belongs in `sum`
                if sum.types().iter()
                    .find(|type_| {
                        type_.get_interface(self.grammar, interface_node_name)
                            .is_some()
                    }).is_none()
                {
                    return Err(self.raise_error(Error::UnexpectedValue(format!("Unexpected interface {interface} doesn't fit in sum {sum:?}",
                        interface = interface_name,
                        sum = sum.types()))));
                }

                // 3. Parse within interface.
                self.decode_object_contents(interface, mapped_field_names, guard)
            }
            Void => Ok(self.register(JSON::Null))
        }
    }
}