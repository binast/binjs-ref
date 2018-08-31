//! With the help of an underlying `TokenReader`, decode a stream of bytes
//! to a JSON matching a specific grammar.

use binjs_io::{ Guard, TokenReader };
use binjs_meta::spec::*;

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
    grammar: &'a Spec,

    /// Latest value decoded. Used for debugging/troubleshooting.
    latest: JSON,
}

impl<'a, E> Decoder<'a, E> where E: TokenReader {
    pub fn new(grammar: &'a Spec, extractor: E) -> Self {
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
        self.decode_from_named_type(&start, false)
    }
    fn decode_from_named_type(&mut self, named: &NamedType, is_optional: bool) -> Result<JSON, Error<E::Error>> {
        match *named {
            NamedType::Typedef(ref type_) =>
                return self.decode_from_type(type_, is_optional),
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
                debug!(target: "decoder", "decoder: found kind {:?} while looking for {:?}", object_name, interface.name().to_str());

                // 2. If necessary, substitute null to any interface.
                // FIXME: Check above that `null` is acceptable.
                if object_name == self.grammar.get_null_name().to_str() {
                    debug!(target: "decoder", "decoder: substituted null => {}", interface.name().to_str());
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
    pub fn decode_object_contents(&mut self, interface: &Interface, field_names: Option<Rc<Box<[String]>>>, guard: E::TaggedGuard) -> Result<JSON, Error<E::Error>> {
        debug!(target: "decode", "decode_object_contents: Interface {:?} ", interface.name());
        let mut object = Object::new();

        if let Some(field_names) = field_names {
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
            for field in field_names.as_ref().iter() {
                debug!(target: "decode", "decode_object_contents: Looking at field {:?} ", field);
                let field_name = self.grammar.get_field_name(field)
                    .ok_or_else(|| self.raise_error(Error::NoSuchField(field.clone())))?;
                let type_ =
                    if let Some(type_) = expected.remove(field_name) {
                        type_
                    } else {
                        debug!(target: "decode", "decode_object_contents: I didn't expect field {:?}.", field);
                        self.extractor.poison();
                        return Err(self.raise_error(Error::NoSuchField(field.clone())))
                    };
                let item = self.decode_from_type(type_, false)?;
                object.insert(field, item);
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

        } else {
            // Read the fields **in the order** in which they appear in the spec.
            for field in interface.contents().fields() {
                debug!(target: "decode", "decode_object_contents: Looking at field {:?} with type {:?}",
                    field.name(),
                    field.type_());
                let item = self.decode_from_type(field.type_(), false)?;
                object.insert(field.name().to_str(), item);
            }
        }


        // Don't forget `"type"`.
        debug!(target: "decode", "decode_object_contents: Adding type");
        object.insert("type", json::from(interface.name().to_str()));

        guard.done()
            .map_err(Error::TokenReaderError)?;

        Ok(self.register(JSON::Object(object)))
    }
    pub fn decode_from_type(&mut self, kind: &Type, is_optional: bool) -> Result<JSON, Error<E::Error>> {
        use binjs_meta::spec::TypeSpec::*;
        debug!(target: "decoder", "decode: {:?}", kind);
        let is_optional = kind.is_optional() || is_optional;
        match *kind.spec() {
            Array { contents: ref kind, supports_empty } => {
                let (len, guard) = self.extractor.list()
                    .map_err(Error::TokenReaderError)?;
                if len == 0 && !supports_empty {
                    return Err(self.raise_error(Error::InvalidValue("Empty list".to_string())));
                }
                let mut values = Vec::with_capacity(len as usize);
                for _ in 0..len {
                    values.push(self.decode_from_type(kind, false)?);
                }
                guard.done()
                    .map_err(Error::TokenReaderError)?;
                Ok(self.register(JSON::Array(values)))
            }
            String => {
                let extracted = self.extractor.string()
                    .map_err(Error::TokenReaderError)?;
                match extracted {
                    None if is_optional =>
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
            Offset => {
                let offset = self.extractor.offset()
                    .map_err(Error::TokenReaderError)?;
                Ok(self.register(json::from(offset)))
            }
            Number => {
                let extracted = self.extractor.float()
                    .map_err(Error::TokenReaderError)?;
                match extracted {
                    None =>
                        Err(self.raise_error(Error::UnexpectedValue("null float".to_owned()))),
                    Some(f) =>
                        Ok(self.register(json::from(f)))
                }
            }
            UnsignedLong => {
                let extracted = self.extractor.unsigned_long()
                    .map_err(Error::TokenReaderError)?;
                Ok(self.register(json::from(extracted)))
            }
            NamedType(ref name) => {
                let named_type = self.grammar.get_type_by_name(name)
                    .ok_or_else(|| self.raise_error(Error::NoSuchType(name.to_string().clone())))?;
                let result = self.decode_from_named_type(&named_type, is_optional)?;
                if let JSON::Null = result {
                    if !is_optional {
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
                debug!(target: "decoder", "decoder: found kind {:?}", interface_name);
                let interface_node_name = self.grammar.get_node_name(&interface_name)
                    .ok_or_else(|| Error::NoSuchInterface(interface_name.to_string().clone()))?;

                if interface_node_name == self.grammar.get_null_name() {
                    if is_optional {
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
