use estree::grammar::*;
use estree::io::*;

use std;
use std::cell::*;
use std::ops::Deref;

use serde_json::Value;
type Object = std::collections::BTreeMap<String, Value>;

#[derive(Debug)]
pub enum Error<E> {
    Mismatch(String),
    NoSuchInterface(String),
    NoSuchRefinement(String),
    NoSuchKind(String),
    MissingField(String),
    NoSuchLiteral {strings: Vec<String>, or_null: bool},
    BuilderError(E),
}

impl<E> From<E> for Error<E> {
    fn from(value: E) -> Self {
        Error::BuilderError(value)
    }
}


pub struct Encoder<B, Tree, E> where B: Builder<Tree=Tree, Error=E> {
    grammar: Syntax,
    builder: RefCell<B>,
}
impl<B, Tree, E> Encoder<B, Tree, E> where B: Builder<Tree=Tree, Error=E> {
    pub fn new(syntax: Syntax, builder: B) -> Self {
        Encoder {
            grammar: syntax,
            builder: RefCell::new(builder),
        }
    }

    /// Encode a JSON into a SerializeTree based on a grammar.
    /// This step doesn't perform any interesting check on the JSON.
    pub fn encode(&self, value: &Value, kind: &Type) -> Result<Tree, Error<E>> {
        use estree::grammar::Type::*;
        match *kind {
            Array(ref kind) => {
                let list = value.as_array()
                    .ok_or_else(|| Error::Mismatch("Array".to_string()))?;
                let mut encoded = Vec::with_capacity(list.len());
                for item in list {
                    let item = self.encode(item, kind)?;
                    encoded.push(item);
                }
                let result = self.builder.borrow_mut().list(encoded)
                    .map_err(Error::BuilderError)?;
                Ok(result)
            }
            Obj(ref structure) => {
                let object = value.as_object()
                    .ok_or_else(|| Error::Mismatch("Object".to_string()))?;
                let contents = self.encode_structure(object, structure.fields())?;
                // This is an anonymous structure, so we expect that the order
                // of fields has been specified elsewhere.
                let result = self.builder.borrow_mut().tuple(contents, None)
                    .map_err(Error::BuilderError)?;
                Ok(result)
            }
            Enum(ref enum_) => {
                if enum_.or_null() {
                    if value.is_null() {
                        let result = self.builder.borrow_mut().no_string()
                            .map_err(Error::BuilderError)?;
                        return Ok(result)
                    }
                }
                let string = value.as_string()
                    .ok_or_else(|| Error::Mismatch("String".to_string()))?;
                for candidate in enum_.strings() {
                    if candidate == string {
                        let result = self.builder.borrow_mut().string(&candidate)
                            .map_err(Error::BuilderError)?;
                        return Ok(result)
                    }
                }
                Err(Error::NoSuchLiteral {
                    strings: enum_.strings().iter().cloned().collect(),
                    or_null: enum_.or_null()
                })
           }
           Interfaces(ref interfaces) => {
               let object = value.as_object()
                   .ok_or_else(|| Error::Mismatch("Object".to_string()))?;
               let kind_name = object.get("type")
                   .ok_or_else(|| Error::MissingField("type".to_string()))?
                   .as_string()
                   .ok_or_else(|| Error::Mismatch("type".to_string()))?;
               let kind = self.grammar.get_kind(kind_name)
                   .ok_or_else(|| Error::NoSuchKind(kind_name.to_string().clone()))?;

               // We have a kind, so we know how to encode the data. We just need
               // to make sure that we expected this interface here.
               if let Some(interface) = self.grammar.get_interface_by_kind(&kind) {
                   if self.grammar
                        .get_ancestors_by_name_including_self(interface.name())
                        .unwrap()
                        .iter()
                        .find(|ancestor|
                            interfaces.iter()
                                .find(|candidate| candidate == ancestor)
                                .is_some()
                        ).is_none() {
                        return Err(Error::NoSuchRefinement(kind.to_string().clone()))
                   }
                   let fields = interface.contents().fields();
                   let contents = self.encode_structure(object, fields)?;
                   // Write the contents with the tag of the refined interface.
                   let labelled = self.builder
                       .borrow_mut()
                       .tuple(contents, Some(interface.deref()))
                       .map_err(Error::BuilderError)?;
                   return Ok(labelled)
               }
               return Err(Error::NoSuchRefinement(kind.to_string().clone()));
           }
           Boolean => {
                let value = value.as_boolean()
                    .ok_or_else(|| Error::Mismatch("boolean".to_string()))?;
                let result = self.builder.borrow_mut().bool(value)
                    .map_err(Error::BuilderError)?;
                Ok(result)
           }
           String => {
                let value = value.as_string()
                   .ok_or_else(|| Error::Mismatch("String".to_string()))?;
                let result = self.builder.borrow_mut().string(&value)
                    .map_err(Error::BuilderError)?;
                Ok(result)
           }
           Number => {
               let value = value.as_f64()
                   .ok_or_else(|| Error::Mismatch("Number".to_string()))?;
               let result = self.builder.borrow_mut().float(value)
                   .map_err(Error::BuilderError)?;
               Ok(result)
           }
        }
    }
    fn encode_structure(&self, object: &Object, fields: &[Field]) -> Result<Vec<B::Tree>, Error<E>> {
        let mut result = Vec::with_capacity(fields.len());
        for field in fields {
            if let Some(source) = object.get(field.name().to_string()) {
                let encoded = self.encode(source, field.type_())?;
                result.push(encoded)
            } else {
                return Err(Error::MissingField(field.name().to_string().clone()))
            }
        }
        Ok(result)
    }
}
