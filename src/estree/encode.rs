use estree::grammar::*;
use estree::io::*;

use std;
use std::collections::HashSet;

use serde_json::Value;
type Object = std::collections::BTreeMap<String, Value>;

#[derive(Debug)]
pub enum Error<E> {
    Mismatch(String),
    NoSuchInterface(String),
    NoSuchRefinement(String),
    NoSuchTag(String),
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
    interfaces: HashSet<InterfaceName>,
    builder: B
}
impl<B, Tree, E> Encoder<B, Tree, E> where B: Builder<Tree=Tree, Error=E> {
    pub fn new(syntax: Syntax, builder: B) -> Self {
        Encoder {
            grammar: syntax,
            interfaces: HashSet::new(),
            builder,
        }
    }

    /// Encode a JSON into a SerializeTree based on a grammar.
    /// This step doesn't perform any interesting check on the JSON.
    pub fn encode(&mut self, value: &Value, kind: &Type) -> Result<Tree, Error<E>> {
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
                let result = self.builder.list(encoded)
                    .map_err(Error::BuilderError)?;
                Ok(result)
            }
            Structure(ref structure) => {
                let object = value.as_object()
                    .ok_or_else(|| Error::Mismatch("Object".to_string()))?;
                let contents = self.encode_structure(object, structure.fields())?;
                let result = self.builder.tuple(contents, None)
                    .map_err(Error::BuilderError)?;
                Ok(result)
            }
            Enum(ref enum_) => {
                use estree::grammar::Enum;
                let Enum {ref strings, or_null} = *enum_;
                if or_null {
                    if value.is_null() {
                        let result = self.builder.no_string()
                            .map_err(Error::BuilderError)?;
                        return Ok(result)
                    }
                }
                let string = value.as_string()
                    .ok_or_else(|| Error::Mismatch("String".to_string()))?;
                for candidate in strings {
                    if candidate == string {
                        let result = self.builder.string(&candidate)
                            .map_err(Error::BuilderError)?;
                        return Ok(result)
                    }
                }
                Err(Error::NoSuchLiteral { strings: strings.clone(), or_null })
           }
           Interfaces(ref interfaces) => {
               let object = value.as_object()
                   .ok_or_else(|| Error::Mismatch("Object".to_string()))?;
               let kind = object.get("type")
                   .ok_or_else(|| Error::MissingField("type".to_string()))?
                   .as_string()
                   .ok_or_else(|| Error::Mismatch("type".to_string()))?;
               let kind = self.grammar.get_tag(kind)
                   .ok_or_else(|| Error::NoSuchTag(kind.to_string().clone()))?;
               for name in interfaces {
                    let interface = self.grammar.get_interface(&name)
                        .ok_or_else(|| Error::NoSuchInterface(name.to_string().clone()))?;
                    if let Some(refinement) = interface.get_refinement(&kind) {
                        let fields = interface.contents().fields();
                        let contents = self.encode_structure(object, fields)?;
                        // Write the contents with the tag of the refined interface.
                        let labelled = self.builder
                            .tuple(contents, Some(refinement))
                            .map_err(Error::BuilderError)?;
                        self.interfaces.insert(refinement.name().clone());
                        return Ok(labelled)
                    }
               }
               Err(Error::NoSuchRefinement(kind.to_string().clone()))
           }
           Boolean => {
                let value = value.as_boolean()
                    .ok_or_else(|| Error::Mismatch("boolean".to_string()))?;
                let result = self.builder.bool(value)
                    .map_err(Error::BuilderError)?;
                Ok(result)
           }
           String => {
                let value = value.as_string()
                   .ok_or_else(|| Error::Mismatch("String".to_string()))?;
                let result = self.builder.string(&value)
                    .map_err(Error::BuilderError)?;
                Ok(result)
           }
           Number => {
               let value = value.as_f64()
                   .ok_or_else(|| Error::Mismatch("Number".to_string()))?;
               let result = self.builder.float(value)
                   .map_err(Error::BuilderError)?;
               Ok(result)
           }
        }
    }
    fn encode_structure(&mut self, object: &Object, fields: &[Field]) -> Result<Vec<B::Tree>, Error<E>> {
        let mut result = Vec::with_capacity(fields.len());
        for field in fields {
            if let Some(source) = object.get(field.name()) {
                let encoded = self.encode(source, field.type_())?;
                result.push(encoded)
            } else {
                return Err(Error::MissingField(field.name().clone()))
            }
        }
        Ok(result)
    }
}
