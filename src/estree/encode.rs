use estree::grammar::*;
use bintree::io::*;

use std;
use std::collections::HashSet;

use serde_json::Value;
type Object = std::collections::BTreeMap<String, Value>;

#[derive(Debug)]
pub enum Error {
    Mismatch(String),
    NoSuchInterface(String),
    NoSuchRefinement(String),
    NoSuchTag(String),
    MissingField(String),
    NoSuchLiteral {strings: Vec<String>, or_null: bool}
}

pub struct Encoder<B, Tree> where B: Builder<Tree=Tree> {
    grammar: Syntax,
    interfaces: HashSet<InterfaceName>,
    builder: B
}
impl<B, Tree> Encoder<B, Tree> where B: Builder<Tree=Tree> {
    pub fn new(syntax: Syntax, builder: B) -> Self {
        Encoder {
            grammar: syntax,
            interfaces: HashSet::new(),
            builder,
        }
    }

    /// Encode a JSON into a SerializeTree based on a grammar.
    /// This step doesn't perform any interesting check on the JSON.
    pub fn encode(&mut self, value: &Value, kind: &Type) -> Result<Tree, Error> {
        use estree::grammar::Type::*;
        match *kind {
            Array(ref kind) => {
                let list = value.as_array()
                    .ok_or_else(|| Error::Mismatch("Array".to_string()))?;
                let mut encoded = Vec::with_capacity(list.len());
                for item in list {
                    encoded.push(self.encode(item, kind)?);
                }
                Ok(self.builder.list(encoded))
            }
            Structure(ref structure) => {
                let object = value.as_object()
                    .ok_or_else(|| Error::Mismatch("Object".to_string()))?;
                let contents = self.encode_structure(object, structure.fields())?;
                Ok(self.builder.tuple(contents, None))
            }
           OneOfStrings {ref strings, or_null} => {
                if or_null {
                    if value.is_null() {
                        return Ok(self.builder.no_string())
                    }
                }
                let string = value.as_string()
                    .ok_or_else(|| Error::Mismatch("String".to_string()))?;
                for candidate in strings {
                    if candidate == string {
                        return Ok(self.builder.string(&candidate))
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
                        let unlabelled = self.builder
                            .tuple(contents, Some(refinement));
                        let labelled = self.builder
                            .label(unlabelled, name.to_string());
                        self.interfaces.insert(refinement.name().clone());
                        return Ok(labelled)
                    }
               }
               Err(Error::NoSuchRefinement(kind.to_string().clone()))
           }
           Boolean => {
                let value = value.as_boolean()
                    .ok_or_else(|| Error::Mismatch("boolean".to_string()))?;
                Ok(self.builder.bool(value))
           }
           String => {
               let value = value.as_string()
                   .ok_or_else(|| Error::Mismatch("String".to_string()))?;
               Ok(self.builder.string(&value))
           }
           Number => {
               let value = value.as_f64()
                   .ok_or_else(|| Error::Mismatch("Number".to_string()))?;
               Ok(self.builder.float(value))
           }
        }
    }
    fn encode_structure(&mut self, object: &Object, fields: &[Field]) -> Result<Vec<B::Tree>, Error> {
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
