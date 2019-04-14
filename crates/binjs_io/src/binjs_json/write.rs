//! A JSON writer for BinAST file structure.
//! The written file is supposed to be filtered with some command to generate
//! an invalid content, and then fed to binjs_convert_from_json command to
//! generate BinAST file with the invalid content.
//! See mod.rs for the detail of the filter.

use io::{Path, TokenWriter};
use TokenWriterError;

use binjs_shared::{FieldName, IdentifierName, InterfaceName, Node, PropertyKey, SharedString};

use escaped_wtf8;

use serde_json::Value as JSON;

/// Context for either list or tagged tuple.
enum Context {
    List {
        /// The list of items.
        items: Vec<JSON>,
    },
    TaggedTuple {
        /// The interface name.
        interface: InterfaceName,

        /// The list of field values.
        field_values: Vec<JSON>,
    },
}
impl Context {
    /// Create a new list context.
    fn new_list() -> Self {
        Context::List { items: Vec::new() }
    }

    /// Create a new tagged tuple context.
    fn new_tagged_tuple(tag: InterfaceName) -> Self {
        Context::TaggedTuple {
            interface: tag,
            field_values: Vec::new(),
        }
    }
}

pub struct TreeTokenWriter {
    /// The context stack.
    contexts: Vec<Context>,
}

impl TreeTokenWriter {
    pub fn new() -> Self {
        Self {
            contexts: vec![Context::new_list()],
        }
    }

    /// Add the given item/field to the current list/tagged tuple.
    fn push(&mut self, v: JSON) {
        match self.contexts.last_mut() {
            Some(Context::TaggedTuple { field_values, .. }) => {
                field_values.push(v);
            }
            Some(Context::List { items }) => {
                items.push(v);
            }
            _ => {
                panic!("context mismatch");
            }
        }
    }
}

impl TokenWriter for TreeTokenWriter {
    type Data = Box<[u8]>;

    fn done(mut self) -> Result<Self::Data, TokenWriterError> {
        match self.contexts.pop() {
            Some(Context::List { mut items }) => {
                let top_level_item = items
                    .pop()
                    .expect("There should be only one item at the top level");
                let source = format!("{:#}", top_level_item);
                let result = escaped_wtf8::to_unicode_escape(source).into_bytes().into();
                Ok(result)
            }
            _ => {
                panic!("context mismatch");
            }
        }
    }

    fn enter_tagged_tuple_at(
        &mut self,
        _node: &Node,
        tag: &InterfaceName,
        _children: &[&FieldName],
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        self.contexts.push(Context::new_tagged_tuple(tag.clone()));
        Ok(())
    }
    fn exit_tagged_tuple_at(
        &mut self,
        _node: &Node,
        _tag: &InterfaceName,
        children: &[&FieldName],
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        match self.contexts.pop() {
            Some(Context::TaggedTuple {
                interface: tag,
                field_values,
            }) => {
                let mut obj = serde_json::Map::with_capacity(2);
                obj.insert(
                    "@TYPE".to_string(),
                    JSON::String("tagged tuple".to_string()),
                );
                obj.insert(
                    "@INTERFACE".to_string(),
                    JSON::String(tag.as_str().to_string()),
                );

                let mut fields: Vec<JSON> = Vec::new();
                assert!(children.len() == field_values.len());
                let mut i = 0;
                for field_value in field_values {
                    let mut field = serde_json::Map::with_capacity(2);
                    field.insert(
                        "@FIELD_NAME".to_string(),
                        JSON::String(children[i].as_str().to_string()),
                    );
                    field.insert("@FIELD_VALUE".to_string(), field_value);
                    fields.push(JSON::Object(field));
                    i += 1;
                }
                obj.insert("@FIELDS".to_string(), JSON::Array(fields));

                self.push(JSON::Object(obj));
            }
            _ => {
                panic!("context mismatch");
            }
        };
        Ok(())
    }

    fn enter_list_at(&mut self, _len: usize, _path: &Path) -> Result<(), TokenWriterError> {
        self.contexts.push(Context::new_list());
        Ok(())
    }
    fn exit_list_at(&mut self, _path: &Path) -> Result<(), TokenWriterError> {
        match self.contexts.pop() {
            Some(Context::List { items }) => {
                let mut obj = serde_json::Map::with_capacity(2);
                obj.insert("@TYPE".to_string(), JSON::String("list".to_string()));
                obj.insert("@VALUE".to_string(), JSON::Array(items));
                self.push(JSON::Object(obj));
            }
            _ => {
                panic!("context mismatch");
            }
        };

        Ok(())
    }

    fn string_at(
        &mut self,
        value: Option<&SharedString>,
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        let mut obj = serde_json::Map::with_capacity(2);
        obj.insert("@TYPE".to_string(), JSON::String("string".to_string()));
        match value {
            Some(v) => {
                obj.insert("@VALUE".to_string(), JSON::String(v.to_string()));
            }
            None => {
                obj.insert("@VALUE".to_string(), JSON::Null);
            }
        };
        self.push(JSON::Object(obj));
        Ok(())
    }

    fn string_enum_at(
        &mut self,
        value: &SharedString,
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        let mut obj = serde_json::Map::with_capacity(2);
        obj.insert("@TYPE".to_string(), JSON::String("enum".to_string()));
        obj.insert("@VALUE".to_string(), JSON::String(value.to_string()));
        self.push(JSON::Object(obj));
        Ok(())
    }

    fn float_at(&mut self, value: Option<f64>, _path: &Path) -> Result<(), TokenWriterError> {
        let mut obj = serde_json::Map::with_capacity(2);
        obj.insert("@TYPE".to_string(), JSON::String("float".to_string()));
        obj.insert(
            "@VALUE".to_string(),
            value
                .and_then(serde_json::Number::from_f64)
                .map(JSON::Number)
                .unwrap_or(JSON::Null),
        );
        self.push(JSON::Object(obj));
        Ok(())
    }

    fn unsigned_long_at(&mut self, value: u32, _path: &Path) -> Result<(), TokenWriterError> {
        let mut obj = serde_json::Map::with_capacity(2);
        obj.insert(
            "@TYPE".to_string(),
            JSON::String("unsigned long".to_string()),
        );
        obj.insert("@VALUE".to_string(), JSON::Number(value.into()));
        self.push(JSON::Object(obj));
        Ok(())
    }

    fn bool_at(&mut self, value: Option<bool>, _path: &Path) -> Result<(), TokenWriterError> {
        let mut obj = serde_json::Map::with_capacity(2);
        obj.insert("@TYPE".to_string(), JSON::String("bool".to_string()));
        match value {
            Some(v) => {
                obj.insert("@VALUE".to_string(), JSON::Bool(v));
            }
            None => {
                obj.insert("@VALUE".to_string(), JSON::Null);
            }
        };
        self.push(JSON::Object(obj));
        Ok(())
    }

    fn offset_at(&mut self, _path: &Path) -> Result<(), TokenWriterError> {
        unimplemented!()
    }

    fn property_key_at(
        &mut self,
        value: Option<&PropertyKey>,
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        let mut obj = serde_json::Map::with_capacity(2);
        obj.insert(
            "@TYPE".to_string(),
            JSON::String("property key".to_string()),
        );
        match value {
            Some(v) => {
                obj.insert("@VALUE".to_string(), JSON::String(v.as_str().to_string()));
            }
            None => {
                obj.insert("@VALUE".to_string(), JSON::Null);
            }
        };
        self.push(JSON::Object(obj));
        Ok(())
    }

    fn identifier_name_at(
        &mut self,
        value: Option<&IdentifierName>,
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        let mut obj = serde_json::Map::with_capacity(2);
        obj.insert(
            "@TYPE".to_string(),
            JSON::String("identifier name".to_string()),
        );
        match value {
            Some(v) => {
                obj.insert("@VALUE".to_string(), JSON::String(v.as_str().to_string()));
            }
            None => {
                obj.insert("@VALUE".to_string(), JSON::Null);
            }
        };
        self.push(JSON::Object(obj));
        Ok(())
    }
}
