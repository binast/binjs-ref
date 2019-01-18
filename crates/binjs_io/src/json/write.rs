//! A JSON writer for BinAST file structure.
//! The written file is supposed to be filtered with some command to generate
//! an invalid content and then fed to binjs_convert_from_json command to
//! generate BinAST file with the invalid content.

use io::{Path, TokenWriter};
use TokenWriterError;

use binjs_shared::{FieldName, IdentifierName, InterfaceName, Node, PropertyKey, SharedString};

use escaped_wtf8;

/// Context for either list or tuple.
enum Context {
    List {
        /// The list of stringified items.
        items: Vec<String>,
    },
    Tuple {
        /// The interface name.
        interface: InterfaceName,

        /// The list of stringified fields.
        fields: Vec<String>,
    },
}

pub struct TreeTokenWriter {
    /// The context stack.
    contexts: Vec<Context>,
}

impl TreeTokenWriter {
    pub fn new() -> Self {
        Self {
            contexts: vec![Context::List { items: Vec::new() }],
        }
    }

    /// Add the given stringified item/field to the current list/tuple.
    fn push(&mut self, v: String) {
        match self.contexts.last_mut() {
            Some(Context::Tuple { fields, .. }) => {
                fields.push(v);
            }
            Some(Context::List { items }) => {
                items.push(v);
            }
            _ => {
                panic!("context mismatch");
            }
        }
    }

    /// Append whitespaces for indent to the given string
    fn indent(&self, target: &mut String) {
        target.push_str(str::repeat("    ", self.contexts.len() - 1).as_str());
    }
}

impl TokenWriter for TreeTokenWriter {
    type Data = Vec<u8>;

    fn done(mut self) -> Result<Self::Data, TokenWriterError> {
        match self.contexts.pop() {
            Some(Context::List { mut items }) => Ok(items
                .pop()
                .expect("There should be only one item at the top level")
                .as_bytes()
                .to_vec()),
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
        self.contexts.push(Context::Tuple {
            interface: tag.clone(),
            fields: Vec::new(),
        });
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
            Some(Context::Tuple {
                interface: tag,
                fields,
            }) => {
                // Format
                //   {
                //     "type": "tagged tuple",
                //     "interface": "<interface name>",
                //     "field_names": [
                //       "<field_name>",
                //       ...
                //     ],
                //     "fields": [
                //       <field>,
                //       ...
                //     ],
                //   }

                let mut s = String::new();
                self.indent(&mut s);
                s.push_str("{\n");

                self.indent(&mut s);
                s.push_str("  \"type\": \"tagged tuple\",\n");
                self.indent(&mut s);

                s.push_str("  \"interface\": \"");
                s.push_str(tag.as_str());
                s.push_str("\",\n");

                self.indent(&mut s);
                s.push_str("  \"field_names\": [\n");
                {
                    let mut first = true;
                    for field_name in children {
                        if !first {
                            s.push_str(",\n");
                        }
                        self.indent(&mut s);
                        s.push_str("    ");
                        s.push_str(escaped_wtf8::to_json(field_name.as_shared_string()).as_str());
                        first = false;
                    }
                    if !first {
                        s.push_str("\n");
                    }
                }
                self.indent(&mut s);
                s.push_str("  ],\n");

                self.indent(&mut s);
                s.push_str("  \"fields\": [\n");
                {
                    let mut first = true;
                    for field in fields {
                        if !first {
                            s.push_str(",\n");
                        }
                        s.push_str(field.as_str());
                        first = false;
                    }
                    if !first {
                        s.push_str("\n");
                    }
                }
                self.indent(&mut s);
                s.push_str("  ]\n");

                self.indent(&mut s);
                s.push_str("}");
                self.push(s);
            }
            _ => {
                panic!("context mismatch");
            }
        };
        Ok(())
    }

    fn enter_list_at(&mut self, _len: usize, _path: &Path) -> Result<(), TokenWriterError> {
        self.contexts.push(Context::List { items: Vec::new() });
        Ok(())
    }
    fn exit_list_at(&mut self, _path: &Path) -> Result<(), TokenWriterError> {
        match self.contexts.pop() {
            Some(Context::List { items }) => {
                // Format
                //   {
                //     "type": "list",
                //     "interface": "<interface name>",
                //     "value": [
                //       <item>,
                //       ...
                //     ],
                //   }

                let mut s = String::new();
                self.indent(&mut s);
                s.push_str("{\n");

                self.indent(&mut s);
                s.push_str("  \"type\": \"list\",\n");

                self.indent(&mut s);
                s.push_str("  \"value\": [\n");
                let mut first = true;
                for item in items {
                    if !first {
                        s.push_str(",\n");
                    }
                    s.push_str(item.as_str());
                    first = false;
                }
                if !first {
                    s.push_str("\n");
                }
                self.indent(&mut s);
                s.push_str("  ]\n");

                self.indent(&mut s);
                s.push_str("}");
                self.push(s);
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
        let mut s = String::new();
        self.indent(&mut s);
        s.push_str("{ \"type\": \"string\", \"value\": ");
        match value {
            Some(v) => {
                s.push_str(escaped_wtf8::to_json(v).as_str());
            }
            None => {
                s.push_str("null");
            }
        };
        s.push_str(" }");
        self.push(s);
        Ok(())
    }

    fn string_enum_at(
        &mut self,
        value: &SharedString,
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        let mut s = String::new();
        self.indent(&mut s);
        s.push_str("{ \"type\": \"enum\", \"value\": ");
        s.push_str(escaped_wtf8::to_json(value).as_str());
        s.push_str(" }");
        self.push(s);
        Ok(())
    }

    fn float_at(&mut self, value: Option<f64>, _path: &Path) -> Result<(), TokenWriterError> {
        let mut s = String::new();
        self.indent(&mut s);
        s.push_str("{ \"type\": \"float\", \"value\": ");
        match value {
            Some(v) => {
                s.push_str(format!("{}", v).as_str());
            }
            None => {
                s.push_str("null");
            }
        };
        s.push_str(" }");
        self.push(s);
        Ok(())
    }

    fn unsigned_long_at(&mut self, value: u32, _path: &Path) -> Result<(), TokenWriterError> {
        let mut s = String::new();
        self.indent(&mut s);
        s.push_str("{ \"type\": \"unsigned long\", \"value\": ");
        s.push_str(format!("{}", value).as_str());
        s.push_str(" }");
        self.push(s);
        Ok(())
    }

    fn bool_at(&mut self, value: Option<bool>, _path: &Path) -> Result<(), TokenWriterError> {
        let mut s = String::new();
        self.indent(&mut s);
        s.push_str("{ \"type\": \"bool\", \"value\": ");
        match value {
            Some(v) => {
                s.push_str(format!("{}", v).as_str());
            }
            None => {
                s.push_str("null");
            }
        };
        s.push_str(" }");
        self.push(s);
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
        let mut s = String::new();
        self.indent(&mut s);
        s.push_str("{ \"type\": \"property key\", \"value\": ");
        match value {
            Some(v) => {
                s.push_str(escaped_wtf8::to_json(v.as_shared_string()).as_str());
            }
            None => {
                s.push_str("null");
            }
        };
        s.push_str(" }");
        self.push(s);
        Ok(())
    }

    fn identifier_name_at(
        &mut self,
        value: Option<&IdentifierName>,
        _path: &Path,
    ) -> Result<(), TokenWriterError> {
        let mut s = String::new();
        self.indent(&mut s);
        s.push_str("{ \"type\": \"identifier name\", \"value\": ");
        match value {
            Some(v) => {
                s.push_str(escaped_wtf8::to_json(v.as_shared_string()).as_str());
            }
            None => {
                s.push_str("null");
            }
        };
        s.push_str(" }");
        self.push(s);
        Ok(())
    }
}
