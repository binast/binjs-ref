//! A JSON reader with extra support for JSON->BinAST converter.
//! The input JSON file is supposed to be the output of the JSON writer,
//! filtered with some command to generate an invalid content.
//! See mod.rs for the detail of the filter.
//! Some methods implemented on Decoder are for binjs_convert_from_json command.

use io::{FileStructurePrinter, Path, TokenReader};
use TokenReaderError;

use binjs_shared::{FieldName, FromJSON, IdentifierName, InterfaceName, PropertyKey, SharedString};

pub type PathItem = binjs_shared::ast::PathItem<InterfaceName, /* Field */ (usize, FieldName)>;

use std::collections::VecDeque;

use json;
use json::JsonValue as JSON;

use std::io::Read;
use std::marker::PhantomData;
use std::rc::Rc;

/// The element of the token list, which is converted from JSON file.
/// Each token corresponds to each function call in TokenReader, and values
/// correspond to the function's parameters.
enum Token {
    /// string_at
    String(Option<SharedString>),

    /// string_enum_at
    Enum(SharedString),

    /// identifier_name_at
    IdentifierName(Option<IdentifierName>),

    /// property_key_at
    PropertyKey(Option<PropertyKey>),

    /// float_at
    Float(Option<f64>),

    /// unsigned_long_at
    UnsignedLong(u32),

    /// bool_at
    Bool(Option<bool>),

    /// enter_list_at
    List { len: u32 },

    /// exit_list_at
    EndOfList,

    /// enter_tagged_tuple_at
    TaggedTuple {
        interface: InterfaceName,
        field_names: Rc<Box<[FieldName]>>,
    },

    /// exit_tagged_tuple_at
    EndOfTaggedTuple,
}
impl Token {
    /// Return string representation of the token's kind, for error reporting.
    fn to_kind(&self) -> &'static str {
        match self {
            Token::String(_) => "string",
            Token::Enum(_) => "enum",
            Token::IdentifierName(_) => "identifier name",
            Token::PropertyKey(_) => "property key",
            Token::Float(_) => "float",
            Token::UnsignedLong(_) => "unsigned long",
            Token::Bool(_) => "bool",
            Token::List { .. } => "list",
            Token::EndOfList => "end of list",
            Token::TaggedTuple { .. } => "tagged tuple",
            Token::EndOfTaggedTuple => "end of tagged tuple",
        }
    }
}

/// Utility for reading JSON.
struct JSONUtil {}
impl JSONUtil {
    /// Get the specified property of the given object.
    fn get_property<'a>(obj: &'a JSON, name: &str) -> Result<&'a JSON, TokenReaderError> {
        if obj.has_key(name) {
            Ok(&obj[name])
        } else {
            Err(TokenReaderError::GenericError(format!(
                "{} property doesn't exist",
                name
            )))
        }
    }

    /// Get the specified property of the given object, and convert it to a
    /// string.
    fn get_string_property(obj: &JSON, name: &str) -> Result<SharedString, TokenReaderError> {
        let value = Self::get_property(obj, name)?;
        match value.as_str() {
            Some(s) => Ok(SharedString::from_string(s.to_string())),
            None => Err(TokenReaderError::GenericError(format!(
                "'{}' property should be a string",
                name
            ))),
        }
    }

    /// Get the specified property of the given object, and convert it to a
    /// string or null.
    fn get_string_or_null_property(
        obj: &JSON,
        name: &str,
    ) -> Result<Option<SharedString>, TokenReaderError> {
        let value = Self::get_property(obj, name)?;
        if value.is_null() {
            return Ok(None);
        }

        match value.as_str() {
            Some(s) => Ok(Some(SharedString::from_string(s.to_string()))),
            None => Err(TokenReaderError::GenericError(format!(
                "'{}' property should be either a string or a null",
                name
            ))),
        }
    }

    /// Get the specified property of the given object, and convert it to a
    /// number.
    fn get_number_property(obj: &JSON, name: &str) -> Result<f64, TokenReaderError> {
        let value = Self::get_property(obj, name)?;
        f64::import(&value).map_err(|_| {
            TokenReaderError::GenericError(format!("'{}' property should be a number", name))
        })
    }

    /// Get the specified property of the given object, and convert it to a
    /// number or null.
    fn get_number_or_null_property(
        obj: &JSON,
        name: &str,
    ) -> Result<Option<f64>, TokenReaderError> {
        let value = Self::get_property(obj, name)?;
        if value.is_null() {
            return Ok(None);
        }
        let n = f64::import(&value).map_err(|_| {
            TokenReaderError::GenericError(format!(
                "'{}' property should be either a number or a null",
                name
            ))
        })?;
        Ok(Some(n))
    }

    /// Get the specified property of the given object, and convert it to a
    /// bool or null.
    fn get_bool_or_null_property(obj: &JSON, name: &str) -> Result<Option<bool>, TokenReaderError> {
        let value = Self::get_property(obj, name)?;
        if value.is_null() {
            return Ok(None);
        }

        match value.as_bool() {
            Some(b) => Ok(Some(b)),
            None => Err(TokenReaderError::GenericError(format!(
                "'{}' property should be either a bool or a null",
                name
            ))),
        }
    }

    /// Get the specified property of the given object, and convert it to an
    /// array.
    fn get_array_property<'a>(
        obj: &'a JSON,
        name: &str,
    ) -> Result<&'a Vec<JSON>, TokenReaderError> {
        let value = Self::get_property(obj, name)?;
        match value {
            JSON::Array(a) => Ok(a),
            _ => Err(TokenReaderError::GenericError(format!(
                "'{}' property should be an array",
                name
            ))),
        }
    }
}

/// The next item in the tree, designates which method to call on the decoder.
///
/// A list and a tuple have corresponding EndOf* when all items/fields are read.
pub enum NextType {
    String,
    Enum,
    IdentifierName,
    PropertyKey,
    Float,
    UnsignedLong,
    Bool,
    List,
    EndOfList,
    TaggedTuple,
    EndOfTaggedTuple,
    End,
}

/// Convert JSON to the list of tokens.
struct JSONReader {
    /// The list of tokens, converted from JSON.
    tokens: VecDeque<Token>,
}
impl JSONReader {
    fn new() -> Self {
        Self {
            tokens: VecDeque::new(),
        }
    }

    /// Convert the JSON to the list of tokens.
    fn read_all_tokens(&mut self, buffer: String) -> Result<(), TokenReaderError> {
        let root = json::parse(buffer.as_str()).map_err(|err| {
            TokenReaderError::GenericError(format!("Failed to parse source: {}", err))
        })?;

        if !root.is_object() {
            return Err(TokenReaderError::GenericError(
                "The top level value should be an object".to_string(),
            ));
        }

        self.convert_item(&root)?;

        Ok(())
    }

    /// Convert a string to tokens.
    fn convert_string(&mut self, obj: &JSON) -> Result<(), TokenReaderError> {
        let value = JSONUtil::get_string_or_null_property(obj, "@VALUE")?;
        self.tokens.push_back(Token::String(value));
        Ok(())
    }

    /// Convert an enum to tokens.
    fn convert_enum(&mut self, obj: &JSON) -> Result<(), TokenReaderError> {
        let value = JSONUtil::get_string_property(obj, "@VALUE")?;
        self.tokens.push_back(Token::Enum(value));
        Ok(())
    }

    /// Convert an identifier name to tokens.
    fn convert_identifier_name(&mut self, obj: &JSON) -> Result<(), TokenReaderError> {
        let value = JSONUtil::get_string_or_null_property(obj, "@VALUE")?;
        self.tokens
            .push_back(Token::IdentifierName(value.map(IdentifierName)));
        Ok(())
    }

    /// Convert a property key to tokens.
    fn convert_property_key(&mut self, obj: &JSON) -> Result<(), TokenReaderError> {
        let value = JSONUtil::get_string_or_null_property(obj, "@VALUE")?;
        self.tokens
            .push_back(Token::PropertyKey(value.map(PropertyKey)));
        Ok(())
    }

    /// Convert a float to tokens.
    fn convert_float(&mut self, obj: &JSON) -> Result<(), TokenReaderError> {
        let value = JSONUtil::get_number_or_null_property(obj, "@VALUE")?;
        self.tokens.push_back(Token::Float(value));
        Ok(())
    }

    /// Convert an unsigned long to tokens.
    fn convert_unsigned_long(&mut self, obj: &JSON) -> Result<(), TokenReaderError> {
        let value = JSONUtil::get_number_property(obj, "@VALUE")?;
        self.tokens.push_back(Token::UnsignedLong(value as u32));
        Ok(())
    }

    /// Convert a bool to tokens.
    fn convert_bool(&mut self, obj: &JSON) -> Result<(), TokenReaderError> {
        let value = JSONUtil::get_bool_or_null_property(obj, "@VALUE")?;
        self.tokens.push_back(Token::Bool(value));
        Ok(())
    }

    /// Convert a list to tokens.
    fn convert_list(&mut self, obj: &JSON) -> Result<(), TokenReaderError> {
        let items = JSONUtil::get_array_property(obj, "@VALUE")?;

        self.tokens.push_back(Token::List {
            len: items.len() as u32,
        });

        for item in items {
            self.convert_item(item)?;
        }

        self.tokens.push_back(Token::EndOfList);

        Ok(())
    }

    /// Convert a tagged tuple to tokens.
    fn convert_tagged_tuple(&mut self, obj: &JSON) -> Result<(), TokenReaderError> {
        let interface = InterfaceName(JSONUtil::get_string_property(obj, "@INTERFACE")?);
        let fields = JSONUtil::get_array_property(obj, "@FIELDS")?;

        let mut field_names = Vec::with_capacity(fields.len());
        let mut field_values = Vec::with_capacity(fields.len());
        for field in fields {
            let name = JSONUtil::get_string_property(field, "@FIELD_NAME")?;
            let value = JSONUtil::get_property(field, "@FIELD_VALUE")?;
            field_names.push(FieldName(name));
            field_values.push(value);
        }

        self.tokens.push_back(Token::TaggedTuple {
            interface: interface.clone(),
            field_names: Rc::new(field_names.into_boxed_slice()),
        });

        for item in field_values {
            self.convert_item(item)?;
        }

        self.tokens.push_back(Token::EndOfTaggedTuple);

        Ok(())
    }

    /// Convert arbitrary item (either list item or tagged tuple field) to
    /// tokens.
    fn convert_item(&mut self, obj: &JSON) -> Result<(), TokenReaderError> {
        if !obj.is_object() {
            return Err(TokenReaderError::GenericError(
                "Expected object".to_string(),
            ));
        }

        match JSONUtil::get_string_property(obj, "@TYPE")?.as_str() {
            "string" => {
                self.convert_string(obj)?;
            }
            "enum" => {
                self.convert_enum(obj)?;
            }
            "identifier name" => {
                self.convert_identifier_name(obj)?;
            }
            "property key" => {
                self.convert_property_key(obj)?;
            }
            "float" => {
                self.convert_float(obj)?;
            }
            "unsigned long" => {
                self.convert_unsigned_long(obj)?;
            }
            "bool" => {
                self.convert_bool(obj)?;
            }
            "list" => {
                self.convert_list(obj)?;
            }
            "tagged tuple" => {
                self.convert_tagged_tuple(obj)?;
            }
            v => {
                return Err(TokenReaderError::GenericError(format!(
                    "Unknown @TYPE value: '{}'",
                    v
                )));
            }
        }
        Ok(())
    }
}

/// Context for either list or tagged tuple.
/// Used to generate a path.
enum Context {
    /// List.
    /// We don't track the list context's internal, but just tell it's not
    /// inside taged tuple.
    List,

    /// Tagged tuple.
    TaggedTuple {
        /// The tagged tuple's tag.
        interface: InterfaceName,

        /// The tagged tuple's field names.
        field_names: Rc<Box<[FieldName]>>,

        /// The next field's index.
        index: usize,
    },
}
impl Context {
    /// Create a new tuple context.
    fn new_list() -> Self {
        Context::List
    }

    /// Create a new tuple context.
    fn new_tagged_tuple(interface: InterfaceName, field_names: Rc<Box<[FieldName]>>) -> Self {
        Context::TaggedTuple {
            interface,
            field_names,
            index: 0,
        }
    }

    /// Convert the context to PathItem.
    /// If this is a list context, returns None.
    /// If this is a tagged tuple context, returns Some(PathItem).
    fn to_path_item(&self) -> Option<PathItem> {
        match self {
            Context::List => None,
            Context::TaggedTuple {
                ref interface,
                ref field_names,
                ref index,
            } => {
                if *index == field_names.len() {
                    return None;
                }

                Some(PathItem {
                    interface: interface.clone(),
                    field: (*index, field_names[*index].clone()),
                })
            }
        }
    }

    /// Increment the next field's index.
    /// If this is a list context, do nothing.
    fn next(&mut self) {
        match self {
            Context::List => {
                // We don't track the current list item.
            }
            Context::TaggedTuple { ref mut index, .. } => {
                *index += 1;
            }
        }
    }
}

pub struct Decoder<R: Read> {
    /// The context stack, to generate the current path.
    contexts: Vec<Context>,

    /// The list of tokens, converted from JSON.
    tokens: VecDeque<Token>,

    /// Just to consume `R` type.
    phantom: PhantomData<R>,
}
impl<R: Read> Decoder<R> {
    /// Read JSON from `source`, parse it, and convert it to tokens.
    pub fn new(mut source: R) -> Result<Self, TokenReaderError> {
        let mut buffer = String::new();
        source
            .read_to_string(&mut buffer)
            .map_err(|_| TokenReaderError::GenericError("Failed to read source".to_string()))?;
        let mut reader = JSONReader::new();
        reader.read_all_tokens(buffer)?;
        Ok(Decoder {
            contexts: Vec::new(),
            tokens: reader.tokens,
            phantom: PhantomData,
        })
    }

    /// Get the next token and update the context.
    /// Returns error if there's no more token.
    fn next_checked(&mut self, kind: &str) -> Result<Token, TokenReaderError> {
        match self.next_unchecked()? {
            Some(v) => Ok(v),
            None => Err(TokenReaderError::GenericError(format!(
                "Expected '{}', but no more token",
                kind
            ))),
        }
    }

    /// Get the next token and update the context.
    /// When there's no more token, this returns Ok(None), instead of returning
    /// an error.
    fn next_unchecked(&mut self) -> Result<Option<Token>, TokenReaderError> {
        let token = self.tokens.pop_front();

        // Increment the current field index if necessary.
        match self.contexts.last_mut() {
            Some(context) => {
                context.next();
            }
            None => {}
        }

        // Update context.
        match token {
            Some(Token::List { .. }) => self.contexts.push(Context::new_list()),
            Some(Token::EndOfList) => match self.contexts.pop() {
                Some(Context::List) => {}
                Some(Context::TaggedTuple { .. }) => {
                    return Err(TokenReaderError::GenericError(
                        "Unmatching context, expected list, got tagged tuple".to_string(),
                    ));
                }
                None => {
                    return Err(TokenReaderError::GenericError(
                        "Unmatching context, expected list, but no more context".to_string(),
                    ));
                }
            },
            Some(Token::TaggedTuple {
                ref interface,
                ref field_names,
            }) => {
                self.contexts.push(Context::new_tagged_tuple(
                    interface.clone(),
                    field_names.clone(),
                ));
            }
            Some(Token::EndOfTaggedTuple) => match self.contexts.pop() {
                Some(Context::List) => {
                    return Err(TokenReaderError::GenericError(
                        "Unmatching context, expected tagged tuple, got list".to_string(),
                    ));
                }
                Some(Context::TaggedTuple { .. }) => {}
                None => {
                    return Err(TokenReaderError::GenericError(
                        "Unmatching context, expected tagged tuple, but no more context"
                            .to_string(),
                    ));
                }
            },
            _ => {}
        }

        Ok(token)
    }

    /// Returns which method to call to get the next item.
    ///
    /// The consumer is supposed to call this and branch to each
    /// TokenReader::*_at method call.
    pub fn next_type(&self) -> NextType {
        match self.tokens.front() {
            Some(Token::String(_)) => NextType::String,
            Some(Token::Enum(_)) => NextType::Enum,
            Some(Token::IdentifierName(_)) => NextType::IdentifierName,
            Some(Token::PropertyKey(_)) => NextType::PropertyKey,
            Some(Token::Float(_)) => NextType::Float,
            Some(Token::UnsignedLong(_)) => NextType::UnsignedLong,
            Some(Token::Bool(_)) => NextType::Bool,
            Some(Token::List { .. }) => NextType::List,
            Some(Token::EndOfList) => NextType::EndOfList,
            Some(Token::TaggedTuple { .. }) => NextType::TaggedTuple,
            Some(Token::EndOfTaggedTuple) => NextType::EndOfTaggedTuple,
            None => NextType::End,
        }
    }

    /// Returns then path to the current context.
    /// Should be called before calling *_at functions.
    ///
    /// Supposed to be fed to {TokenReader,TokenWriter}::*_at methods.
    pub fn get_path(&self) -> Path {
        let mut items = Vec::new();
        for context in self.contexts.iter() {
            match context.to_path_item() {
                Some(item) => {
                    items.push(item);
                }
                None => {}
            }
        }

        if items.len() == 0 {
            Path::from(Vec::new())
        } else {
            Path::from(items)
        }
    }

    /// Returns the current tagged tuple context's interface name and field
    /// names.
    ///
    /// Supposed to be fed to TokenWriter::exit_tagged_tuple_at.
    ///
    /// This method should be called before TokenReader::exit_tagged_tuple_at
    /// for the corresponding tagged tuple.
    pub fn current_tagged_tuple_info(&self) -> (InterfaceName, Rc<Box<[FieldName]>>) {
        match self.contexts.last() {
            Some(Context::TaggedTuple {
                interface,
                field_names,
                ..
            }) => (interface.clone(), field_names.clone()),
            _ => {
                panic!("The current context should be tuple");
            }
        }
    }
}

impl<R: Read> FileStructurePrinter for Decoder<R> {}

impl<R: Read> TokenReader for Decoder<R> {
    // ---- String types

    fn string_at(&mut self, _path: &Path) -> Result<Option<SharedString>, TokenReaderError> {
        match self.next_checked("string")? {
            Token::String(v) => Ok(v),
            v => Err(TokenReaderError::GenericError(format!(
                "Token mismatch, expected 'string', got {}",
                v.to_kind()
            ))),
        }
    }

    fn string_enum_at(&mut self, _path: &Path) -> Result<SharedString, TokenReaderError> {
        match self.next_checked("enum")? {
            Token::Enum(v) => Ok(v),
            v => Err(TokenReaderError::GenericError(format!(
                "Token mismatch, expected 'enum', got {}",
                v.to_kind()
            ))),
        }
    }

    fn identifier_name_at(
        &mut self,
        _path: &Path,
    ) -> Result<Option<IdentifierName>, TokenReaderError> {
        match self.next_checked("identifier name")? {
            Token::IdentifierName(v) => Ok(v),
            v => Err(TokenReaderError::GenericError(format!(
                "Token mismatch, expected 'identifier name', got {}",
                v.to_kind()
            ))),
        }
    }

    fn property_key_at(&mut self, _path: &Path) -> Result<Option<PropertyKey>, TokenReaderError> {
        match self.next_checked("property key")? {
            Token::PropertyKey(v) => Ok(v),
            v => Err(TokenReaderError::GenericError(format!(
                "Token mismatch, expected 'property key', got {}",
                v.to_kind()
            ))),
        }
    }

    // ---- Primitive types

    fn float_at(&mut self, _path: &Path) -> Result<Option<f64>, TokenReaderError> {
        match self.next_checked("float")? {
            Token::Float(v) => Ok(v),
            v => Err(TokenReaderError::GenericError(format!(
                "Token mismatch, expected 'float', got {}",
                v.to_kind()
            ))),
        }
    }

    fn unsigned_long_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        match self.next_checked("unsigned long")? {
            Token::UnsignedLong(v) => Ok(v),
            v => Err(TokenReaderError::GenericError(format!(
                "Token mismatch, expected 'unsigned long', got {}",
                v.to_kind()
            ))),
        }
    }

    fn bool_at(&mut self, _path: &Path) -> Result<Option<bool>, TokenReaderError> {
        match self.next_checked("bool")? {
            Token::Bool(v) => Ok(v),
            v => Err(TokenReaderError::GenericError(format!(
                "Token mismatch, expected 'bool', got {}",
                v.to_kind()
            ))),
        }
    }

    // ---- Lazy

    fn offset_at(&mut self, __path: &Path) -> Result<u32, TokenReaderError> {
        unimplemented!()
    }

    // ---- Composed types

    fn enter_list_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        match self.next_checked("list")? {
            Token::List { len } => Ok(len),
            v => Err(TokenReaderError::GenericError(format!(
                "Token mismatch, expected 'list', got {}",
                v.to_kind()
            ))),
        }
    }
    fn exit_list_at(&mut self, _path: &Path) -> Result<(), TokenReaderError> {
        match self.next_unchecked()? {
            Some(Token::EndOfList) => Ok(()),
            Some(v) => Err(TokenReaderError::GenericError(format!(
                "Token mismatch, expected the end of list, got {}",
                v.to_kind()
            ))),
            None => Err(TokenReaderError::GenericError(
                "Premature end of tokens, expected the end of list".to_string(),
            )),
        }
    }

    fn enter_tagged_tuple_at(
        &mut self,
        _path: &Path,
    ) -> Result<(InterfaceName, Option<std::rc::Rc<Box<[FieldName]>>>), TokenReaderError> {
        match self.next_checked("tagged tuple")? {
            Token::TaggedTuple {
                interface,
                field_names,
            } => Ok((interface, Some(field_names))),
            v => Err(TokenReaderError::GenericError(format!(
                "Token mismatch, expected 'tagged tuple', got {}",
                v.to_kind()
            ))),
        }
    }
    fn exit_tagged_tuple_at(&mut self, _path: &Path) -> Result<(), TokenReaderError> {
        match self.next_unchecked()? {
            Some(Token::EndOfTaggedTuple) => Ok(()),
            Some(v) => Err(TokenReaderError::GenericError(format!(
                "Token mismatch, expected the end of tuple, got {}",
                v.to_kind()
            ))),
            None => Err(TokenReaderError::GenericError(
                "Premature end of tokens, expected the end of tuple".to_string(),
            )),
        }
    }

    fn enter_untagged_tuple_at(&mut self, __path: &Path) -> Result<(), TokenReaderError> {
        unimplemented!()
    }
}
