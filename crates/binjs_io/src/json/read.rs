//! A JSON reader with extra support for JSON->BinAST converter.
//! The input JSON file is supposed to be the output of the JSON writer,
//! filtered with some command to generate an invalid content.
//! Some methods implemented on Decoder are for binjs_convert_from_json command.

use io::{FileStructurePrinter, Path, TokenReader};
use TokenReaderError;

use binjs_shared::{FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString};

pub type PathItem = binjs_shared::ast::PathItem<InterfaceName, /* Field */ (usize, FieldName)>;

use super::parser::Parser;
use super::value::{ObjectValue, Value};

use std::io::Read;
use std::marker::PhantomData;
use std::rc::Rc;

/// Context for either list or tuple.
enum Context {
    List {
        /// The list's items.
        items: Rc<Vec<Rc<Value>>>,

        /// The next item's index.
        index: usize,
    },
    Tuple {
        /// The tuple's tag.
        interface: InterfaceName,

        /// The tuple's field names.
        field_names: Rc<Box<[FieldName]>>,

        /// The tuple's field values.
        fields: Rc<Vec<Rc<Value>>>,

        /// The next field's index.
        index: usize,
    },
}
impl Context {
    /// Create a new list context.
    fn new_list(items: Rc<Vec<Rc<Value>>>) -> Self {
        Context::List { items, index: 0 }
    }

    /// Create a new tuple context.
    fn new_tuple(
        interface: InterfaceName,
        field_names: Rc<Box<[FieldName]>>,
        fields: Rc<Vec<Rc<Value>>>,
    ) -> Self {
        Context::Tuple {
            interface,
            field_names,
            fields,
            index: 0,
        }
    }

    /// Convert the context to PathItem.
    /// If this is a list context, returns None.
    /// If this is a tuple context, returns Some(PathItem).
    fn to_path_item(&self) -> Option<PathItem> {
        match self {
            Context::List { .. } => None,
            Context::Tuple {
                interface,
                field_names,
                index,
                ..
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
    Error,
}

pub struct Decoder<R: Read> {
    /// The context stack.
    contexts: Vec<Context>,

    phantom: PhantomData<R>,
}
impl<R: Read> Decoder<R> {
    pub fn new(source: R) -> Result<Self, TokenReaderError> {
        let mut parser = Parser::new(source);
        let root = parser.parse()?;
        Ok(Decoder {
            contexts: vec![Context::new_list(Rc::new(vec![root]))],
            phantom: PhantomData,
        })
    }

    /// Enter the given list.
    fn enter_list(&mut self, items: Rc<Vec<Rc<Value>>>) -> Result<(), TokenReaderError> {
        self.contexts.push(Context::new_list(items));
        Ok(())
    }

    /// Enter the given tuple.
    fn enter_tuple(
        &mut self,
        interface: InterfaceName,
        field_names: Rc<Box<[FieldName]>>,
        fields: Rc<Vec<Rc<Value>>>,
    ) -> Result<(), TokenReaderError> {
        self.contexts
            .push(Context::new_tuple(interface, field_names, fields));
        Ok(())
    }

    /// Exit the current list.
    /// Fails if not all items are read.
    fn exit_list(&mut self) -> Result<(), TokenReaderError> {
        match self.contexts.pop() {
            Some(Context::List { items, index }) => {
                if index == items.len() {
                    Ok(())
                } else {
                    Err(TokenReaderError::GenericError(
                        "Exiting a list without reading all items".to_string(),
                    ))
                }
            }
            Some(Context::Tuple { .. }) => Err(TokenReaderError::GenericError(
                "Unmatching exit, expected list, got tuple".to_string(),
            )),
            None => Err(TokenReaderError::GenericError(
                "No more context to exit".to_string(),
            )),
        }
    }

    /// Exit the current list.
    /// Fails if not all items are read.
    fn exit_tuple(&mut self) -> Result<(), TokenReaderError> {
        match self.contexts.pop() {
            Some(Context::Tuple { fields, index, .. }) => {
                if index == fields.len() {
                    Ok(())
                } else {
                    Err(TokenReaderError::GenericError(
                        "Exiting a tuple without reading all fields".to_string(),
                    ))
                }
            }
            Some(Context::List { .. }) => Err(TokenReaderError::GenericError(
                "Unmatching exit, expected tuple, got list".to_string(),
            )),
            None => Err(TokenReaderError::GenericError(
                "No more context to exit".to_string(),
            )),
        }
    }

    /// Convert the "field_names" value from the JSON to the array of FieldName.
    fn convert_field_names(
        field_names: Rc<Vec<Rc<Value>>>,
    ) -> Result<Rc<Box<[FieldName]>>, TokenReaderError> {
        let mut v = Vec::with_capacity(field_names.len());
        for item in field_names.iter() {
            v.push(FieldName(item.as_string()?));
        }
        Ok(Rc::new(v.into_boxed_slice()))
    }

    /// Get the next item from the topmost context.
    /// Fails if there's no more context, or there's no more items/fields in
    /// the current list/tuple.
    fn next_impl(&mut self, consume: bool) -> Result<Rc<Value>, TokenReaderError> {
        match self.contexts.last_mut() {
            Some(Context::List {
                ref mut items,
                ref mut index,
            }) => {
                if *index < (*items).len() {
                    let val = (*items)[*index].clone();
                    if consume {
                        *index += 1;
                    }
                    Ok(val)
                } else {
                    Err(TokenReaderError::GenericError(
                        "No more item in the list".to_string(),
                    ))
                }
            }
            Some(Context::Tuple {
                ref mut fields,
                ref mut index,
                ..
            }) => {
                if *index < (*fields).len() {
                    let val = (*fields)[*index].clone();
                    if consume {
                        *index += 1;
                    }
                    Ok(val)
                } else {
                    Err(TokenReaderError::GenericError(
                        "No more field in the tuple".to_string(),
                    ))
                }
            }
            None => Err(TokenReaderError::GenericError("No more item".to_string())),
        }
    }

    /// Returns the corresponding NextType for the next value.
    fn type_of(val: Rc<Value>) -> NextType {
        let obj = match val.as_object() {
            Ok(obj) => obj,
            Err(_) => {
                return NextType::Error;
            }
        };
        let type_ = match obj.get_string_property("type") {
            Ok(type_) => type_,
            Err(_) => {
                return NextType::Error;
            }
        };
        if type_.as_str() == "string" {
            return NextType::String;
        }
        if type_.as_str() == "enum" {
            return NextType::Enum;
        }
        if type_.as_str() == "identifier name" {
            return NextType::IdentifierName;
        }
        if type_.as_str() == "property key" {
            return NextType::PropertyKey;
        }
        if type_.as_str() == "float" {
            return NextType::Float;
        }
        if type_.as_str() == "unsigned long" {
            return NextType::UnsignedLong;
        }
        if type_.as_str() == "bool" {
            return NextType::Bool;
        }
        if type_.as_str() == "list" {
            return NextType::List;
        }
        if type_.as_str() == "tagged tuple" {
            return NextType::TaggedTuple;
        }
        return NextType::Error;
    }

    /// Returns which method to call to get the next item.
    ///
    /// The consumer is supposed to call this and branch to each
    /// TokenReader::*_at method call.
    pub fn next_type(&self) -> NextType {
        match self.contexts.last() {
            Some(Context::List { items, index }) => {
                if *index < items.len() {
                    Self::type_of(items[*index].clone())
                } else {
                    if self.contexts.len() == 1 {
                        // The top level is also using List.
                        NextType::End
                    } else {
                        NextType::EndOfList
                    }
                }
            }
            Some(Context::Tuple { fields, index, .. }) => {
                if *index < fields.len() {
                    Self::type_of(fields[*index].clone())
                } else {
                    NextType::EndOfTaggedTuple
                }
            }
            None => NextType::End,
        }
    }

    /// Get the next item, and increment index.
    fn next(&mut self) -> Result<Rc<Value>, TokenReaderError> {
        self.next_impl(true)
    }

    /// Get the next item, but don't increment index.
    fn next_without_consume(&mut self) -> Result<Rc<Value>, TokenReaderError> {
        self.next_impl(false)
    }

    /// Increment the index which is skipped by next_without_consume.
    fn consume(&mut self) -> Result<(), TokenReaderError> {
        // Reuse next_impl code just to increment index.
        self.next_impl(true)?;
        Ok(())
    }

    /// Returns then path to the current context.
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

    /// Returns the current tuple context's interface name and field names.
    ///
    /// Supposed to be fed to TokenWriter::exit_tagged_tuple_at.
    ///
    /// This method should be called before TokenReader::exit_tagged_tuple_at
    /// for the corresponding tagged tuple.
    pub fn current_tagged_tuple_info(&self) -> (InterfaceName, Rc<Box<[FieldName]>>) {
        match self.contexts.last() {
            Some(Context::Tuple {
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
        let obj = self.next()?.as_object()?;
        obj.expect_string_property("type", "string")?;
        Ok(obj.get_string_or_null_property("value")?)
    }

    fn string_enum_at(&mut self, _path: &Path) -> Result<SharedString, TokenReaderError> {
        let obj = self.next()?.as_object()?;
        obj.expect_string_property("type", "enum")?;
        Ok(obj.get_string_property("value")?)
    }

    fn identifier_name_at(
        &mut self,
        _path: &Path,
    ) -> Result<Option<IdentifierName>, TokenReaderError> {
        let obj = self.next()?.as_object()?;
        obj.expect_string_property("type", "identifier name")?;
        Ok(obj
            .get_string_or_null_property("value")?
            .map(IdentifierName))
    }

    fn property_key_at(&mut self, _path: &Path) -> Result<Option<PropertyKey>, TokenReaderError> {
        let obj = self.next()?.as_object()?;
        obj.expect_string_property("type", "property key")?;
        Ok(obj.get_string_or_null_property("value")?.map(PropertyKey))
    }

    // ---- Primitive types

    fn float_at(&mut self, _path: &Path) -> Result<Option<f64>, TokenReaderError> {
        let obj = self.next()?.as_object()?;
        obj.expect_string_property("type", "float")?;
        Ok(obj.get_number_or_null_property("value")?)
    }

    fn unsigned_long_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        let obj = self.next()?.as_object()?;
        obj.expect_string_property("type", "unsigned long")?;
        Ok(obj.get_number_property("value")? as u32)
    }

    fn bool_at(&mut self, _path: &Path) -> Result<Option<bool>, TokenReaderError> {
        let obj = self.next()?.as_object()?;
        obj.expect_string_property("type", "bool")?;
        Ok(obj.get_bool_or_null_property("value")?)
    }

    // ---- Lazy

    fn offset_at(&mut self, __path: &Path) -> Result<u32, TokenReaderError> {
        unimplemented!()
    }

    // ---- Composed types

    fn enter_list_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        let obj = self.next_without_consume()?.as_object()?;
        obj.expect_string_property("type", "list")?;
        let value = obj.get_array_property("value")?;
        let len = value.len() as u32;

        self.enter_list(value)?;

        Ok(len)
    }
    fn exit_list_at(&mut self, _path: &Path) -> Result<(), TokenReaderError> {
        self.exit_list()?;
        self.consume()?;
        Ok(())
    }

    fn enter_tagged_tuple_at(
        &mut self,
        _path: &Path,
    ) -> Result<(InterfaceName, Option<std::rc::Rc<Box<[FieldName]>>>), TokenReaderError> {
        let obj = self.next_without_consume()?.as_object()?;
        obj.expect_string_property("type", "tagged tuple")?;
        let name = InterfaceName(obj.get_string_property("interface")?);
        let field_names = Self::convert_field_names(obj.get_array_property("field_names")?)?;
        let fields = obj.get_array_property("fields")?;

        if field_names.len() != fields.len() {
            return Err(TokenReaderError::GenericError(
                "field_names and fields should have the same length".to_string(),
            ));
        }

        self.enter_tuple(name.clone(), field_names.clone(), fields)?;

        Ok((name, Some(field_names)))
    }
    fn exit_tagged_tuple_at(&mut self, _path: &Path) -> Result<(), TokenReaderError> {
        self.exit_tuple()?;
        self.consume()?;
        Ok(())
    }

    fn enter_untagged_tuple_at(&mut self, __path: &Path) -> Result<(), TokenReaderError> {
        unimplemented!()
    }
}
