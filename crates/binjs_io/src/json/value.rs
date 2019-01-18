/// JS value in JSON.
use TokenReaderError;

use binjs_shared::SharedString;

use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

/// JS value in JSON.
pub enum Value {
    String(SharedString),
    Number(f64),
    Object(Rc<HashMap<SharedString, Rc<Value>>>),
    Array(Rc<Vec<Rc<Value>>>),
    Bool(bool),
    Null,
}
impl Value {
    /// Get the string content from the value.
    /// Fails if the value is not string.
    pub fn as_string(&self) -> Result<SharedString, TokenReaderError> {
        match self {
            Value::String(s) => Ok(s.clone()),
            _ => Err(TokenReaderError::GenericError(
                "Expected a string".to_string(),
            )),
        }
    }

    /// Get the object content from the value.
    /// Fails if the value is not object.
    pub fn as_object(&self) -> Result<Rc<HashMap<SharedString, Rc<Value>>>, TokenReaderError> {
        match self {
            Value::Object(obj) => Ok(obj.clone()),
            _ => Err(TokenReaderError::GenericError(
                "Expected an object".to_string(),
            )),
        }
    }
}

/// Methods to treat HashMap as JS Object.
pub trait ObjectValue {
    /// Get the property from the object.
    /// Fails if the property doesn't exist.
    fn get_property(&self, key: &'static str) -> Result<Rc<Value>, TokenReaderError>;

    /// Get the property from the object and return bool or null content.
    /// Returns Some(bool) if the property is bool.
    /// Returns None if the property is null
    ///
    /// Fails if the property doesn't exist, or the property value is neither
    /// bool nor null.
    fn get_bool_or_null_property(
        &self,
        key: &'static str,
    ) -> Result<Option<bool>, TokenReaderError>;

    /// Get the property from the object and return number content.
    /// Fails if the property doesn't exist, or the property value is not number.
    fn get_number_property(&self, key: &'static str) -> Result<f64, TokenReaderError>;

    /// Get the property from the object and return number or null content.
    /// Returns Some(f64) if the property is number.
    /// Returns None if the property is null
    ///
    /// Fails if the property doesn't exist, or the property value is neither
    /// number nor null.
    fn get_number_or_null_property(
        &self,
        key: &'static str,
    ) -> Result<Option<f64>, TokenReaderError>;

    /// Get the property from the object and return string content.
    /// Fails if the property doesn't exist, or the property value is not string.
    fn get_string_property(&self, key: &'static str) -> Result<SharedString, TokenReaderError>;

    /// Get the property from the object and return string or null content.
    /// Returns Some(SharedString) if the property is string.
    /// Returns None if the property is null
    ///
    /// Fails if the property doesn't exist, or the property value is neither
    /// string nor null.
    fn get_string_or_null_property(
        &self,
        key: &'static str,
    ) -> Result<Option<SharedString>, TokenReaderError>;

    /// Get the property from the object and check if the value is a string
    /// with the expected content.
    ///
    /// Fails if the property doesn't exist, or the property value is not string,
    /// or the string doesn't match.
    fn expect_string_property(
        &self,
        key: &'static str,
        value: &'static str,
    ) -> Result<(), TokenReaderError>;

    /// Get the property from the object and return array content.
    /// Fails if the property doesn't exist, or the property value is not array.
    fn get_array_property(&self, key: &'static str)
        -> Result<Rc<Vec<Rc<Value>>>, TokenReaderError>;
}

impl ObjectValue for HashMap<SharedString, Rc<Value>> {
    fn get_property(&self, key: &'static str) -> Result<Rc<Value>, TokenReaderError> {
        match self.get(&SharedString::from_str(key)) {
            Some(val) => Ok(val.clone()),
            None => Err(TokenReaderError::GenericError(format!(
                "Missing \"{}\" property",
                key
            ))),
        }
    }

    fn get_bool_or_null_property(
        &self,
        key: &'static str,
    ) -> Result<Option<bool>, TokenReaderError> {
        let prop = self.get_property(key)?;
        match prop.borrow() {
            Value::Bool(b) => Ok(Some(*b)),
            Value::Null => Ok(None),
            _ => Err(TokenReaderError::GenericError(format!(
                "Property \"{}\" should be a bool or a null",
                key
            ))),
        }
    }

    fn get_number_property(&self, key: &'static str) -> Result<f64, TokenReaderError> {
        let prop = self.get_property(key)?;
        match prop.borrow() {
            Value::Number(n) => Ok(*n),
            _ => Err(TokenReaderError::GenericError(format!(
                "Property \"{}\" should be a number",
                key
            ))),
        }
    }

    fn get_number_or_null_property(
        &self,
        key: &'static str,
    ) -> Result<Option<f64>, TokenReaderError> {
        let prop = self.get_property(key)?;
        match prop.borrow() {
            Value::Number(n) => Ok(Some(*n)),
            Value::Null => Ok(None),
            _ => Err(TokenReaderError::GenericError(format!(
                "Property \"{}\" should be a number or a null",
                key
            ))),
        }
    }

    fn get_string_property(&self, key: &'static str) -> Result<SharedString, TokenReaderError> {
        let prop = self.get_property(key)?;
        match prop.borrow() {
            Value::String(s) => Ok(s.clone()),
            _ => Err(TokenReaderError::GenericError(format!(
                "Property \"{}\" should be a string",
                key
            ))),
        }
    }

    fn get_string_or_null_property(
        &self,
        key: &'static str,
    ) -> Result<Option<SharedString>, TokenReaderError> {
        let prop = self.get_property(key)?;
        match prop.borrow() {
            Value::String(s) => Ok(Some(s.clone())),
            Value::Null => Ok(None),
            _ => Err(TokenReaderError::GenericError(format!(
                "Property \"{}\" should be a string or a null",
                key
            ))),
        }
    }

    fn expect_string_property(
        &self,
        key: &'static str,
        value: &'static str,
    ) -> Result<(), TokenReaderError> {
        let s = self.get_string_property(key)?;
        if s.as_str() == value {
            Ok(())
        } else {
            Err(TokenReaderError::GenericError(format!(
                "Property \"{}\" should be \"{}\", got \"{}\"",
                key,
                value,
                s.as_str()
            )))
        }
    }

    fn get_array_property(
        &self,
        key: &'static str,
    ) -> Result<Rc<Vec<Rc<Value>>>, TokenReaderError> {
        let prop = self.get_property(key)?;
        match prop.borrow() {
            Value::Array(a) => Ok(a.clone()),
            _ => Err(TokenReaderError::GenericError(format!(
                "Property \"{}\" should be an array",
                key
            ))),
        }
    }
}
