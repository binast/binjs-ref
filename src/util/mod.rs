use binjs_generic::syntax::ASTError;

use json::object::Object;
use json::JsonValue as JSON;

pub trait JSONAs {
    fn as_array(&self, description: &str) -> Result<&Vec<JSON>, ASTError>;
    fn as_array_mut(&mut self, description: &str) -> Result<&mut Vec<JSON>, ASTError>;
    fn as_object(&self, description: &str) -> Result<&Object, ASTError>;
    fn as_object_mut(&mut self, description: &str) -> Result<&mut Object, ASTError>;
}

impl JSONAs for JSON {
    fn as_array(&self, description: &str) -> Result<&Vec<JSON>, ASTError> {
        if let JSON::Array(ref array) = *self {
            Ok(array)
        } else {
            Err(ASTError::InvalidValue {
                got: self.dump(),
                expected: description.to_owned(),
            })
        }
    }
    fn as_array_mut(&mut self, description: &str) -> Result<&mut Vec<JSON>, ASTError> {
        if let JSON::Array(ref mut array) = *self {
            Ok(array)
        } else {
            Err(ASTError::InvalidValue {
                got: self.dump(),
                expected: description.to_owned(),
            })
        }
    }
    fn as_object(&self, description: &str) -> Result<&Object, ASTError> {
        if let JSON::Object(ref object) = *self {
            Ok(object)
        } else {
            Err(ASTError::InvalidValue {
                got: self.dump(),
                expected: description.to_owned(),
            })
        }
    }
    fn as_object_mut(&mut self, description: &str) -> Result<&mut Object, ASTError> {
        if let JSON::Object(ref mut object) = *self {
            Ok(object)
        } else {
            Err(ASTError::InvalidValue {
                got: self.dump(),
                expected: description.to_owned(),
            })
        }
    }
}

/// Utilities to simplify dealing with JSON.
///
/// Most of these tools are useful largely because lifetime management in a mutable JSON AST is
/// complicated.
pub trait JSONGetter {
    fn get_bool(&self, name: &str, description: &str) -> Result<bool, ASTError>;
    fn get_string(&self, name: &str, description: &str) -> Result<&str, ASTError>;
    fn get_array(&self, name: &str, description: &str) -> Result<&Vec<JSON>, ASTError>;
    fn get_array_mut(&mut self, name: &str, description: &str) -> Result<&mut Vec<JSON>, ASTError>;
    fn get_object(&self, name: &str, description: &str) -> Result<&Object, ASTError>;
    fn get_object_mut(&mut self, name: &str, description: &str) -> Result<&mut Object, ASTError>;
}

impl JSONGetter for Object {
    fn get_bool(&self, name: &str, description: &str) -> Result<bool, ASTError> {
        match self[name].as_bool() {
            Some(b) => Ok(b),
            None => Err(ASTError::InvalidValue {
                got: self.dump(),
                expected: description.to_owned(),
            }),
        }
    }
    fn get_string(&self, name: &str, description: &str) -> Result<&str, ASTError> {
        let found;
        if let Some(field) = self.get(name) {
            found = field.as_str().is_some()
        } else {
            found = false;
        }
        if found {
            // Workaround for borrow checks
            Ok(self
                .get(name)
                .unwrap() // Checked above.
                .as_str()
                .unwrap()) // Checked above
        } else {
            Err(ASTError::InvalidValue {
                got: self.dump(),
                expected: description.to_owned(),
            })
        }
    }
    fn get_array(&self, name: &str, description: &str) -> Result<&Vec<JSON>, ASTError> {
        self[name].as_array(description)
    }
    fn get_array_mut(&mut self, name: &str, description: &str) -> Result<&mut Vec<JSON>, ASTError> {
        self[name].as_array_mut(description)
    }
    fn get_object(&self, name: &str, description: &str) -> Result<&Object, ASTError> {
        self[name].as_object(description)
    }
    fn get_object_mut(&mut self, name: &str, description: &str) -> Result<&mut Object, ASTError> {
        self[name].as_object_mut(description)
    }
}

impl JSONGetter for JSON {
    fn get_bool(&self, name: &str, description: &str) -> Result<bool, ASTError> {
        match self[name] {
            JSON::Boolean(b) => return Ok(b),
            _ => {}
        };
        Err(ASTError::InvalidValue {
            got: self.dump(),
            expected: description.to_owned(),
        })
    }
    fn get_string(&self, name: &str, description: &str) -> Result<&str, ASTError> {
        if let Some(str) = self[name].as_str() {
            return Ok(str);
        }
        Err(ASTError::InvalidValue {
            got: self.dump(),
            expected: description.to_owned(),
        })
    }
    fn get_array(&self, name: &str, description: &str) -> Result<&Vec<JSON>, ASTError> {
        if let JSON::Array(ref array) = self[name] {
            return Ok(array);
        };
        Err(ASTError::InvalidValue {
            got: self.dump(),
            expected: description.to_owned(),
        })
    }
    fn get_array_mut(&mut self, name: &str, description: &str) -> Result<&mut Vec<JSON>, ASTError> {
        self[name].as_array_mut(description)
    }
    fn get_object(&self, name: &str, description: &str) -> Result<&Object, ASTError> {
        match self[name] {
            JSON::Object(ref obj) => return Ok(obj),
            _ => {}
        };
        Err(ASTError::InvalidValue {
            got: self.dump(),
            expected: description.to_owned(),
        })
    }
    fn get_object_mut(&mut self, name: &str, description: &str) -> Result<&mut Object, ASTError> {
        self[name].as_object_mut(description)
    }
}
