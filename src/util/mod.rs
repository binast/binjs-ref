use ast::grammar::ASTError;

use rand;
use serde_json;
use serde_json::{ Value as JSON, Number };

use std;
use std::fs::File;
use std::io::Seek;
use std::path::*;

type Object = serde_json::Map<String, JSON>;


/// Strip a tree from meaningless information (location information, comments, ...)
#[allow(unused)]
pub fn strip(tree: &mut JSON) {
    use serde_json::Value::*;
    match *tree {
        Object(ref mut map) => {
            map.remove("loc");
            map.remove("comments");
            map.remove("start");
            map.remove("end");
            for (_, value) in map.iter_mut() {
                strip(value);
            }
        }
        Array(ref mut array) => {
            for value in array.iter_mut() {
                strip(value);
            }
        }
        _ => {}
    }
}

/// Utility: return a string describing a JSON value
/// (e.g. `"Object"`, `"Bool"`, ...)
pub fn type_of(tree: &JSON) -> String {
    use serde_json::Value::*;
    match *tree {
        Object(_) => "Object",
        String(_) => "String",
        Number(_) => "Number",
        Null      => "Null",
        Bool(_)   => "Bool",
        Array(_)  => "Array"
    }.to_owned()
}

pub fn f64_of(number: &Number) -> f64 {
    if number.is_i64() {
        number.as_i64().unwrap() as f64
    } else if number.is_u64() {
        number.as_u64().unwrap() as f64
    } else {
        number.as_f64().unwrap()
    }
}

pub fn get_temporary_file(extension: &str) -> std::result::Result<(PathBuf, File), std::io::Error> {
    use rand::Rng;
    let directory = std::env::temp_dir();
    let mut rng = rand::os::OsRng::new()
        .unwrap();
    let mut ascii = rng.gen_ascii_chars();
    let mut buf = Vec::with_capacity(8);
    let mut error = None;
    const ATTEMPTS : usize = 1024;
    for _ in 0..ATTEMPTS { // Limit number of attempts
        // FIXME: There must be a nicer way to do this.
        buf.clear();
        for _ in 0..8 {
            buf.push(ascii.next().unwrap());
        }
        let name : String = buf.iter().collect();
        let path = directory.as_path()
            .join(format!("binjs-{}.{}", name, extension));
        let result = File::create(&path);
        match result {
            Ok(file) => return Ok((path, file)),
            Err(err) => error = Some(err)
        }
    }
    Err(error.unwrap())
}

/// Utilities to simplify dealing with JSON.
///
/// Most of these tools are useful largely because lifetime management in a mutable JSON AST is
/// complicated.
pub trait JSONGetter {
    fn contains(&self, name: &str) -> bool;
    fn get_bool(&self, name: &str, description: &str) -> Result<bool, ASTError>;
    fn get_string(&self, name: &str, description: &str) -> Result<&str, ASTError>;
    fn get_array(&self, name: &str, description: &str) -> Result<&Vec<JSON>, ASTError>;
    fn get_array_mut(&mut self, name: &str, description: &str) -> Result<&mut Vec<JSON>, ASTError>;
    fn get_object(&self, name: &str, description: &str) -> Result<&Object, ASTError>;
    fn get_object_mut(&mut self, name: &str, description: &str) -> Result<&mut Object, ASTError>;
}

impl JSONGetter for serde_json::Map<String, JSON> {
    fn contains(&self, name: &str) -> bool {
        self.contains_key(name)
    }
    fn get_bool(&self, name: &str, description: &str) -> Result<bool, ASTError> {
        let found;
        if let Some(field) = self.get(name) {
            found = field.as_bool().is_some()
        } else {
            found = false;
        }
        if found { // Workaround for borrow checks
            Ok(self.get(name)
                .unwrap() // Checked above.
                .as_bool()
                .unwrap()) // Checked above
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
    fn get_string(&self, name: &str, description: &str) -> Result<&str, ASTError> {
        let found;
        if let Some(field) = self.get(name) {
            found = field.as_str().is_some()
        } else {
            found = false;
        }
        if found { // Workaround for borrow checks
            Ok(self.get(name)
                .unwrap() // Checked above.
                .as_str()
                .unwrap()) // Checked above
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
    fn get_array(&self, name: &str, description: &str) -> Result<&Vec<JSON>, ASTError> {
        let found;
        if let Some(field) = self.get(name) {
            found = field.as_array().is_some()
        } else {
            found = false;
        }

        if found { // Workaround for borrow checks
            Ok(self.get(name)
                .unwrap() // Checked above.
                .as_array()
                .unwrap()) // Checked above
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
    fn get_array_mut(&mut self, name: &str, description: &str) -> Result<&mut Vec<JSON>, ASTError> {
        let found;
        if let Some(field) = self.get_mut(name) {
            found = field.as_array_mut().is_some()
        } else {
            found = false;
        }

        if found { // Workaround for borrow checks
            Ok(self.get_mut(name)
                .unwrap() // Checked above.
                .as_array_mut()
                .unwrap()) // Checked above
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
    fn get_object(&self, name: &str, description: &str) -> Result<&Object, ASTError> {
        let found;
        if let Some(field) = self.get(name) {
            found = field.as_object().is_some()
        } else {
            found = false;
        }

        if found { // Workaround for borrow checks
            Ok(self.get(name)
                .unwrap() // Checked above.
                .as_object()
                .unwrap()) // Checked above
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
    fn get_object_mut(&mut self, name: &str, description: &str) -> Result<&mut Object, ASTError> {
        let found;
        if let Some(field) = self.get_mut(name) {
            found = field.as_object_mut().is_some()
        } else {
            found = false;
        }

        if found { // Workaround for borrow checks
            Ok(self.get_mut(name)
                .unwrap() // Checked above.
                .as_object_mut()
                .unwrap()) // Checked above
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
}

impl JSONGetter for JSON {
    fn contains(&self, name: &str) -> bool {
        if let JSON::Object(ref obj) = *self {
            obj.contains_key(name)
        } else {
            false
        }
    }
    fn get_bool(&self, name: &str, description: &str) -> Result<bool, ASTError> {
        if self.is_object() {
            // Avoid borrowing `self`.
            self.as_object()
                .unwrap() // Checked just above.
                .get_bool(name, description)
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
    fn get_string(&self, name: &str, description: &str) -> Result<&str, ASTError> {
        if self.is_object() {
            // Avoid borrowing `self`.
            self.as_object()
                .unwrap() // Checked just above.
                .get_string(name, description)
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
    fn get_array(&self, name: &str, description: &str) -> Result<&Vec<JSON>, ASTError> {
        if self.is_object() {
            // Avoid borrowing `self`.
            self.as_object()
                .unwrap() // Checked just above.
                .get_array(name, description)
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
    fn get_array_mut(&mut self, name: &str, description: &str) -> Result<&mut Vec<JSON>, ASTError> {
        if self.is_object() {
            // Avoid borrowing `self`.
            self.as_object_mut()
                .unwrap() // Checked just above.
                .get_array_mut(name, description)
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
    fn get_object(&self, name: &str, description: &str) -> Result<&Object, ASTError> {
        if self.is_object() {
            // Avoid borrowing `self`.
            self.as_object()
                .unwrap() // Checked just above.
                .get_object(name, description)
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
    fn get_object_mut(&mut self, name: &str, description: &str) -> Result<&mut Object, ASTError> {
        if self.is_object() {
            // Avoid borrowing `self`.
            self.as_object_mut()
                .unwrap() // Checked just above.
                .get_object_mut(name, description)
        } else {
            Err(ASTError::InvalidValue {
                got: serde_json::to_string(self).unwrap(),
                expected: description.to_owned()
            })
        }
    }
}

pub trait Dispose {
    fn dispose(&mut self);
}


/// An object (typically a reader) that knows its position and size.
pub trait Pos {
    /// The current position in the stream, in bytes.
    fn pos(&mut self) -> usize;

    /// The total number of bytes available in the stream.
    fn size(&mut self) -> usize;
}
impl<T> Pos for T where T: Seek {
    fn pos(&mut self) -> usize {
        self.seek(std::io::SeekFrom::Current(0))
            .expect("Could not check position")
        as usize
    }
    fn size(&mut self) -> usize {
        let old = self.seek(std::io::SeekFrom::Current(0))
            .expect("Could not check position");
        let size = self.seek(std::io::SeekFrom::End(0))
            .expect("Could not look for end of stream");
        self.seek(std::io::SeekFrom::Start(old))
            .expect("Could not rewind");
        size as usize
    }
}

/// An extension of `Read` that knows how to check that the following few bytes
/// match some value.
pub trait ReadConst {
    /// Succeed if the next few bytes match `bytes`, otherwise fail.
    fn read_const(&mut self, bytes: &[u8]) -> Result<(), std::io::Error>;
}
impl<T> ReadConst for T where T: std::io::Read {
    fn read_const(&mut self, data: &[u8]) -> Result<(), std::io::Error> {
        let mut buf = Vec::with_capacity(data.len());
        unsafe { buf.set_len(data.len()); }
        let bytes = self.read(&mut buf)?;
        if bytes != data.len() || &buf as &[u8] != data {
            let details = String::from_utf8(data.to_vec())
                .unwrap_or_else(|_| "<invalid read_const string>".to_string());
            return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, details));
        }
        Ok(())
    }
}

/// A structure used to make sure that every access to a state
/// goes through `try` and that any error poisons the state.
pub struct PoisonLock<S> {
    state: S,
    poisoned: bool
}

impl<S> PoisonLock<S> {
    pub fn new(state: S) -> Self {
        PoisonLock {
            state,
            poisoned: false
        }
    }

    /// Access the state for an operation.
    ///
    /// If the operation fails, the state is poisoned.
    ///
    /// # Panics
    ///
    /// If the `ReaderState` is poisoned.
    pub fn try<T, E, F>(&mut self, f: F) -> Result<T, E> where F: FnOnce(&mut S) -> Result<T, E> {
        assert!(!self.poisoned, "State is poisoned");
        f(&mut self.state)
            .map_err(|err| {
                self.poisoned = true;
                err
            })
    }

    pub fn poison(&mut self) {
        self.poisoned = true;
    }

    pub fn is_poisoned(&self) -> bool {
        self.poisoned
    }
}