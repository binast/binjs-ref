use ast::grammar::ASTError;

use inflector;
use rand;
use json;
use json::JsonValue as JSON;
use json::object::Object as Object;

use std;
use std::fs::File;
use std::io::Seek;
use std::path::*;

/// Strip a tree from meaningless information (location information, comments, ...)
#[allow(unused)]
pub fn strip(tree: &mut JSON) {
    use json::JsonValue::*;
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
    use json::JsonValue::*;
    match *tree {
        Object(_) => "Object",
        String(_) | Short(_) => "String",
        Number(_) => "Number",
        Null      => "Null",
        Boolean(_) => "Bool",
        Array(_)  => "Array"
    }.to_owned()
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
                expected: description.to_owned()
            })
        }
    }
    fn as_array_mut(&mut self, description: &str) -> Result<&mut Vec<JSON>, ASTError> {
        if let JSON::Array(ref mut array) = *self {
            Ok(array)
        } else {
            Err(ASTError::InvalidValue {
                got: self.dump(),
                expected: description.to_owned()
            })
        }
    }
    fn as_object(&self, description: &str) -> Result<&Object, ASTError> {
        if let JSON::Object(ref object) = *self {
            Ok(object)
        } else {
            Err(ASTError::InvalidValue {
                got: self.dump(),
                expected: description.to_owned()
            })
        }
    }
    fn as_object_mut(&mut self, description: &str) -> Result<&mut Object, ASTError> {
        if let JSON::Object(ref mut object) = *self {
            Ok(object)
        } else {
            Err(ASTError::InvalidValue {
                got: self.dump(),
                expected: description.to_owned()
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
            None => {
                Err(ASTError::InvalidValue {
                    got: self.dump(),
                    expected: description.to_owned()
                })
            }
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
                got: self.dump(),
                expected: description.to_owned()
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
            expected: description.to_owned()
        })
    }
    fn get_string(&self, name: &str, description: &str) -> Result<&str, ASTError> {
        if let Some(str) = self[name].as_str() {
            return Ok(str);
        }
        Err(ASTError::InvalidValue {
            got: self.dump(),
            expected: description.to_owned()
        })
    }
    fn get_array(&self, name: &str, description: &str) -> Result<&Vec<JSON>, ASTError> {
        if let JSON::Array(ref array) = self[name] {
            return Ok(array)
        };
        Err(ASTError::InvalidValue {
            got: self.dump(),
            expected: description.to_owned()
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
            expected: description.to_owned()
        })
    }
    fn get_object_mut(&mut self, name: &str, description: &str) -> Result<&mut Object, ASTError> {
        self[name].as_object_mut(description)
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

pub fn pick<'a, T: rand::Rng, U,>(rng: &mut T, slice: &'a [U]) -> &'a U {
    let index = rng.gen_range(0, slice.len());
    &slice[index]
}


pub trait ToCases {
    fn to_str(&self) -> &str;
    fn to_class_cases(&self) -> String {
        self.to_str().to_class_cases()
    }
    fn to_cpp_enum_case(&self) -> String {
        self.to_str().to_cpp_enum_case()
    }
    fn to_rust_identifier_case(&self) -> String {
        self.to_str().to_rust_identifier_case()
    }
    fn to_cpp_field_case(&self) -> String {
        self.to_str().to_cpp_field_case()
    }
}

impl ToCases for String {
    fn to_str(&self) -> &str {
        &self
    }
}

impl ToCases for str {
    fn to_str(&self) -> &str {
        self
    }
    fn to_class_cases(&self) -> String {
        inflector::cases::classcases::to_class_cases(self)
    }
    fn to_cpp_enum_case(&self) -> String {
        match self {
            "+=" => "PlusAssign".to_string(),
            "-=" => "MinusAssign".to_string(),
            "*=" => "MulAssign".to_string(),
            "/=" => "DivAssign".to_string(),
            "%=" => "ModAssign".to_string(),
            "**=" => "PowAssign".to_string(),
            "<<=" => "LshAssign".to_string(),
            ">>=" => "RshAssign".to_string(),
            ">>>=" => "UrshAssign".to_string(),
            "|=" => "BitOrAssign".to_string(),
            "^=" => "BitXorAssign".to_string(),
            "&=" => "BitAndAssign".to_string(),
            "," => "Comma".to_string(),
            "||" => "LogicalOr".to_string(),
            "&&" => "LogicalAnd".to_string(),
            "|" => "BitOr".to_string(),
            "^" => "BitXor".to_string(),
            "&" => "BitAnd".to_string(),
            "==" => "Eq".to_string(),
            "!=" => "Neq".to_string(),
            "===" => "StrictEq".to_string(),
            "!==" => "StrictNeq".to_string(),
            "<" => "LessThan".to_string(),
            "<=" => "LeqThan".to_string(),
            ">" => "GreaterThan".to_string(),
            ">=" => "GeqThan".to_string(),
            "<<" => "Lsh".to_string(),
            ">>" => "Rsh".to_string(),
            ">>>" => "Ursh".to_string(),
            "+" => "Plus".to_string(),
            "-" => "Minus".to_string(),
            "~" => "BitNot".to_string(),
            "*" => "Mul".to_string(),
            "/" => "Div".to_string(),
            "%" => "Mod".to_string(),
            "**" => "Pow".to_string(),
            "!" => "Not".to_string(),
            "++" => "Incr".to_string(),
            "--" => "Decr".to_string(),
            _ => {
                let class_cased = self.to_class_cases();
                assert!(&class_cased != "");
                class_cased
            }
        }
    }
    fn to_cpp_field_case(&self) -> String {
        let snake = inflector::cases::camelcase::to_camel_case(self);
        match &snake as &str {
            "class" => "class_".to_string(),
            "operator" => "operator_".to_string(),
            "const" => "const_".to_string(),
            "void" => "void_".to_string(),
            "delete" => "delete_".to_string(),
            "in" => "in_".to_string(),
            // Names reserved by us
            "result" => "result_".to_string(),
            "kind" => "kind_".to_string(),
            // Special cases
            "" => unimplemented!(),
            _ => snake
        }
    }
    fn to_rust_identifier_case(&self) -> String {
        let snake = inflector::cases::snakecase::to_snake_case(self);
        match &snake as &str {
            "super" => "super_".to_string(),
            "type" => "type_".to_string(),
            _ => snake
        }
    }
}


pub trait Reindentable {
    fn reindent(&self, prefix: &str) -> String;
}

impl Reindentable for String {
    fn reindent(&self, prefix: &str) -> String {
        use itertools::Itertools;

        // Determine the number of whitespace chars on the first line.
        // Trim that many whitespace chars on the following lines.
        if let Some(first_line) = self.lines().next() {
            let remove_indent = first_line.chars()
                .take_while(|c| char::is_whitespace(*c))
                .count();
            let mut lines = vec![];
            for line in self.lines() {
                lines.push(format!("{prefix}{text}\n",
                    prefix = prefix,
                    text = line[remove_indent..].to_string()
                ));
            }
            format!("{}", lines.iter().format(""))
        } else {
            "".to_string()
        }
    }
}

impl Reindentable for Option<String> {
    fn reindent(&self, prefix: &str) -> String {
        match *self {
            None => "".to_string(),
            Some(ref string) => string.reindent(prefix)
        }
    }
}

pub struct FromJSONError {
    pub expected: String,
    pub got: String,
}

pub trait FromJSON: Sized {
    fn import(json: &JSON) -> Result<Self, FromJSONError>; // FIXME: Having error messages would be nicer.
}
impl FromJSON for bool {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        match value.as_bool() {
            None => Err(FromJSONError {
                expected: "Boolean".to_string(),
                got: value.dump()
            }),
            Some(ref s) => Ok(*s)
        }
    }
}
impl FromJSON for f64 {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        match value.as_f64() {
            None => Err(FromJSONError {
                expected: "Number".to_string(),
                got: value.dump()
            }),
            Some(ref s) => Ok(*s)
        }
    }
}
impl FromJSON for String {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        match value.as_str() {
            None => Err(FromJSONError {
                expected: "String".to_string(),
                got: value.dump()
            }),
            Some(ref s) => Ok(s.to_string())
        }
    }
}
impl<T> FromJSON for Vec<T> where T: FromJSON {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        match *value {
            JSON::Array(ref array) => {
                let mut result = Vec::with_capacity(array.len());
                for item in array {
                    let imported = FromJSON::import(item)?;
                    result.push(imported);
                }
                Ok(result)
            },
            _ => Err(FromJSONError {
                expected: "Array".to_string(),
                got: value.dump(),
            })
        }
    }
}
impl<T> FromJSON for Option<T> where T: FromJSON {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        if value.is_null() {
            return Ok(None)
        }
        T::import(value)
            .map(Some)
    }
}
impl<T> FromJSON for Box<T> where T: FromJSON {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        T::import(value)
            .map(Box::new)
    }
}

pub trait ToJSON {
    fn export(&self) -> JSON;
}
impl ToJSON for String {
    fn export(&self) -> JSON {
        json::from(self.clone())
    }
}
impl ToJSON for bool {
    fn export(&self) -> JSON {
        json::from(self.clone())
    }
}
impl ToJSON for u32 {
    fn export(&self) -> JSON {
        json::from(self.clone())
    }
}
impl<T> ToJSON for Vec<T> where T: ToJSON {
    fn export(&self) -> JSON {
        let vec = self.iter()
            .map(ToJSON::export)
            .collect();
        JSON::Array(vec)
    }    
}
impl<T> ToJSON for Option<T> where T: ToJSON {
    fn export(&self) -> JSON {
        match *self {
            None => JSON::Null,
            Some(ref x) => x.export()
        }
    }
}
impl<T> ToJSON for Box<T> where T: ToJSON {
    fn export(&self) -> JSON {
        (**self).export()
    }
}
