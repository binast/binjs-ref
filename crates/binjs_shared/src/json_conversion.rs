use {IdentifierName, PropertyKey, SharedString};

use json;
use json::JsonValue as JSON;

#[derive(Debug)]
pub struct FromJSONError {
    pub expected: String,
    pub got: String,
}

/// A data structure that may be imported from JSON.
pub trait FromJSON: Sized {
    fn import(json: &JSON) -> Result<Self, FromJSONError>;
}
impl FromJSON for bool {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        match value.as_bool() {
            None => Err(FromJSONError {
                expected: "Boolean".to_string(),
                got: value.to_string(),
            }),
            Some(ref s) => Ok(*s),
        }
    }
}
impl FromJSON for f64 {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        // JSON.as_f64() doesn't keep the precision.
        match value {
            JSON::Number(n) => {
                if n.is_nan() {
                    Ok(std::f64::NAN)
                } else {
                    let (positive, mantissa, exponent) = n.as_parts();
                    let as_str = if positive {
                        format!("{}e{}", mantissa, exponent)
                    } else {
                        format!("-{}e{}", mantissa, exponent)
                    };
                    Ok(as_str.parse::<f64>().expect("cannot parse the number!"))
                }
            }
            _ => Err(FromJSONError {
                expected: "Number".to_string(),
                got: value.to_string(),
            }),
        }
    }
}
impl FromJSON for u32 {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        match value.as_u32() {
            None => Err(FromJSONError {
                expected: "Number".to_string(),
                got: value.to_string(),
            }),
            Some(ref s) => Ok(*s as u32),
        }
    }
}
impl FromJSON for String {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        match value.as_str() {
            None => Err(FromJSONError {
                expected: "String".to_string(),
                got: value.to_string(),
            }),
            Some(ref s) => Ok(s.to_string()),
        }
    }
}
impl FromJSON for SharedString {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        match value.as_str() {
            None => Err(FromJSONError {
                expected: "String".to_string(),
                got: value.to_string(),
            }),
            Some(ref s) => Ok(SharedString::from_string(s.to_string())),
        }
    }
}
impl FromJSON for IdentifierName {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        match value.as_str() {
            None => Err(FromJSONError {
                expected: "Identifier or IdentifierName".to_string(),
                got: value.to_string(),
            }),
            Some(ref s) => Ok(IdentifierName::from_string(s.to_string())),
        }
    }
}
impl FromJSON for PropertyKey {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        match value.as_str() {
            None => Err(FromJSONError {
                expected: "PropertyKey".to_string(),
                got: value.to_string(),
            }),
            Some(ref s) => Ok(PropertyKey::from_string(s.to_string())),
        }
    }
}
impl<T> FromJSON for Vec<T>
where
    T: FromJSON,
{
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        match *value {
            JSON::Array(ref array) => {
                let mut result = Vec::with_capacity(array.len());
                for item in array {
                    let imported = FromJSON::import(item)?;
                    result.push(imported);
                }
                Ok(result)
            }
            _ => Err(FromJSONError {
                expected: "Array".to_string(),
                got: value.to_string(),
            }),
        }
    }
}
impl<T> FromJSON for Option<T>
where
    T: FromJSON,
{
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        if value.is_null() {
            return Ok(None);
        }
        T::import(value).map(Some)
    }
}
impl<T> FromJSON for Box<T>
where
    T: FromJSON,
{
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        T::import(value).map(Box::new)
    }
}

pub trait ToJSON {
    fn export(&self) -> JSON;
}
impl ToJSON for str {
    fn export(&self) -> JSON {
        json::from(self)
    }
}
impl ToJSON for bool {
    fn export(&self) -> JSON {
        json::from(self.clone())
    }
}
impl ToJSON for f64 {
    fn export(&self) -> JSON {
        json::from(self.clone())
    }
}
impl ToJSON for u32 {
    fn export(&self) -> JSON {
        json::from(self.clone())
    }
}
impl<T> ToJSON for Vec<T>
where
    T: ToJSON,
{
    fn export(&self) -> JSON {
        let vec = self.iter().map(ToJSON::export).collect();
        JSON::Array(vec)
    }
}
impl<T> ToJSON for Option<T>
where
    T: ToJSON,
{
    fn export(&self) -> JSON {
        match *self {
            None => JSON::Null,
            Some(ref x) => x.export(),
        }
    }
}
impl<T> ToJSON for Box<T>
where
    T: ToJSON,
{
    fn export(&self) -> JSON {
        (**self).export()
    }
}

impl FromJSON for ::Offset {
    fn import(value: &JSON) -> Result<Self, FromJSONError> {
        u32::import(value).map(::Offset)
    }
}
impl ToJSON for ::Offset {
    fn export(&self) -> JSON {
        self.0.export()
    }
}

impl ToJSON for IdentifierName {
    fn export(&self) -> JSON {
        self.as_str().to_string().export()
    }
}

impl ToJSON for PropertyKey {
    fn export(&self) -> JSON {
        self.as_str().to_string().export()
    }
}

impl ToJSON for SharedString {
    fn export(&self) -> JSON {
        self.as_str().to_string().export()
    }
}
