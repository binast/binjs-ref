use std;
use std::hash::Hash;
use std::ops::Deref;
use std::rc::Rc;


/// An implementation of strings that may easily be shared without copies.
///
/// Static strings may be imported without copy, while dynamic strings
/// are converted into `Rc`.
#[derive(Clone, Debug, Eq, Ord)]
pub enum SharedString {
    Dynamic(Rc<String>),
    Static(&'static str)
}
impl Deref for SharedString {
    type Target = str;
    fn deref(&self) -> &str {
        match *self {
            SharedString::Static(ref s) => *s,
            SharedString::Dynamic(ref rc) => rc.deref()
        }
    }
}
impl PartialEq for SharedString {
    fn eq(&self, other: &SharedString) -> bool {
        self.deref() == other.deref()
    }
}
impl PartialEq<str> for SharedString {
    fn eq(&self, other: &str) -> bool {
        self.deref().eq(other)
    }
}
impl<'a> PartialEq<&'a str> for SharedString {
    fn eq(&self, other: &&'a str) -> bool {
        self.deref().eq(*other)
    }
}
impl PartialOrd for SharedString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.deref().partial_cmp(other.deref())
    }
}
impl Hash for SharedString {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        self.deref().hash(hasher)
    }
}
impl std::fmt::Display for SharedString {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        self.deref().fmt(formatter)
    }
}
impl Default for SharedString {
    fn default() -> Self {
        SharedString::Static("<uninitialized SharedString>")
    }
}
impl SharedString {
    pub fn as_str(&self) -> &str {
        self.deref()
    }
    pub fn from_str(value: &'static str) -> Self {
        SharedString::Static(value)
    }
    pub fn from_rc(value: Rc<String>) -> Self {
        SharedString::Dynamic(value)
    }
    pub fn from_string(value: String) -> Self {
        SharedString::Dynamic(Rc::new(value))
    }
}