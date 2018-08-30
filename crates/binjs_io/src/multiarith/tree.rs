use util::Counter;

use std;
use std::cell::RefCell;
use std::hash::{ Hash, Hasher };
use std::rc::Rc;

/// A constant used to initialize the maximal path depth.
/// Changing it will only affect performance.
pub const EXPECTED_PATH_DEPTH: usize = 2048;

/// A constant used to initialize the maximal scope depth.
/// Changing it will only affect performance.
pub const EXPECTED_SCOPE_DEPTH: usize = 128;

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Path<T> where T: Clone {
    items: Vec<T>
}
impl<T> Path<T> where T: Clone {
    pub fn with_capacity(len: usize) -> Self {
        Path {
            items: Vec::with_capacity(len)
        }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn push(&mut self, value: T) {
        self.items.push(value);
    }

    pub fn pop(&mut self) {
        self.items.pop()
            .expect("Popping from an empty path");
    }

    pub fn last(&self) -> Option<&T> {
        self.items.last()
    }

    pub fn tail(&self, len: usize) -> Self {
        let start = if self.items.len() <= len {
            0
        } else {
            self.items.len() - len
        };
        let (_, tail) = &self.items[..].split_at(start);
        let tail = tail.iter()
            .cloned()
            .collect();
        Path {
            items: tail
        }
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
        self.items.iter()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Tag(Rc<String>);
impl Default for Tag {
    fn default() -> Tag {
        Self::new("")
    }
}
impl Tag {
    pub fn new(s: &str) -> Self {
        Tag(Rc::new(s.to_string()))
    }
    pub fn content(&self) -> &Rc<String> {
        &self.0
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord)]
pub enum Label {
    /// A non-identifier string (may be `None` if string is `null`).
    String(Option<Rc<String>>),

    /// A number (may be `None` if number is `null`).
    Number(Option<F64>),

    /// A boolean (may be `None` if boolean is `null`).
    Bool(Option<bool>),

    /// A list, along with its length
    /// (or `None` if the list is `null`).
    List(Option<u32>),

    Tag(Tag),

    /// Scope. Any `Declare` within the `Scope`
    /// stays in the `Scope`.
    Scope(ScopeIndex),

    /// Declare a variable throughout the current `Scope`.
    Declare(Option<Rc<String>>),

    /// Reference a variable throughout the current `Scope`.
    LiteralReference(Option<Rc<String>>),
}

impl Eq for Label { /* Yes, it's probably not entirely true for f64 */ }
impl Hash for Label { /* Again, not entirely true for f64 */
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        use self::Label::*;
        match *self {
            String(ref s) => s.hash(state),
            Bool(ref b) => b.hash(state),
            List(ref l) => l.hash(state),
            Tag(ref s) => s.hash(state),
            Number(ref num) => num.map(|x| f64::to_bits(x.0)).hash(state), // Not really true for a f64.
            Scope(ref s) => s.hash(state),
            Declare(ref d) => d.hash(state),
            LiteralReference(ref r) => r.hash(state),
        }
    }
}


#[derive(Clone, Debug)]
pub struct SubTree {
    pub label: Label,
    pub children: Vec<SharedTree>,
}
pub type SharedTree = Rc<RefCell<SubTree>>;

pub trait Visitor {
    type Error;
    fn enter_label(&mut self, _label: &Label, _path: &Path<(Tag, usize)>, _scopes: &Path<ScopeIndex>) -> Result<(), Self::Error> {
        Ok(())
    }
    fn exit_label(&mut self, _label: &Label, _path: &Path<(Tag, usize)>, _scopes: &Path<ScopeIndex>) -> Result<(), Self::Error> {
        Ok(())
    }
}
pub trait WalkTree {
    fn walk<V>(&self, visitor: &mut V, path: &mut Path<(Tag, usize)>, scopes: &mut Path<ScopeIndex>) -> Result<(), V::Error>
        where V: Visitor;
}

impl WalkTree for SubTree {
    fn walk<V>(&self, visitor: &mut V, path: &mut Path<(Tag, usize)>, scopes: &mut Path<ScopeIndex>) -> Result<(), V::Error>
        where V: Visitor
    {
        visitor.enter_label(&self.label, path, scopes)?;
        match self.label {
            Label::Tag(ref tag) => {
                for (index, child) in self.children.iter().enumerate() {
                    path.push((tag.clone(), index));
                    child.walk(visitor, path, scopes)?;
                    path.pop();
                }
            }
            Label::Scope(ref scope) => {
                scopes.push(scope.clone());
                for child in self.children.iter() {
                    child.walk(visitor, path, scopes)?;
                }
                scopes.pop();
            }
            _ => {
                for child in self.children.iter() {
                    child.walk(visitor, path, scopes)?;
                }
            }
        }
        visitor.exit_label(&self.label, path, scopes)?;
        Ok(())
    }
}

impl WalkTree for SharedTree {
    fn walk<V>(&self, visitor: &mut V, path: &mut Path<(Tag, usize)>, scopes: &mut Path<ScopeIndex>) -> Result<(), V::Error>
        where V: Visitor
    {
        self.borrow().walk(visitor, path, scopes)
    }
}

/// A trivial wrapping of f64 with Hash and Eq.
///
/// FIXME: Check whether it's still needed
#[derive(Clone, Debug, PartialEq, PartialOrd, Copy)]
pub struct F64(pub f64);
impl Eq for F64 { } // Not strictly true.
impl Ord for F64 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
} // Not strictly true.
impl Hash for F64 {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.0.to_bits().hash(state)
    }
}

/// A simple counter for giving a unique ID to scopes.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ScopeIndex(usize);
impl Counter for ScopeIndex {
    fn internal_make(value: usize) -> Self {
        ScopeIndex(value)
    }
}
