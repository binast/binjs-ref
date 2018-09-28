use binjs_shared;
use util::Counter;

use std;
use std::cell::RefCell;
use std::hash::{ Hash, Hasher };
use std::rc::Rc;

/// A constant used to initialize the maximal path depth.
/// Changing it will only affect performance by increasing
/// or decreasing the number of reallocations inside the `ASTPath`.
pub const EXPECTED_PATH_DEPTH: usize = 2048;

/// A constant used to initialize the maximal scope depth.
/// Changing it will only affect performance by increasing
/// or decreasing the number of reallocations inside the `ScopePath`.
pub const EXPECTED_SCOPE_DEPTH: usize = 128;

pub type ASTPath = binjs_shared::ast::Path<Rc<String>, usize>;
pub type ASTPathItem = binjs_shared::ast::PathItem<Rc<String>, usize>;

pub type ScopePath = binjs_shared::ast::Path<ScopeIndex, ()>;

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
    /// A non-identifier string. may be `None` if
    /// we deal with a `string?` (in webidl) and the
    /// string is `null`.
    String(Option<Rc<String>>),

    /// A number. May be `None` if we're dealing
    /// with a `number?` (in webidl) and the number
    /// is `null`.
    ///
    /// FIXME: we're currently using `F64` rather than
    /// `f64` because we need the ability to order
    /// trees. We should find a better solution.
    Number(Option<F64>),

    /// A boolean. May be `None` if we're dealing
    /// with a `bool?` (in webidl) and the value
    /// is `null`.
    Bool(Option<bool>),

    /// A list, along with its length May be `None` if we're dealing
    /// with a `FrozenArray<...>?` (in webidl) and the value
    /// is `null`.
    List(Option<u32>),

    Tag(Tag),

    /// Scope. Any `Declare` within the `Scope`
    /// stays in the `Scope`.
    Scope(ScopeIndex),

    /// Declare a variable throughout the current `Scope`.
    Declare(Rc<String>),

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
    fn enter_label(&mut self, _label: &Label, _path: &ASTPath, _scopes: &ScopePath) -> Result<(), Self::Error> {
        Ok(())
    }
    fn exit_label(&mut self, _label: &Label, _path: &ASTPath, _scopes: &ScopePath) -> Result<(), Self::Error> {
        Ok(())
    }
}
pub trait WalkTree {
    fn walk<V>(&self, visitor: &mut V, path: &mut ASTPath, scopes: &mut ScopePath) -> Result<(), V::Error>
        where V: Visitor;
}

impl WalkTree for SubTree {
    fn walk<V>(&self, visitor: &mut V, path: &mut ASTPath, scopes: &mut ScopePath) -> Result<(), V::Error>
        where V: Visitor
    {
        visitor.enter_label(&self.label, path, scopes)?;
        match self.label {
            Label::Tag(ref tag) => {
                path.enter_interface(tag.0.clone());
                for (index, child) in self.children.iter().enumerate() {
                    path.enter_field(index);
                    child.walk(visitor, path, scopes)?;
                    path.exit_field(index);
                }
                path.exit_interface(tag.0.clone())
            }
            Label::Scope(ref scope) => {
                scopes.enter_interface(scope.clone());
                scopes.enter_field(());
                for child in self.children.iter() {
                    child.walk(visitor, path, scopes)?;
                }
                scopes.exit_field(());
                scopes.exit_interface(scope.clone());
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
    fn walk<V>(&self, visitor: &mut V, path: &mut ASTPath, scopes: &mut ScopePath) -> Result<(), V::Error>
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
