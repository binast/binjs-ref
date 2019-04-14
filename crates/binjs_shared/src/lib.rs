#[macro_use]
extern crate downcast_rs;
extern crate itertools;
extern crate serde_json;
#[macro_use]
extern crate log;
#[macro_use]
extern crate serde;

pub mod ast;
pub use ast::Node;

pub mod mru;
mod shared_string;
pub use shared_string::SharedString;

#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Offset(pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VisitMe<T> {
    /// Visit the children of this node.
    ///
    /// The value is a guard
    HoldThis(T),

    /// Skip the children of this node, skip the `exit_` method, return immediately.
    DoneHere,
}

shared_string!(
    /// An identifier, inside the grammar.
    pub IdentifierName
);
pub type Identifier = IdentifierName;

shared_string!(
    /// A property, inside the grammar.
    pub PropertyKey
);

shared_string!(
    /// An interface *of* the grammar.
    pub InterfaceName
);

shared_string!(
    /// A field name *of* the grammar.
    pub FieldName
);

/// A container for f64 values that implements an *arbitrary*
/// total order, equality relation, hash.
#[derive(Clone, Debug, Copy, Deserialize, Serialize)]
pub struct F64(f64);
impl From<f64> for F64 {
    fn from(value: f64) -> F64 {
        F64(value)
    }
}
impl Into<f64> for F64 {
    fn into(self) -> f64 {
        self.0
    }
}
impl PartialOrd for F64 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.to_bits().cmp(&other.0.to_bits()))
    }
}
impl Ord for F64 {
    // An arbitrary total order on F64.
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl PartialEq for F64 {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}
impl Eq for F64 {} // Bitwise equality on F64.
impl std::hash::Hash for F64 {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.0.to_bits().hash(state)
    }
}

/// The type of paths used most commonly throughout our code.
pub type IOPath = ast::Path<
    InterfaceName,
    (
        /* child index */ usize,
        /* field name */ FieldName,
    ),
>;

/// The type of path items used most commonly throughout our code.
pub type IOPathItem = ast::PathItem<
    InterfaceName,
    (
        /* child index */ usize,
        /* field name */ FieldName,
    ),
>;

/// From a set of macro definitions, derive a module with a set of matching `&'static str`.
///
/// Example:
///
/// ```rust,no_run
/// # #[macro_use]
/// # extern crate binjs_shared;
/// # fn main() {
///
/// const_with_str! {
///    const FOO: usize = 0;
///    mod my_strings;
/// }
///
/// # }
/// ```
///
/// will produce
///
/// ```rust,no_run
///
/// const FOO: usize = 0;
/// mod my_strings {
///   const FOO: &'static str = "0";
/// }
/// ```
#[macro_export]
macro_rules! const_with_str {
    (
        $(#[$outer:meta])*
        $(const $const_name: ident: $ty: ty = $init: expr;)*
        mod $mod_name: ident;
    ) => {
        // Documentation comments are actually syntactic sugar for #[doc="Some documentation comment"].
        // We capture them and insert them in the generated macro.
        $(#[$outer])*
        mod $mod_name {
            $(pub const $const_name: &'static str = stringify!($init);)*
        }
        $(const $const_name: $ty = $init;)*
    }
}
