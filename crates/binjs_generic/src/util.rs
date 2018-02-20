use json::JsonValue as JSON;
use rand;

/// Return a string describing a JSON value
/// without dumping the entire AST.
/// 
/// ```
/// # #[macro_use] extern crate json;
/// extern crate binjs_generic;
/// # fn main() {
/// 
/// // Objects
/// assert_eq!(&binjs_generic::util::type_of(&object!{}), "Object");
/// assert_eq!(&binjs_generic::util::type_of(&object!{"foo" => 1}), "Object");
///
/// // Arrays
/// assert_eq!(&binjs_generic::util::type_of(&array![]), "Array");
/// assert_eq!(&binjs_generic::util::type_of(&array![1, 2, 3]), "Array");
///
/// // Strings
/// let strings = [
///     json::from("some string"),
///     json::from(""), // Short string
///     json::from("111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"), // Long string
/// ];
/// for string in &strings {
///     assert_eq!(&binjs_generic::util::type_of(string), "String");
/// }
///
/// // Numbers
/// let numbers = [
///     json::from(1),
///     json::from(0.0)
/// ];
/// for num in &numbers {
///     assert_eq!(&binjs_generic::util::type_of(num), "Number");
/// }
///
/// // Booleans
/// let booleans = [
///     json::from(false),
///     json::from(true),
/// ];
/// for bool in &booleans {
///     assert_eq!(&binjs_generic::util::type_of(bool), "Bool")
/// }
///
/// // Null
/// assert_eq!(&binjs_generic::util::type_of(&json::JsonValue::Null), "Null")
/// # }
/// ```
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

/// Return a random item from a slice.
pub fn pick<'a, T: rand::Rng, U,>(rng: &mut T, slice: &'a [U]) -> &'a U {
    let index = rng.gen_range(0, slice.len());
    &slice[index]
}
