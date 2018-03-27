extern crate json;
extern crate itertools;

mod json_conversion;
pub use json_conversion::*;

pub mod ast;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Offset(pub u32);
