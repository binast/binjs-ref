extern crate easter;
extern crate esprit;
extern crate joker;
extern crate serde_json;

/// Encoding/decoding an AST for an EcmaScript source represented in ESTree format to/from BinTree.
pub mod estree;

/// Encoding/decoding a BinTree to/from a stream of bytes.
pub mod bintree;

mod util;

/*
pub mod decode;
pub mod encode;

pub mod ast;
mod atoms;
mod kind;
mod varnum;
*/