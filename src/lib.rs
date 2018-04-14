//! A reference implementation for the BinJS binary source format for JavaScript.
//!
//! # About BinJS
//!
//! As websites become more sophisticated, the amount of JavaScript source code keeps
//! increasing. By itself, this is not a problem. However, with the amount of code
//! loaded by large websites such as Facebook's chat, it is now common to witness
//! page loads during which both the loading and the parsing of JS code can take
//! several seconds each – this is assuming a fast connection, and taking into
//! account that the code is both compressed and optimized for loading and parsing
//! speed.
//!
//! There is no reason to believe that the size of JS code will decrease or will
//! even stop increasing, nor that every large webdev team has the means to profile
//! loading and parsing speed of all their code.
//!
//! This crate offers a (WIP) reference implementation for BinJS, a vendor-neutral
//! JavaScript format designed to optimize parsing speed and, when possible,
//! loading speed.

#![feature(box_patterns)]
#![feature(iter_rfind)]

extern crate binjs_generic;
extern crate binjs_es6;
extern crate binjs_io;
extern crate binjs_meta;
extern crate binjs_shared;

#[allow(unused_imports)]
#[macro_use]
extern crate assert_matches;
extern crate brotli;
#[cfg(test)]
extern crate env_logger;
extern crate flate2;
extern crate itertools;
#[macro_use]
extern crate json;
#[macro_use]
extern crate log;
extern crate rand;
extern crate lzw;
extern crate vec_map;

/// Working with a generic (i.e. JSON-based) representation
/// of the JavaScript AST.
pub mod generic {
    pub use binjs_generic::*;
    pub use binjs_shared::*;
}

/// Working with a specialized (i.e. strongly-typed)
/// representation of the JavaScript AST.
pub mod specialized {
    pub mod es6 {
        pub use binjs_es6::*;
    }
}

/// Language-agnostic and representation-agnostic primitives for
/// tokenization and detokenization of ASTs, including (de)compression.
///
/// Weakly-typed constructs (using a dynamic language specification and
/// a JSON AST) based on these primitives are in module `generic::io`.
/// Strongly-typed constructs (using a strongly-typed AST) based on
/// these primitives will appear in modules `specialized::*::io`.
pub mod io {
    pub use binjs_io::*;
}

/// Dynamic manipulation of the specifications.
pub mod meta {
    pub use binjs_meta::*;
}

/// Parsing source JavaScript.
pub mod source;

/// Misc utilities.
pub mod util;
