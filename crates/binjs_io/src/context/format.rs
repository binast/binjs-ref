// Fake module. This is only a placeholder for documentation.

// As of this writing, doc inclusion only works on Nightly compiler.
#![cfg_attr(feature = "unstable", doc(include = "../../../../spec/context.md"))]

#![cfg_attr(not(feature = "unstable"), doc = "To generate the format spec documentation, please use `cargo doc --features unstable`. This requires Rust Nightly.")]
