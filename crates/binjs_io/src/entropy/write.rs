
use entropy::{ EncodingModel, Model };
use entropy::predict::{ Symbol };
use entropy::tree:: { EXPECTED_PATH_DEPTH, EXPECTED_SCOPE_DEPTH, ASTPath, F64, Label, ScopeIndex, ScopePath, SharedTree, SubTree, Visitor, WalkTree };

use io::TokenWriter;
use ::TokenWriterError;
use util::GenericCounter;

use binjs_shared:: { FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString };

use range_encoding::opus;

use std;
use std::cell::RefCell;
use std::io::Read;
use std::rc::Rc;

/// An array of 16 bytes. Used for initializing stuff on the stack.
const EMPTY_16_BYTES_ARRAY : [u8; 16] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

const HEADER_STARTING_CAPACITY : usize = 10 * 1024;

pub struct TreeTokenWriter<'a> {
    root: SharedTree,
    scope_counter: GenericCounter<ScopeIndex>,
    model: &'a Model,
    options: Options,
    encoder: opus::Writer<Vec<u8>>,
}

struct Compressor<'a> {
    model: Box<EncodingModel>,
    encoder: &'a mut opus::Writer<Vec<u8>>,
    options: Options,
}

impl<'a> Compressor<'a> {
    fn encode_uncompressed_bit(&mut self, value: bool) -> Result<(), std::io::Error> {
        let symbol = self.model.iso_bit_frequency_for_encoding(value)
            .unwrap();
        let mut distribution = symbol.distribution.borrow_mut();
        self.encoder.symbol(symbol.index, &mut distribution)?;
        Ok(())
    }
    fn encode_uncompressed_bits(&mut self, bits: &[bool]) -> Result<(), std::io::Error>
    {
        for bit in bits {
            self.encode_uncompressed_bit(*bit)?;
        }
        Ok(())
    }
    fn encode_uncompressed_varnum(&mut self, value: Option<u32>)-> Result<(), std::io::Error> {
        use bytes::varnum::*;

        // A few shennanigans to avoid heap allocating needlessly.
        let mut buf = EMPTY_16_BYTES_ARRAY;
        let mut buf = std::io::Cursor::new(&mut buf as &mut [u8]);
        buf.write_maybe_varnum(value)?;
        for byte in buf.bytes().map(Result::unwrap) { // Read from a Cursor's Bytes cannot fail.
            for shift in 0..7 {
                let bit = (byte & 1 << shift) != 0;
                self.encode_uncompressed_bit(bit)?;
            }
        }
        Ok(())
    }

    fn encode_uncompressed_float(&mut self, value: Option<f64>)-> Result<(), std::io::Error> {
        use bytes::float::*;

        // A few shennanigans to avoid heap allocating needlessly.
        let mut buf = EMPTY_16_BYTES_ARRAY;
        let mut buf = std::io::Cursor::new(&mut buf as &mut [u8]);
        buf.write_maybe_varfloat(value)?;
        for byte in buf.bytes() {
            let byte = byte.unwrap(); // Read from a Cursor cannot fail.
            for shift in 0..7 {
                let bit = (byte & 1 << shift) != 0;
                self.encode_uncompressed_bit(bit)?;
            }
        }
        Ok(())
    }

    fn encode_uncompressed_frequency(&mut self, value: u32) -> Result<(), std::io::Error> {
        self.encode_uncompressed_varnum(Some(value))
    }

    fn encode_value<F>(&mut self, name: &str, symbol: Symbol, f: F) -> Result<(), std::io::Error>
        where F: FnOnce(&mut Self) -> Result<(), std::io::Error>
    {
        let mut distribution = symbol.distribution.borrow_mut();

        let requirements = distribution.requirements_for_index(symbol.index)
            .expect("Symbol is not in the distribution");

        // Without the layout of the distribution, we can't decode the symbol.
        // So, if this the first time we encounter this distribution, write
        // that layout.
        if requirements.distribution_total() {
            if !self.options.inline_dictionaries {
                debug!(target: "entropy", "Skipping encoding of CDF for {}", name);
            }
            for width in distribution.widths() {
                self.encode_uncompressed_frequency(width)?;
            }
            // Terminal 0.
            self.encode_uncompressed_frequency(0)?;
        }

        // Now write the index of the symbol.
        self.encoder.symbol(symbol.index, &mut *distribution)?;

        // Now, if this is the first time we encounter this symbol, we need to write its definition.
        if requirements.symbol() {
            if !self.options.inline_dictionaries {
                debug!(target: "entropy", "Skipping encoding of symbol {}", name);
            } else {
                debug!(target: "entropy", "Encoding symbol {}", name);
                // First, write the symbol itself.
                f(self)?;
                // Then, write its frequency.
                let segment = distribution.at_index(symbol.index).unwrap();
                    // We would have failed in `self.encoder.symbol` if `distribution.at_index` failed.
                self.encode_uncompressed_frequency(segment.width())?;
            }
        }
        if requirements.distribution_total() {
            // Finally, if needed, write the total frequency.
            if !self.options.inline_dictionaries {
                debug!(target: "entropy", "Skipping encoding of CDT for {}", name);
            } else {
                debug!(target: "entropy", "Encoding CDT for {}", name);
                self.encode_uncompressed_frequency(distribution.width())?;
            }
        }
        Ok(())
    }

    fn encode_uncompressed_string(&mut self, value: &SharedString) -> Result<(), std::io::Error> {
        self.encode_uncompressed_varnum(Some(value.len() as u32))?;
        for byte in value.bytes() {
            for shift in 0..7 {
                let bit = (byte & 1 << shift) != 0;
                self.encode_uncompressed_bit(bit)?;
            }
        }
        Ok(())
    }
    fn encode_uncompressed_maybe_string(&mut self, value: &Option<SharedString>) -> Result<(), std::io::Error> {
        match value {
            None => self.encode_uncompressed_varnum(None),
            Some(ref s) => self.encode_uncompressed_string(s)
        }
    }
}

impl<'a> Visitor for Compressor<'a> {
    type Error = std::io::Error;
    fn enter_label(&mut self, label: &Label, path: &ASTPath, scopes: &ScopePath) -> Result<(), Self::Error> {
        match label {
            Label::Tag(ref tag) => {
                let symbol = self.model.tag_frequency_for_encoding(tag, path)
                    .expect("Could not compute tag frequency");
                if !self.options.encode_tags {
                    return Ok(())
                }
                self.encode_value("tag", symbol, move |me|
                    me.encode_uncompressed_string(tag.as_shared_string()))?;
            }
            Label::Number(ref value) => {
                let symbol = self.model.number_frequency_for_encoding(value, path)
                    .expect("Could not compute number frequency");
                if !self.options.encode_numbers {
                    return Ok(())
                }
                self.encode_value("number", symbol, move |me| me.encode_uncompressed_float(value.map(|x| x.0.clone())))?;
            }
            Label::List(ref len) => {
                let symbol = self.model.list_length_frequency_for_encoding(len, path)
                    .expect("Could not compute list length frequency");
                if !self.options.encode_list_lengths {
                    return Ok(())
                }
                self.encode_value("list", symbol, move |me| me.encode_uncompressed_varnum(len.map(|x| x.clone())))?;
            }
            Label::String(ref string) => {
                let symbol = self.model.string_frequency_for_encoding(string, path)
                    .expect("Could not compute string frequency");
                if !self.options.encode_strings {
                    return Ok(())
                }
                self.encode_value("string", symbol, move |me| me.encode_uncompressed_maybe_string(string))?;
            }
            Label::Bool(ref value) => {
                let symbol = self.model.bool_frequency_for_encoding(value, path)
                    .expect("Could not compute bool frequency");
                if !self.options.encode_bools {
                    return Ok(())
                }
                self.encode_value("bool", symbol, move |me| {
                    match value { // FIXME: Would be more efficient if we knew ahead of time whether it's a bool or an Option<bool>
                        None => me.encode_uncompressed_bits(&[true, true]),
                        Some(true) => me.encode_uncompressed_bits(&[false, true]),
                        Some(false) => me.encode_uncompressed_bits(&[false, false]),
                    }
                })?;
            }
            Label::Declare(ref string) | Label::LiteralReference(Some(ref string)) => {
                let symbol = self.model.identifier_frequency_for_encoding(string, scopes)
                    .expect("Could not compute identifier frequency");
                if !self.options.encode_identifiers {
                    return Ok(())
                }
                self.encode_value("id", symbol, move |me| {
                    let string = string.as_shared_string()
                        .clone();
                    me.encode_uncompressed_maybe_string(&Some(string))
                })?
            }
            _ => {
                warn!(target: "entropy", "Skipping serialization of label {:?} (not implemented yet)", label);
            }
        }
        Ok(())
    }
}

impl<'a> TreeTokenWriter<'a> {
    pub fn new(model: &'a Model, options: Options) -> Self {
        Self {
            scope_counter: GenericCounter::new(),
            model,
            options,
            encoder: opus::Writer::new(Vec::new()),
            root: Rc::new(RefCell::new(SubTree {
                label: Label::String(None),
                children: vec![]
            }))
        }
    }
    fn new_tree(&mut self, tree: SubTree) -> Result<SharedTree, TokenWriterError> {
        self.root = Rc::new(RefCell::new(tree));
        Ok(self.root.clone())
    }
}


impl<'a> TokenWriter for TreeTokenWriter<'a> {
    type Statistics = usize; // Placeholder
    type Tree = SharedTree;
    type Data = Vec<u8>;

    fn tagged_tuple(&mut self, tag: &InterfaceName, children: &[(FieldName, Self::Tree)]) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Tag(tag.clone()),
            children: children.iter()
                .map(|(_, tree)| tree.clone())
                .collect()
        })
    }

/*
    fn tagged_scope_tuple(&mut self, tag: &str, children: &[(&str, Self::Tree)]) -> Result<Self::Tree, TokenWriterError> {
        let tuple = self.tagged_tuple(tag, children)?;
        let index = self.scope_counter.next();
        self.new_tree(SubTree {
            label: Label::Scope(index),
            children: vec![tuple]
        })
    }
*/

    fn identifier_name(&mut self, name: Option<&IdentifierName>) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }

    fn offset(&mut self) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }


    fn unsigned_long(&mut self, value: u32) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }

    fn bool(&mut self, value: Option<bool>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Bool(value),
            children: vec![]
        })
    }

    fn float(&mut self, value: Option<f64>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Number(value.map(F64)),
            children: vec![]
        })
    }

    fn string(&mut self, value: Option<&SharedString>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::String(value.cloned()),
            children: vec![]
        })
    }

    fn string_enum(&mut self, value: &SharedString) -> Result<Self::Tree, TokenWriterError> {
        self.tagged_tuple(&InterfaceName(value.clone()), &[])
    }

    fn list(&mut self, children: Vec<Self::Tree>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::List(Some(children.len() as u32)),
            children
        })
    }

    fn done(mut self) -> Result<(Self::Data, Self::Statistics), TokenWriterError> {
        let model = self.model.encoding(&self.root);
        let root = self.root.clone();
        {
            let mut compressor = Compressor {
                model,
                options: self.options.clone(),
                encoder: &mut self.encoder
            };
            root.walk(&mut compressor,
                &mut ASTPath::with_capacity(EXPECTED_PATH_DEPTH),
                &mut ScopePath::with_capacity(EXPECTED_SCOPE_DEPTH)
            ).unwrap(); // FIXME: Handle errors
        }
        let data = self.encoder.done()
            .unwrap(); // FIXME: Handle errors

        // let mut headers = Vec::with_capacity(HEADER_STARTING_CAPACITY);
        // 1. Write the Tags. We'll need them for everything else.
            // FIXME: In fact, we should assume that we have a built-in, ordered list of tags.
            // FIXME: Should we auto-build it now? <= Nah, followup.
        // 2. For each table
        //   a. Write list of values.
        //   b. Write list of (tag-id, child-index, instances, value-index). // FIXME: Aren't we missing the Path?
            // FIXME: In fact, we should assume that we have a built-in, ordered list of `(tag-id, child-index)`
            // FIXME: Should we auto-build it now? <= Nah, followup.
            // FIXME: We could enumerate paths.
        Ok((data, 0))
    }
}

/// Options to customize the encoding process.
#[derive(Clone)]
pub struct Options {
    pub inline_dictionaries: bool,

    /// If `true`, encode the tree structure in the file
    /// (tag names, string enums, booleans)
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_tags: bool,

    /// If `true`, encode the bools in the file
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_bools: bool,

    /// If `true`, encode numbers in the file.
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_numbers: bool,

    /// If `true`, encode list lengths in the file.
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_list_lengths: bool,

    /// If `true`, encode strings proper (i.e. not identifiers,
    /// not string enums) in the file.
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_strings: bool,

    /// If `true`, encode identifiers (i.e. not identifiers)
    /// in the file.
    ///
    /// Generally, keep it to `true`. Set it to `false`
    /// to experiment with contribution of this stuff to file size..
    pub encode_identifiers: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            inline_dictionaries: true,
            encode_tags: true,
            encode_bools: true,
            encode_strings: true,
            encode_identifiers: true,
            encode_numbers: true,
            encode_list_lengths: true,
        }
    }
}
