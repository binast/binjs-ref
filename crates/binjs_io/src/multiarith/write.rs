
use multiarith::{ ContextPredict, EncodingModel, F64, Label, Model, Path, PathPredict, Segment, ScopeIndex, SharedTree, SubTree, Tag };

use io::TokenWriter;
use ::TokenWriterError;
use util::GenericCounter;

use std;
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

const EXPECTED_PATH_DEPTH: usize = 2048;

impl Segment {
    /// Mark that a symbol has been defined in a context.
    fn mark_as_defined(&mut self) {
        self.needs_definition = false;
    }
}

/// An encoding model which starts by analyzing the full AST to determine
/// exact statistics.
pub struct ExactEncodingModel {
    /// Tag prediction based on path (depth 1 as of this writing).
    tags: PathPredict<Tag, Segment>,

    /// String prediction based on path (depth 1 as of this writing).
    strings: PathPredict<Option<Rc<String>>, Segment>,

    identifiers: ContextPredict<Option<ScopeIndex>, Rc<String>, Segment>,
}
impl ExactEncodingModel {
    fn init_path<T>(predictor: &mut PathPredict<T, usize>, value: &T, path: &mut Path)
        where T: Eq + std::hash::Hash + Clone + std::fmt::Debug
    {
        let (_, entry) = predictor.entry(path, value);
        entry.and_modify(|instances| {
                *instances += 1
            })
            .or_insert(1);
    }
    fn get_from_path<T>(predictor: &mut PathPredict<T, Segment>, value: &T, path: &Path) -> Result<Segment, ()>
        where T: Eq + std::hash::Hash + Clone + std::fmt::Debug
    {
        let segment = predictor.get_mut(path, value)
            .ok_or(())?;
        let result = segment.clone();
        segment.mark_as_defined();
        Ok(result)
    }
}
impl EncodingModel for ExactEncodingModel {
    fn string_frequency_for_encoding(&mut self, string: &Option<Rc<String>>, path: &Path) -> Result<Segment, ()> {
        Self::get_from_path(&mut self.strings, string, path)
    }
    fn tag_frequency_for_encoding(&mut self, tag: &Tag, path: &Path) -> Result<Segment, ()> {
        Self::get_from_path(&mut self.tags, tag, path)
    }
}
impl ExactEncodingModel {
    fn walk_path(tags: &mut PathPredict<Tag, /* instances */ usize>,
                 strings: &mut PathPredict<Option<Rc<String>>, usize>,
                 identifiers: &mut ContextPredict<Option<ScopeIndex>, Rc<String>, usize>,
                 subtree: &SharedTree,
                 path: &mut Path)
        {
        let borrow = subtree.borrow();
        match borrow.label {
            Label::Tag(ref tag) => {
                Self::init_path(tags, tag, path);
            }
            Label::String(ref string) => {
                Self::init_path(strings, string, path);
            }
            Label::Declare(Some(ref string)) | Label::LiteralReference(Some(ref string)) => {
                // FIXME: Find the current scope in `subtree`.
                // FIXME: Update `identifiers`
                unimplemented!();
            }
            _ => {
                warn!(target: "multiarith", "Skipping initialization of predictor for label {:?} (not implemented yet)", borrow.label);
            }
        }
        // Recur towards children.
        match borrow.label {
            Label::Tag(ref tag) => {
                for (index, child) in borrow.children.iter().enumerate() {
                    path.push(tag.clone(), index);
                    Self::walk_path(tags, strings, identifiers, child, path);
                    path.pop();
                }
            }
            _ => {
                for (index, child) in borrow.children.iter().enumerate() {
                    Self::walk_path(tags, strings, identifiers, child, path);
                }
            }
        }
    }

    pub fn new(tree: &SharedTree) -> Self {
        let mut tags = PathPredict::new(1); // FIXME: Test with other depths
        let mut strings = PathPredict::new(1); // FIXME: Test with other depths
        let mut identifiers = ContextPredict::new();

        // Initialize number of instances.
        Self::walk_path(
            &mut tags,
            &mut strings,
            &mut identifiers,
            tree,
            &mut Path::with_capacity(EXPECTED_PATH_DEPTH));

        // Deduce probabilities.

        Self {
            tags: tags.instances_to_probabilities(),
            strings: strings.instances_to_probabilities(),
            identifiers: identifiers.instances_to_probabilities(),
        }
    }
}

pub struct TreeTokenWriter<'a> {
    root: SharedTree,
    scope_counter: GenericCounter<ScopeIndex>,
    model: &'a Model,
    encoder: SymbolEncoder<Vec<u8>>,
}
impl<'a> TreeTokenWriter<'a> {
    pub fn new(model: &'a Model) -> Self {
        Self {
            scope_counter: GenericCounter::new(),
            model,
            encoder: SymbolEncoder::new(Vec::new()),
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

    fn compress(&mut self, model: &mut Box<EncodingModel>, subtree: &SharedTree, path: &mut Path) -> Result<(), std::io::Error> {
        let borrow = subtree.borrow();
        match borrow.label {
            Label::Tag(ref tag) => {
                let segment = model.tag_frequency_for_encoding(tag, path)
                    .expect("Could not compute tag frequency");
                self.encoder.append_segment(&segment)?;
                if segment.needs_definition {
                    self.encoder.flush_symbols()?;
                    warn!(target: "multiarith", "FIXME: Append definition of the current tag {:?} in {:?}", tag, path.len());
                }
            }
            Label::String(ref string) => {
                let segment = model.string_frequency_for_encoding(string, path)
                    .expect("Could not compute string frequency");
                self.encoder.append_segment(&segment)?;
                if segment.needs_definition {
                    self.encoder.flush_symbols()?;
                    warn!(target: "multiarith", "FIXME: Append definition of the current string {:?} in {:?}", string, path.len());
                }
            }
            _ => {
                warn!(target: "multiarith", "Skipping serialization of label {:?} (not implemented yet)", borrow.label);
            }
        }
        // Recur towards children.
        match borrow.label {
            Label::Tag(ref tag) => {
                for (index, child) in borrow.children.iter().enumerate() {
                    path.push(tag.clone(), index);
                    self.compress(model, child, path)?;
                    path.pop();
                }
            }
            _ => {
                for child in &borrow.children {
                    self.compress(model, child, path)?;
                }
            }
        }
        Ok(())
    }
}


impl<'a> TokenWriter for TreeTokenWriter<'a> {
    type Statistics = usize; // Placeholder
    type Tree = SharedTree;
    type Data = Vec<u8>;

    fn tagged_tuple(&mut self, tag: &str, children: &[(&str, Self::Tree)]) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Tag(Tag::new(tag)),
            children: children.iter()
                .map(|(_, tree)| tree.clone())
                .collect()
        })
    }

    fn tagged_scope_tuple(&mut self, tag: &str, children: &[(&str, Self::Tree)]) -> Result<Self::Tree, TokenWriterError> {
        let tuple = self.tagged_tuple(tag, children)?;
        let index = self.scope_counter.next();
        self.new_tree(SubTree {
            label: Label::Scope(index),
            children: vec![tuple]
        })
    }

    fn identifier_definition(&mut self, name: Option<&str>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Declare(name.map(|name|
                Rc::new(name.to_string())
            )),
            children: vec![]
        })
    }

    fn identifier_reference(&mut self, name: Option<&str>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::LiteralReference(name.map(|name|
                Rc::new(name.to_string())
            )),
            children: vec![]
        })
    }

    fn offset(&mut self) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }

    fn bool(&mut self, value: Option<bool>) -> Result<Self::Tree, TokenWriterError> {
        // FIXME: Once we start introducing dictionaries, we won't want to use strings anymore.
        let string = match value {
            None => "",
            Some(true) => "true",
            Some(false) => "false"
        };
        self.tagged_tuple(&string, &[])
    }

    fn float(&mut self, value: Option<f64>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Number(value.map(F64)),
            children: vec![]
        })
    }

    fn string(&mut self, value: Option<&str>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::String(value.map(|x| Rc::new(x.to_string()))),
            children: vec![]
        })
    }

    fn string_enum(&mut self, value: &str) -> Result<Self::Tree, TokenWriterError> {
        self.tagged_tuple(value, &[])
    }

    fn list(&mut self, children: Vec<Self::Tree>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::List(Some(children.len() as u32)),
            children
        })
    }

    fn untagged_tuple(&mut self, _: &[Self::Tree]) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }

    fn done(mut self) -> Result<(Self::Data, Self::Statistics), TokenWriterError> {
        let mut model = self.model.encoding(&self.root);
        let root = self.root.clone();
        self.compress(&mut model, &root, &mut Path::with_capacity(EXPECTED_PATH_DEPTH))
            .unwrap(); // FIXME: Handle errors
        self.encoder.flush()
            .unwrap(); // FIXME: Handle errors
        let data = self.encoder.done();

        Ok((data, 0))
    }
}

struct SymbolEncoder<W> where W: Write {
    low: u32,
    /// Length of the current segment.
    length: u32,
    /// Bits waiting to be written.
    buffer: u8,
    /// Number of bits waiting to be written.
    ///
    /// Invariant: < sizeof(buffer)
    bits_ready: u8,
    recentering_bits: u8,
    writer: W,
}
impl<W> SymbolEncoder<W> where W: Write {
    pub fn new(writer: W) -> Self {
        Self {
            low: 0,
            buffer: 0,
            bits_ready: 0,
            recentering_bits: 0,
            writer,
            length: std::u32::MAX,
        }
    }

    /// Append a new symbol, as represented by its probability segment.
    ///
    /// Note that it may take an unbounded amount of operations between
    /// the call to `append_segment` and the actual write of the symbol
    /// to the underlying writer.
    ///
    /// Consider the extreme case of a succession of symbols of probability 1
    /// (`symbol.low == 0`, `symbol.length == symbol.context_length`). As each
    /// of these symbols has a probability 1, they will be coded as 0 bits,
    /// and will therefore cause no write to the underlying writer.
    ///
    /// If you need to ensure that a symbol is written, you may want to call
    /// `flush_symbols()`.
    pub fn append_segment(&mut self, segment: &Segment) -> Result<(), std::io::Error> {
        debug!(target: "multiarith", "Adding segment {:?}", segment);

        // Update segment.
        assert!(self.length != 0);
        let context_length = segment.context_length as u64;
        self.low = self.low + (((self.length as u64 * segment.low as u64) / context_length) as u32);
        self.length = ((self.length as u64 * segment.length as u64) / context_length) as u32;
            // Assuming that `segment.length <= context_length`, this is decreasing,
            // so we can't overflow in the `as u32` conversion.

        // Since `length` is decreasing, it will eventually become smaller than `u32::max/2`,
        // which means that we have (at least) one bit of information. As soon as this
        // happens, we need to flush the bits and regrow `length`, to ensure that we
        // won't lose precision.

        let interval_quarter = 1u32.rotate_right(2);
        let interval_half    = 1u32.rotate_right(1);
        let interval_three_quarters = interval_half + interval_quarter;

        let mut high : u32 = self.low + self.length;
        'one_bit: loop {
            if high < interval_half {
                // Information: we're in the first half of the interval.
                self.append_bit(false)?;
            } else if self.low >= interval_half {
                // Information: we're in the second half of the interval.
                self.append_bit(true)?;
            } else if self.low >= interval_quarter && high < interval_three_quarters {
                // Information: we're in the half of the interval around the center.
                self.low  -= interval_quarter;
                high -= interval_quarter;
                self.recentering_bits += 1;
            } else {
                break 'one_bit
            };
            // Renormalize low (last digit must always be 0)
            self.low = self.low << 1;
            // Renormalize high (last digit must always be 1)
            high = high << 1 | 1u32;
            assert!(high > self.low);
        }

        // At this stage, we can't have high = low:
        // - if we have not entered the loop
        //    - by definition, high - low > interval/4, so high != low
        // - otherwise
        //    - at the end of the loop, we have set a different last digit for low and high, so high != low

        // Convert back to `self.length`.
        debug!(target: "multiarith", "Converting back to length: [{}, {}(", self.low, high);
        self.length = high - self.low;
        assert!(self.length != 0); // In the loop, we ensure that the last digit of high and low is different, so it can't be 0.
        Ok(())
    }

    /// Flush all the symbols that haven't been written yet.
    pub fn flush_symbols(&mut self) -> Result<bool, std::io::Error> {
        // Write the digits of `self.low`.
        let high_bit: u32 = 1u32.rotate_right(1);
        let mut wrote = false;
        for _ in 0.. std::mem::size_of_val(&self.low) * 8 {
            let bit = self.low & high_bit != 0;
            wrote |= self.append_bit(bit)?;
            self.low = self.low << 1;
        }
        assert_eq!(self.low, 0);
        self.length = std::u32::MAX;
        wrote |= self.flush_bits_always()?;
        Ok(wrote)
    }

    pub fn flush(&mut self) -> Result<(), std::io::Error> {
        self.flush_symbols()?;
        self.flush_bits_always()?;
        self.writer.flush()?;
        Ok(())
    }

    pub fn done(mut self) -> W {
        if self.length != std::u32::MAX {
            self.flush()
                .expect("Could not flush SymbolEncoder");
        }
        self.writer
    }

    fn append_bit(&mut self, bit: bool) -> Result<bool, std::io::Error> {
        debug!(target: "multiarith", "append_bit with {} bits ready", self.bits_ready);
        assert!((self.bits_ready as usize) < std::mem::size_of_val(&self.buffer) * 8);
        let mut wrote = false;

        self.bits_ready += 1;
        self.buffer = self.buffer * 2 + if bit { 1 } else { 0 };
        wrote |= self.flush_bits_if_necessary()?;
        for _ in 0 .. self.recentering_bits {
            self.bits_ready += 1;
            self.buffer = self.buffer * 2 + if bit { 0 } else { 1 };
            wrote |= self.flush_bits_if_necessary()?;
        }
        self.recentering_bits = 0;
        Ok(wrote)
    }

    fn flush_bits_if_necessary(&mut self) -> Result<bool, std::io::Error> {
        debug!(target: "multiarith", "flush_bits_if_necessary with {} bits ready", self.bits_ready);
        if self.bits_ready as usize == std::mem::size_of_val(&self.buffer) * 8 {
            self.flush_bits_internal()?;
            return Ok(true)
        }
        Ok(false)
    }

    fn flush_bits_always(&mut self) -> Result<bool, std::io::Error> {
        if self.bits_ready == 0 {
            return Ok(false)
        }
        for _ in self.bits_ready..std::mem::size_of_val(&self.buffer) as u8 * 8 {
            self.bits_ready += 1;
            self.buffer *= 2;
        }
        self.flush_bits_internal()?;
        Ok(true)
    }

    /// Flush `self.buffer`.
    ///
    /// Does NOT flush `self.writer`, `self.low` or `self.high`.
    fn flush_bits_internal(&mut self) -> Result<(), std::io::Error> {
        assert!(self.bits_ready as usize == std::mem::size_of_val(&self.buffer) * 8);
        self.writer.write(&[self.buffer])?;
        self.bits_ready = 0;
        self.buffer = 0;
        Ok(())
    }
}