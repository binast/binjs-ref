
use multiarith::{ ContextPredict, EncodingModel, F64, Label, Model, Path, PathPredict, Segment, ScopeIndex, SharedTree, SubTree, Tag, Visitor, WalkTree };

use io::TokenWriter;
use ::TokenWriterError;
use util::GenericCounter;

use std;
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

const EXPECTED_PATH_DEPTH: usize = 2048;
const EXPECTED_SCOPE_DEPTH: usize = 128;

impl Segment {
    /// Mark that a symbol has been defined in a context.
    fn mark_as_defined(&mut self) {
        self.needs_definition = false;
    }
}

struct ExactEncodingModelData<T> {
    /// Tag prediction based on path (depth 1 as of this writing).
    tags: PathPredict<Tag, T>,

    /// String prediction based on path (depth 1 as of this writing).
    strings: PathPredict<Option<Rc<String>>, T>,

    identifiers: ContextPredict<Option<ScopeIndex>, Rc<String>, T>,
}

/// Initialize the ExactEncodingModel
impl Visitor for ExactEncodingModelData</* Number of instances */ usize> {
    type Error = ();
    fn enter_label(&mut self, label: &Label, path: &Path<(Tag, usize)>, scopes: &Path<ScopeIndex>) -> Result<(), Self::Error> {
        match label {
            Label::Tag(ref tag) => {
                let (_, entry) = self.tags.entry(path, tag);
                entry.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            Label::String(ref string) => {
                let (_, entry) = self.strings.entry(path, string);
                entry.and_modify(|instances| {
                    *instances += 1
                }).or_insert(1);
            }
            Label::Declare(Some(ref string)) | Label::LiteralReference(Some(ref string)) => {
                let scope = scopes.last()
                    .map(Clone::clone);
                let (_, entry) = self.identifiers.entry(scope, string);
                entry.and_modify(|instances| {
                        *instances += 1
                    }).or_insert(1);
            }
            _ => {
                warn!(target: "multiarith", "Skipping initialization of predictor for label {:?} (not implemented yet)", label);
            }
        }
        Ok(())
    }
}

/// An encoding model which starts by analyzing the full AST to determine
/// exact statistics.
pub struct ExactEncodingModel {
    probabilities: ExactEncodingModelData<Segment>,
}
impl ExactEncodingModel {
    fn get_from_path<T>(predictor: &mut PathPredict<T, Segment>, value: &T, path: &Path<(Tag, usize)>) -> Result<Segment, ()>
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
    fn string_frequency_for_encoding(&mut self, string: &Option<Rc<String>>, path: &Path<(Tag, usize)>) -> Result<Segment, ()> {
        Self::get_from_path(&mut self.probabilities.strings, string, path)
    }
    fn tag_frequency_for_encoding(&mut self, tag: &Tag, path: &Path<(Tag, usize)>) -> Result<Segment, ()> {
        Self::get_from_path(&mut self.probabilities.tags, tag, path)
    }
    fn identifier_frequency_for_encoding(&mut self, string: &Rc<String>, scopes: &Path<ScopeIndex>) -> Result<Segment, ()> {
        let scope = scopes.last()
            .cloned();
        let segment = self.probabilities.identifiers.get_mut(&scope, string)
            .ok_or(())?;
        let result = segment.clone();
        segment.mark_as_defined();
        Ok(result)
    }
}
impl ExactEncodingModel {
    pub fn new(tree: &SharedTree) -> Self {
        // Compute number of instances
        let mut instances = ExactEncodingModelData {
            tags: PathPredict::new(1), // FIXME: Test with other depths
            strings: PathPredict::new(1), // FIXME: Test with other depths
            identifiers: ContextPredict::new(),
        };

        tree.walk(&mut instances,
            &mut Path::with_capacity(EXPECTED_PATH_DEPTH),
            &mut Path::with_capacity(EXPECTED_SCOPE_DEPTH))
            .expect("Could not compute number of instances");

        // Deduce probabilities.

        Self {
            probabilities: ExactEncodingModelData {
                tags: instances.tags.instances_to_probabilities(),
                strings: instances.strings.instances_to_probabilities(),
                identifiers: instances.identifiers.instances_to_probabilities(),
            }
        }
    }
}

pub struct TreeTokenWriter<'a> {
    root: SharedTree,
    scope_counter: GenericCounter<ScopeIndex>,
    model: &'a Model,
    encoder: SymbolEncoder<Vec<u8>>,
}

struct Compressor<'a> {
    model: Box<EncodingModel>,
    encoder: &'a mut SymbolEncoder<Vec<u8>>,
}

impl<'a> Visitor for Compressor<'a> {
    type Error = std::io::Error;
    fn enter_label(&mut self, label: &Label, path: &Path<(Tag, usize)>, scopes: &Path<ScopeIndex>) -> Result<(), Self::Error> {
        match label {
            Label::Tag(ref tag) => {
                let segment = self.model.tag_frequency_for_encoding(tag, path)
                    .expect("Could not compute tag frequency");
                self.encoder.append_segment(&segment)?;
                if segment.needs_definition {
                    // self.encoder.flush_symbols()?; // FIXME: Should we flush?
                    warn!(target: "multiarith", "FIXME: Append definition of the current tag {:?} in {:?}", tag, path.len());
                }
            }
            Label::String(ref string) => {
                let segment = self.model.string_frequency_for_encoding(string, path)
                    .expect("Could not compute string frequency");
                self.encoder.append_segment(&segment)?;
                if segment.needs_definition {
                    // self.encoder.flush_symbols()?; // FIXME: Should we flush?
                    warn!(target: "multiarith", "FIXME: Append definition of the current string {:?} in {:?}", string, path.len());
                }
            }
            Label::Declare(Some(ref string)) | Label::LiteralReference(Some(ref string)) => {
                let segment = self.model.identifier_frequency_for_encoding(string, scopes)
                    .expect("Could not compute identifier frequency");
                self.encoder.append_segment(&segment)?;
                if segment.needs_definition {
                    // self.encoder.flush_symbols()?; // FIXME: Should we flush?
                    warn!(target: "multiarith", "FIXME: Append definition of the current identifier {:?} in {:?}", string, path.len());
                }
            }
            _ => {
                warn!(target: "multiarith", "Skipping serialization of label {:?} (not implemented yet)", label);
            }
        }
        Ok(())
    }
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
        {
            let mut compressor = Compressor {
                model,
                encoder: &mut self.encoder
            };
            root.walk(&mut compressor,
                &mut Path::with_capacity(EXPECTED_PATH_DEPTH),
                &mut Path::with_capacity(EXPECTED_SCOPE_DEPTH)
            ).unwrap(); // FIXME: Handle errors
        }
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