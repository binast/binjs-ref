
use multiarith::{ F64, Label, Predict1, ScopeIndex, SharedTree, SubTree, Tag };

use io::TokenWriter;
use ::TokenWriterError;
use util::GenericCounter;

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

use vec_map::VecMap;

#[derive(Clone, Copy, Debug)]
enum Direction {
    Enter,
    Exit
}

#[derive(Clone, Debug)]
pub struct Segment { // FIXME: Maybe we don't want `u32` but `u16` or `u8`.
    /// Low value for this segment.
    ///
    /// The probability of the segment is `(high - low)/context_length`.
    low:  u32,

    /// Length of the segment.
    ///
    /// MUST never be 0.
    length: u32,


    /// The highest possible value of `high` in this context.
    ///
    /// MUST be consistent across segments for the same context.
    ///
    /// MUST be greater or equal to `high`.
    ///
    /// MUST be > 0.
    context_length: u32,

    /// If `true`, this is the first occurrence of this symbol in the
    /// context, so a definition must be injected.
    needs_definition: bool,
}

impl Segment {
    /// Mark that a symbol has been defined in a context.
    fn mark_as_defined(&mut self) {
        self.needs_definition = false;
    }
}

pub trait EncodingModel {
    /// Get the frequency of a tag as a child of a given parent.
    ///
    /// If the model is adaptative, this will increase the number of uses of the tag in this context by 1.
    ///
    /// `needs_definition` will always be `false` after the first call for a given tag/parent.
    fn tag_frequency_for_encoding(&mut self, tag: &Tag, parent: Option<(&Tag, usize)>) -> Result<Segment, ()>;
}

struct LinearAdaptiveEncodingPseudoModel {
    tags: Predict1<Tag, Segment>,
}


/// An encoding model which starts by analyzing the full AST to determine
/// exact statistics.
pub struct ExactEncodingModel {
    tags: Predict1<Tag, Segment>,
}
impl EncodingModel for ExactEncodingModel {
    fn tag_frequency_for_encoding(&mut self, tag: &Tag, parent: Option<(&Tag, usize)>) -> Result<Segment, ()> {
        let by_index = match parent {
            None => self.tags.by_parent.get_mut(&None),
            Some((parent_tag, _)) => self.tags.by_parent.get_mut(&Some(parent_tag.clone()))
        }.ok_or(())?;
        let by_tag = match parent {
            None => by_index.get_mut(0),
            Some((_, index)) => by_index.get_mut(index)
        }.ok_or(())?;
        let this_tag = by_tag.get_mut(tag)
            .ok_or(())?;
        let mut result = (*this_tag).clone();
        this_tag.mark_as_defined();
        Ok(result)
    }
}
impl ExactEncodingModel {
    fn init_tags(predict_1: &mut Predict1<Tag, /* instances */ usize>, subtree: &SharedTree, parent: Option<(&Tag, usize)>) {
        let borrow = subtree.borrow();
        match borrow.label {
            Label::Tag(ref tag) => {
                let mut by_index = match parent {
                    None => {
                        let mut by_parent = predict_1.by_parent.entry(None)
                            .or_insert_with(|| VecMap::with_capacity(1));
                        by_parent.entry(0)
                            .or_insert_with(|| HashMap::new())
                    }
                    Some((parent, index)) => {
                        let mut by_parent = predict_1.by_parent.entry(Some(parent.clone()))
                            .or_insert_with(|| VecMap::with_capacity(5));
                        by_parent.entry(index)
                            .or_insert_with(|| HashMap::new())
                    }
                };
                let symbols = by_index.len();
                by_index.entry(tag.clone())
                    .and_modify(|instances| {
                        *instances += 1
                    })
                    .or_insert(1);

            }
            _ => {
                warn!(target: "multiarith", "Skipping initialization of predictor for label {:?} (not implemented yet)", borrow.label);
            }
        }
        // Recur towards children.
        match borrow.label {
            Label::Tag(ref tag) => {
                for (index, child) in borrow.children.iter().enumerate() {
                    Self::init_tags(predict_1, child, Some((tag, index)));
                }
            }
            _ => {
                for (index, child) in borrow.children.iter().enumerate() {
                    Self::init_tags(predict_1, child, parent);
                }
            }
        }
    }

    pub fn new(tree: &SharedTree) -> Self {
        let mut predict_instances_1 = Predict1::default();
        // Initialize number of instances.
        Self::init_tags(&mut predict_instances_1, tree, None);

        // Deduce probabilities.
        let probabilities = predict_instances_1.by_parent.drain()
            .map(|(parent, mut by_child_index)| {
                let by_child_index = by_child_index.drain()
                    .map(|(child_index, mut by_symbol)| {
                        let number_of_symbols = by_symbol.len();
                        let total_instances : usize = by_symbol.values()
                            .sum();
                        let mut cursor = 0;
                        let by_symbol = by_symbol.drain()
                            .enumerate()
                            .map(|(index, (tag, instances))| {
                                let low = cursor;
                                cursor += instances;
                                let segment = Segment {
                                    low: low as u32,
                                    length: instances as u32,
                                    context_length: total_instances as u32,
                                    needs_definition: true,
                                };
                                (tag, segment)
                            })
                            .collect();
                        assert_eq!(cursor, total_instances);
                        (child_index, by_symbol)
                    })
                    .collect();
                (parent, by_child_index)
            }).collect();
        Self {
            tags: Predict1 {
                by_parent: probabilities
            }
        }
    }
}

impl SubTree {
    fn with_labels<F: FnMut(&Label)>(&self, f: &mut F) {
        f(&self.label);
        for child in &self.children {
            child.borrow().with_labels(f);
        }
    }
    fn with_labels_mut<F: FnMut(Direction, &mut Label)>(&mut self, f: &mut F) {
        f(Direction::Enter, &mut self.label);
        for child in &self.children {
            child.borrow_mut().with_labels_mut(f);
        }
        f(Direction::Exit, &mut self.label);
    }
}


pub struct TreeTokenWriter<M, W> where M: EncodingModel, W: Write {
    root: SharedTree,
    scope_counter: GenericCounter<ScopeIndex>,
    model: M,
    encoder: SymbolEncoder<W>,
}
impl<M, W> TreeTokenWriter<M, W> where M: EncodingModel, W: Write {
    pub fn new(model: M, writer: W) -> Self {
        Self {
            scope_counter: GenericCounter::new(),
            model,
            encoder: SymbolEncoder::new(writer),
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

    fn number_references(&mut self) -> Result<SharedTree, TokenWriterError> {
        // Undeclared references
        let top = Rc::new(RefCell::new(vec![]));
        let stack = Rc::new(RefCell::new(vec![top.clone()]));
        self.root.borrow_mut().with_labels_mut(&mut |direction, label| {
            let rewrite = match (direction, &label) {
                (Direction::Enter, Label::Scope(_)) => {
                    let mut borrow_stack = stack.borrow_mut();
                    borrow_stack.push(Rc::new(RefCell::new(vec![])));
                    None
                }
                (Direction::Exit, Label::Scope(_)) => {
                    let mut borrow_stack = stack.borrow_mut();
                    borrow_stack.pop();
                    None
                }
                (Direction::Enter, Label::Declare(Some(ref s))) => {
                    let borrow_stack = stack.borrow();
                    let mut borrow_frame = borrow_stack.last()
                        .unwrap()
                        .borrow_mut();
                    borrow_frame
                        .push(s.clone());
                    None
                }
                (Direction::Enter, Label::LiteralReference(None)) => {
                    Some(Label::NumberedReference(None))
                }
                (Direction::Enter, Label::LiteralReference(Some(ref s))) => {
                    let mut depth = 0;
                    let mut found = None;
                    {
                        let borrow_stack = stack.borrow();
                        'find_in_stack: for frame in borrow_stack.iter().rev() {
                            let borrow_frame = frame.borrow();
                            if let Some(index) = borrow_frame.iter()
                                .position(|name| name == s)
                            {
                                found = Some(index);
                                break 'find_in_stack;
                            } else {
                                depth += borrow_frame.len()
                            }
                        }
                    }
                    let index = match found {
                        Some(found) => found,
                        None => {
                            let mut borrow_top = top.borrow_mut();
                            borrow_top.push(s.clone());
                            borrow_top.len()
                        }
                    };
                    Some(Label::NumberedReference(Some((depth + index) as u32)))
                }
                _ => None,
            };
            if let Some(rewrite) = rewrite {
                *label = rewrite;
            }
        });

        // Now declare all undeclared variables.
        let root = self.root.clone();
        let top = top.borrow()
            .iter()
            .map(|name| Rc::new(RefCell::new(SubTree {
                label: Label::Declare(Some(name.clone())),
                children: vec![]
            })))
            .collect();
        let declared_undeclared_variables = self.new_tree(SubTree {
            label: Label::Tag(Tag::new("_undeclared_variables")),
            children: top
        })?;
        let scope_index = self.scope_counter.next();
        self.new_tree(SubTree {
            label: Label::Scope(scope_index),
            children: vec![
                declared_undeclared_variables,
                root
            ]
        })
    }

    fn compress(&mut self, subtree: &SharedTree, parent: Option<(&Tag, usize)>) -> Result<(), std::io::Error> {
        let borrow = subtree.borrow();
        match borrow.label {
            Label::Tag(ref tag) => {
                let segment = self.model.tag_frequency_for_encoding(tag, parent)
                    .expect("Could not compute tag frequency");
                self.encoder.append_segment(&segment)?;
                if segment.needs_definition {
                    self.encoder.flush_symbols()?;
                    unimplemented!("FIXME: Append definition of the current label");
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
                    self.compress(child, Some((tag, index)))?;
                }
            }
            _ => {
                for (index, child) in borrow.children.iter().enumerate() {
                    self.compress(child, parent)?;
                }
            }
        }
        Ok(())
    }
}


impl<M, W> TokenWriter for TreeTokenWriter<M, W> where M: EncodingModel, W: Write {
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

    fn offset(&mut self) -> Result<Self::Tree, TokenWriterError> {
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
        self.number_references()?;

        unimplemented!("FIXME: Compress")
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

        let mut high = self.low + self.length;
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
        }

        // Convert back to `self.length`.
        self.length = high - self.low + 1;
        assert!(self.length != 0); // FIXME: How do we ensure that this never happens?
        Ok(())
    }

    /// Flush all the symbols that haven't been written yet.
    pub fn flush_symbols(&mut self) -> Result<bool, std::io::Error> {
        // Write the digits of `self.low`.
        let mut wrote = false;
        let high_bit: u32 = 1u32.rotate_right(1);
        for _ in 0.. std::mem::size_of_val(&self.low) {
            let bit = self.low & high_bit != 0;
            wrote |= self.append_bit(bit)?;
            self.low = self.low << 1;
        }
        assert_eq!(self.low, 0);
        self.length = std::u32::MAX;
        Ok(wrote)
    }

    pub fn flush(&mut self) -> Result<(), std::io::Error> {
        self.flush_symbols()?;
        self.flush_bits()?;
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
        assert!((self.bits_ready as usize) < std::mem::size_of_val(&self.buffer));
        let mut wrote = false;

        self.bits_ready += 1;
        self.buffer = self.buffer * 2 + if bit { 1 } else { 0 };
        wrote = wrote || self.flush_bits_if_necessary()?;
        for _ in 0 .. self.recentering_bits {
            self.bits_ready += 1;
            self.buffer = self.buffer * 2 + if bit { 0 } else { 1 };
            wrote = wrote || self.flush_bits_if_necessary()?;
        }
        self.recentering_bits = 0;
        Ok(wrote)
    }

    fn flush_bits_if_necessary(&mut self) -> Result<bool, std::io::Error> {
        if self.bits_ready as usize == std::mem::size_of_val(&self.buffer) {
            self.flush_bits()?;
            return Ok(true)
        }
        Ok(false)
    }

    /// Flush `self.buffer`.
    ///
    /// Does NOT flush `self.writer`, `self.low` or `self.high`.
    fn flush_bits(&mut self) -> Result<(), std::io::Error> {
        self.writer.write(&[self.buffer])?;
        self.bits_ready = 0;
        self.buffer = 0;
        Ok(())
    }
}