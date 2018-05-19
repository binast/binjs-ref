//! A format in which we split everything per nature.
//!
//! - One stream of node kinds
//! - One stream of strings
//! - One stream of floats
//! - ...
//!
//! Each stream is compressed separately, which should increase compressibility.

// FIXME: Can we easily interleave streams of e.g. gzip, brotli?

// FIXME: Since we're going to inline the definition of node kinds, we could try and toy with
// with the bytes used to represent child instances to make them use the same alphabet.

use io::TokenWriter;
use labels::{ Dictionary, Label as WritableLabel };
use ::{ DictionaryPlacement, CompressionTarget, TokenWriterError };

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{ Hash, Hasher };
use std::io::Write;
use std::rc::Rc;

use itertools::Itertools;

#[derive(Clone, Debug)]
pub struct Options {
    pub sibling_labels_together: bool,
    pub dictionary_placement: DictionaryPlacement,
}
#[derive(Clone)]
pub struct Targets {
    pub contents: PerCategory<CompressionTarget>,
    pub header_strings: CompressionTarget,
    pub header_tags: CompressionTarget,
}
impl Targets {
    pub fn reset(&mut self) {
        self.contents.reset();
        self.header_strings.reset();
        self.header_tags.reset();
    }
}

#[derive(Clone, Debug, Default)]
pub struct PerCategory<T> {
    pub strings: T,
    pub numbers: T,
    pub bools: T,
    pub lists: T,
    pub tags: T,
}
impl PerCategory<CompressionTarget> {
    pub fn reset(&mut self) {
        self.strings.reset();
        self.numbers.reset();
        self.bools.reset();
        self.lists.reset();
        self.tags.reset();
    }
}

impl std::ops::Add<Self> for PerCategory<usize> {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self {
            strings: self.strings + other.strings,
            numbers: self.numbers + other.numbers,
            bools: self.bools + other.bools,
            lists: self.lists + other.lists,
            tags: self.tags + other.tags,
        }
    }
}
impl std::fmt::Display for PerCategory<usize> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "strings (b): {strings}, tags (b): {tags}, numbers (b): {numbers}, bools (b): {bools}, lists (b): {lists}",
            strings = self.strings,
            numbers = self.numbers,
            bools = self.bools,
            lists = self.lists,
            tags = self.tags,
        )
    }
}

struct Compressor<W: Write> {
    stream: W,
    dictionary: Box<Dictionary<Label, W>>,
}

#[derive(Debug, Default)]
pub struct Statistics {
    pub header_strings: usize,
    pub header_tags: usize,
    pub contents: PerCategory<usize>,
}
impl std::ops::Add<Self> for Statistics {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self {
            header_strings: self.header_strings + other.header_strings,
            header_tags: self.header_tags + other.header_tags,
            contents: self.contents + other.contents,
        }
    }
}

impl std::fmt::Display for Statistics {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "Header strings (b): {strings}, header tags (b): {tags}, {rest}",
            strings = self.header_strings,
            tags = self.header_tags,
            rest = self.contents,
        )
    }
}

#[derive(Clone, Debug)]
pub struct SubTree {
    label: Label,
    children: Vec<SharedTree>,
}
pub type SharedTree = Rc<RefCell<SubTree>>;

impl SubTree {
    fn with_labels<F: FnMut(&Label)>(&self, f: &mut F) {
        f(&self.label);
        for child in &self.children {
            child.borrow().with_labels(f);
        }
    }
    fn serialize_label<W: Write>(&self, parent: Option<&Label>, compressors: &mut PerCategory<Compressor<W>>) -> Result<(), std::io::Error> {
        let compressor = match self.label {
            Label::String(_) => &mut compressors.strings,
            Label::Number(_) => &mut compressors.numbers,
            Label::Bool(_)   => &mut compressors.bools,
            Label::List(_)   => &mut compressors.lists,
            Label::Tag(_)    => &mut compressors.tags,
        };
        compressor.dictionary.write_label(&self.label, parent, &mut compressor.stream)?;
        Ok(())
    }
    fn serialize_children<W: Write>(&self, options: &Options, parent: Option<&Label>, compressors: &mut PerCategory<Compressor<W>>) -> Result<(), std::io::Error> {
        let new_parent = match self.label {
            Label::Tag(_) => Some(&self.label),
            _ => parent
        };
        if options.sibling_labels_together {
            // First all the labels of children.
            for child in &self.children {
                let borrow = child.borrow();
                borrow.serialize_label(new_parent, compressors)?;
            }
            // Then actually walk the children.
            for child in &self.children {
                let borrow = child.borrow();
                borrow.serialize_children(options, new_parent, compressors)?;
            }
        } else {
            // Everything at once.
            for child in &self.children {
                let borrow = child.borrow();
                borrow.serialize_label(new_parent, compressors)?;
                borrow.serialize_children(options, new_parent, compressors)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Label {
    String(Option<Rc<String>>),
    Number(Option<f64>),
    Bool(Option<bool>),
    List(Option<u32>),
    Tag(Rc<String>),
}
impl Label {
    // FIXME: Make this more robust.
    fn string_byte_len(&self) -> usize {
        let result = match *self {
            Label::String(None) => 2, // FIXME: We could turn this into `0`, if we wanted. Not really necessary, though.
            Label::String(Some(ref string)) => string.len(),
            Label::Tag(ref string) => string.len(),
            _ => panic!("string_byte_len not implemented for this case"),
        };
        result
    }
}
impl Eq for Label { /* Yes, it's probably not entirely true for f64 */ }
impl Hash for Label {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        use self::Label::*;
        match *self {
            String(ref s) => s.hash(state),
            Bool(ref b) => b.hash(state),
            List(ref l) => l.hash(state),
            Tag(ref s) => s.hash(state),
            Number(ref num) => num.map(f64::to_bits).hash(state),
        }
    }
}

impl WritableLabel for Label {
    fn write_definition<W: Write, L: Dictionary<Self, W>>(&self, _: Option<&Self>, _: &mut L, out: &mut W) -> Result<(), std::io::Error> {
        use self::Label::*;
        match *self {
            String(Some(ref s)) => {
                out.write_all(s.as_bytes())?;
            },
            String(None) => {
              // FIXME: Put this magic constant safely in a module
              out.write_all(&[255, 0])?;
            },
            Number(maybe_num) => {
              out.write_all(&::bytes::float::varbytes_of_float(maybe_num))?;
            }
            Bool(maybe_bool) => {
              out.write_all(&::bytes::bool::bytes_of_bool(maybe_bool))?;
            }
            List(maybe_len) => {
                use ::bytes::varnum::*;
                out.write_maybe_varnum(maybe_len)?;
            }
            Tag(ref s) => {
                out.write_all(s.as_bytes())?;
            }
        }
        Ok(())
    }
}

pub struct TreeTokenWriter {
    root: SharedTree,
    options: Options,
    targets: Targets,
}
impl TreeTokenWriter {
    pub fn new(options: Options, targets: Targets) -> Self {
        Self {
            options,
            targets,
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
impl TokenWriter for TreeTokenWriter {
    type Error = TokenWriterError;
    type Statistics = Statistics;
    type Tree = SharedTree;
    type Data = Vec<u8>;

    fn tagged_tuple(&mut self, tag: &str, children: &[(&str, Self::Tree)]) -> Result<Self::Tree, Self::Error> {
        self.new_tree(SubTree {
            label: Label::Tag(Rc::new(tag.to_string())),
            children: children.iter()
                .map(|(_, tree)| tree.clone())
                .collect()
        })
    }

    fn offset(&mut self) -> Result<Self::Tree, Self::Error> {
        unimplemented!()
    }

    fn bool(&mut self, value: Option<bool>) -> Result<Self::Tree, Self::Error> {
        self.new_tree(SubTree {
            label: Label::Bool(value),
            children: vec![]
        })
    }

    fn float(&mut self, value: Option<f64>) -> Result<Self::Tree, Self::Error> {
        self.new_tree(SubTree {
            label: Label::Number(value),
            children: vec![]
        })
    }

    fn string(&mut self, value: Option<&str>) -> Result<Self::Tree, Self::Error> {
        let value = match (value, &self.options.dictionary_placement) {
            (None, _) => None,
            (Some(ref s), &DictionaryPlacement::Inline) => {
                let mut string = s.to_string();
                string.push('\0');
                Some(string)
            }
            (Some(ref s), _) => Some(s.to_string())
        };

        self.new_tree(SubTree {
            label: Label::String(value.map(Rc::new)),
            children: vec![]
        })
    }

    fn list(&mut self, children: Vec<Self::Tree>) -> Result<Self::Tree, Self::Error> {
        self.new_tree(SubTree {
            label: Label::List(Some(children.len() as u32)),
            children
        })
    }

    fn untagged_tuple(&mut self, _: &[Self::Tree]) -> Result<Self::Tree, Self::Error> {
        unimplemented!()
    }

    fn done(self) -> Result<(Self::Data, Self::Statistics), Self::Error> {
        use labels:: { ExplicitIndexLabeler, MRULabeler, ParentPredictionLabeler, RawLabeler };
        let mut tag_instances = HashMap::new();
        let mut string_instances = HashMap::new();
        self.root.borrow_mut().with_labels(&mut |label: &Label| {
            match *label {
                Label::String(_) => {
                    let mut entry = string_instances.entry(label.clone())
                        .or_insert(0);
                    *entry += 1;
                }
                Label::Tag(_) => {
                    let mut entry = tag_instances.entry(label.clone())
                        .or_insert(0);
                    *entry += 1;
                }
                _ => {}
            }
        });
        debug!(target: "multistream", "Detected {} tag definitions, {} occurrences",
            tag_instances.len(),
            tag_instances.values()
                .cloned()
                .sum::<usize>()
        );
        debug!(target: "multistream", "Detected {} strings definitions, {} occurrences",
            string_instances.len(),
            string_instances.values()
                .cloned()
                .sum::<usize>()
        );

        let tag_frequencies : HashMap<_, _> = tag_instances.into_iter()
            .sorted_by(|a,b| usize::cmp(&b.1, &a.1))
            .into_iter()
            .enumerate()
            .map(|(position, (s, _))| (s, position))
            .collect();
        let tag_frequency_dictionary = ExplicitIndexLabeler::new(tag_frequencies.clone());

        let string_frequencies : HashMap<_, _> = string_instances.into_iter()
            .sorted_by(|a,b| usize::cmp(&b.1, &a.1))
            .into_iter()
            .enumerate()
            .map(|(position, (s, _))| (s, position))
            .collect();
        let string_frequency_dictionary = ExplicitIndexLabeler::new(string_frequencies.clone());

        let mut header_tags = Compressor {
            dictionary: Box::new(ParentPredictionLabeler::new(tag_frequency_dictionary)),
            stream: self.targets.header_tags.clone(),
        };
        let mut header_strings = Compressor {
            dictionary: Box::new(MRULabeler::new()), // FIXME: Experiment with ParentPredictionLabeler
            stream: self.targets.header_strings,
        };

        if let DictionaryPlacement::Header = self.options.dictionary_placement {
            debug!(target: "multistream", "Writing header");
            use bytes::varnum::WriteVarNum;
            // Pre-write tags and strings.
            // First, list of lengths (probably hard to compress), followed by a single concatenated string (normally, easy to compress).
            // FIXME: See if we need to rewrite the string to improve compression, e.g. Burrows–Wheeler transform
            header_tags.stream.write_varnum(tag_frequencies.len() as u32).unwrap();
            for tag in tag_frequencies.keys() {
                header_tags.stream.write_varnum(tag.string_byte_len() as u32).unwrap();
            }
            for tag in tag_frequencies.keys() {
                header_tags.dictionary.write_label(tag, None, &mut header_tags.stream).unwrap();
            }
            debug!(target: "multistream", "Wrote {} bytes ({} tags) to header",
                header_tags.stream.len(),
                tag_frequencies.len());

            header_strings.stream.write_varnum(string_frequencies.len() as u32).unwrap();
            // Write from least common to most common.
            let string_keys = string_frequencies.iter()
                .sorted_by(|a,b| usize::cmp(&b.1, &a.1));
            for (ref string, _) in &string_keys {
                header_strings.stream.write_varnum(string.string_byte_len() as u32).unwrap();
            }
            for (ref string, _) in &string_keys {
                header_strings.dictionary.write_label(string, None, &mut header_strings.stream).unwrap();
            }
            debug!(target: "multistream", "Wrote {} bytes ({} strings) to header",
                header_strings.stream.len(),
                string_frequencies.len());
        }

        let mut compressors = PerCategory {
            tags: Compressor {
                dictionary: header_tags.dictionary, // Reuse dictionary.
                stream: self.targets.contents.tags,
            },
            numbers: Compressor {
                dictionary: Box::new(RawLabeler::new()), // FIXME: Experiment with MRULabeler
                stream: self.targets.contents.numbers,
            },
            bools: Compressor {
                dictionary: Box::new(RawLabeler::new()),
                stream: self.targets.contents.bools,
            },
            lists: Compressor {
                dictionary: Box::new(RawLabeler::new()), // FIXME: Experiment with ParentPredictionLabeler
                stream: self.targets.contents.lists,
            },
            strings: Compressor {
                dictionary: header_strings.dictionary, // Reuse dictionary.
                stream: self.targets.contents.strings,
            },
        };

        // Write the tree to the various streams.
        self.root.borrow().serialize_label(None, &mut compressors)
            .unwrap_or_else(|_| unimplemented!());
        self.root.borrow().serialize_children(&self.options, None, &mut compressors)
            .unwrap_or_else(|_| unimplemented!());

        let stats = Statistics {
            header_tags: header_tags.stream.len(),
            header_strings: header_strings.stream.len(),
            contents: PerCategory {
                tags: compressors.tags.stream.len(),
                strings: compressors.strings.stream.len(),
                numbers: compressors.numbers.stream.len(),
                bools: compressors.bools.stream.len(),
                lists: compressors.lists.stream.len(),
            }
        };

        debug!(target: "multistream", "Compression complete: {:?}", stats);

        let mut result = vec![];
        result.extend_from_slice(header_tags.stream.done().unwrap().0.as_ref());
        result.extend_from_slice(header_strings.stream.done().unwrap().0.as_ref());
        result.extend_from_slice(compressors.tags.stream.done().unwrap().0.as_ref());
        result.extend_from_slice(compressors.strings.stream.done().unwrap().0.as_ref());
        result.extend_from_slice(compressors.numbers.stream.done().unwrap().0.as_ref());
        result.extend_from_slice(compressors.bools.stream.done().unwrap().0.as_ref());
        result.extend_from_slice(compressors.lists.stream.done().unwrap().0.as_ref());


        Ok((result, stats))
    }
}



/*
    fn children(&self) -> impl Iterator<Item = &SharedTree> {
        self.children.iter()
    }
    fn iter(&self) -> impl Iterator<Item = &SharedTree> {
        self.children.iter()
            .cloned()
            .flat_map(|child| {
                let borrow = child.borrow();
                borrow.iter()
            })
    }
*/

/*
/*
impl SubTree {
    fn iter(&self) -> impl Iterator<Item = &Label> {

    }
}
*/
enum Walk {
    Pop,
    Push(SharedTree)
}
struct SubTreePreIterator<'a> {
    phantom: std::marker::PhantomData<&'a ()>,
    stack: Vec<(SharedTree, usize)>,
}
impl<'a> Iterator for SubTreePreIterator<'a> {
    type Item = &'a Label;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let walk = match self.stack.last_mut() {
                None => return None,
                Some((ref node, ref mut position)) if *position == 0 => {
                    // Start visiting node.
                    *position = 1;
                    return Some(&node.borrow().label)
                },
                Some((ref node, ref position)) if *position >= node.borrow().children.len() => {
                    // We have finished visiting the node, get back up.
                    Walk::Pop
                }
                Some((ref node, ref mut position)) => {
                    // We have finished visiting a child, continue to next child.
                    let index = *position;
                    *position += 1;
                    Walk::Push(node.borrow().children[index].clone())
                }
            };
            match walk {
                Walk::Pop => {
                    self.stack.pop();
                }
                Walk::Push(subtree) => {
                    self.stack.push((subtree, 0));
                }
            }
        }
    }
}

*/
