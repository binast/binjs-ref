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
use ::{ DictionaryPlacement, TokenWriterError };

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{ Hash, Hasher };
use std::io::Write;
use std::rc::Rc;

use itertools::Itertools;

#[derive(Clone)]
pub struct Options {
    pub sibling_labels_together: bool,
    pub dictionary_placement: DictionaryPlacement,
}

#[derive(Debug, Default)]
pub struct PerCategory<T> {
    strings: T,
    numbers: T,
    bools: T,
    lists: T,
    tags: T,
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
        write!(fmt, "Strings (b): {strings}, numbers (b): {numbers}, bools (b): {bools}, lists (b): {lists}, tags (b): {tags}",
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

pub type Statistics = PerCategory<usize>;


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
                out.write_all(&[0])?;
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
                out.write_all(&[0])?;
            }
        }
        Ok(())
    }
}

pub struct TreeTokenWriter {
    root: SharedTree,
    options: Options,
}
impl TreeTokenWriter {
    pub fn new(options: Options) -> Self {
        Self {
            options,
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
        self.new_tree(SubTree {
            label: Label::String(value.map(str::to_string).map(Rc::new)),
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
        use labels:: { RawLabeler, ExplicitIndexLabeler, ParentPredictionLabeler };
        let mut tag_instances = HashMap::new();
        let mut string_instances = HashMap::new();
        self.root.borrow().with_labels(&mut |label: &Label| {
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

        let mut tags = Compressor {
            dictionary: Box::new(ParentPredictionLabeler::new(tag_frequency_dictionary)),
            stream: vec![],
        };
        let mut strings = Compressor {
            dictionary: Box::new(string_frequency_dictionary), // FIXME: Experiment with ParentPredictionLabeler
            stream: vec![],
        };

        if let DictionaryPlacement::Header = self.options.dictionary_placement {
            // Pre-write tags and strings.
            for tag in tag_frequencies.keys() {
                tags.dictionary.write_label(tag, None, &mut tags.stream).unwrap();
            }
            debug!(target: "multistream", "Wrote {} bytes ({} tags) to header",
                tags.stream.len(),
                tag_frequencies.len());

            for string in string_frequencies.keys() {
                strings.dictionary.write_label(string, None, &mut strings.stream).unwrap();
            }
            debug!(target: "multistream", "Wrote {} bytes ({} strings) to header",
                strings.stream.len(),
                string_frequencies.len());
        }

        let mut compressors = PerCategory {
            tags,
            numbers: Compressor {
                dictionary: Box::new(RawLabeler::new()), // FIXME: Experiment with MRULabeler
                stream: vec![],
            },
            bools: Compressor {
                dictionary: Box::new(RawLabeler::new()),
                stream: vec![],
            },
            lists: Compressor {
                dictionary: Box::new(RawLabeler::new()), // FIXME: Experiment with ParentPredictionLabeler
                stream: vec![],
            },
            strings,
        };

        // Now write actual tree.
        self.root.borrow().serialize_label(None, &mut compressors)
            .unwrap_or_else(|_| unimplemented!());
        self.root.borrow().serialize_children(&self.options, None, &mut compressors)
            .unwrap_or_else(|_| unimplemented!());

        let stats = Statistics {
            tags: compressors.tags.stream.len(),
            numbers: compressors.numbers.stream.len(),
            bools: compressors.bools.stream.len(),
            lists: compressors.lists.stream.len(),
            strings: compressors.strings.stream.len(),
        };

        debug!(target: "multistream", "Statistics: {:?}", stats);

        // For the end value, we can concatenate in any order, as long as we start with `tags`.
        let mut result = compressors.tags.stream;
        result.extend(compressors.strings.stream.into_iter());
        result.extend(compressors.numbers.stream.into_iter());
        result.extend(compressors.bools.stream.into_iter());
        result.extend(compressors.lists.stream.into_iter());

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
