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
use ::{ TokenWriterError };

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{ Hash, Hasher };
use std::io::Write;
use std::rc::Rc;

use itertools::Itertools;

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

type Streams<W> = PerCategory<W>;
type Dictionaries<W> = PerCategory<Box<Dictionary<Label, W>>>;
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
    fn serialize<W: Write>(&self, parent: Option<&Label>, dictionaries: &mut Dictionaries<W>, streams: &mut Streams<W>) -> Result<(), std::io::Error> {
        let new_parent = {
            let (new_parent, dictionary, stream) = match self.label {
                Label::String(_) => (parent,     &mut dictionaries.strings, &mut streams.strings),
                Label::Number(_) => (parent,     &mut dictionaries.numbers, &mut streams.numbers),
                Label::Bool(_)   => (parent,     &mut dictionaries.bools,   &mut streams.bools),
                Label::List(_)   => (parent,     &mut dictionaries.lists,   &mut streams.lists),
                Label::Tag(_)    => (Some(&self.label), &mut dictionaries.tags,    &mut streams.tags),
            };
            dictionary.write_label(&self.label, parent, stream)?;
            new_parent
        };
        for child in &self.children {
            let borrow = child.borrow();
            borrow.serialize(new_parent, dictionaries, streams)?;
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
}
impl TreeTokenWriter {
    pub fn new() -> Self {
        Self {
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
        let string_frequencies = string_instances.into_iter()
            .sorted_by(|a,b| usize::cmp(&b.1, &a.1))
            .into_iter()
            .enumerate()
            .map(|(position, (s, _))| (s, position))
            .collect();
        let tag_frequencies = tag_instances.into_iter()
            .sorted_by(|a,b| usize::cmp(&b.1, &a.1))
            .into_iter()
            .enumerate()
            .map(|(position, (s, _))| (s, position))
            .collect();

         let tags: Box<Dictionary<Label, Vec<u8>>> = Box::new(ParentPredictionLabeler::new(ExplicitIndexLabeler::new(tag_frequencies)));

        let mut dictionaries = PerCategory {
            tags,
            numbers: Box::new(RawLabeler::new()), // FIXME: Experiment with MRULabeler
            bools: Box::new(RawLabeler::new()),
            lists: Box::new(RawLabeler::new()), // FIXME: Experiment with ParentPredictionLabeler
            strings: Box::new(ExplicitIndexLabeler::new(string_frequencies)), // FIXME: Experiment with ParentPredictionLabeler
        };
        let mut streams = PerCategory {
            tags: vec![],
            numbers: vec![],
            bools: vec![],
            lists: vec![],
            strings: vec![],
        };
        self.root.borrow().serialize(None, &mut dictionaries, &mut streams)
            .unwrap_or_else(|_| unimplemented!());

        let stats = Statistics {
            tags: streams.tags.len(),
            numbers: streams.numbers.len(),
            bools: streams.bools.len(),
            lists: streams.lists.len(),
            strings: streams.strings.len(),
        };

        debug!(target: "multistream", "Statistics: {:?}", stats);

        // For the end value, we can concatenate in any order, as long as we start with `tags`.
        let mut result = streams.tags;
        result.extend(streams.strings.into_iter());
        result.extend(streams.numbers.into_iter());
        result.extend(streams.bools.into_iter());
        result.extend(streams.lists.into_iter());

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
