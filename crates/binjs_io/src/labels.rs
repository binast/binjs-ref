use binjs_shared;

use bytes::varnum::*;

use std;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::io::Write;

pub trait Label : Sized {
    fn write_definition<W: Write, L: Dictionary<Self>>(&self, parent: Option<&Self>, strategy: &mut L, out: &mut W) -> Result<(), std::io::Error>;
}

pub trait Dictionary<T> {
    /// Return `true` if we just added the definition of the label to the dictionary,
    /// `false` if it was already present.
    fn write_label<W: Write>(&mut self, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error>;
}

pub struct MRULabeler<T> where T: Eq + Label + Clone {
    mru: binjs_shared::mru::MRU<T>
}
impl<T> MRULabeler<T> where T: Eq + Label + Clone {
    pub fn new() -> Self {
        Self {
            mru: binjs_shared::mru::MRU::new()
        }
    }
}
impl<T> Dictionary<T> for MRULabeler<T> where T: Eq + Label + Clone {
    fn write_label<W: Write>(&mut self, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        use binjs_shared::mru::Seen::*;
        match self.mru.access(label) {
            Age(index) => {
                // The label has already been seen, just wrote how many accesses ago.
                out.write_varnum(index as u32)?;
                Ok(false)
            }
            Never(index) => {
                // This is the first time the label is seen. Write the "new" index followed by its definition.
                out.write_varnum(index as u32)?;
                label.write_definition(parent, self, out)?;
                Ok(true)
            }
        }
    }
}

struct Seen<T> {
    index: T,
    is_first: bool,
}

/// Label entries with a dictionary.
///
/// Typically used for labeling with global frequencies.
pub struct ExplicitIndexLabeler<T> where T: Eq + Hash + Label {
    dictionary: HashMap<T, Seen<usize>>
}
impl<T> ExplicitIndexLabeler<T> where T: Eq + Hash + Label {
    pub fn new(mut dictionary: HashMap<T, usize>) -> Self {
        Self {
            dictionary: dictionary.drain()
                .map(|(k, index)| (k, Seen {
                    is_first: true,
                    index
                }))
                .collect()
        }
    }
}
impl<T> Dictionary<T> for ExplicitIndexLabeler<T> where T: Eq + Hash + Label + Debug {
    fn write_label<W: Write>(&mut self, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        let is_first = {
            let found = self.dictionary.get_mut(label)
                .unwrap_or_else(|| panic!("Could not find label {:?} in ExplicitIndexLabeler"));
            let is_first = found.is_first;
            found.is_first = false;

            // Write the index.
            out.write_varnum(found.index as u32)?;
            is_first
        };

        if is_first {
            // Write the definition.
            debug!(target: "dictionary", "ExplicitIndexLabeler writing definition {:?}", label);
            label.write_definition(parent, self, out)?;
        }
        Ok(is_first)
    }
}

pub struct ParentPredictionLabeler<T, U> where T: Eq + Hash + Label + Clone, U: Dictionary<T> {
    per_parent: HashMap<T, HashMap<T, usize>>,
    strategy: U,
}
impl<T, U> ParentPredictionLabeler<T, U> where T: Eq + Hash + Label + Clone, U: Dictionary<T> {
    pub fn new(strategy: U) -> Self {
        Self {
            per_parent: HashMap::new(),
            strategy
        }
    }
}

impl<T, U> Dictionary<T> for ParentPredictionLabeler<T, U> where T: Eq + Hash + Label + Clone + Debug, U: Dictionary<T> {
    fn write_label<W: Write>(&mut self, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        use std::collections::hash_map::Entry::*;
        let introduced_namespaced_number = if let Some(parent) = parent {
            let this_parent = self.per_parent.entry(parent.clone())
                .or_insert_with(|| HashMap::new());
            let number_of_children = this_parent.len();
            match this_parent.entry(label.clone()) {
                Vacant(entry) => {
                    // Assign a number, write that number, use `strategy` to (maybe) write a definition.
                    debug!(target: "dictionary", "ParentPredictionLabel introducing a new number {} for {:?} > {:?}",
                        number_of_children,
                        parent,
                        label);
                    entry.insert(number_of_children);
                    out.write_varnum(number_of_children as u32)?;
                    true
                }
                Occupied(entry) => {
                    debug!(target: "dictionary", "ParentPredictionLabel reusing number {} for {:?} > {:?}",
                        number_of_children,
                        parent,
                        label);
                    // Reuse existing number, no need to write the definition.
                    out.write_varnum(*entry.get() as u32)?;
                    false
                }
            }
        } else {
            true
        };
        if introduced_namespaced_number {
            // We have introduced a namespaced number.
            // Whether we write the actual definition of the label depends on whether we have already done this globally.
            self.strategy.write_label(label, parent, out)
        } else {
            Ok(false)
        }
    }
}
