use binjs_shared;

use bytes::varnum::*;

use std;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::io::Write;

pub trait Label : Sized {
    /// Write the definition of the label.
    ///
    /// `index`: The index assigned to the label. May be `None` for a dictionary
    /// that doesn't use indices.
    /// `parent`: The parent of the label. May be `None` for a root or a dictionary
    /// that doesn't use parents.
    fn write_definition<W: Write, L: Dictionary<Self, W>>(&self, index: Option<usize>, parent: Option<&Self>, strategy: &mut L, out: &mut W) -> Result<(), std::io::Error>;
}

pub trait Dictionary<T, W: Write> {
    /// Return `true` if we just added the definition of the label to the dictionary,
    /// `false` if it was already present.
    fn write_label(&mut self, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        self.write_label_at(None, label, parent, out)
    }
    fn write_label_at(&mut self, baseline: Option<usize>, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error>;
}

fn add_baseline(index: usize, baseline: Option<usize>) -> usize {
    match baseline {
        None => index,
        Some(x) => index + x,
    }
}

impl<T, U, W: Write> Dictionary<T, W> for Box<U> where U: Dictionary<T, W> {
    /// Return `true` if we just added the definition of the label to the dictionary,
    /// `false` if it was already present.
    fn write_label(&mut self, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        self.write_label_at(None, label, parent, out)
    }
    fn write_label_at(&mut self, baseline: Option<usize>, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        use std::ops::DerefMut;
        self.deref_mut().write_label_at(baseline, label, parent, out)
    }
}


/// The dumbest possible labeler: always copy the definition.
///
/// Useful for numbers, booleans, etc.
pub struct RawLabeler<T> {
    phantom: std::marker::PhantomData<T>,
}
impl<T> RawLabeler<T> {
    pub fn new() -> Self {
        Self {
            phantom: std::marker::PhantomData,
        }
    }
}
impl<T, W: Write> Dictionary<T, W> for RawLabeler<T> where T: Label {
    fn write_label_at(&mut self, baseline: Option<usize>, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        label.write_definition(baseline, parent, self, out)?;
        Ok(true)
    }
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
impl<T, W: Write> Dictionary<T, W> for MRULabeler<T> where T: Eq + Label + Clone {
    fn write_label_at(&mut self, baseline: Option<usize>, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        use binjs_shared::mru::Seen::*;
        match self.mru.access(label) {
            Age(index) => {
                // The label has already been seen, just write how many accesses ago.
                out.write_varnum(add_baseline(index, baseline) as u32)?;
                Ok(false)
            }
            Never(index) => {
                // This is the first time the label is seen. Write the "new" and its definition.
                label.write_definition(Some(add_baseline(index, baseline)), parent, self, out)?;
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
impl<T, W: Write> Dictionary<T, W> for ExplicitIndexLabeler<T> where T: Eq + Hash + Label + Debug {
    fn write_label_at(&mut self, baseline: Option<usize>, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        let (index, is_first) = {
            let found = self.dictionary.get_mut(label)
                .unwrap_or_else(|| panic!("Could not find label {:?} in ExplicitIndexLabeler", label));
            let is_first = found.is_first;
            found.is_first = false;
            (found.index, is_first)
        };
        if is_first {
            label.write_definition(Some(add_baseline(index, baseline)), parent, self, out)?;
        } else {
            out.write_varnum(add_baseline(index, baseline) as u32)?;
        };

        Ok(is_first)
    }
}

/// The first time a node `A` contains a node `B`, assign a new number to `A > B`. For each
/// `A`, numbers start at 0.
pub struct ParentPredictionDumbLabeler<T, U, W> where T: Eq + Hash + Label + Clone, U: Dictionary<T, W>, W: Write {
    per_parent: HashMap<Option<T>, HashMap<T, usize>>,
    strategy: U,
    phantom: std::marker::PhantomData<W>,
}
impl<T, U, W> ParentPredictionDumbLabeler<T, U, W> where T: Eq + Hash + Label + Clone, U: Dictionary<T, W>, W: Write {
    pub fn new(strategy: U) -> Self {
        Self {
            per_parent: HashMap::new(),
            phantom: std::marker::PhantomData,
            strategy
        }
    }
}

impl<T, U, W> Dictionary<T, W> for ParentPredictionDumbLabeler<T, U, W> where T: Eq + Hash + Label + Clone + Debug, U: Dictionary<T, W>, W: Write {
    fn write_label_at(&mut self, baseline: Option<usize>, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        use std::collections::hash_map::Entry::*;
        let introduced_namespaced_number = {
            let this_parent = self.per_parent.entry(parent.map(T::clone))
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
                    Some(number_of_children)
                }
                Occupied(entry) => {
                    debug!(target: "dictionary", "ParentPredictionLabel reusing number {} for {:?} > {:?}",
                        *entry.get(),
                        parent,
                        label);
                    // Reuse existing number, no need to write the definition.
                    out.write_varnum(add_baseline(*entry.get(), baseline) as u32)?;
                    None
                }
            }
        };
        if let Some(index) = introduced_namespaced_number {
            // We have introduced a namespaced number.
            // Whether we write the actual definition of the label depends on whether we have already done this globally.
            self.strategy.write_label_at(Some(add_baseline(index, baseline)), label, parent, out)
        } else {
            Ok(false)
        }
    }
}

struct Entries<T> {
    label: T,
    instances: usize,
}

/// The most common child of a label `A` so far gets number 0,
/// the second most common gets number 1, etc.
pub struct ParentPredictionFrequencyLabeler<T, U, W> where T: Eq + Hash + Label + Clone, U: Dictionary<T, W>, W: Write {
    per_parent: HashMap<Option<T>, Vec<Entries<T>>>, // Invariant: always sorted by decreasing order of `instances`.
    strategy: U,
    phantom: std::marker::PhantomData<W>,
}
impl<T, U, W> ParentPredictionFrequencyLabeler<T, U, W> where T: Eq + Hash + Label + Clone, U: Dictionary<T, W>, W: Write {
    pub fn new(strategy: U) -> Self {
        Self {
            per_parent: HashMap::new(),
            phantom: std::marker::PhantomData,
            strategy
        }
    }
}

impl<T, U, W> Dictionary<T, W> for ParentPredictionFrequencyLabeler<T, U, W> where T: Eq + Hash + Label + Clone + Debug, U: Dictionary<T, W>, W: Write {
    fn write_label_at(&mut self, baseline: Option<usize>, label: &T, parent: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        use std::collections::hash_map::Entry::*;
        let this_parent = self.per_parent.entry(parent.map(T::clone))
            .or_insert_with(|| Vec::new());

        if let Some(position) = this_parent.iter().position(|entries| entries.label == *label) {
            // This label has already shown up.
            // Reuse the index previously computed.
            out.write_varnum(add_baseline(position, baseline) as u32)?;


            // Before leaving, update statistics.
            let number_of_instances = this_parent[position].instances + 1;
            this_parent[position].instances = number_of_instances;

            // Bubble entry to ensure that no entry with a lower index has more instances.
            if let Some(prefix) = this_parent[0..position]
                .iter()
                .position(|entry| {
                    entry.instances < number_of_instances
                })
            {
                this_parent[prefix .. position + 1].rotate_right(1);
            }

            Ok(false)
        } else {
            // First time we see this label.
            // Delegate writing to the underlying strategy.
            let is_newdef = self.strategy.write_label_at(Some(add_baseline(this_parent.len(), baseline)), label, parent, out)?;

            // Before leaving, update statistics.
            this_parent.push(Entries {
                instances: 1,
                label: label.clone(),
            });
            Ok(is_newdef)
        }
    }
}

/// Remove any parent information.
pub struct GlobalLabeler<T, U, W> where U: Dictionary<T, W>, W: Write {
    strategy: U,
    phantom: std::marker::PhantomData<(T, W)>,
}
impl<T, U, W> GlobalLabeler<T, U, W> where T: Eq + Hash + Label + Clone, U: Dictionary<T, W>, W: Write {
    pub fn new(strategy: U) -> Self {
        Self {
            strategy,
            phantom: std::marker::PhantomData,
        }
    }
}
impl<T, U, W> Dictionary<T, W> for GlobalLabeler<T, U, W> where U: Dictionary<T, W>, W: Write {
    fn write_label_at(&mut self, baseline: Option<usize>, label: &T, _: Option<&T>, out: &mut W) -> Result<bool, std::io::Error> {
        self.strategy.write_label_at(baseline, label, None, out)
    }
}
