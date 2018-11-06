use entropy::probabilities::{ InstancesToProbabilities, Symbol };

use binjs_shared::{ FieldName, InterfaceName };

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;

use range_encoding;

pub type IOPath = binjs_shared::ast::Path<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;
pub type IOPathItem = binjs_shared::ast::PathItem<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;

pub struct Entry<'a, K, T> where K: 'a, T: 'a {
    /// The number of entries for this key in the context.
    pub len: usize,
    pub value: std::collections::hash_map::Entry<'a, K, T>
}

/// An access by index, used in `ContextPredict<C, K, T>`, to
/// extract a `K` (value) from a `C` (context, as provided by
/// `entropy::read::Decoder`) and a symbol number (as provided
/// by the bit-level symbol decoder).
///
/// If the `ContextPredict<C, K, T>` is used for building the
/// dictionary (e.g. `T = usize`), this access by index is
/// meaningless as the indices are unstable. Therefore, we only
/// initialize `ByIndex<T, K>` when converting from a
/// `ContextPredict<C, K, usize>` to a `ContextPredict<C, J, Symbol>`.
///
/// This is encoded at type-level by ensuring that `ByIndex<T, K>::get`
/// is only implemented when `T = Symbol`.
#[derive(Clone, Debug)]
struct ByIndex<T, K> {
    /// Phantom data, used to represent phantom type `T`.
    ///
    /// Doesn't carry data, used only for type-checking.
    phantom: PhantomData<T>,

    /// The mapping from index => value.
    by_index: Vec<K>
}
impl<T, K> ByIndex<T, K> {
    pub fn new() -> Self {
        ByIndex {
            phantom: PhantomData,
            by_index: Vec::new(),
        }
    }
}
impl<K> ByIndex<Symbol, K> {
    pub fn get(&self, index: usize) -> Option<&K> {
        self.by_index.get(index)
    }
}
impl<'de, T, K> serde::de::Deserialize<'de> for ByIndex<T, K> where K: serde::de::Deserialize<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>
    {
        let vec = Vec::deserialize(deserializer)?;
        Ok(ByIndex {
            phantom: PhantomData,
            by_index: vec,
        })
    }
}
impl<T, K> serde::ser::Serialize for ByIndex<T, K> where K: serde::ser::Serialize {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::ser::Serializer
    {
        self.by_index.serialize(serializer)
    }
}
impl<K> From<Vec<K>> for ByIndex<Symbol, K> {
    fn from(by_index: Vec<K>) -> Self {
        ByIndex {
            phantom: PhantomData,
            by_index
        }
    }
}

/// A generic predictor, associating a context and a key to a value.
///
/// For most use cases, you probably want one of the more specialized predictors.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct ContextPredict<C, K, T> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    by_context: HashMap<C, (HashMap<K, T>, ByIndex<T, K>)>,
}
impl<C, K, T> ContextPredict<C, K, T> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    pub fn new() -> Self {
        Self {
            by_context: HashMap::new(),
        }
    }

    pub fn get<C2: ?Sized>(&self, context: &C2, key: &K) -> Option<&T>
        where
            C: std::borrow::Borrow<C2>,
            C2: Hash + Eq
    {
        let (by_key, _) = self.by_context.get(context)?;
        by_key.get(key)
    }

    pub fn get_mut<C2: ?Sized>(&mut self, context: &C2, key: &K) -> Option<&mut T>
        where
            C: std::borrow::Borrow<C2>,
            C2: Hash + Eq
    {
        let (by_key, _) = self.by_context.get_mut(context)?;
        by_key.get_mut(key)
    }

    pub fn entry(&mut self, context: C, key: &K) -> Entry<K, T> {
        let by_key = self.by_context.entry(context)
            .or_insert_with(|| (HashMap::new(), ByIndex::new()));
        Entry {
            len: by_key.0.len(),
            value: by_key.0.entry(key.clone()),
        }
    }

    /// The number of states in this predictor.
    pub fn len(&self) -> usize {
        self.by_context.values()
            .map(|(map, _)| map.len())
            .sum()
    }

    /// All the contexts known to this predictor.
    ///
    /// Used mainly for debugging.
    pub fn contexts(&self) -> impl Iterator<Item=&C> {
        self.by_context.keys()
    }

    /// All the values known to this predictor in a given context.
    ///
    /// Used mainly for debugging.
    pub fn keys_at<C2: ?Sized>(&self, context: &C2) -> Option<impl Iterator<Item=&K>>
        where
            C: std::borrow::Borrow<C2>,
            C2: Hash + Eq
    {
        Some(self.by_context.get(context)?
            .0
            .keys())
    }
}
impl<C, K> ContextPredict<C, K, Symbol> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    /// Get a value by context and index.
    ///
    /// This method is only implemented when `T=Symbol` as the index is initialized
    /// by `instances_to_probabilities`.
    pub fn get_at<C2: ?Sized>(&mut self, context: &C2, index: usize) -> Option<&K>
        where
            C: std::borrow::Borrow<C2>,
            C2: Hash + Eq
    {
        let (_, by_index) = self.by_context.get(context)?;
        by_index.get(index)
    }
}

impl<C, K> InstancesToProbabilities for ContextPredict<C, K, usize> where C: Eq + Hash + Clone + std::fmt::Debug, K: Eq + Hash + Clone {
    type AsProbabilities = ContextPredict<C, K, Symbol>;
    fn instances_to_probabilities(self, description: &str) -> ContextPredict<C, K, Symbol> {
        debug!(target: "entropy", "Converting ContextPredict {} to probabilities", description);
        let by_context = self.by_context.into_iter()
            .map(|(context, (by_key, _))| {
                let instances : Vec<_> = by_key.values()
                    .map(|x| *x as u32)
                    .collect();
                let distribution = Rc::new(RefCell::new(range_encoding::CumulativeDistributionFrequency::new(instances).
                    unwrap()));
                    // FIXME: This will fail if `by_key` is empty.
                    // FIXME: We should have a fallback distribution in case everything is empty.

                let (by_key, by_index): (HashMap<_, _>, Vec<_>) = by_key.into_iter()
                    .enumerate()
                    .map(|(index, (key, _))| {
                        let entry_key = (key.clone(), Symbol {
                            index,
                            distribution: distribution.clone(),
                        });
                        (entry_key, key)
                    })
                    .unzip();
                (context, (by_key, by_index.into()))
            })
            .collect();
        ContextPredict {
            by_context,
        }
    }
}


/// A predictor used to predict the probability of a symbol based on
/// its position in a tree. The predictor is typically customized to
/// limit the prediction depth, e.g. to 0 (don't use any context),
/// 1 (use only the parent + child index) or 2 (use parent +
/// grand-parent and both child indices).
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct PathPredict<K, T> where K: Eq + Hash + Clone {
    context_predict: ContextPredict<IOPath, K, T>,
}


impl<K> InstancesToProbabilities for PathPredict<K, usize> where K: Eq + Hash + Clone {
    type AsProbabilities = PathPredict<K, Symbol>;
    fn instances_to_probabilities(self, description: &str) -> PathPredict<K, Symbol> {
        PathPredict {
            context_predict: self.context_predict
                .instances_to_probabilities(description)
        }
    }
}

impl<K, T> PathPredict<K, T> where K: Eq + Hash + Clone + std::fmt::Debug {
    pub fn new() -> Self {
        PathPredict {
            context_predict: ContextPredict::new(),
        }
    }

    pub fn get(&mut self, tail: &[IOPathItem], key: &K) -> Option<&T> {
        self.context_predict.get(tail, key)
    }

    pub fn get_mut(&mut self, tail: &[IOPathItem], key: &K) -> Option<&mut T> {
        self.context_predict.get_mut(tail, key)
    }

    pub fn entry(&mut self, tail: &[IOPathItem], key: &K) -> Entry<K, T> {
        let tail : Vec<_> = tail.iter()
            .cloned()
            .collect(); // Note: that's bound to be expensive. Ideally, we'd like to avoid most allocations here.
        self.context_predict.entry(tail.into(), key)
    }

    /// The number of states in this predictor.
    pub fn len(&self) -> usize {
        self.context_predict.len()
    }

    /// All the paths known to this predictor.
    ///
    /// Used mainly for debugging.
    pub fn paths(&self) -> impl Iterator<Item=&IOPath> {
        self.context_predict.contexts()
    }

    /// All the values known to this predictor at a given path.
    ///
    /// Used mainly for debugging.
    pub fn keys_at(&self, path: &[IOPathItem]) -> Option<impl Iterator<Item=&K>> {
        self.context_predict.keys_at(path)
    }
}

impl<K> PathPredict<K, Symbol> where K: Eq + Hash + Clone {
    /// Get a value by path and index.
    ///
    /// This method is only implemented when `T=Symbol` as the index is initialized
    /// by `instances_to_probabilities`.
    pub fn get_at(&mut self, tail: &[IOPathItem], index: usize) -> Option<&K> {
        self.context_predict.get_at(tail, index)
    }

    /// Get frequency information for a given path.
    pub fn frequencies_at(&mut self, path: &[IOPathItem]) -> Option<&Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>> {
        let in_context =
            if let Some((in_context, _)) = self.context_predict
                .by_context
                .get_mut(path)
            {
                in_context
            } else {
                return None;
            };
        in_context.values_mut()
            .next()
            .map(|any| &any.distribution)
    }
}