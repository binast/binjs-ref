use entropy::probabilities::{ InstancesToProbabilities, Symbol };

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use range_encoding;

use binjs_shared::{ FieldName, InterfaceName };

pub type IOPath = binjs_shared::ast::Path<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;
pub type IOPathItem = binjs_shared::ast::PathItem<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;

pub struct Entry<'a, K, T> where K: 'a, T: 'a {
    /// The number of entries for this key in the context.
    pub len: usize,
    pub value: std::collections::hash_map::Entry<'a, K, T>
}

/// A generic predictor, associating a context and a key to a value.
#[derive(Debug, Default)]
pub struct ContextPredict<C, K, T> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    by_context: HashMap<C, HashMap<K, T>>,
}
impl<C, K, T> ContextPredict<C, K, T> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    pub fn new() -> Self {
        Self {
            by_context: HashMap::new()
        }
    }
    pub fn get_mut<C2: ?Sized>(&mut self, context: &C2, key: &K) -> Option<&mut T>
        where
            C: std::borrow::Borrow<C2>,
            C2: Hash + Eq
    {
        let by_key = self.by_context.get_mut(context)?;
        by_key.get_mut(key)
    }

    pub fn entry(&mut self, context: C, key: &K) -> Entry<K, T> {
        let by_key = self.by_context.entry(context)
            .or_insert_with(|| HashMap::new());
        Entry {
            len: by_key.len(),
            value: by_key.entry(key.clone()),
        }
    }

    /// The number of states in this predictor.
    pub fn len(&self) -> usize {
        self.by_context.values()
            .map(|map| map.len())
            .sum()
    }
}


impl<C, K> InstancesToProbabilities for ContextPredict<C, K, usize> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    type  AsProbabilities = ContextPredict<C, K, Symbol>;
    fn instances_to_probabilities(self) -> ContextPredict<C, K, Symbol> {
        let by_context = self.by_context.into_iter()
            .map(|(context, by_key)| {
                let instances : Vec<_> = by_key.values()
                    .map(|x| *x as u32)
                    .collect();
                let distribution = Rc::new(RefCell::new(range_encoding::CumulativeDistributionFrequency::new(instances).
                    unwrap()));
                    // FIXME: This will fail if `by_key` is empty.
                    // FIXME: We should have a fallback distribution in case everything is empty.

                let by_key = by_key.into_iter()
                    .enumerate()
                    .map(|(index, (key, _))| {
                        (key, Symbol {
                            index,
                            distribution: distribution.clone(),
                        })
                    })
                    .collect();
                // FIXME: Actually put the number of instances in `distribution`.
                (context, by_key)
            })
            .collect();
        ContextPredict {
            by_context
        }
    }
}


/// A predictor used to predict the probability of a symbol based on
/// its position in a tree. The predictor is typically customized to
/// limit the prediction depth, e.g. to 0 (don't use any context),
/// 1 (use only the parent + child index) or 2 (use parent +
/// grand-parent and both child indices).
#[derive(Debug, Default)]
pub struct PathPredict<K, T> where K: Eq + Hash + Clone {
    context_predict: ContextPredict<IOPath, K, T>,
}


impl<K> InstancesToProbabilities for PathPredict<K, usize> where K: Eq + Hash + Clone {
    type AsProbabilities = PathPredict<K, Symbol>;
    fn instances_to_probabilities(self) -> PathPredict<K, Symbol> {
        PathPredict {
            context_predict: self.context_predict.instances_to_probabilities()
        }
    }
}

impl<K, T> PathPredict<K, T> where K: Eq + Hash + Clone + std::fmt::Debug {
    pub fn new() -> Self {
        PathPredict {
            context_predict: ContextPredict::new(),
        }
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
}
