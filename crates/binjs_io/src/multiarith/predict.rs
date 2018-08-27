use multiarith::tree::{ Path, Tag };

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use range_encoding;

/// Representation of a symbol in a Cumulative Distribution Frequency.
#[derive(Clone)]
pub struct Symbol {
    /// The index of the symbol in the CDF.
    pub index: usize,

    /// The CDF, shared between a number of symbols.
    pub distribution: Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>,
}

/// A generic predictor, associating a context and a key to a value.
pub struct ContextPredict<C, K, T> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    by_context: HashMap<C, HashMap<K, T>>,
}
impl<C, K, T> ContextPredict<C, K, T> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    pub fn new() -> Self {
        Self {
            by_context: HashMap::new()
        }
    }
    pub fn get_mut(&mut self, context: &C, key: &K) -> Option<&mut T> {
        let by_key = self.by_context.get_mut(context)?;
        by_key.get_mut(key)
    }

    pub fn entry(&mut self, context: C, key: &K) -> (usize, std::collections::hash_map::Entry<K, T>) {
        let by_key = self.by_context.entry(context)
            .or_insert_with(|| HashMap::new());
        (by_key.len(), by_key.entry(key.clone()))
    }
}
impl<C, K> ContextPredict<C, K, usize> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    /// Utility: convert a number of instances for each symbol into
    /// a probability distribution.
    pub fn instances_to_probabilities(mut self) -> ContextPredict<C, K, Symbol> {
        let by_context = self.by_context.drain()
            .map(|(context, by_value)| {
                let instances : Vec<_> = by_value.values()
                    .map(|x| *x as u32)
                    .collect();
                let distribution = Rc::new(RefCell::new(range_encoding::CumulativeDistributionFrequency::new(instances).
                    unwrap())); // FIXME: This will fail if `by_value` is empty.

                let by_value = by_value.into_iter()
                    .enumerate()
                    .map(|(index, (key, _))| {
                        (key, Symbol {
                            index,
                            distribution: distribution.clone(),
                        })
                    })
                    .collect();
                // FIXME: Actually put the number of instances in `distribution`.
                (context, by_value)
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
pub struct PathPredict<K, T> where K: Eq + Hash + Clone {
    /// Depth to use for prediction.
    ///
    /// 0: no context
    /// 1: use parent
    /// 2: use parent + grand parent
    /// ...
    depth: usize,

    context_predict: ContextPredict<Path<(Tag, usize)>, K, T>,
}

impl<K> PathPredict<K, usize> where K: Eq + Hash + Clone {
    /// Utility: convert a number of instances for each symbol into
    /// a probability distribution.
    pub fn instances_to_probabilities(self) -> PathPredict<K, Symbol> {
        PathPredict {
            depth: self.depth,
            context_predict: self.context_predict.instances_to_probabilities()
        }
    }
}

impl<K, T> PathPredict<K, T> where K: Eq + Hash + Clone + std::fmt::Debug {
    pub fn new(depth: usize) -> Self {
        PathPredict {
            depth,
            context_predict: ContextPredict::new(),
        }
    }

    pub fn get_mut(&mut self, path: &Path<(Tag, usize)>, key: &K) -> Option<&mut T> {
        self.context_predict.get_mut(&path.tail(self.depth), key)
    }

    pub fn entry(&mut self, path: &Path<(Tag, usize)>, key: &K) -> (usize, std::collections::hash_map::Entry<K, T>) {
        self.context_predict.entry(path.tail(self.depth), key)
    }
}
