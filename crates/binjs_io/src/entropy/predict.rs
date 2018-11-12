use entropy::probabilities::{ InstancesToProbabilities, SymbolInfo };

use binjs_shared::{ FieldName, InterfaceName };

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use range_encoding;

pub type IOPath = binjs_shared::ast::Path<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;
pub type IOPathItem = binjs_shared::ast::PathItem<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;

mod context_information {
    use entropy::probabilities::SymbolInfo;
    use std::collections::HashMap;
    use std::hash::Hash;

    /// Information available at a given prediction context (e.g. at a path in the AST).
    ///
    /// This data structure is only meant to be used in two settings:
    ///
    /// - to count the number of instances of values in a context (instantiated with `T=usize`,
    ///     in which case the `usize` is a number of instances); or
    /// - once number of instances have been converted to frequency information and unique indices
    ///     (instantiated with `T=SymbolInfo`, in which case the `SymbolInfo` contains the unique index).
    ///
    /// For this reason, all the meaningful methods of this struct are implemented only if `T=usize`
    /// or `T=SymbolInfo`.
    #[derive(Clone, Debug, Deserialize, Serialize)]
    pub struct ContextInformation<K, T> where K: Eq + Hash {
        /// K => T mapping, always valid
        by_value: HashMap<K, T>,

        /// usize => K mapping.
        ///
        /// This vector is populated only when `T = SymbolInfo`. When that is the case,
        /// `by_index` is effectively the reverse mapping from `by_value` (using the
        /// index embedded in `SymbolInfo`).
        by_index: Vec<K>,
    }
    impl<K, T> ContextInformation<K, T> where K: Eq + Hash {
        pub fn new() -> Self {
            ContextInformation {
                by_value: HashMap::new(),
                by_index: Vec::new(),
            }
        }

        /// Return the number of entries.
        pub fn len(&self) -> usize {
            self.by_value.len()
        }
    }

    // Methods that make sense only when we have finished computing frequency information.
    impl<K> ContextInformation<K, SymbolInfo> where K: Eq + Hash {
        pub fn by_value(&self) -> &HashMap<K, SymbolInfo> {
            &self.by_value
        }

        pub fn by_value_mut(&mut self) -> &mut HashMap<K, SymbolInfo> {
            &mut self.by_value
        }

        pub fn by_index(&self, index: usize) -> Option<&K> {
            self.by_index.get(index)
        }
    }

    // Methods that make sense only while we are collecting instances.

    impl<K> ContextInformation<K, usize> where K: Eq + Hash {
        pub fn add(&mut self, key: K) {
            self.by_value.entry(key)
            .and_modify(|instances| *instances += 1)
            .or_insert(1);
        }
    }

    impl<K> ::entropy::probabilities::InstancesToProbabilities for ContextInformation<K, usize> where K: Clone + Eq + Hash {
        type AsProbabilities = ContextInformation<K, SymbolInfo>;
        fn instances_to_probabilities(self, _description: &str) -> ContextInformation<K, SymbolInfo> {
            let instances: Vec<_> = self
                .by_value
                .values()
                .map(|x| *x as u32)
                .collect();

            let distribution = std::rc::Rc::new(std::cell::RefCell::new(range_encoding::CumulativeDistributionFrequency::new(instances).
                unwrap()));
            // FIXME: This will fail if `by_key` is empty.
            // FIXME: We should have a fallback distribution in case everything is empty.

            let (by_value, by_index): (HashMap<_, _>, Vec<_>) = self.by_value
                .into_iter()
                .enumerate()
                .map(|(index, (key, _))| {
                    let entry_key = (key.clone(), SymbolInfo {
                        index,
                        distribution: distribution.clone(),
                    });
                    (entry_key, key)
                })
                .unzip();
            ContextInformation {
                by_value,
                by_index
            }
        }
    }
}
use self::context_information::ContextInformation;

/// A generic predictor, associating a context and a key to a value.
///
/// For most use cases, you probably want one of the more specialized predictors.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ContextPredict<C, K, T> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    by_context: HashMap<C, ContextInformation<K, T>>,
}
impl<C, K, T> ContextPredict<C, K, T> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    pub fn new() -> Self {
        Self {
            by_context: HashMap::new()
        }
    }

    /// All the contexts known to this predictor.
    ///
    /// Used mainly for debugging.
    pub fn contexts(&self) -> impl Iterator<Item=&C> {
        self.by_context.keys()
    }

    /// The number of states in this predictor.
    pub fn len(&self) -> usize {
        self.by_context.values()
            .map(ContextInformation::len)
            .sum()
    }
}

impl<C, K> ContextPredict<C, K, usize> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    /// Insert a value in a context.
    ///
    /// Only usable when we're still collecting values (`T = usize`).
    pub fn add(&mut self, context: C, key: K) {
        let by_key = self.by_context.entry(context)
            .or_insert_with(|| ContextInformation::new());
        by_key.add(key)
    }
}

impl<C, K> ContextPredict<C, K, SymbolInfo> where C: Eq + Hash + Clone, K: Eq + Hash + Clone {
    /// Get a key by context and index.
    ///
    /// This method is only implemented when `T=SymbolInfo` as the index is initialized
    /// by `instances_to_probabilities`. The index corresponds to the one defined in
    /// the `SymbolInfo`.
    pub fn by_index<C2: ?Sized>(&mut self, context: &C2, index: usize) -> Option<&K>
        where
            C: std::borrow::Borrow<C2>,
            C2: Hash + Eq
    {
        self.by_context.get(context)?
            .by_index(index)
    }

    pub fn by_key<C2: ?Sized>(&self, context: &C2, key: &K) -> Option<&SymbolInfo>
        where
            C: std::borrow::Borrow<C2>,
            C2: Hash + Eq
    {
        self.by_context.get(context)?
            .by_value()
            .get(key)
    }

    pub fn by_key_mut<C2: ?Sized>(&mut self, context: &C2, key: &K) -> Option<&mut SymbolInfo>
        where
            C: std::borrow::Borrow<C2>,
            C2: Hash + Eq
    {
        self.by_context.get_mut(context)?
            .by_value_mut()
            .get_mut(key)
    }
}

impl<C, K> InstancesToProbabilities for ContextPredict<C, K, usize> where C: Eq + Hash + Clone + std::fmt::Debug, K: Eq + Hash + Clone {
    type AsProbabilities = ContextPredict<C, K, SymbolInfo>;
    fn instances_to_probabilities(self, description: &str) -> ContextPredict<C, K, SymbolInfo> {
        debug!(target: "entropy", "Converting ContextPredict {} to probabilities", description);
        let by_context = self.by_context.into_iter()
            .map(|(context, info)| (context, info.instances_to_probabilities("ContextInformation")))
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
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct PathPredict<K, T> where K: Eq + Hash + Clone {
    context_predict: ContextPredict<IOPath, K, T>,
}


impl<K> InstancesToProbabilities for PathPredict<K, usize> where K: Eq + Hash + Clone {
    type AsProbabilities = PathPredict<K, SymbolInfo>;
    fn instances_to_probabilities(self, description: &str) -> PathPredict<K, SymbolInfo> {
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
}
impl<K> PathPredict<K, usize> where K: Eq + Hash + Clone {
    pub fn add(&mut self, path: &[IOPathItem], key: K) {
        let mut as_path = IOPath::new();
        as_path.extend_from_slice(path);
        self.context_predict.add(as_path, key);
    }
}
impl<K> PathPredict<K, SymbolInfo> where K: Eq + Hash + Clone {
    /// Get a value by path and index.
    ///
    /// This method is only implemented when `T=SymbolInfo` as the index is initialized
    /// by `instances_to_probabilities`.
    pub fn by_index(&mut self, tail: &[IOPathItem], index: usize) -> Option<&K> {
        self.context_predict.by_index(tail, index)
    }


    pub fn by_key(&mut self, tail: &[IOPathItem], key: &K) -> Option<&SymbolInfo> {
        self.context_predict.by_key(tail, key)
    }

    pub fn by_key_mut(&mut self, tail: &[IOPathItem], key: &K) -> Option<&mut SymbolInfo> {
        self.context_predict.by_key_mut(tail, key)
    }


    /// Get frequency information for a given path.
    pub fn frequencies_at(&mut self, path: &[IOPathItem]) -> Option<&Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>> {
        let info =
            if let Some(info) = self.context_predict
                .by_context
                .get_mut(path)
            {
                info
            } else {
                return None;
            };
        info
            .by_value_mut()
            .values_mut()
            .next()
            .map(|any| &any.distribution)
    }
}

