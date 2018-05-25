//! An implementation of TreeRePair http://www.eti.uni-siegen.de/ti/veroeffentlichungen/12-repair.pdf

use bytes;

use ::{ DictionaryPlacement, NumberingStrategy, TokenWriterError };
use io::TokenWriter;
use labels:: { self, Label as WritableLabel, Dictionary };
use util::{ Counter, GenericCounter, Pos };

use std;
use std::cell::{ RefCell, RefMut };
use std::collections::{ HashMap };
use std::fmt::Debug;
use std::hash::Hash;
use std::io::Write;
use std::rc::{ Rc, Weak };

use itertools::Itertools;

#[derive(Clone, Debug)]
pub struct Options {
    /// If specified, reduce all nodes (including lists) to ensure that they have at
    /// most `max_rank` children. The original paper recomments a value of 4.
    pub max_rank: Option<usize>,

    /// The numbering strategy used for tree labels.
    pub numbering_strategy: NumberingStrategy,

    /// Where to put the dictionary in the file.
    pub dictionary_placement: DictionaryPlacement,
}

type SharedCell<T> = Rc<RefCell<T>>;



#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct NodeIndex(usize);
impl Counter for NodeIndex {
    fn internal_make(value: usize) -> Self {
        NodeIndex(value)
    }
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct LabelIndex(usize);
impl Counter for LabelIndex {
    fn internal_make(value: usize) -> Self {
        LabelIndex(value)
    }
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct DigramIndex(usize);
impl Counter for DigramIndex {
    fn internal_make(value: usize) -> Self {
        DigramIndex(value)
    }
}


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct GeneratedLabel(usize);
impl Counter for GeneratedLabel {
    fn internal_make(value: usize) -> Self {
        GeneratedLabel(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Label<T> where T: Eq + Hash + Ord + Clone + Debug {
    Original {
        content: T,
        children: usize,
    },
    Generated {
        label: GeneratedLabel,

        /// The digram to which this label expands.
        /// When we serialize, the first instance of the generated
        /// label is immediately followed by its number of children
        /// and digram. As the digram may itself contain generated
        /// labels, serialization of the digram may itself contain
        /// [number of children; digrams] sequences.
        digram: Rc<Digram<T>>,
        children: usize,
    },
}

impl<T> Label<T> where T: Eq + Hash + Ord + Clone + Debug {
    /// Return the number of children of a label.
    fn len(&self) -> usize {
        use self::Label::*;
        match *self {
            Original { ref children, .. }
            | Generated { ref children, .. } => *children,
        }
    }
}

impl<T> Label<T> where T: Eq + Ord + Hash + Clone + Debug {
    fn collect(&self, labels: &mut HashMap<Label<T>, usize> ){
        {
            let entry = labels.entry(self.clone())
                .or_insert(0);
            *entry += 1;
        }
        if let Label::Generated { ref digram, .. } = *self {
            digram.parent.collect(labels);
            digram.child.collect(labels);
        }
    }
}

/// A subtree.
///
/// Designed to be used as `Rc<RefCell<SubTree<T>>>`.
#[derive(Debug, Clone)]
pub struct SubTree<T> where T: Eq + Hash + Ord + Clone + Debug { // FIXME: Make it private, eventually.
    /// An index, used to quickly compare tree nodes.
    index: NodeIndex,

    /// The label
    label: Label<T>,

    /// Children.
    children: Vec<SharedCell<SubTree<T>>>,

    /// The parent. May be Weak::default() if this is the root.
    parent: Weak<RefCell<SubTree<T>>>,

    /// A mechanism to remove oneself from a list of subtrees.
    /// Index `i` corresponds to the digram `(label, i, children[i].label)`.
    digrams: Vec<(Weak<DigramInstances<T>>, list::Remover<SharedCell<SubTree<T>>>)>,
}

/// Equality only compares `index`.
impl<T> PartialEq for SubTree<T> where T: Eq + Hash + Ord + Clone + Debug {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}
/// Equality only compares `index`.
impl<T> Eq for SubTree<T> where T: Eq + Hash + Ord + Clone + Debug { }

/// Order only compares `index`.
impl<T> PartialOrd for SubTree<T> where T: Eq + Hash + Ord + Clone + Debug {
    fn partial_cmp(&self, other: &SubTree<T>) -> std::option::Option<std::cmp::Ordering> {
        NodeIndex::partial_cmp(&self.index, &other.index)
    }
}
/// Order only compares `index`.
impl<T> Ord for SubTree<T> where T: Eq + Hash + Ord + Clone + Debug {
    fn cmp(&self, other: &SubTree<T>) -> std::cmp::Ordering {
        NodeIndex::cmp(&self.index, &other.index)
    }
}
/// Hashing only uses `index`.
impl<T> std::hash::Hash for SubTree<T> where T: Eq + Hash + Ord + Clone + Debug {
    fn hash<H>(&self, state: &mut H) where H: std::hash::Hasher {
        self.index.hash(state)
    }
}

trait SanityCheck {
    fn sanity_check(&self) -> bool;
}
impl<T> SanityCheck for Rc<RefCell<SubTree<T>>> where T: Eq + Hash + Ord + Clone + Debug {
    fn sanity_check(&self) -> bool {
        debug_assert!(self.borrow().sanity_check());
        if let Some(parent) = self.borrow().parent.upgrade() {
            let borrow_parent = parent.borrow();
            borrow_parent.children()
                .position(|sibling| Rc::ptr_eq(sibling, self))
                .unwrap_or_else(|| panic!("My parent doesn't have me as a child (by ptr) {:?} > {:?}", borrow_parent.label, self.borrow().label));
        }
        true
    }
}
impl<T> SanityCheck for SubTree<T> where T: Eq + Hash + Ord + Clone + Debug {
    fn sanity_check(&self) -> bool {
        assert_eq!(self.children.len(), self.label.len());
        for child in &self.children {
            let child_parent = child.borrow()
                .parent
                .upgrade()
                .expect("My child doesn't have a parent");
            assert_eq!(child_parent.borrow().index, self.index, "My child's parent is not me.");
        }
        if let Some(parent) = self.parent.upgrade() {
            let ref my_index = self.index;
            let borrow_parent = parent.borrow();
            borrow_parent.children()
                .position(|sibling| sibling.borrow().index == *my_index)
                .unwrap_or_else(|| panic!("My parent doesn't have me as a child (by index): got [\n{:?}\n    ], looking for {:?}",
                    borrow_parent.children()
                        .map(|c| format!("        {:?}", c.borrow().label))
                        .format(",\n"),
                    self.label));
        }
        if self.digrams.len() != 0 {
            assert_eq!(self.digrams.len(), self.label.len());
            for (weak_instance, _) in &self.digrams {
                if let Some(instance) = weak_instance.upgrade() {
                    debug_assert!(instance.sanity_check());
                }
            }
        }
        true
    }
}

impl<T> SubTree<T> where T: Eq + Hash + Ord + Clone + Debug {
    /// Iterate over the children of the tree.
    fn children(&self) -> impl Iterator<Item = &SharedCell<SubTree<T>>> {
        self.children.iter()
    }
}

impl<T> SubTree<T> where T: Eq + Hash + Ord + Clone + Debug {
    fn collect_labels(&self) -> HashMap<Label<T>, usize> {
        fn aux<T>(tree: &SubTree<T>, labels: &mut HashMap<Label<T>, usize>) where T: Eq + Hash + Ord + Clone + Debug {
            tree.label.collect(labels);
            for child in &tree.children {
                aux(&*child.borrow(), labels);
            }
        }
        let mut map = HashMap::new();
        aux(self, &mut map);
        map
    }
}

struct Root<T> where T: Eq + Hash + Ord + Clone + Debug {
    tree: Option<SharedCell<SubTree<T>>>,
    node_counter: GenericCounter<NodeIndex>,
    generated_label_counter: GenericCounter<GeneratedLabel>,
}
impl<T> Root<T> where T: Eq + Hash + Ord + Clone + Debug {
    fn new(max_rank: Option<usize>) -> Self {
        let mut node_counter = GenericCounter::new();
        let generated_label_counter = GenericCounter::new();
        let index = node_counter.next();
        Root {
            node_counter,
            generated_label_counter,
            tree: None,
        }
    }
    fn new_generated_label(&mut self, children: usize, digram: Rc<Digram<T>>) -> Label<T> {
        Label::Generated {
            label: self.generated_label_counter.next(),
            digram,
            children,
        }
    }
    fn new_subtree(&mut self, mut label: Label<T>, mut children: Vec<SharedCell<SubTree<T>>>) -> SharedCell<SubTree<T>> {
        debug_assert!{
            children.iter()
                .find(|child| {
                    child.borrow()
                        .digrams
                        .len() != 0
                })
                .is_none(),
            "We shouldn't be calling `new_subtree` once we have started filling the digrams."
        }
        let parent = Rc::new(RefCell::new(SubTree {
            index: self.node_counter.next(),
            label,
            parent: Weak::default(),
            digrams: vec![],
            children: children
        }));
        let weak = Rc::downgrade(&parent);
        for child in &parent.borrow().children {
            let mut child_borrow = child.borrow_mut();
            debug_assert!(child_borrow.parent.upgrade().is_none(), "We shouldn't use new_subtree for a child that already has a parent");
            child_borrow.parent = weak.clone();
        }
        self.tree = Some(parent.clone());
        parent
    }
}

type SharedTree<T> = SharedCell<SubTree<T>>;

pub struct Encoder<T> where T: Eq + Hash + Ord + Clone + Debug {
    root: Root<T>,
    digram_counter: GenericCounter<DigramIndex>,
    pending_digrams: HashMap<Digram<T>, Rc<DigramInstances<T>>>,
    numbering_strategy: NumberingStrategy,
    dictionary_placement: DictionaryPlacement,
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct Digram<T> where T: Eq + Hash + Ord + Clone + Debug { // FIXME: Should be private, really.
    /// The parent label. It will be replaced by a new, per-digram, generated label.
    parent: Label<T>,
    /// The index of the child of the parent to replace.
    position: usize,
    /// The child label. It will be replaced by its own children.
    child: Label<T>,
}

/// Places where we can substitute a digram.
// FIXME: We probably don't need that much sharing.
#[derive(Debug)]
struct DigramInstances<T> where T: Eq + Hash + Ord + Clone + Debug {
    /// An index, used for debugging purposes.
    index: DigramIndex,

    digram: Rc<Digram<T>>,

    /// Invariant: these instances are always in post-order (descendants appear before
    /// ancestors).
    ///
    /// Note that some of the instances may be invalid, if a previous substitution has
    /// conflicted with this digram.
    instances: SharedCell<list::List<SharedCell<SubTree<T>>>>,

    /// Remover, used by the `DigramPriorityQueue` to remove this set of instances
    /// from a slot. This property is meant to be manipulated **only** by `DigramPriorityQueue`.
    remover: RefCell<Option<prio::Remover<Rc<DigramInstances<T>>>>>,
}
impl<T> DigramInstances<T> where T: Eq + Hash + Ord + Clone + Debug {
    fn new(digram: Digram<T>, index: DigramIndex) -> Self {
        Self {
            index,
            digram: Rc::new(digram),
            instances: Rc::new(RefCell::new(list::List::new())),
            remover: RefCell::new(None)
        }
    }

    /// Return the expected number of occurrences of this digram in the AST.
    fn len(&self) -> usize {
        self.instances.borrow().len()
    }
}

trait RcDigramInstances where Self::T: Eq + Hash + Ord + Clone + Debug {
    type T;
    fn insert(&self, node: &SharedCell<SubTree<Self::T>>) {
        let mut borrow_node = node.borrow_mut();
        self.insert_borrowed(node, &mut borrow_node);
    }
    fn insert_borrowed<'a>(&self, node: &SharedCell<SubTree<Self::T>>, borrow_node: &mut RefMut<'a, SubTree<Self::T>>);
}
impl<T> RcDigramInstances for Rc<DigramInstances<T>> where T: Eq + Hash + Ord + Clone + Debug {
    type T = T;
    fn insert_borrowed<'a>(&self, node: &SharedCell<SubTree<T>>, borrow_node: &mut RefMut<'a, SubTree<T>>) {
        let node_remover = self.instances
            .as_ref()
            .borrow_mut()
            .push(node.clone());
        let weak = Rc::downgrade(&self);
        if borrow_node.digrams.len() > self.digram.position {
            // We're replacing an existing remover.
            borrow_node.digrams[self.digram.position] = (weak, node_remover);
        } else if borrow_node.digrams.len() == self.digram.position {
            // We're pushing a new remover.
            borrow_node.digrams.push((weak, node_remover));
        } else {
            // The entire node has been regenerated, ignore.
            return;
        }
    }
}

impl<T> SanityCheck for DigramInstances<T> where T: Eq + Hash + Ord + Clone + Debug {
    fn sanity_check(&self) -> bool {
        self.remover.borrow()
            .as_ref()
            .unwrap_or_else(|| panic!("Sanity check: Missing remover in DigramInstances ({} occurences)", self.instances.borrow().len()));
        true
    }
}
impl<T> PartialEq for DigramInstances<T> where T: Eq + Hash + Ord + Clone + Debug {
    fn eq(&self, other: &Self) -> bool {
        self.digram == other.digram
    }
}
impl<T> Eq for DigramInstances<T> where T: Eq + Hash + Ord + Clone + Debug { }
/*
impl<T> PartialOrd for DigramInstances<T> {
    fn partial_cmp(&self, other: &Self) -> std::option::Option<std::cmp::Ordering> {
        let my_len = self.len();
        let other_len = other.len();
        usize::partial_cmp(&my_len, &other_len)
    }
}
// FIXME: That's not a good `Ord`.
impl<T> Ord for DigramInstances<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let my_len = self.len();
        let other_len = other.len();
        usize::cmp(&my_len, &other_len)
    }
}
*/

const MINIMAL_NUMBER_OF_INSTANCE : usize = 2;

impl<T> Encoder<T> where T: Eq + Hash + Ord + Clone + Debug {
    pub fn new(options: Options) -> Self {
        info!(target: "repair", "Repair compression with options {:?}", options);
        Self {
            root: Root::new(options.max_rank),
            numbering_strategy: options.numbering_strategy,
            digram_counter: GenericCounter::new(),
            pending_digrams: HashMap::with_capacity(4096),
            dictionary_placement: options.dictionary_placement,
        }
    }

    /// Compute a single digram for a node.
    fn compute_digram_at(&mut self, node: &Rc<RefCell<SubTree<T>>>, index: NodeIndex) {
        debug_assert!(node.sanity_check());
        let mut borrow_node = node.borrow_mut();
        let position = borrow_node.children.iter()
            .position(|child| child.borrow().index == index);
        let position = match position {
            // This child was already removed
            None => { return; }
            Some(position) => position
        };
        let digram = Digram {
            parent: borrow_node.label.clone(),
            position,
            child: borrow_node.children[position].borrow().label.clone(),
        };
        let next = self.digram_counter.next();
        let instances = self.pending_digrams
            .entry(digram.clone())
            .or_insert_with(|| {
                debug!(target: "repair", "compute_digram_at: new DigramInstances for {:?}", digram);
                Rc::new(DigramInstances::new(digram, next))
            });
        instances.insert_borrowed(node, &mut borrow_node);
    }

    /// Compute all digrams for a node.
    fn compute_digrams_for_node(&mut self, node: &Rc<RefCell<SubTree<T>>>) {
        debug_assert!(node.sanity_check());
        let mut borrow_node = node.borrow_mut();
        let parent = borrow_node.label.clone();
        let children : Vec<_> = borrow_node.children.iter().cloned().collect(); // FIXME: Find a better way to workaround sharing issues
        for (position, child) in children.into_iter().enumerate() {
            let digram = Digram {
                parent: parent.clone(),
                position,
                child: child.borrow().label.clone(),
            };
            let next = self.digram_counter.next();
//            debug!(target: "repair", "compute_digrams_for_node: Pending digrams has {} entries, looking for DigramInstances for {:?}", self.pending_digrams.len(), digram);
            let instances = self.pending_digrams
                .entry(digram.clone())
                .or_insert_with(|| {
                    debug!(target: "repair", "compute_digrams_for_node: new DigramInstances for {:?}", digram);
                    Rc::new(DigramInstances::new(digram, next))
                });
            instances.insert_borrowed(node, &mut borrow_node);
        }
    }

    /// Compute all digrams for a subtree
    fn compute_digrams_for_subtree(&mut self, node: &Rc<RefCell<SubTree<T>>>) {
        {
            let borrow_node = node.borrow();
            // First, the children.
            for child in borrow_node.children.iter() {
                self.compute_digrams_for_subtree(child)
            }
        }
        // Then, the node itself.
        self.compute_digrams_for_node(node);
    }

    fn proceed_with_tree_repair(&mut self) {
        // Replacement phase.

        // Compute the initial set of digrams.
        let mut digrams_per_priority = {
            let root = self.root.tree.clone()
                .unwrap();
            self.compute_digrams_for_subtree(&root);
            let highest_priority = self.pending_digrams.values()
                .map(|instances| instances.as_ref().instances.borrow().len())
                .max()
                .expect("No digrams found!");
            info!(target: "repair", "During startup, the highest digram priority found was {}", highest_priority);
            let mut digrams_per_priority = DigramPriorityQueue::with_capacity(highest_priority);
            for (_, instances) in self.pending_digrams.drain() {
                digrams_per_priority.insert(&instances);
            }
            digrams_per_priority
        };

        // Pick most frequent digram.
        let mut parent_cleanup = vec![];
        'per_digram: while let Some((priority, digram_instances)) = digrams_per_priority.pop() {
            debug_assert!(digram_instances.sanity_check());
            if digram_instances.len() < MINIMAL_NUMBER_OF_INSTANCE {
                // Only so few instances? It's not worth replacing.
                continue 'per_digram;
            }

            let mut actual_instances_encountered = 0;

            debug!(target: "repair", "Considering digram {:?} with {} occurrences", digram_instances.digram, digram_instances.len());
            // Generate a new label `generated`.
            let number_of_children = digram_instances.digram.parent.len() + digram_instances.digram.child.len() - 1;
            let generated = self.root.new_generated_label(number_of_children, digram_instances.digram.clone());

            // Replace instances of `digram` with `generated` all over the tree.
            let mut borrow_instances = digram_instances.instances.borrow_mut();
            // debug!(target: "repair", "This digram should have {} instances", borrow_instances.len());
            'per_node: for instance in borrow_instances.iter() {
                {
                    let borrow_instance = instance.borrow();
                    if digram_instances.digram.parent != borrow_instance.label
                    || borrow_instance.children[digram_instances.digram.position].borrow().label != digram_instances.digram.child {
                        // The node has been rewritten, the digram doesn't apply anymore.
                        continue 'per_node;
                    }
                }
                debug_assert!(instance.sanity_check());
                actual_instances_encountered += 1;

                // Since we're changing the label of the instance, any digram list to which it
                // belongs is now invalid, so downgrade appropriately.
                digrams_per_priority.remove_full_node_except(&instance, &*digram_instances.digram);

                let mut borrow_instance = instance.borrow_mut();

                let mut children = Vec::with_capacity(number_of_children);

                std::mem::swap(&mut borrow_instance.children, &mut children);
                let mut iter = children.into_iter();

                // Keep the first `digram.position` children. // FIXME: A convenient LinkedList would be faster
                for _ in 0 .. digram_instances.digram.position {
                    borrow_instance.children.push(iter.next().unwrap());
                }

                // Inline the children of child `digram.position`.
                let removed = iter.next().unwrap();

                // Since `removed` is being removed, remove it from any priority.
                digrams_per_priority.remove_full_node_except(&removed, &*digram_instances.digram);

                {
                    let mut borrow_removed = removed.borrow_mut();
                    debug_assert_eq!(borrow_removed.label, digram_instances.digram.child);

                    // Since we're removing `removed`, it doesn't belong to any digram list anymore.
                    for child in &borrow_removed.children {
                        let mut borrow_child = child.borrow_mut();
                        borrow_child.parent = Rc::downgrade(&instance);
                        borrow_instance.children.push(child.clone());
                    }
                }

                // Then copy the remaining children.
                borrow_instance.children.extend(iter);

                debug_assert_eq!(borrow_instance.children.len(), number_of_children);

                // Finally, change the label.
                borrow_instance.label = generated.clone();

                // We have **replaced** one new possible digram in the parent node.
                if let Some(parent) = borrow_instance.parent.upgrade() {
                    // May be `None` if we are the root node.

                    parent_cleanup.push((parent.clone(), borrow_instance.index.clone(), digram_instances.digram.clone()));
                }


                // Now, do stuff that will need to reborrow `instance`.
                drop(borrow_instance);

                debug_assert!(instance.sanity_check());
                self.compute_digrams_for_node(&instance);
            }

            drop(borrow_instances);

            // Now, do stuff that will need to reborrow `instances`.
            for (parent, index, expected_digram) in parent_cleanup.drain(..) {
                // Note that we can perform parent cleanup at the end of the loop because, during
                // a single loop, a child may only be modified once.

                // However, the parent may itself have been entirely overwritten.
                // If so, the calls to remove/compute will be ignored, remove/compute has already happened.

                // Remove old digram from parent.
                if digrams_per_priority.remove_single_child(&parent, index, &*expected_digram) {
                    // Replace with new digram.
                    self.compute_digram_at(&parent, index);
                }
            }

            // We can now insert all these new digrams in the priority queue.
            for (_, instances) in self.pending_digrams.drain() {
                digrams_per_priority.insert(&instances);
            }
        }

        // FIXME: 2. Pruning phase.
        // FIXME: Eliminate productions that increase final size.
        debug!(target: "repair", "Skipping unimplemented pruning phase");
    }
}

mod prio {
    use repair2::list;

    /// A priority queue designed as a vector of linked lists with the
    /// ability to quickly reslot an item.
    ///
    /// By design, the number of slots can never grow.
    #[derive(Debug)]
    pub struct Queue<T> {
        data: Vec<list::List<T>>,
    }
    impl<T> Queue<T> {
        /// Create a new queue with a maximal number of slots.
        ///
        /// Any attempt to prioritize an item with a priority higher than `len` will fail.
        pub fn with_capacity(len: usize) -> Self {
            let mut data = Vec::with_capacity(len + 1);
            data.resize_default(len + 1);
            Queue {
                data,
            }
        }
        pub fn insert(&mut self, data: T, priority: usize)  -> Remover<T> {
            assert!(priority < self.data.len());
            let list_remover = self.data[priority].push(data);
            Remover::new(list_remover)
        }
        pub fn pop(&mut self) -> Option<(usize, T)> {
            loop {
                let len = self.data.len();
                if let Some(ref mut list) = self.data.last_mut() {
                    if let Some(result) = list.pop() {
                        return Some((len, result))
                    }
                }
                if self.data.pop().is_none() {
                    return None;
                }
            }
        }
    }

    #[derive(Debug)]
    pub struct Remover<T> {
        list_remover: list::Remover<T>,
    }
    impl<T> Remover<T> {
        fn new(list_remover: list::Remover<T>) -> Self {
            Self {
                list_remover
            }
        }
        pub fn remove(&mut self) {
            self.list_remover.remove();
        }
    }
}

struct DigramPriorityQueue<T> where T: Eq + Hash + Ord + Clone + Debug {
    queue: prio::Queue<Rc<DigramInstances<T>>>
}
impl<T> DigramPriorityQueue<T> where T: Eq + Hash + Ord + Clone + Debug {
    fn with_capacity(size: usize) -> Self {
        DigramPriorityQueue {
            queue: prio::Queue::with_capacity(size)
        }
    }
    fn pop(&mut self) -> Option<(usize, Rc<DigramInstances<T>>)> {
        self.queue.pop()
    }

    fn reprioritize(&mut self, digram_instances: &Rc<DigramInstances<T>>) {
        self.remove(digram_instances);
        self.insert(digram_instances);
    }

    fn remove_single_child(&mut self, node: &SharedCell<SubTree<T>>, index: NodeIndex, expected_digram: &Digram<T>) -> bool {
        let instance = {
            let borrow = node.borrow();
            if borrow.digrams.len() == 0 {
                // Everything has already been removed
                return false;
            }
            let position = borrow.children.iter()
                .position(|child| child.borrow().index == index);
            let position = match position {
                // This child was already removed
                None => { return false; }
                Some(position) => position
            };
            let (ref weak_digram, ref digram_instance_remover) = borrow.digrams[position];
            let instance = match weak_digram.upgrade() {
                None => {
                    // The entire instance has ceased existing.
                    return false;
                }
                Some(instance) => instance
            };
            if &*instance.digram != expected_digram {
                // The parent has already been rewritten.
                return false;
            }
            digram_instance_remover.remove();
            instance
        };
        self.reprioritize(&instance);
        true
    }
    fn remove_full_node_except(&mut self, node: &SharedCell<SubTree<T>>, maintain_digram: &Digram<T>) {
        let mut borrow = node.borrow_mut();
        for (weak_digram, digram_instance_remover) in borrow.digrams.drain(..) {
            let instance = match weak_digram.upgrade() {
                Some(instance) => instance,
                None => {
                    // Nothing to remove from.
                    continue;
                }
            };
            if &*instance.digram == maintain_digram {
                // Don't remove the current digram!
                continue;
            }
            digram_instance_remover.remove();
            self.reprioritize(&instance);
        }
    }
    fn remove(&mut self, digram_instances: &Rc<DigramInstances<T>>) {
        let mut borrow_remover = digram_instances.remover
            .borrow_mut();
        let mut remover = borrow_remover
            .take()
            .expect("DigramInstances<T> does not have a remover");
        remover.remove();
    }
    fn insert(&mut self, digram_instances: &Rc<DigramInstances<T>>) -> usize {
        let len = digram_instances.len();
        let remover = self.queue.insert(digram_instances.clone(), len);
        let mut borrow_remover = digram_instances.remover
            .borrow_mut();
        assert!(borrow_remover.is_none());
        *borrow_remover = Some(remover);
        len
    }
}

/// A variant on linked lists, with the ability to
/// remove from the middle in O(1).
mod list {
    use itertools;

    use std::cell::RefCell;
    use std::rc::{ Rc, Weak };

    #[derive(Debug)]
    pub struct List<T> {
        list: Rc<RefCell<ListImpl<T>>>,
    }
    impl<T> List<T> {
        pub fn new() -> Self {
            List {
                list: Rc::new(RefCell::new(ListImpl::new()))
            }
        }
        pub fn len(&self) -> usize {
            self.list.borrow().len
        }
        pub fn push(&mut self, data: T) -> Remover<T> {
            let mut borrow = self.list.borrow_mut();
            borrow.push(self, data)

        }
        pub fn pop(&mut self) -> Option<T> {
            let mut borrow = self.list.borrow_mut();
            borrow.pop()
        }
    }

    impl<U> List<Rc<U>> {
        pub fn iter(&self) -> impl Iterator<Item = Rc<U>> {
            let borrow = self.list.borrow();
            let mut position = 0;
            let len = borrow.len;
            // debug_assert_eq!(borrow.compute_length(), len);
            itertools::unfold(borrow.head.clone(), move |cursor| {
                let (next, item) = match cursor {
                    None => {
                        assert_eq!(position, len);
                        (None, None)
                    }
                    Some(ref next) => {
                        let borrow = next.borrow();
                        assert!(position < len);
                        position += 1;
                        let result = borrow.data.clone().unwrap();
                        (borrow.next.clone(), Some(result))
                    }
                };
                *cursor = next;
                item
            })
        }
    }
    impl<T> Default for List<T> {
        fn default() -> Self {
            Self::new()
        }
    }


    #[derive(Debug)]
    struct ListImpl<T> {
        len: usize,
        head: Option<Rc<RefCell<Link<T>>>>,
        tail: Option<Weak<RefCell<Link<T>>>>,
    }
    impl<T> ListImpl<T> {
        fn new() -> Self {
            Self {
                len: 0,
                head: None,
                tail: None,
            }
        }
        fn compute_length(&self) -> usize {
            let mut position = 0;
            let mut cursor = self.head.clone();
            while let Some(next) = cursor {
                position += 1;
                cursor = next.borrow().next.clone();
            }
            position
        }
        fn push(&mut self, list: &List<T>, data: T) -> Remover<T> {
            let remover = match self.tail {
                None => {
                    debug_assert_eq!(self.len, 0);
                    debug_assert!(self.head.is_none());
                    let link = Rc::new(RefCell::new(Link::new(data)));
                    self.tail = Some(Rc::downgrade(&link));
                    self.head = Some(link.clone());
                    self.len = 1;
                    Remover::new(Rc::downgrade(&list.list), Rc::downgrade(&link))
                }
                Some(ref mut tl) => {
                    debug_assert!(self.len != 0);
                    debug_assert!(self.head.is_some());
                    let tail = tl.upgrade()
                        .unwrap();   // If the list is not empty, we MUST be able to upgrade the pointer to the tail.
                    let mut tail_borrow = tail
                        .borrow_mut();
                    let link = Rc::new(RefCell::new(Link::new(data)));
                    link.borrow_mut().prev = tl
                        .clone();
                    *tl = Rc::downgrade(&link);
                    self.len += 1;
                    tail_borrow.next = Some(link.clone());
                    Remover::new(Rc::downgrade(&list.list), Rc::downgrade(&link))
                }
            };
            //debug_assert_eq!(self.len, self.compute_length());
            remover
        }

        fn pop(&mut self) -> Option<T> {
            let (tail, result) = match self.tail {
                None => {
                    debug_assert_eq!(self.len, 0);
                    debug_assert!(self.head.is_none());
                    return None
                }
                Some(ref tl) => {
                    debug_assert!(self.len != 0);
                    debug_assert!(self.head.is_some());
                    let tl = tl.upgrade()
                        .unwrap(); // The tail MUST be upgradable.
                    let mut tl_borrow = tl
                        .borrow_mut();
                    debug_assert!(tl_borrow.next.is_none());
                    self.len -= 1;

                    let result = tl_borrow.data.take()
                        .unwrap(); // We only steal this data once during the lifetime of the tail.
                    match tl_borrow.prev.upgrade() {
                        None => {
                            // First (and only) element in the list
                            debug_assert_eq!(self.len, 0);
                            (None, result)
                        }
                        Some(prev) => {
                            // Not the first
                            debug_assert!(self.len != 0);
                            prev.borrow_mut().next = None;
                            (Some(tl_borrow.prev.clone()), result)
                        }
                    }
                }
            };
            self.tail = tail;
            if self.len == 0 {
                self.head = None;
            }
            // debug_assert_eq!(self.compute_length(), self.len);
            Some(result)
        }
    }


    #[derive(Clone, Debug)]
    pub struct Remover<T> {
        list: Weak<RefCell<ListImpl<T>>>,
        link: Weak<RefCell<Link<T>>>,
    }
    impl<T> Remover<T> {
        fn new(list: Weak<RefCell<ListImpl<T>>>, link: Weak<RefCell<Link<T>>>) -> Self {
            Self {
                list,
                link,
            }
        }

        /// Remove oneself from the list, returning the new list length.
        pub fn remove(&self) -> usize {
            let list = match self.list.upgrade() {
                Some(list) => list,
                None => return 0/* list may have been removed because it was too short*/,
            };
            let link = match self.link.upgrade() {
                Some(link) => link,
                None => return 0/* list may have been deprecated already*/,
            };
            let mut borrow_list = list.borrow_mut();
            let mut borrow_link = link.borrow_mut();
            match (borrow_link.prev.upgrade(), borrow_link.next.as_ref()) {
                (Some(prev), Some(next)) => {
                    // Neither first nor last.
                    prev.borrow_mut().next = borrow_link.next.clone();
                    next.borrow_mut().prev = Rc::downgrade(&prev);
                }
                (None, None) => {
                    // The only item in the list.
                    assert_eq!(borrow_list.len, 1);
                    borrow_list.head = None;
                    borrow_list.tail = None;
                }
                (Some(prev), None) => {
                    // Last item in the list, but not first.
                    prev.borrow_mut().next = None;
                    borrow_list.tail = Some(Rc::downgrade(&prev));
                }
                (None, Some(next)) => {
                    // First item in the list, but not last.
                    next.borrow_mut().prev = Weak::default();
                    borrow_list.head = Some(next.clone());
                }
            }
            borrow_link.next = None;
            borrow_list.len -= 1;
            // debug_assert_eq!(borrow_list.compute_length(), borrow_list.len);
            // debug!(target: "repair", "Removing item");
            borrow_list.len
        }
    }

    #[derive(Debug)]
    struct Link<T> {
        data: Option<T>, // We make this an `Option` to allow stealing in `pop`
        next: Option<Rc<RefCell<Link<T>>>>,
        prev: Weak<RefCell<Link<T>>>,
    }
    impl<T> Link<T> {
        fn new(data: T) -> Self {
            Link {
                data: Some(data),
                next: None,
                prev: Weak::default(),
            }
        }
    }
}
