//! An implementation of TreeRePair http://www.eti.uni-siegen.de/ti/veroeffentlichungen/12-repair.pdf

use bytes;
use bytes::varnum::WriteVarNum;

use ::{ NumberingStrategy, TokenWriterError };
use io::TokenWriter;

use std;
use std::cell::RefCell;
use std::collections::{ BinaryHeap, HashMap, LinkedList };
use std::rc::{ Rc, Weak };

use itertools::Itertools;

#[derive(Clone)]
pub struct Options {
    /// If specified, reduce all nodes (including lists) to ensure that they have at
    /// most `max_rank` children. The original paper recomments a value of 4.
    pub max_rank: Option<usize>,

    pub numbering_strategy: NumberingStrategy,
}

type SharedCell<T> = Rc<RefCell<T>>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct NodeIndex(usize);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct LabelIndex(usize);


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct GeneratedLabel(usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Label {
    /// A well-known label with well-known arity.
    Named {
        label: Rc<String>,
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
        digram: Rc<Digram>,
        children: usize,
    },
    Leaf(Rc<Vec<u8>>)
}

impl std::fmt::Display for Label {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
        use self::Label::*;
        match *self {
            Named { ref label, .. } => label.fmt(formatter),
            Generated { ref digram, .. } => {
                write!(formatter, "{parent}(...{child}(...)...)",
                    parent = digram.parent,
                    child = digram.child,
                )
            }
            List { ref len } => write!(formatter, "[{}]", len),
            Leaf(..) => write!(formatter, "(leaf)"),
        }
    }
}

impl Label {
    fn len(&self) -> usize {
        use self::Label::*;
        match *self {
            Leaf(_) => 0,
            Named { ref children, .. }
            | Generated { ref children, .. } => *children,
            List { ref len, .. } => *len
        }
    }
    fn set_len(&mut self, len: usize)  {
        use self::Label::*;
        match *self {
            Named { ref mut children,  .. } => *children = len,
            List { len: ref mut children , ..} => *children = len,
            _ => panic!()
        }
    }

    /// Return an approximation of the number of bytes taken by an instance of a label.
    fn byte_len(&self) -> usize {
        use self::Label::*;
        match *self {
            Leaf(ref buf) => buf.len(),
            Named { ref label, .. } => label.len(),
            Generated { .. } => 4, /* FIXME: let's say it takes 32 bits */
            List { .. } => 4
        }
    }

    fn collect(&self, labels: &mut HashMap<Label, usize> ){
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

    fn serialize<W: Write>(&self, mru: &mut ::io::NumberingStrategy<Label>, headers: &mut usize, out: &mut W) {
        use self::Label::*;

        let index = mru.get_index(self);
        // Write the number.
        out.write_varnum(index.index() as u32).unwrap();

        if index.is_first() {
            // Never seen: write the smallest possible "unknown" value, then the definition.
            info!(target: "repair", "Adding to dictionary: {}", *self);

            match *self {
                Leaf(ref buf) => {
                    *headers += buf.len();
                    out.write_all(&buf).unwrap()
                },
                Named { ref label, ..} => {
                    *headers += label.as_bytes().len();
                    out.write_all(label.as_bytes()).unwrap()
                },
                List { .. } => { /* Nothing else to write */ }
                Generated { ref digram, .. } => {
                    use bytes::varnum::WriteVarNum;
                    // The definition of a digram requires writing its labels, possibly for the first time,
                    // which may in turn contain digrams, etc.
                    digram.parent.serialize(mru, headers, out);
                    *headers += out.write_varnum(digram.position as u32).unwrap();
                    digram.child.serialize(mru, headers, out);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SubTree { // FIXME: Make it private, eventually.
    /// An index, used to quickly compare tree nodes.
    index: NodeIndex,

    /// The label
    label: Label,

    /// Children.
    children: LinkedList<SharedCell<SubTree>>,

    /// The parent. May be Weak::default() if this is the root.
    parent: Weak<RefCell<SubTree>>,

    /// For each `i`, a pointer to the list of instances of digram `(label, i, children[i])`.
    /// FIXME: In the future, we should try and make this a pointer that can be
    /// used for O(1) removal from the list.
    digrams: Vec<Rc<DigramInstances>>,
}
impl PartialEq for Label {
    fn eq(&self, other: &Label) -> bool {
        self.index == other.index
    }
}
impl Eq for Label { }
impl PartialOrd for Label {
    fn partial_cmp(&self, other: &Label) -> std::option::Option<std::cmp::Ordering> {
        NodeIndex::partial_cmp(&self.index, &other.index)
    }
}
impl Ord for Label {
    fn cmp(&self, other: &Label) -> std::cmp::Ordering {
        NodeIndex::cmp(&self.index, &other.index)
    }
}
impl std::hash::Hash for Label {
    fn hash<H>(&self, state: &mut H) where H: std::hash::Hasher {
        self.index.hash(state)
    }
}
impl SubTree {
    fn len(&self) -> usize {
        self.children.len()
    }
    fn into_shared(self) -> SharedTree {
        Rc::new(RefCell::new(Some(self)))
    }
    fn children(&self) -> impl Iterator<Item = &SharedCell<SubTree>> {
        self.children.iter()
    }

    fn collect_labels(&self) -> HashMap<Label, usize> {
        fn aux(tree: &SubTree, labels: &mut HashMap<Label, usize>) {
            tree.label.collect(labels);
            for child in &tree.children {
                aux(&*child.borrow(), labels);
            }
        }
        let mut map = HashMap::new();
        aux(self, &mut map);
        map
    }
    fn serialize<W: Write>(&self, mru: &mut ::io::NumberingStrategy<Label>, out: &mut W) {
        let mut dictionary_size = 0;
        fn aux<W: Write>(tree: &SubTree, mru: &mut ::io::NumberingStrategy<Label>, headers: &mut usize, out: &mut W) {
            // Write header.
            tree.label.serialize(mru, headers, out);
            // Then write children in the order.
            for child in &tree.children {
                aux(&*child.borrow(), mru, headers, out)
            }
        }
        aux(self, mru, &mut dictionary_size, out);
        info!(target: "repair", "Inline dictionary takes {} bytes", dictionary_size);
    }
}

struct Root {
    labels: HashMap<Label, LabelIndex>,
    label_counter: usize,
    tree: SharedCell<SubTree>,
}
impl Root {
    fn new_leaf(&mut self, leaf: Vec<u8>) -> SubTree {
        unimplemented!()
    }
    fn new_named_label(&mut self, name: &str, children: usize) -> Label {
        unimplemented!()
    }
    fn new_generated_label(&mut self, children: usize) -> Label {
        unimplemented!()
    }
    fn new_subtree(&mut self, label: Label, children: LinkedList<SharedCell<SubTree>>) -> SubTree {
        unimplemented!()
    }
}

type SharedTree = SharedCell<Option<SubTree>>;

pub struct Encoder {
    node_counter: usize,
    generated_counter: usize,
    root: Root,
    digram_counter: GenericCounter<DigramIndex>,
    pending_digrams: HashMap<Digram, Rc<DigramInstances>>,
    numbering_strategy: NumberingStrategy,
}

impl TokenWriter for Encoder {
    type Error = TokenWriterError;
    type Statistics = u32; // Ignored for the time being.
    type Tree = Rc<RefCell<Option<trees::Tree<Label>>>>;
    type Data = Vec<u8>;

    fn bool(&mut self, data: Option<bool>) -> Result<Self::Tree, Self::Error> {
        let bytes = bytes::bool::bytes_of_bool(data).iter().cloned().collect();
        Ok(Rc::new(RefCell::new(Some(Tree::new(self.leaf(Rc::new(bytes)))))))
    }

    fn float(&mut self, data: Option<f64>) -> Result<Self::Tree, Self::Error> {
        let bytes = bytes::float::bytes_of_float(data).iter().cloned().collect();
        Ok(Rc::new(RefCell::new(Some(Tree::new(self.leaf(Rc::new(bytes)))))))
    }

    fn string(&mut self, data: Option<&str>) -> Result<Self::Tree, Self::Error> {
        const EMPTY_STRING: [u8; 2] = [255, 0];
        let byte_len = match data {
            None => EMPTY_STRING.len(),
            Some(ref x) => x.len()
        } as u32;
        let buf_len : [u8; 4] = unsafe { std::mem::transmute(byte_len) }; // FIXME: Make this little-endian
        assert!(std::mem::size_of_val(&buf_len) == std::mem::size_of_val(&byte_len));


        let mut buf = Vec::new();
        buf.extend_from_slice(&buf_len);
        match data {
            None => buf.extend_from_slice(&EMPTY_STRING),
            Some(ref x) => buf.extend(x.bytes())
        }
        Ok(Rc::new(RefCell::new(Some(Tree::new(self.leaf(Rc::new(buf)))))))
    }

    fn untagged_tuple(&mut self, _data: &[Self::Tree]) -> Result<Self::Tree, Self::Error> {
        unimplemented!()
    }

    fn tagged_tuple(&mut self, tag: &str, items: &[(&str, Self::Tree)]) -> Result<Self::Tree, Self::Error> {
        let mut tree = Tree::new(self.internal(tag.to_string(), items.len()));
        for (_, item)  in items {
            tree.push_back(take(item));
        }

        Ok(Rc::new(RefCell::new(Some(tree))))
    }

    fn list(&mut self, items: Vec<Self::Tree>) -> Result<Self::Tree, Self::Error> {
        self.list_aux(&items)
    }

    fn offset(&mut self) -> Result<Self::Tree, Self::Error> {
        unimplemented!()
    }

    fn done(mut self) -> Result<(Self::Data, Self::Statistics), Self::Error> {
        // Rewrite tree with digrams.
        info!(target: "repair", "Compressing tree to digrams.");
        self.proceed_with_tree_repair();

        info!(target: "repair", "Generating numbering strategy.");
        let mut strategy = match self.numbering_strategy {
            NumberingStrategy::MRU => ::io::NumberingStrategy::mru(),
            NumberingStrategy::GlobalFrequency => {
                let frequency = self.root.tree.borrow().collect_labels();
                ::io::NumberingStrategy::frequency(frequency)
            }
        };

        info!(target: "repair", "Generating binary.");
        let mut buf = vec![];
        self.root.tree.borrow().serialize(&mut strategy, &mut buf);

        info!(target: "repair", "Done.");
        Ok((buf, 0))
    }
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct Digram { // FIXME: Should be private, really.
    /// The parent label. It will be replaced by a new, per-digram, generated label.
    parent: Label,
    /// The index of the child of the parent to replace.
    position: usize,
    /// The child label. It will be replaced by its own children.
    child: Label,
}

type Digrams = HashMap<Digram, SharedCell<Vec<SharedCell<SubTree>>>>;

/// Places where we can substitute a digram.
// FIXME: We probably don't need that much sharing.
#[derive(Debug)]
struct DigramInstances {
    digram: Rc<Digram>,

    /// Invariant: these instances are always in post-order (descendants appear before
    /// ancestors).
    ///
    /// Note that some of the instances may be invalid, if a previous substitution has
    /// conflicted with this digram.
    instances: SharedCell<Vec<SharedCell<SubTree>>>,

    /// The number of instances that are no longer valid.
    /// FIXME: Ideally, we should remove directly from `instances`, but this would require
    /// implementing a more sophisticated intrusive linked list mechanism, so this will
    /// have to wait.
    removed: RefCell<usize>,
}
impl DigramInstances {
    fn len(&self) -> usize {
        self.instances.borrow().len() - *self.removed.borrow()
    }
}
impl PartialEq for DigramInstances {
    fn eq(&self, other: &Self) -> bool {
        self.digram == other.digram
    }
}
impl Eq for DigramInstances { }

impl PartialOrd for DigramInstances {
    fn partial_cmp(&self, other: &Self) -> std::option::Option<std::cmp::Ordering> {
        let my_len = self.len();
        let other_len = other.len();
        usize::partial_cmp(&my_len, &other_len)
    }
}
impl Ord for DigramInstances {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let my_len = self.len();
        let other_len = other.len();
        usize::cmp(&my_len, &other_len)
    }
}

impl Encoder {
    pub fn new(options: Options) -> Self {
        Self {
            root: Root::new(options.max_rank),
            numbering_strategy: options.numbering_strategy,
            digram_counter: GenericCounter::new(),
            pending_digrams: HashMap::with_capacity(4096),
        }
    }

    /// Compute a single digram for a node.
    fn compute_digram_at(&mut self, node: &Rc<RefCell<SubTree>>, index: NodeIndex) {
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
    fn compute_digrams_for_node(&mut self, node: &Rc<RefCell<SubTree>>) {
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
    fn compute_digrams_for_subtree(&mut self, node: &Rc<RefCell<SubTree>>) {
        {
            let borrow_node = node.borrow();
            // First, the children.
            for child in borrow_node.children.iter() {
                self.compute_digrams_for_subtree(child)
            }
        }
        aux(tree, &mut digrams);
        digrams
    }

    fn proceed_with_tree_repair(&mut self) {
        // Replacement phase.

        // Compute the initial set of digrams.
        let mut digrams_per_priority = {
            let root = self.root.tree.clone();
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
        }

        // Generated symbol => digram.
        let mut replacements = HashMap::new();

        // Pick most frequent digram.
        // FIXME: The original paper recommends a priority queue based on a list of
        // all digrams with `i` occurrences, itself encoded as a doubly linked list that
        // supports constant-time add/remove from the digram.
        'per_digram: while let Some((_, DigramInstances { digram, instances, .. })) = digrams_per_priority.pop() {
            // Generate a new label `generated`.
            let number_of_children = digram.parent.len() + digram.child.len() - 1;
            let generated = self.root.new_generated_label(number_of_children);
            replacements.insert(generated.clone(), digram.clone());

            // Replace instances of `digram` with `generated` all over the tree.
            let mut borrow_instances = instances.borrow_mut();
            'per_node: for mut instance in borrow_instances.iter_mut() {
                let mut borrow_instance = instance.borrow_mut();

                if digram.parent != borrow_instance.label {
                    // The node has been rewritten, the digram doesn't apply anymore.
                    continue 'per_node;
                }

                let mut children = LinkedList::new();
                std::mem::swap(&mut borrow_instance.children, &mut children);

                let mut prefix = children;
                let mut removed = prefix.split_off(digram.position);
                let mut suffix = removed.split_off(1);

                assert_eq!(removed.len(), 1);
                let mut removed = removed.pop_front()
                    .unwrap();

                {
                    let mut borrow_removed = removed.borrow_mut();
                    if borrow_removed.label == digram.child {
                        let mut replacement = borrow_removed.children.clone();

                        prefix.append(&mut replacement);
                        prefix.append(&mut suffix);

                        borrow_instance.replace(generated.clone(), prefix);
                    }
                }

                let mut children = Vec::with_capacity(digram.parent.len() + digram.child.len() - 1);

                std::mem::swap(&mut borrow_instance.children, &mut children);
                let mut iter = children.into_iter();

                // Keep the first `digram.position` children. // FIXME: A convenient LinkedList would be faster
                for _ in 0 .. digram.position {
                    borrow_instance.children.push(iter.next().unwrap());
                }

                // Inline the children of child `digram.position`.
                let removed = iter.next().unwrap();
                let mut borrow_removed = removed.borrow_mut();

                // Since we're removing `removed`, it doesn't belong to any digram list anymore.
                debug_assert_eq!(borrow_removed.label, digram.child);
                for list in &borrow_removed.digrams {
                    *list.removed.borrow_mut() != 1;
                    // FIXME: Move it to another slot of the priority queue.
                }
                for child in &borrow_removed.children {
                    let mut borrow_child = child.borrow_mut();
                    borrow_child.parent = Rc::downgrade(instance);
                }

                // Then copy the remaining children.
                borrow_instance.children.extend(iter);

                debug_assert_eq!(borrow_instance.children.len(), digram.parent.len() + digram.child.len() - 1);

                // Finally, change the label.
                borrow_instance.label = generated.clone();

                // Since we're changing the label of the instance, any digram list to which it
                // belongs is now invalid.
                for list in &borrow_instance.digrams {
                    *list.removed.borrow_mut() != 1;
                    // FIXME: Move it to another slot of the priority queue.
                }
            }

            // FIXME: Update list of most frequent digrams.
            // FIXME:
            // - generate new digrams that have `generated` as parent
            //     can be done in the above loop (N instances)
            // - generate new digrams that have `generated` as child
            //     in the above loop, inspect the parent (1 instance)
            // FIXME: Then sort again
        }
        aux(tree.borrow(), &mut digrams);
        digrams
    }
}

mod list {
    use std::cell::RefCell;
    use std::rc::{ Rc, Weak };

    struct List<T> {
        list: Rc<RefCell<ListImpl<T>>>,
    }
    struct ListImpl<T> {
        len: usize,
        head: Option<Link<T>>,
        tail: Option<Link<T>>,
    }

    #[derive(Clone)]
    struct Link<T> {
        link: Rc<RefCell<LinkImpl<T>>>,
    }
    impl<T> Link<T> {
        /// Remove oneself from the list of
        fn remove(&mut self) {
            unimplemented!()
        }
    }
    struct LinkImpl<T> {
        list: Weak<RefCell<ListImpl<T>>>,
    }
}
