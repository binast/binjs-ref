//! An implementation of TreeRePair http://www.eti.uni-siegen.de/ti/veroeffentlichungen/12-repair.pdf

use bytes;

use ::TokenWriterError;
use io::TokenWriter;

use std;
use std::cell::RefCell;
use std::collections::{ HashMap };
use std::io::Write;
use std::rc::{ Rc, Weak };

use itertools::Itertools;

type SharedCell<T> = Rc<RefCell<T>>;

trait Counter {
    fn internal_make(value: usize) -> Self;
}
#[derive(Default)]
struct GenericCounter<T> where T: Counter {
    count: usize,
    phantom: std::marker::PhantomData<T>,
}
impl<T> GenericCounter<T> where T: Counter {
    pub fn new() -> Self {
        GenericCounter {
            count: 0,
            phantom: std::marker::PhantomData,
        }
    }
    pub fn next(&mut self) -> T {
        let result = T::internal_make(self.count);
        self.count += 1;
        result
    }
}

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
pub struct GeneratedLabel(usize);
impl Counter for GeneratedLabel {
    fn internal_make(value: usize) -> Self {
        GeneratedLabel(value)
    }
}

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

impl Label {
    /// Return the number of children of a label.
    fn len(&self) -> usize {
        use self::Label::*;
        match *self {
            Leaf(_) => 0,
            Named { ref children, .. }
            | Generated { ref children, .. } => *children
        }
    }

    /// Return an approximation of the number of bytes taken by an instance of a label.
    fn byte_len(&self) -> usize {
        use self::Label::*;
        match *self {
            Leaf(ref buf) => buf.len(),
            Named { ref label, .. } => label.len(),
            Generated { .. } => 4 /* FIXME: let's say it takes 32 bits */
        }
    }

    fn serialize<W: Write>(&self, labels: &HashMap<Label, (Vec<u8>, RefCell<bool>)>, out: &mut W) {
        use self::Label::*;

        let (varnum, seen) = labels.get(self).unwrap();
        // No matter what, write our varnum.
        out.write_all(varnum).unwrap();
        // If this is the first time we see the header, write our definition.
        let mut borrow_seen = seen.borrow_mut();
        if *borrow_seen {
            // We're done here.
            return;
        }
        *borrow_seen = true;
        match *self {
            Leaf(ref buf) => out.write_all(&buf).unwrap(),
            Named { ref label, ..} => out.write_all(label.as_bytes()).unwrap(),
            Generated { ref digram, .. } => {
                use bytes::varnum::WriteVarNum;
                // The definition of a digram requires writing its labels, possibly for the first time,
                // which may in turn contain digrams, etc.
                digram.parent.serialize(labels, out);
                out.write_varnum(digram.position as u32).unwrap();
                digram.child.serialize(labels, out);
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
    children: Vec<SharedCell<SubTree>>,

    /// The parent. May be Weak::default() if this is the root.
    parent: Weak<RefCell<SubTree>>,

    /// A mechanism to remove oneself from a list of subtrees.
    /// Index `i` corresponds to the digram `(label, i, children[i].label)`.
    digrams: Vec<(Weak<DigramInstances>, list::Remover<SharedCell<SubTree>>)>,
}

impl PartialEq for SubTree {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}
impl Eq for SubTree { }
impl PartialOrd for SubTree {
    fn partial_cmp(&self, other: &SubTree) -> std::option::Option<std::cmp::Ordering> {
        NodeIndex::partial_cmp(&self.index, &other.index)
    }
}
impl Ord for SubTree {
    fn cmp(&self, other: &SubTree) -> std::cmp::Ordering {
        NodeIndex::cmp(&self.index, &other.index)
    }
}
impl std::hash::Hash for SubTree {
    fn hash<H>(&self, state: &mut H) where H: std::hash::Hasher {
        self.index.hash(state)
    }
}
impl SubTree {
    fn children(&self) -> impl Iterator<Item = &SharedCell<SubTree>> {
        self.children.iter()
    }
    fn collect_labels(&self) -> HashMap<Label, usize> {
        fn aux(tree: &SubTree, labels: &mut HashMap<Label, usize>) {
            {
                let entry = labels.entry(tree.label.clone())
                    .or_insert(0);
                *entry += 1;
            }
            for child in &tree.children {
                aux(&*child.borrow(), labels);
            }
        }
        let mut map = HashMap::new();
        aux(self, &mut map);
        map
    }
    fn serialize<W: Write>(&self, substitutions: &HashMap<Label, (Vec<u8>, RefCell<bool>)>, out: &mut W) {
        fn aux<W: Write>(tree: &SubTree, labels: &HashMap<Label, (Vec<u8>, RefCell<bool>)>, out: &mut W) {
            // Write header.
            tree.label.serialize(labels, out);
            // Then write children in the order.
            for child in &tree.children {
                aux(&*child.borrow(), labels, out)
            }
        }
        aux(self, substitutions, out)
    }
}

struct Root {
    tree: SharedCell<SubTree>,
    node_counter: GenericCounter<NodeIndex>,
    generated_label_counter: GenericCounter<GeneratedLabel>,
}
impl Root {
    fn new() -> Self {
        let mut node_counter = GenericCounter::new();
        let generated_label_counter = GenericCounter::new();
        let index = node_counter.next();
        Root {
            node_counter,
            generated_label_counter,
            tree: Rc::new(RefCell::new(SubTree {
                index,
                label: Label::Leaf(Rc::new(vec![])),
                parent: Weak::default(),
                digrams: vec![],
                children: vec![]
            }))
        }
    }
    fn new_leaf(&mut self, leaf: Vec<u8>) -> SharedCell<SubTree> {
        let result = Rc::new(RefCell::new(SubTree {
            index: self.node_counter.next(),
            label: Label::Leaf(Rc::new(leaf)),
            parent: Weak::default(),
            digrams: vec![],
            children: vec![],
        }));
        self.tree = result.clone();
        result
    }
    fn new_named_label(&mut self, name: &str, children: usize) -> Label {
        Label::Named {
            label: Rc::new(name.to_string()),
            children: children,
        }
    }
    fn new_generated_label(&mut self, children: usize, digram: Rc<Digram>) -> Label {
        Label::Generated {
            label: self.generated_label_counter.next(),
            digram,
            children,
        }
    }
    fn new_subtree(&mut self, label: Label, children: Vec<SharedCell<SubTree>>) -> SharedCell<SubTree> {
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
            debug_assert!(child_borrow.parent.upgrade().is_none());
            child_borrow.parent = weak.clone();
        }
        self.tree = parent.clone();
        parent
    }
}

type SharedTree = SharedCell<SubTree>;

pub struct Encoder {
    root: Root,
}

impl Encoder {
    /// Convert a list into a binary representation.
    fn list_aux(&mut self, items: &[SharedTree]) -> Result<SharedTree, TokenWriterError> {
        if items.len() == 0 {
            return self.tagged_tuple("_Nil", &[])
        }
        let children = [("hd" /* ignored*/, items[0].clone()), ("tl", /*ignored */ self.list_aux(&items[1..])?) ];
        self.tagged_tuple("_List", &children)
    }
}
impl TokenWriter for Encoder {
    type Error = TokenWriterError;
    type Statistics = u32; // Ignored for the time being.
    type Tree = SharedTree;
    type Data = Vec<u8>;

    fn bool(&mut self, data: Option<bool>) -> Result<Self::Tree, Self::Error> {
        let bytes = bytes::bool::bytes_of_bool(data).iter().cloned().collect();
        Ok(self.root.new_leaf(bytes))
    }

    fn float(&mut self, data: Option<f64>) -> Result<Self::Tree, Self::Error> {
        let bytes = bytes::float::bytes_of_float(data).iter().cloned().collect();
        Ok(self.root.new_leaf(bytes))
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
        Ok(self.root.new_leaf(buf))
    }

    fn untagged_tuple(&mut self, _data: &[Self::Tree]) -> Result<Self::Tree, Self::Error> {
        unimplemented!()
    }

    fn tagged_tuple(&mut self, tag: &str, items: &[(&str, Self::Tree)]) -> Result<Self::Tree, Self::Error> {
        let label = self.root.new_named_label(tag, items.len());
        let children = items.iter()
            .map(|(_, tree)| tree.clone())
            .collect();
        Ok(self.root.new_subtree(label, children))
    }

    fn list(&mut self, items: Vec<Self::Tree>) -> Result<Self::Tree, Self::Error> {
        self.list_aux(&items)
    }

    fn offset(&mut self) -> Result<Self::Tree, Self::Error> {
        // FIXME: We'll want to build a forest and put skippable stuff after the rest.
        unimplemented!()
    }

    fn done(mut self) -> Result<(Self::Data, Self::Statistics), Self::Error> {
        // Rewrite tree with digrams.
        self.proceed_with_tree_repair();
        // Collect statistics on most commonly labels.
        let statistics = self.root.tree.borrow()
            .collect_labels()
            .into_iter()
            .sorted_by(|a, b| Ord::cmp(&a.1, &b.1));
        let number_of_labels = statistics.len();
        // FIXME We could eliminate lookup by making this part of the label itself.
        let label_representation : HashMap<_, _> = statistics.into_iter()
            .map(|(label, instances)| {
                use bytes::varnum::WriteVarNum;
                // Compute varnum index.
                let mut encoded_length = vec![];
                encoded_length.write_varnum((number_of_labels - instances) as u32).unwrap();

                (label, (encoded_length, /* encountered */ RefCell::new(false)))
            })
            .collect();
        let mut buf = vec![];
        self.root.tree.borrow().serialize(&label_representation, &mut buf);
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
    instances: SharedCell<list::List<SharedCell<SubTree>>>,

    remover: RefCell<Option<prio::Remover<Rc<DigramInstances>>>>,
}
impl DigramInstances {
    fn len(&self) -> usize {
        self.instances.borrow().len()
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

const MINIMAL_NUMBER_OF_INSTANCE : usize = 1;

impl Encoder {
    pub fn new() -> Self {
        Self {
            root: Root::new(),
        }
    }
    fn compute_single_startup_digram(node: &Rc<RefCell<SubTree>>, label: &Label, position: usize, child: &SubTree, set: &mut HashMap<Digram, Rc<DigramInstances>>, removers: &mut Vec<(Weak<DigramInstances>, list::Remover<SharedCell<SubTree>>)>) {
        let digram = Digram {
            parent: label.clone(),
            position,
            child: child.label.clone(),
        };

        // Now expand to parent.
        let this_digram = set.entry(digram.clone())
            .or_insert_with(|| Rc::new(DigramInstances {
                digram: Rc::new(digram),
                instances: Rc::new(RefCell::new(list::List::new())),
                remover: RefCell::new(None)
            }));
        let remover = this_digram.instances
            .as_ref()
            .borrow_mut()
            .push(node.clone());
        let weak = Rc::downgrade(&this_digram);
        removers.push((weak, remover));
    }

    fn compute_startup_digrams(tree: &SharedCell<SubTree>) -> HashMap<Digram, Rc<DigramInstances>> {
        let mut digrams = HashMap::new();
        fn aux(node: &SharedCell<SubTree>, set: &mut HashMap<Digram, Rc<DigramInstances>> ) {
            let mut removers = vec![];
            {
                let node_borrow = node.borrow();
                for (position, child) in node_borrow.children().enumerate() {
                    // Walk in post-order.
                    aux(child, set);
                    Encoder::compute_single_startup_digram(node, &node_borrow.label, position, &*child.borrow(), set, &mut removers);
                }
            }
            // Now re-borrow as mutable, to update subtree.
            if removers.len() > 0 {
                node.as_ref().borrow_mut().digrams = removers;
            }
        }
        aux(tree, &mut digrams);
        digrams
    }

    fn proceed_with_tree_repair(&mut self) {
        // Replacement phase.

        let insert_new_digrams = |digrams_per_priority: &mut prio::Queue<_>, digrams: HashMap<Digram, Rc<DigramInstances>>| {
            for (_, instances) in digrams {
                let len = instances.len();
                if len <= MINIMAL_NUMBER_OF_INSTANCE {
                    // Skip stuff that's too uncommon in the first place.
                    continue;
                }
                let remover = digrams_per_priority.insert(instances.clone(), len);
                *instances.remover.borrow_mut() = Some(remover);
            }
        };

        // Compute the initial set of digrams.
        let mut digrams_per_priority = {
            let startup_digrams = Self::compute_startup_digrams(&self.root.tree);
            let highest_priority = startup_digrams.values()
                .map(|instances| instances.as_ref().instances.borrow().len())
                .max()
                .expect("No digrams found!");
            debug!(target: "repair", "During startup, the highest digram priority found was {}", highest_priority);
            let mut digrams_per_priority = prio::Queue::with_capacity(highest_priority);
            insert_new_digrams(&mut digrams_per_priority, startup_digrams);
            digrams_per_priority
        };

        // Pick most frequent digram.
        'per_digram: while let Some((priority, digram_instances)) = digrams_per_priority.pop() {
            let mut actual_instances_encountered = 0;

            debug!(target: "repair", "Considering digram {:?}", digram_instances.digram);
            debug!(target: "repair", "This digram should have {} instances", borrow_instances.len());
            // Generate a new label `generated`.
            let number_of_children = digram_instances.digram.parent.len() + digram_instances.digram.child.len() - 1;
            let generated = self.root.new_generated_label(number_of_children, digram_instances.digram.clone());

            // A map to hold all the digrams we are creating during the substitution.
            let mut new_digrams = HashMap::new();

            // Replace instances of `digram` with `generated` all over the tree.
            let mut borrow_instances = digram_instances.instances.borrow_mut();
            'per_node: for instance in borrow_instances.iter() {
                let mut borrow_instance = instance.borrow_mut();

                {
                    if digram_instances.digram.parent != borrow_instance.label
                    || borrow_instance.children[digram_instances.digram.position].borrow().label != digram_instances.digram.child {
                        // The node has been rewritten, the digram doesn't apply anymore.
                        if digram_instances.digram.parent != borrow_instance.label {
                            debug!(target: "repair", "Node has been rewritten: parent {:?} != expected {:?}", borrow_instance.label, digram_instances.digram.parent);
                        }
                        if borrow_instance.children[digram_instances.digram.position].borrow().label != digram_instances.digram.child {
                            debug!(target: "repair", "Node has been rewritten: child {} {:?} != expected {:?}",
                                digram_instances.digram.position,
                                borrow_instance.children[digram_instances.digram.position].borrow().label,
                                digram_instances.digram.child);
                        }
                        continue 'per_node;
                    }
                }

                actual_instances_encountered += 1;
                debug!(target: "repair", "Entering actual instance {}/{}", actual_instances_encountered, borrow_instances.len());

                let mut children = Vec::with_capacity(number_of_children);

                std::mem::swap(&mut borrow_instance.children, &mut children);
                let mut iter = children.into_iter();

                // Keep the first `digram.position` children. // FIXME: A convenient LinkedList would be faster
                for _ in 0 .. digram_instances.digram.position {
                    borrow_instance.children.push(iter.next().unwrap());
                }

                // Inline the children of child `digram.position`.
                let removed = iter.next().unwrap();
                let mut borrow_removed = removed.borrow_mut();

                // Remove a single node from a list of digrams, updating priority list as needed.
                let downgrade_conflict = |digrams_per_priority: &mut prio::Queue<_>, removing_instance: &Weak<DigramInstances>, list_remover: &mut list::Remover<SharedCell<SubTree>>| {
                    let removing_instance = match removing_instance.upgrade() {
                        Some(removing_instance) => removing_instance,
                        None => return, /* stuff was already gc-ed? */
                    };
                    if removing_instance.digram == digram_instances.digram {
                        // Don't remove the current digram!
                        return;
                    }
                    // Remove from the list of digrams.
                    let len = list_remover.remove();
                    eprintln!("Adjusting priority: {:?} => {:?}",
                        removing_instance.digram,
                        len);
                    // Update priority of `digram`, or remove it entirely.
                    {
                        removing_instance.remover
                            .borrow_mut()
                            .as_mut()
                            .expect("Missing diagram instance remover")
                            .remove();
                    }
                    if len >= MINIMAL_NUMBER_OF_INSTANCE {
                        let priority_remover = digrams_per_priority.insert(removing_instance.clone(), len);
                        *removing_instance.remover
                            .borrow_mut() = Some(priority_remover);
                    }
                };


                // Since we're removing `removed`, it doesn't belong to any digram list anymore.
                debug_assert_eq!(borrow_removed.label, digram_instances.digram.child);
                for (digram, list_remover) in &mut borrow_removed.digrams {
                    downgrade_conflict(&mut digrams_per_priority, digram, list_remover)
                }
                for child in &borrow_removed.children {
                    let mut borrow_child = child.borrow_mut();
                    borrow_child.parent = Rc::downgrade(&instance);
                    borrow_instance.children.push(child.clone());
                }

                // Then copy the remaining children.
                borrow_instance.children.extend(iter);

                debug_assert_eq!(borrow_instance.children.len(), number_of_children);

                // Finally, change the label.
                borrow_instance.label = generated.clone();

                // We still need to update priorities.

                // Since we're changing the label of the instance, any digram list to which it
                // belongs is now invalid, so downgrade appropriately.
                for (digram, list_remover) in &mut borrow_instance.digrams {
                    downgrade_conflict(&mut digrams_per_priority, digram, list_remover)
                }

                // Also, we have **replaced** all the possibilities for digrams in this node.
                {
                    let mut removers = Vec::with_capacity(number_of_children);
                    for (position, child) in borrow_instance.children.iter().enumerate() {
                        Self::compute_single_startup_digram(&instance, &generated, position, &*child.borrow(), &mut new_digrams, &mut removers);
                    }
                    borrow_instance.digrams = removers;
                }

                // We have **replaced** one new possible digram in the parent node.
                if let Some(parent) = borrow_instance.parent.upgrade() {
                    // May be `None` if we are the root node.
                    let mut parent_borrow = parent.borrow_mut();
                    let index = parent_borrow.children
                        .iter()
                        .position(|child| {
                            Rc::ptr_eq(child, &instance)
                        })
                        .expect("Could not find child in parent");

                    // Remove old digram.
                    {
                        let (ref digram, ref mut list_remover) = parent_borrow.digrams[index];
                        downgrade_conflict(&mut digrams_per_priority, digram, list_remover);
                    }

                    // Add new digram.
                    let mut remover = Vec::with_capacity(1);
                    Self::compute_single_startup_digram(&parent, &parent_borrow.label, index, &*borrow_instance, &mut new_digrams, &mut remover);
                    parent_borrow.digrams[index] = remover.pop()
                        .unwrap();
                }
            }

            debug!(target: "repair", "Replaced {instances} instances of digram (expected {priority}), for a total of {} bytes saved",
                total = actual_instances_encountered * (digram_instances.digram.parent.byte_len() + digram_instances.digram.child.byte_len() - generated.byte_len()) as isize
                        - (digram_instances.digram.parent.byte_len() + digram_instances.digram.child.byte_len() + 4) as isize,
                priority = priority,
                instances = actual_instances_encountered);

            // We can now insert all these new digrams in the priority queue.
            insert_new_digrams(&mut digrams_per_priority, new_digrams);
        }


        // FIXME: 2. Pruning phase.
        // FIXME: Eliminate productions that increase final size.
        debug!(target: "repair", "Skipping unimplemented pruning phase");
    }
}

/// A priority queue designed as a vector of linked lists with the
/// ability to quickly reslot an item.
///
/// By design, the number of slots can never grow.
mod prio {
    use repair::list;

    #[derive(Debug)]
    pub struct Queue<T> {
        data: Vec<list::List<T>>,
    }
    impl<T> Queue<T> {
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
            debug!(target: "repair", "Iterator starting {}/{}", position, len);
            debug_assert_eq!(borrow.compute_length(), len);
            itertools::unfold(borrow.head.clone(), move |cursor| {
                let (next, item) = match cursor {
                    None => {
                        debug!(target: "repair", "Iterator complete");
                        assert_eq!(position + 1, len);
                        (None, None)
                    }
                    Some(ref next) => {
                        let borrow = next.borrow();
                        debug!(target: "repair", "Iterator next {}/{} (next: {})",
                            position,
                            len,
                            if borrow.next.is_none() { "none" } else { "some" });
                        position += 1;
                        assert!(position < len);
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
            debug_assert_eq!(self.compute_length(), self.len);
            Some(result)
        }
    }


    #[derive(Clone, Debug)]
    pub struct Remover<T> {
        list: Weak<RefCell<ListImpl<T>>>,
        link: Weak<RefCell<Link<T>>>,
        removed: bool,
    }
    impl<T> Remover<T> {
        fn new(list: Weak<RefCell<ListImpl<T>>>, link: Weak<RefCell<Link<T>>>) -> Self {
            Self {
                list,
                link,
                removed: false,
            }
        }

        /// Remove oneself from the list, returning the new list length.
        pub fn remove(&mut self) -> usize {
            assert_eq!(self.removed, false);
            self.removed = true;
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
            debug_assert_eq!(borrow_list.compute_length(), borrow_list.len);
            debug!(target: "repair", "Removing item");
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
