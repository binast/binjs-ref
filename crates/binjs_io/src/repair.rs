//! An implementation of TreeRePair http://www.eti.uni-siegen.de/ti/veroeffentlichungen/12-repair.pdf

use bytes;

use ::TokenWriterError;
use io::TokenWriter;

use std;
use std::cell::RefCell;
use std::collections::{ BinaryHeap, HashMap, LinkedList };
use std::rc::{ Rc, Weak };

use itertools::Itertools;

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

impl Label {
    fn len(&self) -> usize {
        use self::Label::*;
        match *self {
            Leaf(_) => 0,
            Named { ref children, .. }
            | Generated { ref children, .. } => *children
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
    fn len(&self) -> usize {
        self.children.len()
    }
    fn into_shared(self) -> SharedTree {
        Rc::new(RefCell::new(Some(self)))
    }
    fn children(&self) -> impl Iterator<Item = &SharedCell<SubTree>> {
        self.children.iter()
    }
    fn replace(&mut self, label: Label, mut children: Vec<SharedCell<SubTree>>) {
        // FIXME: Regenerate the unique index.
        // FIXME: Replace the parent of `children`.
        unimplemented!()
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
    fn new_subtree(&mut self, label: Label, children: Vec<SharedCell<SubTree>>) -> SubTree {
        unimplemented!()
    }
}

type SharedTree = SharedCell<Option<SubTree>>;

pub struct Encoder {
    node_counter: usize,
    generated_counter: usize,
    root: Root,
}

fn take(item: &SharedTree) -> SubTree {
    let mut borrow = item.borrow_mut();
    let child = borrow.take();
    child.expect("Could not `take` shared tree, it was already taken")
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
        Ok(self.root.new_leaf(bytes).into_shared())
    }

    fn float(&mut self, data: Option<f64>) -> Result<Self::Tree, Self::Error> {
        let bytes = bytes::float::bytes_of_float(data).iter().cloned().collect();
        Ok(self.root.new_leaf(bytes).into_shared())
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
        Ok(self.root.new_leaf(buf).into_shared())
    }

    fn untagged_tuple(&mut self, _data: &[Self::Tree]) -> Result<Self::Tree, Self::Error> {
        unimplemented!()
    }

    fn tagged_tuple(&mut self, tag: &str, items: &[(&str, Self::Tree)]) -> Result<Self::Tree, Self::Error> {
        let label = self.root.new_named_label(tag, items.len());
        let children = items.iter()
            .map(|(_, tree)| Rc::new(RefCell::new(take(tree))))
            .collect();
        Ok(self.root.new_subtree(label, children)
            .into_shared())
    }

    fn list(&mut self, items: Vec<Self::Tree>) -> Result<Self::Tree, Self::Error> {
        self.list_aux(&items)
    }

    fn offset(&mut self) -> Result<Self::Tree, Self::Error> {
        unimplemented!()
    }

    fn done(self) -> Result<(Self::Data, Self::Statistics), Self::Error> {
        unimplemented!()
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

// FIXME: We don't need the `HashMap<NodeIndex, ...>`. We just need a list of `(NodeIndex, Tree)`,
// built in post-order. We then walk the list, skipping substitutions that are not valid anymore.
// If we maintain the invariant that these lists of `(NodeIndex, Tree)` are always in post-order,
// we can extract from them new lists of digrams (for generated non-terminals) that are also
// in post-order, hence in which we can skip, etc.
type Digrams = HashMap<Digram, SharedCell<Vec<SharedCell<SubTree>>>>;

/// Instances of a single Digram.
struct DigramInstances {
    digram: Digram,

    /// Invariant: these instances are always in post-order (descendants appear before
    /// ancestors).
    instances: SharedCell<Vec<SharedCell<SubTree>>>,
}

impl PartialEq for DigramInstances {
    fn eq(&self, other: &Self) -> bool {
        self.digram == other.digram
    }
}
impl Eq for DigramInstances { }

impl PartialOrd for DigramInstances {
    fn partial_cmp(&self, other: &Self) -> std::option::Option<std::cmp::Ordering> {
        let my_len = self.instances.borrow().len();
        let other_len = other.instances.borrow().len();
        usize::partial_cmp(&my_len, &other_len)
    }
}
impl Ord for DigramInstances {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let my_len = self.instances.borrow().len();
        let other_len = other.instances.borrow().len();
        usize::cmp(&my_len, &other_len)
    }
}

impl Encoder {
    fn compute_startup_digrams(tree: &SharedCell<SubTree>) -> Digrams {
        use std::borrow::BorrowMut;
        let mut digrams = HashMap::new();
        fn aux(node: &SharedCell<SubTree>, set: &mut Digrams) {
            let parent_borrow = node.borrow();
            let parent_label = parent_borrow.label.clone();
            for (position, child) in parent_borrow.children().enumerate() {
                let child_borrow = child.borrow();
                let child_label = child_borrow.label.clone();
                let digram = Digram {
                    parent: parent_label.clone(),
                    position,
                    child: child_label.clone(),
                };

                // Walk in post-order. Note that we borrow `child` twice, both for reading.
                aux(child, set);
                // Now expand to parent.
                let this_digram = set.entry(digram)
                    .or_insert_with(|| Rc::new(RefCell::new(vec![node.clone()])));
            }
        }
        aux(tree, &mut digrams);
        digrams
    }

    fn proceed_with_tree_repair(&mut self) {
        // Replacement phase.

        // We use a sorted vector rather than a binary heap, as we regularly need
        // to change the order.
        let mut digrams : Vec<_> = {
            let startup_digrams = Self::compute_startup_digrams(&self.root.tree);
            startup_digrams.into_iter()
                .filter_map(|(digram, instances)| if instances.borrow().len() >= 2 {
                    Some(DigramInstances {digram, instances})
                } else {
                    None
                })
                .sorted()
        };
        let mut replacements = HashMap::new();

        // Pick most frequent digram.
        'per_digram: while let Some(DigramInstances { digram, instances }) = digrams.pop() {
            // Generate a new label `generated`.
            let number_of_children = digram.parent.len() + digram.child.len() - 1;
            let generated = self.root.new_generated_label(number_of_children);
            replacements.insert(generated.clone(), digram.clone());

            // Replace instances of `digram` with `generated` all over the tree.
            let mut borrow_instances = instances.borrow_mut();
            'per_node: for mut instance in borrow_instances.iter_mut() {
                let mut borrow_instance = instance.borrow_mut();

                {
                    if digram.parent != borrow_instance.label || borrow_instance.children[digram.position].borrow().label != digram.child {
                        // The node has been rewritten, the digram doesn't apply anymore.
                        continue 'per_node;
                    }
                }

                let mut children = Vec::with_capacity(digram.parent.len() + digram.child.len() - 1);

                std::mem::swap(&mut borrow_instance.children, &mut children);
                let mut iter = children.into_iter();

                // Keep the first `digram.position` children.
                for _ in 0 .. digram.position {
                    borrow_instance.children.push(iter.next().unwrap());
                }

                // Inline the children of child `digram.position`.
                let removed = iter.next().unwrap();
                let mut borrow_removed = removed.borrow_mut();
                debug_assert_eq!(borrow_removed.label, digram.child);
                for child in &borrow_removed.children {
                    let mut borrow_child = child.borrow_mut();
                    borrow_child.parent = Rc::downgrade(instance);
                }

                // Then copy the remaining children.
                borrow_instance.children.extend(iter);

                debug_assert_eq!(borrow_instance.children.len(), digram.parent.len() + digram.child.len() - 1);

                // Finally, change the label.
                borrow_instance.label = generated.clone();
            }


            // FIXME: Update list of most frequent digrams.
            // FIXME:
            // - digrams that deal with `digram.parent` may be affected
            //     walk all these digrams from priority queue, count instances that have vanished
            // - digrams that deal with `digram.child` may be affected
            //     walk all these digrams from priority queue, count instances that have vanished
            // - generate new digrams that have `generated` as parent
            //     can be done in the above loop (N instances)
            // - generate new digrams that have `generated` as child
            //     in the above loop, inspect the parent (1 instance)
            // FIXME: Then sort again
        }


        // FIXME: 2. Pruning phase.
        // FIXME: Eliminate productions that increase final size.
    }

}
