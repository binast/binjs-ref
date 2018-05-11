//! An implementation of TreeRePair http://www.eti.uni-siegen.de/ti/veroeffentlichungen/12-repair.pdf

use bytes;

use ::TokenWriterError;
use io::TokenWriter;

use std;
use std::cell::RefCell;
use std::collections::{ BinaryHeap, HashMap, LinkedList };
use std::rc::{ Rc, Weak };

use itertools::Itertools;

use trees;

type Tree<T> = trees::Tree<T>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct NodeIndex(usize);

pub struct Encoder {
    node_counter: usize,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone)]
pub enum LabelData {
    /// Labelled leaf with `N` children.
    /// Value `N` is **not** written to the byte stream.
    Labelled(Rc<String>, usize),

    /// Data in leaves is encoded. Rank is 0.
    Leaf(Rc<Vec<u8>>),

    // FIXME: We need generated labels, too.
}

#[derive(Clone)]
pub struct Label {
    /// Unique-per-node index.
    index: NodeIndex,

    /// The label
    label: Label,

    /// Children.
    children: LinkedList<Rc<RefCell<SubTree>>>,

    /// The parent. May be Weak::default() if this is the root.
    parent: Weak<RefCell<SubTree>>,
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
    fn children(&self) -> impl Iterator<Item = &Rc<RefCell<SubTree>>> {
        self.children.iter()
    }
    fn replace(&mut self, label: Label, mut children: LinkedList<Rc<RefCell<SubTree>>>) {
        // FIXME: Regenerate the unique index.
        // FIXME: Replace the parent of `children`.
        unimplemented!()
    }
}

struct Root {
    labels: HashMap<Label, LabelIndex>,
    label_counter: usize,
    tree: Rc<RefCell<SubTree>>,
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
    fn new_subtree(&mut self, label: Label, children: LinkedList<Rc<RefCell<SubTree>>>) -> SubTree {
        unimplemented!()
    }
}

fn take(item: &<Encoder as TokenWriter>::Tree) -> trees::Tree<Label> {
    let mut borrow = item.borrow_mut();
    let child = borrow.take();
    if let Some(child) = child {
        return child;
    } else {
        panic!()
    }
}


impl Encoder {
    fn leaf(&mut self, data: Rc<Vec<u8>>) -> Label {
        let index = NodeIndex(self.node_counter);
        self.node_counter += 1;
        Label {
            index,
            data: LabelData::Leaf(data)
        }
    }
    fn internal(&mut self, label: String, size: usize) -> Label {
        let index = NodeIndex(self.node_counter);
        self.node_counter += 1;
        Label {
            index,
            data: LabelData::Labelled(Rc::new(label), size)
        }
    }

    /// Convert a list into a binary representation.
    fn list_aux(&mut self, items: &[<Self as TokenWriter>::Tree]) -> Result<<Self as TokenWriter>::Tree, <Self as TokenWriter>::Error> {
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

    fn done(self) -> Result<(Self::Data, Self::Statistics), Self::Error> {
        unimplemented!()
    }
}


#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
struct Digram {
    parent: Label,
    position: usize,
    child: Label,
}

type Digrams<'a> = HashMap<Digram, HashMap<NodeIndex, &'a trees::Node<Label>>>;

struct DigramAndInstances {
    digram: Digram,
    instances: Rc<RefCell<HashMap<NodeIndex, Rc<RefCell<SubTree>>>>>,
}

impl PartialEq for DigramAndInstances {
    fn eq(&self, other: &Self) -> bool {
        self.digram == other.digram
    }
}
impl Eq for DigramAndInstances { }

impl PartialOrd for DigramAndInstances {
    fn partial_cmp(&self, other: &Self) -> std::option::Option<std::cmp::Ordering> {
        let my_len = self.instances.borrow().len();
        let other_len = other.instances.borrow().len();
        usize::partial_cmp(&my_len, &other_len)
    }
}
impl Ord for DigramAndInstances {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let my_len = self.instances.borrow().len();
        let other_len = other.instances.borrow().len();
        usize::cmp(&my_len, &other_len)
    }
}

impl Encoder {
    fn compute_startup_digrams<'a> (tree: &'a trees::Tree<Label>) -> Digrams<'a> {
        use std::borrow::Borrow;
        let mut digrams = HashMap::new();
        fn aux<'a>(node: &'a trees::Node<Label>, set: &mut Digrams<'a>) {
            for (position, child) in node.children().enumerate() {
                // Walk in post-order.
                aux(child, set);
                // Now expand to parent.
                let digram = Digram {
                    parent: node.data.clone(),
                    position,
                    child: child.data.clone()
                };
                let this_digram = set.entry(digram)
                    .or_insert_with(|| HashMap::new());
                if node.data == child.data {
                    if let Some(_) = this_digram.get(&child.data.index) {
                        // The digram can be applied either to the child or to the parent, but not both.
                        // We keep the child and ignore the parent.
                        continue;
                    }
                }
                // We now know that there is no conflict between two instances of this specific digram.
                let mut digrams_borrow = this_digram.as_ref().borrow_mut();
                digrams_borrow.insert(parent_borrow.index.clone(), node.clone());
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
                .map(|(digram, instances)| DigramAndInstances {digram, instances})
                .sorted()
        };
        let mut replacements = HashMap::new();

        // Pick most frequent digram.
        while let Some(DigramAndInstances { digram, instances }) = digrams.pop() {
            // Generate a new label `generated`.
            let number_of_children = digram.parent.len() + digram.child.len() - 1;
            let generated = self.root.new_generated_label(number_of_children);
            replacements.insert(generated.clone(), digram.clone());

            // Replace instances of `digram` with `generated` all over the tree.
            let mut borrow_instances = instances.borrow_mut();
            for (_, mut instance) in borrow_instances.iter_mut() {
                let mut borrow_instance = instance.borrow_mut();
                assert_eq!(digram.parent, borrow_instance.label);

                let mut children = LinkedList::new();
                std::mem::swap(&mut borrow_instance.children, &mut children);

                let mut prefix = children;
                let mut removed = prefix.split_off(digram.position);
                let mut suffix = removed.split_off(1);

                assert_eq!(removed.len(), 1);
                let mut removed = removed.pop_front()
                    .unwrap();

                let mut borrow_removed = removed.borrow_mut();
                assert_eq!(digram.child, borrow_removed.label);
                let mut replacement = borrow_removed.children.clone();

                prefix.append(&mut replacement);
                prefix.append(&mut suffix);

                borrow_instance.replace(generated.clone(), prefix);
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
        aux(tree.borrow(), &mut digrams);
        digrams
    }
}
