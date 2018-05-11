//! An implementation of TreeRePair http://www.eti.uni-siegen.de/ti/veroeffentlichungen/12-repair.pdf

use bytes;

use ::TokenWriterError;
use io::TokenWriter;

use std;
use std::cell::RefCell;
use std::collections::{ HashMap, LinkedList };
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct NodeIndex(usize);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct LabelIndex(usize);


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct GeneratedLabel(usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Label {
    Named {
        label: Rc<String>,
        children: usize,
    },
    Generated {
        label: GeneratedLabel,
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
    children: LinkedList<Rc<RefCell<SubTree>>>,
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
    fn children(&self) -> impl Iterator<Item = &Rc<RefCell<SubTree>>> {
        self.children.iter()
    }
    fn set(&mut self, label: Label, mut children: LinkedList<Rc<RefCell<SubTree>>>) {
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

type SharedTree = Rc<RefCell<Option<SubTree>>>;

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


#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Digram {
    parent: Label,
    position: usize,
    child: Label,
}
type Digrams<'a> = HashMap<Digram, Rc<RefCell<HashMap<NodeIndex, Rc<RefCell<SubTree>>>>>>;

impl Encoder {
    fn compute_startup_digrams<'a> (tree: &Rc<RefCell<SubTree>>) -> Digrams<'a> {
        use std::borrow::BorrowMut;
        let mut digrams = HashMap::new();
        fn aux<'a>(node: &Rc<RefCell<SubTree>>, set: &mut Digrams<'a>) {
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
                    .or_insert_with(|| Rc::new(RefCell::new(HashMap::new())));
                if parent_label == child_label {
                    // If `child` is already in `this_digram` (which can only happen if
                    // `parent == me`), we don't want to also put the parent in `this_digram`,
                    // otherwise there will be a conflict.
                    // cause an overlap if `child` is alre. If so, discard the parent.
                    let digrams_borrow : std::cell::Ref<_> = Rc::as_ref(this_digram).borrow();
                    if digrams_borrow.get(&child_borrow.index).is_some() {
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
        let /*mut*/ digrams = Self::compute_startup_digrams(&self.root.tree); // FIXME: We probably want a priority queue here.
        // let mut replacements = vec![];

        loop {
            // Pick most frequent digram `d`.
            let mut max = None;
            for (digram, instances) in &digrams {
                let borrow = instances.borrow();
                if let Some((max_occurrences, _, _)) = max {
                    if borrow.len() <= max_occurrences {
                        // We already have the max.
                        continue;
                    }
                }
                max = Some((borrow.len(), digram.clone(), instances.clone()));
            }

            if let Some((_, digram, mut instances)) = max {
                // Generate a new label `generated`.
                let number_of_children = digram.parent.len() + digram.child.len() - 1;
                let generated = self.root.new_generated_label(number_of_children);

                // Replace instances of `digram` with `generated` all over the tree.
                let mut borrow_instances = instances.borrow_mut();
                for (_, mut instance) in borrow_instances.iter_mut() {
                    // FIXME: We need to change the label!
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

                    borrow_instance.set(generated.clone(), prefix);
                }
                // FIXME:   Update list of most frequent digrams.
            } else {
                // We're done with replacement.
                break;
            }
        }


        // FIXME: 2. Pruning phase.
        // FIXME: Eliminate productions that increase final size.
    }

}
