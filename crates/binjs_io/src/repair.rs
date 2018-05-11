//! An implementation of TreeRePair http://www.eti.uni-siegen.de/ti/veroeffentlichungen/12-repair.pdf

use bytes;

use ::TokenWriterError;
use io::TokenWriter;

use std;
use std::cell::RefCell;
use std::collections::{ HashMap };
use std::rc::Rc;

use trees;

type Tree<T> = trees::Tree<T>;
type SharedTree = Rc<RefCell<Option<trees::Tree<Label>>>>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
struct NodeIndex(usize);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct GeneratedLabel(usize);

pub struct Encoder {
    node_counter: usize,
    generated_counter: usize,
    root: SharedTree,
}

#[derive(Debug, Clone)]
pub enum LabelData {
    /// Labelled leaf with `N` children.
    /// Value `N` is **not** written to the byte stream.
    Labelled(Rc<String>, usize), // FIXME: Check whether we're actually using `N`.

    /// Data in leaves is encoded. Rank is 0.
    Leaf(Rc<Vec<u8>>),

    /// A generated label with `N` children.
    /// Value `N` **is** written to the byte stream.
    Generated(GeneratedLabel, usize),
}
impl LabelData {
    fn len(&self) -> usize {
        use self::LabelData::*;
        match *self {
            Leaf(_) => 0,
            Labelled(_, ref len)
            | Generated(_, ref len) => *len
        }
    }
}

#[derive(Debug, Clone)]
pub struct Label {
    /// Unique-per-node index.
    index: NodeIndex,
    data: LabelData,
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
impl Label {
    fn len(&self) -> usize {
        self.data.len()
    }
}

fn take(item: &SharedTree) -> trees::Tree<Label> {
    let mut borrow = item.borrow_mut();
    let child = borrow.take();
    if let Some(child) = child {
        return child;
    } else {
        panic!()
    }
}


impl Encoder {
    fn new_leaf(&mut self, data: Rc<Vec<u8>>) -> Label {
        let index = NodeIndex(self.node_counter);
        self.node_counter += 1;
        Label {
            index,
            data: LabelData::Leaf(data)
        }
    }
    fn new_internal(&mut self, label: String, size: usize) -> Label {
        let index = NodeIndex(self.node_counter);
        self.node_counter += 1;
        Label {
            index,
            data: LabelData::Labelled(Rc::new(label), size)
        }
    }
    fn generate_label(&mut self) -> GeneratedLabel {
        let result = GeneratedLabel(self.generated_counter);
        self.node_counter += 1;
        result
    }

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
        Ok(Rc::new(RefCell::new(Some(Tree::new(self.new_leaf(Rc::new(bytes)))))))
    }

    fn float(&mut self, data: Option<f64>) -> Result<Self::Tree, Self::Error> {
        let bytes = bytes::float::bytes_of_float(data).iter().cloned().collect();
        Ok(Rc::new(RefCell::new(Some(Tree::new(self.new_leaf(Rc::new(bytes)))))))
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
        Ok(Rc::new(RefCell::new(Some(Tree::new(self.new_leaf(Rc::new(buf)))))))
    }

    fn untagged_tuple(&mut self, _data: &[Self::Tree]) -> Result<Self::Tree, Self::Error> {
        unimplemented!()
    }

    fn tagged_tuple(&mut self, tag: &str, items: &[(&str, Self::Tree)]) -> Result<Self::Tree, Self::Error> {
        let mut tree = Tree::new(self.new_internal(tag.to_string(), items.len()));
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Digram {
    parent: Label,
    position: usize,
    child: Label,
}

type Digrams<'a> = HashMap<Digram, Rc<RefCell<HashMap<NodeIndex, &'a mut trees::Node<Label>>>>>;

impl Encoder {
    fn compute_startup_digrams<'a> (tree: &'a mut trees::Tree<Label>) -> Digrams<'a> {
        use std::borrow::BorrowMut;
        let mut digrams = HashMap::new();
        fn aux<'a>(node: &'a mut trees::Node<Label>, set: &mut Digrams<'a>) {
            let parent = node.data.clone();
            for (position, child) in node.children_mut().enumerate() {
                let me = child.data.clone();
                let digram = Digram {
                    parent: parent.clone(),
                    position,
                    child: me.clone()
                };

                // Walk in post-order.
                aux(child, set);
                // Now expand to parent.
                let this_digram = set.entry(digram)
                    .or_insert_with(|| Rc::new(RefCell::new(HashMap::new())));
                if parent == me {
                    let borrow : std::cell::Ref<_> = Rc::as_ref(this_digram).borrow();
                    if let Some(_) = borrow.get(&me.index) {
                        // The digram can be applied either to the child or to the parent, but not both.
                        // We keep the child and ignore the parent.
                        continue;
                    }
                }
                // We now know that there is no conflict between two instances of this specific digram.
                let mut borrow = this_digram.as_ref().borrow_mut();
                borrow.insert(parent.index.clone(), node);
            }
        }
        aux(tree.borrow_mut(), &mut digrams);
        digrams
    }

    fn proceed_with_tree_repair(&mut self) {
        // Replacement phase.
        let /*mut*/ root = take(&self.root);
        let /*mut*/ digrams = Self::compute_startup_digrams(&mut root); // FIXME: We probably want a priority queue here.
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
                //let generated = self.generate_label();
                // let number_of_children = digram.child.len();

                // Replace instances of `digram` with `generated` all over the tree.
                let mut borrow = instances.borrow_mut();
                for (_, &mut instance) in borrow.iter_mut() {
                    assert_eq!(digram.parent, instance.data);
                    for _child in instance.children_mut() {

                    }
                }
                // FIXME:   Replace `d` all over our tree.
                // FIXME:   Update list of most frequent digrams.
                // FIXME:   Stop when we're out of digrams (or when rank grows beyond some limit?)
            } else {
                // We're done with replacement.
                break;
            }
        }


        // FIXME: 2. Pruning phase.
        // FIXME: Eliminate productions that increase final size.
    }
}
