//! An implementation of TreeRePair http://www.eti.uni-siegen.de/ti/veroeffentlichungen/12-repair.pdf

use bytes;

use ::TokenWriterError;
use io::TokenWriter;

use std;
use std::cell::RefCell;
use std::collections::{ HashMap, HashSet };
use std::rc::Rc;

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

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Digram {
    parent: Label,
    position: usize,
    child: Label,
}

type Digrams<'a> = HashMap<Digram, HashMap<NodeIndex, &'a trees::Node<Label>>>;

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
                this_digram.insert(node.data.index.clone(), node);
            }
        }
        aux(tree.borrow(), &mut digrams);
        digrams
    }
}
