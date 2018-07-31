//! A format in which we adopt an arithmetic encoding per nature.
//!
//! # Overview
//!
//! The general idea of arithmetic coding is that we can achieve a high
//! density of information by encoding a sequence of symbols as a single
//! rational number in [0, 1), with the actual number being determined
//! from the probability of encountering each successive symbol in a
//! given context.
//!
//! For instance, consider `BinaryOperation(op, left, right)` in a context C.
//! For this example, let's assume that we are starting from the full segment
//! [0, 1). In context C, we may encounter a number of subtrees labelled S_1,
//! S_2, ..., S_n with probabilities p(C, S_1), p(C, S_2), ..., p(C, S_n).
//! For simplicity, let's assume that label `BinaryOperation` is S_1
//! (so, with probability p(C, S_1))
//! Any number in [0, p(C, S_1)) may be used to represent `BinaryOperation`.
//!
//! Now, we proceed to encode `op` in a context C + `BinaryOperation`. Again,
//! we have a number of possible values for `op`: op_1, ... op_n'. If `op`
//! is `op_1`, any number of [0, p(C, S_1) * P(C + BinaryOperation, op_1))
//! may be used to represent `op`.
//!
//! Similarly, we'll encode `left` and `right` in context C + `BinaryOperation`.
//!
//!
//! # Extensibility
//!
//! While we can have a built-in initial dictionary, the list of symbols cannot be
//! hardcoded.
//!
//!
//!
//! # Encoding
//!
//! Pass 1: walk the tree, compute the probabilities for each symbol.
//! Pass 2: use probabilities to actually encode the symbols.
//!   - Whenever we encounter a symbol that we have never seen in a given context,
//!     the symbol is followed by its definition as soon as possible.
//!
//! FIXME: Some symbols will need to be defined more than once. Need to estimate
//! how often this appears.

use io::TokenWriter;
use ::{ DictionaryPlacement, CompressionTarget, TokenWriterError };
use util:: { Counter, GenericCounter };

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{ Hash, Hasher };
use std::io::Write;
use std::rc::Rc;

use itertools::Itertools;

struct BitWriter; // Placeholder

enum Appearance {
    /// This is the first time the symbol appears, so it must be defined.
    First,
    /// The symbol has already appeared at least once, no need to redefine it.
    Next
}

/// Instructions for serializing a symbol.
struct Serial {
    /// The index for this symbol.
    ///
    /// For instance, in
    /// ```idl
    /// enum VariableDeclarationKind {
    ///    "var",
    ///    "let",
    ///    "const"
    /// };
    /// ```
    ///
    /// we may decide that "var" has index 0, "let" has index 1, "const" has index 2.
    ///
    /// Note that, since `VariableDeclarationKind` may appear in several context, there
    /// may be several indices for "var" depending on the context. For instance, a
    /// different numbering in `ForInOfBinding` and in `VariableDeclaration`.
    ///
    /// FIXME: How do we make this future-proof? If the order of values changes in
    /// the `enum` or if we add a new value in the middle? Or should we forbid this?
    index: u64,

    /// FIXME: How do we make sure that we *can* add new symbols?
    probability: u64,
    appearance: Appearance,
}

/*
trait Model {
    type Context;
    type Label;
    fn get_frequency(&mut self, context: &Self::Context, label: &Self::Label) -> Result<Serial, ()>;
}

struct Compressor<M> where M: Model {
    model: M,
    sink: BitWriter,
}
impl<M> Compressor<M> where M: Model {
    /// Change internal state to introduce a new symbol.
    ///
    /// This symbol will eventually be written to the sink.
    ///
    /// `Ok(true)` if this is the first time the symbol is written
    /// and we need to call `add_definition` to define it.
    fn compress(&mut self, context: &M::Context, label: &M::Label) -> Result<bool, std::io::Error> {
        unimplemented!()
    }
    fn add_definition(&mut self, data: &[u8]) -> Result<(), std::io::Error> {
        unimplemented!()
    }
    fn flush(&mut self) -> Result<(), std::io::Error> {
        unimplemented!()
    }
    fn done(self) -> (M, BitWriter) {
        unimplemented!()
    }
}
*/

#[derive(Clone, Debug)]
pub struct SubTree {
    label: Label,
    children: Vec<SharedTree>,
}
pub type SharedTree = Rc<RefCell<SubTree>>;

#[derive(Clone, Copy, Debug)]
enum Direction {
    Enter,
    Exit
}

struct PerCategory<T> {
    tags: T,
}

struct Context<'a> {
    tree: &'a SharedTree,
    parent: Option<(&'a SharedTree, usize)>,
}

struct Compressor {
}
impl Compressor {
    fn compress(&mut self, subtree: &SharedTree, path: &mut Vec<(SharedTree, usize)>) -> Result<(), std::io::Error> {
        let borrow = subtree.borrow();
        match borrow.label {
            Label::Tag(ref tag) => {
                // FIXME: Check latest probability of `(subtree, parent)`.
                // FIXME: Deduce probability range.
                // FIXME: Update latest probability.
                // FIXME: Find out if we have already seen `(subtree, parent)`. If not, write to BitWriter.
            }
            _ => {
                warn!(target: "multiarith", "Skipping serialization of label {:?} (not implemented yet)", borrow.label);
            }
        }
        // Compress children.
        if borrow.children.len() == 0 {
            return Ok(())
        }
        if let Label::List(_) = borrow.label {
            // We don't care about the position in a label.
            for child in borrow.children.iter() {
                self.compress(child, path)?;
            }
        } else {
            for (i, child) in borrow.children.iter().enumerate() {
                path.push((subtree.clone(), i)); // FIXME: We don't need that much cloning.
                self.compress(child, path)?;
                path.pop();
            }
        }
        Ok(())
    }

/*
    fn serialize_label<M>(&self, parent: &Option<(SharedTree, usize)>, compressors: &mut PerCategory<Compressor<M>>) -> Result<(), std::io::Error>
        where M: Model<Label = Label, Context = Context>
    {
        match self.label {
            Label::Tag(ref tag) => {
                if compressors.tags.compress(&parent, &self.label)? {
                    // This is the first time we encounter the label, we need to define it.
                    compressors.tags.add_definition(tag.as_bytes())?
                }
            }
            _ => {
                warn!(target: "multiarith", "Skipping serialization of label {:?} (not implemented yet)", self.label);
                return Ok(())
            }
        }
        Ok(())
    }
    fn serialize_children<M>(&self, parent: &Option<SharedTree>, compressors: &mut PerCategory<Compressor<M>>) -> Result<(), std::io::Error>
        where M: Model<Label = Label, Context = Context>
    {
        let new_parent = match self.label {
            Label::Tag(_) => Some(self.clone()),
            _ => (*parent).clone(), // FIXME: We could do without so much cloning.
        };
        // Serialize children in order.
        for (i, child) in self.children.iter().enumerate() {
            let my_parent = match new_parent {
                None => None,
                Some(rc) => Some((rc.clone(), i)) // FIXME: We could do without so much cloning.
            };
            let borrow = child.borrow();
            borrow.serialize_label(my_parent, compressors)?;
            borrow.serialize_children(new_parent, compressors)?;
        }
        Ok(())
    }


    }
*/

}

impl SubTree {
    fn with_labels<F: FnMut(&Label)>(&self, f: &mut F) {
        f(&self.label);
        for child in &self.children {
            child.borrow().with_labels(f);
        }
    }
    fn with_labels_mut<F: FnMut(Direction, &mut Label)>(&mut self, f: &mut F) {
        f(Direction::Enter, &mut self.label);
        for child in &self.children {
            child.borrow_mut().with_labels_mut(f);
        }
        f(Direction::Exit, &mut self.label);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ScopeIndex(usize);
impl Counter for ScopeIndex {
    fn internal_make(value: usize) -> Self {
        ScopeIndex(value)
    }
}

/// A trivial wrapping of f64 with Hash and Eq.
#[derive(Clone, Debug, PartialEq, PartialOrd, Copy)]
pub struct F64(pub f64);
impl Eq for F64 { } // Not strictly true.
impl Ord for F64 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
} // Not strictly true.
impl Hash for F64 {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.0.to_bits().hash(state)
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord)]
pub enum Label {
    String(Option<Rc<String>>),
    Number(Option<F64>),
    Bool(Option<bool>),
    List(Option<u32>),
    Tag(Rc<String>),
    /// Scope. Any `Declare` within the `Scope`
    /// stays in the `Scope`.
    Scope(ScopeIndex),
    /// Declare a variable throughout the current `Scope`.
    Declare(Option<Rc<String>>),
    /// Reference a variable throughout the current `Scope`.
    ///
    /// Initially entered as `LiteralReference`, then processed to a `NumberedReference`.
    LiteralReference(Option<Rc<String>>),
    NumberedReference(Option<u32>),
}

impl Eq for Label { /* Yes, it's probably not entirely true for f64 */ }
impl Hash for Label { /* Again, not entirely true for f64 */
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        use self::Label::*;
        match *self {
            String(ref s) => s.hash(state),
            Bool(ref b) => b.hash(state),
            List(ref l) => l.hash(state),
            Tag(ref s) => s.hash(state),
            Number(ref num) => num.map(|x| f64::to_bits(x.0)).hash(state), // Not really true for a f64.
            Scope(ref s) => s.hash(state),
            Declare(ref d) => d.hash(state),
            LiteralReference(ref r) => r.hash(state),
            NumberedReference(ref r) => r.hash(state),
        }
    }
}


pub struct TreeTokenWriter {
    root: SharedTree,
    scope_counter: GenericCounter<ScopeIndex>,
}
impl TreeTokenWriter {
    pub fn new() -> Self {
        Self {
            scope_counter: GenericCounter::new(),
            root: Rc::new(RefCell::new(SubTree {
                label: Label::String(None),
                children: vec![]
            }))
        }
    }
    fn new_tree(&mut self, tree: SubTree) -> Result<SharedTree, TokenWriterError> {
        self.root = Rc::new(RefCell::new(tree));
        Ok(self.root.clone())
    }

    fn number_references(&mut self) -> Result<SharedTree, TokenWriterError> {
        // Undeclared references
        let top = Rc::new(RefCell::new(vec![]));
        let stack = Rc::new(RefCell::new(vec![top.clone()]));
        self.root.borrow_mut().with_labels_mut(&mut |direction, label| {
            let rewrite = match (direction, &label) {
                (Direction::Enter, Label::Scope(_)) => {
                    let mut borrow_stack = stack.borrow_mut();
                    borrow_stack.push(Rc::new(RefCell::new(vec![])));
                    None
                }
                (Direction::Exit, Label::Scope(_)) => {
                    let mut borrow_stack = stack.borrow_mut();
                    borrow_stack.pop();
                    None
                }
                (Direction::Enter, Label::Declare(Some(ref s))) => {
                    let borrow_stack = stack.borrow();
                    let mut borrow_frame = borrow_stack.last()
                        .unwrap()
                        .borrow_mut();
                    borrow_frame
                        .push(s.clone());
                    None
                }
                (Direction::Enter, Label::LiteralReference(None)) => {
                    Some(Label::NumberedReference(None))
                }
                (Direction::Enter, Label::LiteralReference(Some(ref s))) => {
                    let mut depth = 0;
                    let mut found = None;
                    {
                        let borrow_stack = stack.borrow();
                        'find_in_stack: for frame in borrow_stack.iter().rev() {
                            let borrow_frame = frame.borrow();
                            if let Some(index) = borrow_frame.iter()
                                .position(|name| name == s)
                            {
                                found = Some(index);
                                break 'find_in_stack;
                            } else {
                                depth += borrow_frame.len()
                            }
                        }
                    }
                    let index = match found {
                        Some(found) => found,
                        None => {
                            let mut borrow_top = top.borrow_mut();
                            borrow_top.push(s.clone());
                            borrow_top.len()
                        }
                    };
                    Some(Label::NumberedReference(Some((depth + index) as u32)))
                }
                _ => None,
            };
            if let Some(rewrite) = rewrite {
                *label = rewrite;
            }
        });

        // Now declare all undeclared variables.
        let root = self.root.clone();
        let top = top.borrow()
            .iter()
            .map(|name| Rc::new(RefCell::new(SubTree {
                label: Label::Declare(Some(name.clone())),
                children: vec![]
            })))
            .collect();
        let declared_undeclared_variables = self.new_tree(SubTree {
            label: Label::Tag(Rc::new("_undeclared_variables".to_string())),
            children: top
        })?;
        let scope_index = self.scope_counter.next();
        self.new_tree(SubTree {
            label: Label::Scope(scope_index),
            children: vec![
                declared_undeclared_variables,
                root
            ]
        })
    }
}
impl TokenWriter for TreeTokenWriter {
    type Statistics = usize; // Placeholder
    type Tree = SharedTree;
    type Data = Vec<u8>;

    fn tagged_tuple(&mut self, tag: &str, children: &[(&str, Self::Tree)]) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Tag(Rc::new(tag.to_string())),
            children: children.iter()
                .map(|(_, tree)| tree.clone())
                .collect()
        })
    }

    fn offset(&mut self) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }

    fn bool(&mut self, value: Option<bool>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Bool(value),
            children: vec![]
        })
    }

    fn float(&mut self, value: Option<f64>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::Number(value.map(F64)),
            children: vec![]
        })
    }

    fn string(&mut self, value: Option<&str>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::String(value.map(|x| Rc::new(x.to_string()))),
            children: vec![]
        })
    }

    fn string_enum(&mut self, value: &str) -> Result<Self::Tree, TokenWriterError> {
        self.tagged_tuple(value, &[])
    }

    fn list(&mut self, children: Vec<Self::Tree>) -> Result<Self::Tree, TokenWriterError> {
        self.new_tree(SubTree {
            label: Label::List(Some(children.len() as u32)),
            children
        })
    }

    fn untagged_tuple(&mut self, _: &[Self::Tree]) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }

    fn done(mut self) -> Result<(Self::Data, Self::Statistics), TokenWriterError> {
        use labels:: { ExplicitIndexLabeler, /*MRULabeler,*/ ParentPredictionFrequencyLabeler, RawLabeler };
        self.number_references()?;

        unimplemented!()
    }
}
