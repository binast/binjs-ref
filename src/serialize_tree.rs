/// A raw node in the intermediate serialization tree.
///
/// Once written to disk, an instance of `Unlabelled` does *not* contain
/// by itself sufficient information to determine which enum case is actually
/// used. This information MUST be extrapolated from the context, typically from
/// a parent `Labelled`.
pub enum Unlabelled<T> {
    /// A string designed to be represented as an atom (e.g. literal strings,
    /// identifiers, ...). Will be internalized in the atoms table. Length
    /// will be written to the file.
    Atom(String),

    /// One raw byte.
    RawByte(u8),

    /// A node with a number of children determined by the grammar.
    /// Length will NOT be written to the file. Can have 0 children.
    Tuple(Vec<SerializeTree<T>>),

    /// A node with a number of children left unspecified by the grammar
    /// (typically a list). Length will be written to the file.
    List(Vec<SerializeTree<T>>),
}

impl<K> Unlabelled<K> where K: Default {
    /// Add a label.
    ///
    /// Just a shorthand to keep code readable.
    pub fn label(self, kind: K) -> Labelled<K> {
        Labelled {
            kind,
            tree: self
        }
    }

    // Note: We explicitly use `list` rather than deriving `ToUnlabelled`
    // for `Vec<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    pub fn list<T>(items: &[T]) -> Self where T: ToUnlabelled<K> {
        Unlabelled::List(items.iter()
            .map(|item| item.to_naked().into_tree())
            .collect())
    }

    // Note: We explicitly use `option` rather than deriving `ToUnlabelled`
    // for `Option<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    pub fn option<T>(item: &Option<T>) -> Self where T: ToUnlabelled<K> {
        match *item {
            None => Unlabelled::Tuple(vec![]),
            Some(ref some) => some.to_naked()
        }
    }

    /// Convert into a tree.
    pub fn into_tree(self) -> SerializeTree<K> {
        SerializeTree::Unlabelled(self)
    }

    pub fn walk_tree<T>(&self, f: &mut T) where T: FnMut(&SerializeTree<K>) {
        match *self {
            Unlabelled::Tuple(ref subtrees)
            | Unlabelled::List(ref subtrees) =>
                for subtree in subtrees {
                    subtree.walk(f)
                },
            _ => {}
        }
    }
}

/// A node in the intermediate serialization tree.
///
/// Once written to disk, the `kind` MUST contain sufficient information
/// to determine the structure of the root of `tree`.
pub struct Labelled<K> {
   kind: K,
   tree: Unlabelled<K>
}

impl<K> Labelled<K> {
    // Note: We explicitly use `list` rather than deriving `ToSerializeTree`
    // for `Vec<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    pub fn list<T>(items: &[T]) -> Unlabelled<K> where T: ToLabelled<K> {
        Unlabelled::List(items.iter()
            .map(|item| item.to_labelled().into_tree())
            .collect())
    }
    // Note: We explicitly use `option` rather than deriving `ToSerializeTree`
    // for `Option<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    pub fn option<T>(item: &Option<T>) -> Labelled<K> where T: ToLabelled<K>, K: Default {
        match *item {
            None => Unlabelled::Tuple(vec![]).label(K::default()),
            Some(ref some) => some.to_labelled()
        }
    }

    pub fn into_tree(self) -> SerializeTree<K> {
        SerializeTree::Labelled(self)
    }

    pub fn kind(&self) -> &K {
        &self.kind
    }
}


pub enum SerializeTree<K> {
    Unlabelled(Unlabelled<K>),
    /// Label a subtree with parsing information.
    /// In a followup pass, labels are rewritten as reference to one of several
    /// strings tables (e.g. one table for expressions, one for patterns, etc.)
    /// to ensure that references generally fit in a single byte.
    Labelled(Labelled<K>)
}

impl<K> SerializeTree<K> where K: Default {
    pub fn label(self, kind: K) -> Labelled<K> {
        Labelled {
            kind,
            tree: Unlabelled::Tuple(vec![self])
        }
    }

    pub fn empty() -> Self {
        SerializeTree::Labelled(Labelled {
            kind: K::default(),
            tree: Unlabelled::Tuple(vec![])
        })
    }

    pub fn walk<T>(&self, f: &mut T) where T: FnMut(&SerializeTree<K>) {
        f(self);
        match *self {
            SerializeTree::Unlabelled(ref tree) |
            SerializeTree::Labelled(Labelled { kind: _, ref tree }) =>
                tree.walk_tree(f)
        }
    }

    pub fn walk_unlabelled<T>(&self, f: &mut T) where T: FnMut(&Unlabelled<K>) {
        self.walk(&mut |tree| {
            match *tree {
                SerializeTree::Unlabelled(ref tree) |
                SerializeTree::Labelled(Labelled { kind: _, ref tree }) =>
                    f(tree)
            }
        })
    }

    pub fn walk_labelled<T>(&self, f: &mut T) where T: FnMut(&Labelled<K>) {
        self.walk(&mut |tree| {
            if let SerializeTree::Labelled(ref tree) = *tree {
                f(tree)
            }
        })
    }
}

pub trait ToUnlabelled<K> {
    fn to_naked(&self) -> Unlabelled<K> {
        unimplemented!()
    }
}

impl<T, K> ToUnlabelled<K> for Box<T> where T: ToUnlabelled<K> {
    fn to_naked(&self) -> Unlabelled<K> {
        (self.as_ref()).to_naked()
    }
}

pub trait ToLabelled<K> {
    fn to_labelled(&self) -> Labelled<K> {
        unimplemented!()
    }
}

impl<T, K> ToLabelled<K> for Box<T> where T: ToLabelled<K> {
    fn to_labelled(&self) -> Labelled<K> {
        (self.as_ref()).to_labelled()
    }
}