use easter::prog::*;
use easter::stmt::*;
use easter::decl::*;
use easter::expr::*;
use easter::stmt::*;
use easter::fun::*;
use easter::id::*;

use std::ops::Deref;

// FIXME: How do we make sure that there is never any ambiguity?

pub enum Kind {
    Empty,
    Block,
    Var,
    IfThenElse,
    Label,
    Break,
    Cont,
    With,
    Switch,
    Return,
    Throw,
    Try,
    While,
    DoWhile,
    For,
    ForIn,
    ForOf,
    Debugger
}


pub enum SerializeTree {
    // A string designed to be represented as an atom (e.g. literal strings,
    // identifiers, ...). Will be internalized in the atoms table.
    Atom(String),

    // One raw byte.
    RawByte(u8),

    // A node with a number of children determined by the grammar.
    // Length won't be written to the file.
    Tuple(Vec<SerializeTree>),

    // A node with a number of children left unspecified by the grammar
    // (typically a list). Length will be written to the file.
    List(Vec<SerializeTree>),

    Label(Kind, Box<SerializeTree>)
}

impl SerializeTree {
    fn label(self, kind: Kind) -> Self {
        SerializeTree::Label(kind, Box::new(self))
    }
    fn ternary(a: SerializeTree, b: SerializeTree, c: SerializeTree) -> Self {
        SerializeTree::Tuple(vec![a, b, c])
    }
    fn binary(a: SerializeTree, b: SerializeTree) -> Self {
        SerializeTree::Tuple(vec![a, b])
    }
    fn empty() -> Self {
        SerializeTree::Tuple(vec![])
    }
    // Note: We explicitly use `list` rather than deriving `ToSerializeTree`
    // for `Vec<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn list<T>(items: &[T]) -> Self where T: ToSerializeTree {
        SerializeTree::List(items.iter()
            .map(|item| item.to_serialize())
            .collect())
    }
    // Note: We explicitly use `option` rather than deriving `ToSerializeTree`
    // for `Option<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn option<T>(item: &Option<T>) -> Self where T: ToSerializeTree {
        match *item {
            None => SerializeTree::empty().label(Kind::Empty),
            Some(ref some) => some.to_serialize()
        }
    }
    fn option_list<T>(item: &Option<Vec<T>>) -> Self where T: ToSerializeTree {
        match *item {
            None => SerializeTree::empty().label(Kind::Empty),
            Some(ref some) => SerializeTree::List(some.iter()
                .map(|item| item.to_serialize())
                .collect())
        }
    }
}

pub trait ToSerializeTree {
    fn to_serialize(&self) -> SerializeTree {
        unimplemented!()
    }
}

pub fn compile<T>(ast: &T) -> String where T: ToSerializeTree {
    let _string_tree = ast.to_serialize();
    unimplemented!();
}

impl ToSerializeTree for Script {
    fn to_serialize(&self) -> SerializeTree {
        SerializeTree::list(&self.body)
    }
}

impl ToSerializeTree for StmtListItem {
    fn to_serialize(&self) -> SerializeTree {
        match *self {
            StmtListItem::Decl(ref decl) => decl.to_serialize(),
            StmtListItem::Stmt(ref stmt) => stmt.to_serialize(),
        }
    }
}


impl<T> ToSerializeTree for Box<T> where T: ToSerializeTree {
    fn to_serialize(&self) -> SerializeTree {
        (*self).to_serialize()
    }
}


impl ToSerializeTree for Decl {
    fn to_serialize(&self) -> SerializeTree {
        let Decl::Fun(ref fun) = *self;
        fun.to_serialize()
    }
}

impl ToSerializeTree for Dtor {
    fn to_serialize(&self) -> SerializeTree {
        match *self {
            Dtor::Simple(_, ref id, ref expr) => unimplemented!(),
            Dtor::Compound(_, ref pat, ref expr) => unimplemented!()
        }
    }
}

impl ToSerializeTree for Expr { }

impl ToSerializeTree for Id { }

impl ToSerializeTree for Case { }

impl ToSerializeTree for Catch { }

impl ToSerializeTree for ForHead { }

impl ToSerializeTree for ForInHead { }

impl ToSerializeTree for ForOfHead { }

impl ToSerializeTree for Stmt {
    fn to_serialize(&self) -> SerializeTree {
        use easter::stmt::Stmt::*;
        match *self {
            Empty(_) => SerializeTree::empty().label(Kind::Empty),
            Block(_, ref items) => SerializeTree::list(items).label(Kind::Block),
            Var(_, ref dtor, _) => SerializeTree::list(dtor).label(Kind::Var),
            Expr(_, ref expr, _) => expr.to_serialize(),
            If(_, ref condition, ref then_branch, ref else_branch) =>
                SerializeTree::ternary(
                    condition.to_serialize(),
                    then_branch.to_serialize(),
                    SerializeTree::option(else_branch)
                ).label(Kind::IfThenElse),
            Label(_, ref id, ref statement) =>
                SerializeTree::binary(
                    id.to_serialize(),
                    statement.to_serialize()
                ).label(Kind::Label),
            Break(_, ref id, _) =>
                SerializeTree::option(id).label(Kind::Break),
            Cont(_, ref id, _) =>
                SerializeTree::option(id).label(Kind::Cont),
            With(_, ref expr, ref statement) =>
                SerializeTree::binary(
                    expr.to_serialize(),
                    statement.to_serialize()
                ).label(Kind::With),
            Switch(_, ref expr, ref cases) =>
                SerializeTree::binary(
                    expr.to_serialize(),
                    SerializeTree::list(cases)
                ).label(Kind::Switch),
            Return(_, ref expr, _) =>
                SerializeTree::option(expr).label(Kind::Return),
            Throw(_, ref expr, _) =>
                expr.to_serialize().label(Kind::Throw),
            Try(_, ref list, ref catch, ref next) =>
                SerializeTree::ternary(
                    SerializeTree::list(list),
                    SerializeTree::option(catch),
                    SerializeTree::option_list(next),
                ).label(Kind::Try),
            While(_, ref expr, ref stmt) =>
                SerializeTree::binary(
                    expr.to_serialize(),
                    stmt.to_serialize()
                ).label(Kind::While),
            DoWhile(_, ref expr, ref stmt, _) =>
                SerializeTree::binary(
                    expr.to_serialize(),
                    stmt.to_serialize()
                ).label(Kind::DoWhile),
            For(_, ref head, ref cond, ref incr, ref stmt) =>
                SerializeTree::Tuple(vec![
                    SerializeTree::option(head),
                    SerializeTree::option(cond),
                    SerializeTree::option(incr),
                    stmt.to_serialize()
                ]).label(Kind::For),
            ForIn(_, ref head, ref expr, ref stmt) =>
                SerializeTree::Tuple(vec![
                    head.to_serialize(),
                    expr.to_serialize(),
                    stmt.to_serialize(),
                ]).label(Kind::ForIn),
            ForOf(_, ref head, ref expr, ref stmt) =>
                SerializeTree::Tuple(vec![
                    head.to_serialize(),
                    expr.to_serialize(),
                    stmt.to_serialize(),
                ]).label(Kind::ForOf),
            Debugger(_, _) => SerializeTree::empty().label(Kind::Debugger)
        }
    }
}

impl ToSerializeTree for Fun {
    
}