
// Impl
use easter::prog::*;
use easter::stmt::*;
use easter::decl::*;
use easter::expr::*;
use easter::stmt::*;
use easter::patt::*;
use easter::fun::*;
use easter::id::*;

use std::ops::Deref;

use std::rc::Rc;

/// Labels to differentiate AST nodes.
// FIXME: There is a size bonus if we can make sure that all the commonly used instances
// of `Kind` are fewer than 128, as we can fit this in a single VarNum byte. If this is not
// the case, consider using several pools (e.g. one for Statement, one for VarDecl, ...).
// This is more accident-prone.
pub mod kind {
    pub enum Kind {
        Empty,
        Statement(Statement),
        VarDecl(Variable),
        BindingPattern(BindingPattern),
    }
    pub enum BindingPattern {
        Array,
        Object,
    }
    pub enum Variable {
        Var,
        Let,
        Const,
    }
    pub enum Statement {
        Block,
        Expression,
        VariableDeclaration,
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
        Debugger,
    }
}
use self::kind::*;


pub enum Unlabelled {
    /// A string designed to be represented as an atom (e.g. literal strings,
    /// identifiers, ...). Will be internalized in the atoms table.
    Atom(String),

    /// One raw byte.
    RawByte(u8),

    /// A node with a number of children determined by the grammar.
    /// Length will NOT be written to the file.
    Tuple(Vec<SerializeTree>),

    /// A node with a number of children left unspecified by the grammar
    /// (typically a list). Length will be written to the file.
    List(Vec<SerializeTree>),
}

impl Unlabelled {
    fn label(self, kind: Kind) -> LabelledTree {
        LabelledTree {
            kind,
            tree: Box::new(self)
        }
    }

    fn statement(self, kind: Statement) -> LabelledTree {
        self.label(Kind::Statement(kind))
    }

    // Note: We explicitly use `list` rather than deriving `ToSerializeTree`
    // for `Vec<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn list<T>(items: &[T]) -> Self where T: ToUnlabelled {
        Unlabelled::List(items.iter()
            .map(|item| item.to_naked().into())
            .collect())
    }

    // Note: We explicitly use `option` rather than deriving `ToSerializeTree`
    // for `Option<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn option<T>(item: &Option<T>) -> Self where T: ToUnlabelled {
        match *item {
            None => Unlabelled::Tuple(vec![]),
            Some(ref some) => some.to_naked()
        }
    }

    fn into_tree(self) -> SerializeTree {
        SerializeTree::Unlabelled(self)
    }
}

impl Into<SerializeTree> for Unlabelled {
    fn into(self) -> SerializeTree {
        SerializeTree::Unlabelled(self)
    }
}

pub struct LabelledTree {
   kind: Kind,
   tree: Box<Unlabelled> // Or Unlabelled?
}

impl LabelledTree {
    // Note: We explicitly use `list` rather than deriving `ToSerializeTree`
    // for `Vec<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn list<T>(items: &[T]) -> Unlabelled where T: ToLabelled {
        Unlabelled::List(items.iter()
            .map(|item| item.to_labelled().into_tree())
            .collect())
    }
    fn tuple<T>(items: &[T]) -> Unlabelled where T: ToLabelled {
        Unlabelled::Tuple(items.iter()
            .map(|item| item.to_labelled().into_tree())
            .collect())
    }
    // Note: We explicitly use `option` rather than deriving `ToSerializeTree`
    // for `Option<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn option<T>(item: &Option<T>) -> LabelledTree where T: ToLabelled {
        match *item {
            None => Unlabelled::Tuple(vec![]).label(Kind::Empty),
            Some(ref some) => some.to_labelled()
        }
    }

    fn into_tree(self) -> SerializeTree {
        SerializeTree::Labelled(self)
    }
}


pub enum SerializeTree {
    Unlabelled(Unlabelled),
    /// Label a subtree with parsing information.
    /// In a followup pass, labels are rewritten as reference to one of several
    /// strings tables (e.g. one table for expressions, one for patterns, etc.)
    /// to ensure that references generally fit in a single byte.
    Labelled(LabelledTree)
}

impl SerializeTree {
    fn label(self, kind: Kind) -> LabelledTree {
        LabelledTree {
            kind,
            tree: Box::new(Unlabelled::Tuple(vec![self]))
        }
    }

    fn statement(self, kind: Statement) -> LabelledTree {
        self.label(Kind::Statement(kind))
    }

    fn empty() -> Self {
        SerializeTree::Labelled(LabelledTree {
            kind: Kind::Empty,
            tree: Box::new(Unlabelled::Tuple(vec![]))
        })
    }
}

pub trait ToSerializeTree {
    fn to_serialize(&self) -> SerializeTree {
        unimplemented!()
    }
}

pub trait ToUnlabelled {
    fn to_naked(&self) -> Unlabelled {
        unimplemented!()
    }
}

impl<T> ToUnlabelled for Box<T> where T: ToUnlabelled {
    fn to_naked(&self) -> Unlabelled {
        (self.as_ref()).to_naked()
    }
}

pub trait ToLabelled {
    fn to_labelled(&self) -> LabelledTree {
        unimplemented!()
    }
}

impl<T> ToLabelled for Box<T> where T: ToLabelled {
    fn to_labelled(&self) -> LabelledTree {
        (self.as_ref()).to_labelled()
    }
}

pub fn compile(ast: &Script) -> String {
    let _string_tree = ast.to_naked();
    unimplemented!();
}



impl ToLabelled for StmtListItem {
    fn to_labelled(&self) -> LabelledTree {
        match *self {
            StmtListItem::Decl(ref decl) => decl.to_labelled().into(),
            StmtListItem::Stmt(ref stmt) => stmt.to_labelled().into(),
        }
    }
}


impl<T> ToSerializeTree for Box<T> where T: ToSerializeTree {
    fn to_serialize(&self) -> SerializeTree {
        self.deref().to_serialize()
    }
}

impl ToLabelled for Decl {
    fn to_labelled(&self) -> LabelledTree {
        let Decl::Fun(ref fun) = *self;
        fun.to_labelled()
    }
}

impl ToLabelled for Stmt {
    fn to_labelled(&self) -> LabelledTree {
        use easter::stmt::Stmt::*;
        match *self {
            Empty(_) =>
                Unlabelled::Tuple(vec![])
                    .label(Kind::Empty),
            Block(_, ref items) =>
                LabelledTree::list(items)
                    .statement(Statement::Block),
            Expr(_, ref expr, _) =>
                expr.to_labelled()
                    .into_tree()
                    .statement(Statement::Expression),
            Var(_, ref dtor, _) =>
                LabelledTree::list(dtor)
                    .statement(Statement::VariableDeclaration),
            If(_, ref condition, ref then_branch, ref else_branch) =>
                Unlabelled::Tuple(vec![
                    condition.to_labelled().into_tree(),
                    then_branch.to_labelled().into_tree(),
                    LabelledTree::option(else_branch).into_tree()
                ]).statement(Statement::IfThenElse),
            Label(_, ref id, ref statement) =>
                Unlabelled::Tuple(vec![
                    id.to_labelled().into_tree(),
                    statement.to_labelled().into_tree()
                ]).statement(Statement::Label),
            Break(_, ref id, _) =>
                LabelledTree::option(id).into_tree().statement(Statement::Break),
            Cont(_, ref id, _) =>
                LabelledTree::option(id).into_tree().statement(Statement::Cont),
            With(_, ref expr, ref statement) =>
                Unlabelled::Tuple(vec![
                    expr.to_labelled().into_tree(),
                    statement.to_labelled().into_tree()
                ]).statement(Statement::With),
            Switch(_, ref expr, ref cases) =>
                // We use a pair (expr, list). We could have used a heterogenous
                // list instead, this wouldn't change the performance or size of
                // the file.
                Unlabelled::Tuple(vec![
                    expr.to_labelled().into_tree(),
                    Unlabelled::list(cases).into_tree(),
                ]).statement(Statement::Switch),
            Return(_, ref expr, _) =>
                LabelledTree::option(expr)
                    .into_tree()
                    .statement(Statement::Return),
            Throw(_, ref expr, _) =>
                expr.to_labelled()
                    .into_tree()
                    .statement(Statement::Throw),
            Try(_, ref list, ref catch, Some(ref finalizer)) =>
                Unlabelled::Tuple(vec![
                    LabelledTree::list(list).into_tree(),
                    Unlabelled::option(catch).into_tree(),
                    LabelledTree::list(finalizer).into_tree(),
                ]).statement(Statement::Try),
            Try(_, ref list, ref catch, None) =>
                Unlabelled::Tuple(vec![
                    LabelledTree::list(list).into_tree(),
                    Unlabelled::option(catch).into_tree(),
                    SerializeTree::empty()
                ]).statement(Statement::Try),
            While(_, ref expr, ref stmt) =>
                Unlabelled::Tuple(vec![
                    expr.to_labelled().into_tree(),
                    stmt.to_labelled().into_tree(),
                ]).statement(Statement::While),
            DoWhile(_, ref expr, ref stmt, _) =>
                Unlabelled::Tuple(vec![
                    expr.to_labelled().into_tree(),
                    stmt.to_labelled().into_tree(),
                ]).statement(Statement::DoWhile),
            For(_, ref head, ref cond, ref incr, ref stmt) =>
                Unlabelled::Tuple(vec![
                    LabelledTree::option(head).into_tree(),
                    LabelledTree::option(cond).into_tree(),
                    LabelledTree::option(incr).into_tree(),
                    stmt.to_labelled().into_tree()
                ]).statement(Statement::For),
            ForIn(_, ref head, ref expr, ref stmt) =>
                Unlabelled::Tuple(vec![
                    head.to_labelled().into_tree(),
                    expr.to_labelled().into_tree(),
                    stmt.to_labelled().into_tree()
                ]).statement(Statement::ForIn),
            ForOf(_, ref head, ref expr, ref stmt) =>
                Unlabelled::Tuple(vec![
                    head.to_labelled().into_tree(),
                    expr.to_labelled().into_tree(),
                    stmt.to_labelled().into_tree()
                ]).statement(Statement::ForOf),
            Debugger(_, _) => SerializeTree::empty().statement(Statement::Debugger)
        }
    }
}


impl ToLabelled for Dtor {
    fn to_labelled(&self) -> LabelledTree {
// Note: Here, there's a divergence between Esprima and Esprit.
// Trying to follow Esprima. When parsing to Esprit, we'll use the label of `id`/`pat` to
// differentiate between Simple and Compound. When parsing to Esprima, it's actually the same node.
        match *self {
            Dtor::Simple(_, ref id, ref expr) =>
                Unlabelled::Tuple(vec![
                    id.to_labelled().into_tree(),
                    LabelledTree::option(expr).into_tree()
                ]).label(Kind::VarDecl(Variable::Var)),
            Dtor::Compound(_, ref pat, ref expr) =>
                Unlabelled::Tuple(vec![
                    pat.to_labelled().into_tree(),
                    expr.to_labelled().into_tree()
                ]).label(Kind::VarDecl(Variable::Var))
        }
    }
}

impl ToLabelled for CompoundPatt<Id> {
    fn to_labelled(&self) -> LabelledTree {
        match *self {
            CompoundPatt::Arr(_, ref patterns) => {
                let pat2 : Vec<_> = patterns.iter().map(|x|
                        SerializeTree::Labelled(LabelledTree::option(x))
                    )
                    .collect();
                Unlabelled::List(pat2)
                    .label(Kind::BindingPattern(BindingPattern::Array))
            }
            CompoundPatt::Obj(_, ref patterns) => {
                Unlabelled::list(patterns)
                    .label(Kind::BindingPattern(BindingPattern::Object))
            }
        }
    }
}

impl ToLabelled for Patt<Id> {
    fn to_labelled(&self) -> LabelledTree {
        match *self {
            Patt::Simple(ref id) =>
                id.to_labelled(), // FIXME: Make sure that this is labelled
            Patt::Compound(ref compound) =>
                compound.to_labelled()
        }
    }
}

// FIXME: To implement.

impl ToLabelled for Expr { }

impl ToLabelled for Id { }

impl ToUnlabelled for Case { }

impl ToUnlabelled for Catch { }

impl ToLabelled for ForHead { }

impl ToLabelled for ForInHead { }

impl ToLabelled for ForOfHead { }

impl ToUnlabelled for PropPatt<Id> { }

impl ToLabelled for Fun { }

impl ToUnlabelled for Script { }