
// Impl
use easter::decl::*;
use easter::expr::*;
use easter::fun::*;
use easter::id::*;
use easter::obj::*;
use easter::patt::*;
use easter::prog::*;
use easter::punc::*;
use easter::stmt::*;

use std::ops::Deref;

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
        Expression(Expression),
        FunDecl,
        Name,
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
    pub enum Expression {
        This,
        Array,
        Object,
        Function,
        Sequence,
        UnaryMinus,
        UnaryPlus,
        UnaryNot,
        UnaryBitNot,
        UnaryTypeof,
        UnaryVoid,
        UnaryDelete,
        Eq,
        NEq,
        StrictEq,
        StrictNEq,
        Lt,
        LEq,
        Gt,
        GEq,
        LShift,
        RShift,
        URShift,
        Plus,
        Minus,
        Times,
        Div,
        Mod,
        BitOr,
        BitXor,
        BitAnd,
        In,
        Instanceof,
        LogOr,
        LogAnd,
        PreInc,
        PostInc,
        PreDec,
        PostDec,
        Assign,
        PlusEq,
        MinusEq,
        TimesEq,
        DivEq,
        ModEq,
        LShiftEq,
        RShiftEq,
        URShiftEq,
        BitOrEq,
        BitXorEq,
        BitAndEq,
        Conditional,
        Call,
        New,
        Member,
        NewTarget,
        True,
        False,
        Null,
        Number,
        RegExp,
        String,
        DecimalInt,
        RadixInt,
        Float,
    }
}
use self::kind::*;

/// A raw node in the intermediate serialization tree.
///
/// Once written to disk, an instance of `Unlabelled` does *not* contain
/// by itself sufficient information to determine which enum case is actually
/// used. This information MUST be extrapolated from the context, typically from
/// a parent `Labelled`.
enum Unlabelled {
    /// A string designed to be represented as an atom (e.g. literal strings,
    /// identifiers, ...). Will be internalized in the atoms table. Length
    /// will be written to the file.
    Atom(String),

    /// One raw byte.
    RawByte(u8),

    /// A node with a number of children determined by the grammar.
    /// Length will NOT be written to the file. Can have 0 children.
    Tuple(Vec<SerializeTree>),

    /// A node with a number of children left unspecified by the grammar
    /// (typically a list). Length will be written to the file.
    List(Vec<SerializeTree>),
}

impl Unlabelled {
    /// Add a label.
    ///
    /// Just a shorthand to keep code readable.
    fn label(self, kind: Kind) -> Labelled {
        Labelled {
            kind,
            tree: self
        }
    }

    /// Add a `Kind::Statement` label.
    ///
    /// Just a shorthand to keep code readable.
    fn statement(self, kind: Statement) -> Labelled {
        self.label(Kind::Statement(kind))
    }
    /// Add a `Kind::Statement` label.
    ///
    /// Just a shorthand to keep code readable.
    fn expression(self, kind: Expression) -> Labelled {
        self.label(Kind::Expression(kind))
    }

    // Note: We explicitly use `list` rather than deriving `ToUnlabelled`
    // for `Vec<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn list<T>(items: &[T]) -> Self where T: ToUnlabelled {
        Unlabelled::List(items.iter()
            .map(|item| item.to_naked().into_tree())
            .collect())
    }

    // Note: We explicitly use `option` rather than deriving `ToUnlabelled`
    // for `Option<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn option<T>(item: &Option<T>) -> Self where T: ToUnlabelled {
        match *item {
            None => Unlabelled::Tuple(vec![]),
            Some(ref some) => some.to_naked()
        }
    }

    /// Convert into a tree.
    fn into_tree(self) -> SerializeTree {
        SerializeTree::Unlabelled(self)
    }

}

/// A node in the intermediate serialization tree.
///
/// Once written to disk, the `kind` MUST contain sufficient information
/// to determine the structure of the `tree`.
struct Labelled {
   kind: Kind,
   tree: Unlabelled // Or Unlabelled?
}

impl Labelled {
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
    fn option<T>(item: &Option<T>) -> Labelled where T: ToLabelled {
        match *item {
            None => Unlabelled::Tuple(vec![]).label(Kind::Empty),
            Some(ref some) => some.to_labelled()
        }
    }

    fn into_tree(self) -> SerializeTree {
        SerializeTree::Labelled(self)
    }
}


enum SerializeTree {
    Unlabelled(Unlabelled),
    /// Label a subtree with parsing information.
    /// In a followup pass, labels are rewritten as reference to one of several
    /// strings tables (e.g. one table for expressions, one for patterns, etc.)
    /// to ensure that references generally fit in a single byte.
    Labelled(Labelled)
}

impl SerializeTree {
    fn label(self, kind: Kind) -> Labelled {
        Labelled {
            kind,
            tree: Unlabelled::Tuple(vec![self])
        }
    }

    fn statement(self, kind: Statement) -> Labelled {
        self.label(Kind::Statement(kind))
    }

    fn expression(self, kind: Expression) -> Labelled {
        self.label(Kind::Expression(kind))
    }

    fn empty() -> Self {
        SerializeTree::Labelled(Labelled {
            kind: Kind::Empty,
            tree: Unlabelled::Tuple(vec![])
        })
    }
}

trait ToSerializeTree {
    fn to_serialize(&self) -> SerializeTree {
        unimplemented!()
    }
}

trait ToUnlabelled {
    fn to_naked(&self) -> Unlabelled {
        unimplemented!()
    }
}

impl<T> ToUnlabelled for Box<T> where T: ToUnlabelled {
    fn to_naked(&self) -> Unlabelled {
        (self.as_ref()).to_naked()
    }
}

trait ToLabelled {
    fn to_labelled(&self) -> Labelled {
        unimplemented!()
    }
}

impl<T> ToLabelled for Box<T> where T: ToLabelled {
    fn to_labelled(&self) -> Labelled {
        (self.as_ref()).to_labelled()
    }
}

pub fn compile(ast: &Script) -> String {
    let _string_tree = ast.to_naked();
    unimplemented!();
}



impl ToLabelled for StmtListItem {
    fn to_labelled(&self) -> Labelled {
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
    fn to_labelled(&self) -> Labelled {
        let Decl::Fun(ref fun) = *self;
        fun.to_naked()
            .label(Kind::FunDecl)
    }
}

impl ToLabelled for Stmt {
    fn to_labelled(&self) -> Labelled {
        use easter::stmt::Stmt::*;
        match *self {
            Empty(_) =>
                Unlabelled::Tuple(vec![])
                    .label(Kind::Empty),
            Block(_, ref items) =>
                Labelled::list(items)
                    .statement(Statement::Block),
            Expr(_, ref expr, _) =>
                expr.to_labelled()
                    .into_tree()
                    .statement(Statement::Expression),
            Var(_, ref dtor, _) =>
                Labelled::list(dtor)
                    .statement(Statement::VariableDeclaration),
            If(_, ref condition, ref then_branch, ref else_branch) =>
                Unlabelled::Tuple(vec![
                    condition.to_labelled().into_tree(),
                    then_branch.to_labelled().into_tree(),
                    Labelled::option(else_branch).into_tree()
                ]).statement(Statement::IfThenElse),
            Label(_, ref id, ref statement) =>
                Unlabelled::Tuple(vec![
                    id.to_labelled().into_tree(),
                    statement.to_labelled().into_tree()
                ]).statement(Statement::Label),
            Break(_, ref id, _) =>
                Labelled::option(id).into_tree().statement(Statement::Break),
            Cont(_, ref id, _) =>
                Labelled::option(id).into_tree().statement(Statement::Cont),
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
                Labelled::option(expr)
                    .into_tree()
                    .statement(Statement::Return),
            Throw(_, ref expr, _) =>
                expr.to_labelled()
                    .into_tree()
                    .statement(Statement::Throw),
            Try(_, ref list, ref catch, Some(ref finalizer)) =>
                Unlabelled::Tuple(vec![
                    Labelled::list(list).into_tree(),
                    Unlabelled::option(catch).into_tree(),
                    Labelled::list(finalizer).into_tree(),
                ]).statement(Statement::Try),
            Try(_, ref list, ref catch, None) =>
                Unlabelled::Tuple(vec![
                    Labelled::list(list).into_tree(),
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
                    Labelled::option(head).into_tree(),
                    Labelled::option(cond).into_tree(),
                    Labelled::option(incr).into_tree(),
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
    fn to_labelled(&self) -> Labelled {
// Note: Here, there's a divergence between Esprima and Esprit.
// Trying to follow Esprima. When parsing to Esprit, we'll use the label of `id`/`pat` to
// differentiate between Simple and Compound. When parsing to Esprima, it's actually the same node.
        match *self {
            Dtor::Simple(_, ref id, ref expr) =>
                Unlabelled::Tuple(vec![
                    id.to_labelled().into_tree(),
                    Labelled::option(expr).into_tree()
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
    fn to_labelled(&self) -> Labelled {
        match *self {
            CompoundPatt::Arr(_, ref patterns) => {
                let pat2 : Vec<_> = patterns.iter().map(|x|
                        SerializeTree::Labelled(Labelled::option(x))
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
    fn to_labelled(&self) -> Labelled {
        match *self {
            Patt::Simple(ref id) =>
                id.to_labelled(),
            Patt::Compound(ref compound) =>
                compound.to_labelled()
        }
    }
}

// FIXME: To implement.

impl ToLabelled for Expr {
    fn to_labelled(&self) -> Labelled {
        match *self {
            Expr::This(_) =>
                Unlabelled::Tuple(vec![])
                    .expression(Expression::This),
            Expr::Id(ref id) => id.to_labelled(),
            Expr::Arr(_, ref values) => {
                let list : Vec<_> = values.iter()
                    .map(Labelled::option)
                    .map(Labelled::into_tree)
                    .collect();
                Unlabelled::List(list)
                    .expression(Expression::Array)
            }
            Expr::Obj(_, ref properties) =>
                Unlabelled::list(properties)
                    .expression(Expression::Object),
            Expr::Fun(ref fun) =>
                fun.to_naked()
                    .expression(Expression::Function),
            Expr::Seq(_, ref expr) =>
                Labelled::list(expr)
                    .expression(Expression::Sequence),
            Expr::Unop(_, ref op, ref expr) => {
                let kind = match op.tag {
                    UnopTag::Minus => Expression::UnaryMinus,
                    UnopTag::Plus => Expression::UnaryPlus,
                    UnopTag::Not => Expression::UnaryNot,
                    UnopTag::BitNot => Expression::UnaryBitNot,
                    UnopTag::Typeof => Expression::UnaryTypeof,
                    UnopTag::Void => Expression::UnaryVoid,
                    UnopTag::Delete => Expression::UnaryDelete,
                };
                expr.to_labelled()
                    .into_tree()
                    .expression(kind)
            }
            Expr::Binop(_, ref op, ref left, ref right) => {
                let kind = match op.tag {
                    BinopTag::Eq => Expression::Eq,
                    BinopTag::NEq => Expression::NEq,
                    BinopTag::StrictEq => Expression::StrictEq,
                    BinopTag::StrictNEq => Expression::StrictNEq,
                    BinopTag::Lt => Expression::Lt,
                    BinopTag::LEq => Expression::LEq,
                    BinopTag::Gt => Expression::Gt,
                    BinopTag::GEq => Expression::GEq,
                    BinopTag::LShift => Expression::LShift,
                    BinopTag::RShift => Expression::RShift,
                    BinopTag::URShift => Expression::URShift,
                    BinopTag::Plus => Expression::Plus,
                    BinopTag::Minus => Expression::Minus,
                    BinopTag::Times => Expression::Times,
                    BinopTag::Div => Expression::Div,
                    BinopTag::Mod => Expression::Mod,
                    BinopTag::BitOr => Expression::BitOr,
                    BinopTag::BitXor => Expression::BitXor,
                    BinopTag::BitAnd => Expression::BitAnd,
                    BinopTag::In => Expression::In,
                    BinopTag::Instanceof => Expression::Instanceof,
                };
                Unlabelled::Tuple(vec![
                    left.to_labelled().into_tree(),
                    right.to_labelled().into_tree()
                ]).expression(kind)
            }
            Expr::Logop(_, ref op, ref left, ref right) => {
                let kind = match op.tag {
                    LogopTag::Or => Expression::LogOr,
                    LogopTag::And => Expression::LogAnd,
                };
                Unlabelled::Tuple(vec![
                    left.to_labelled().into_tree(),
                    right.to_labelled().into_tree()
                ]).expression(kind)
            }
            Expr::PreInc(_, ref expr) => {
                expr.to_labelled()
                    .into_tree()
                    .expression(Expression::PreInc)
            }
            Expr::PostInc(_, ref expr) => {
                expr.to_labelled()
                    .into_tree()
                    .expression(Expression::PostInc)
            }
            Expr::PreDec(_, ref expr) => {
                expr.to_labelled()
                    .into_tree()
                    .expression(Expression::PreDec)
            }
            Expr::PostDec(_, ref expr) => {
                expr.to_labelled()
                    .into_tree()
                    .expression(Expression::PostDec)
            }
            Expr::Assign(_, ref op, ref pat, ref expr) => {
                let kind = match op.tag {
                    AssopTag::Eq => Expression::Assign,
                    AssopTag::PlusEq => Expression::PlusEq,
                    AssopTag::MinusEq => Expression::MinusEq,
                    AssopTag::TimesEq => Expression::TimesEq,
                    AssopTag::DivEq => Expression::DivEq,
                    AssopTag::ModEq => Expression::ModEq,
                    AssopTag::LShiftEq => Expression::LShiftEq,
                    AssopTag::RShiftEq => Expression::RShiftEq,
                    AssopTag::URShiftEq => Expression::URShiftEq,
                    AssopTag::BitOrEq => Expression::BitOrEq,
                    AssopTag::BitXorEq => Expression::BitXorEq,
                    AssopTag::BitAndEq => Expression::BitAndEq,
                };
                Unlabelled::Tuple(vec![
                    pat.to_labelled().into_tree(),
                    expr.to_labelled().into_tree(),
                ]).expression(kind)
            }
            Expr::Cond(_, ref cond, ref then_branch, ref else_branch) => {
                Unlabelled::Tuple(vec![
                    cond.to_labelled().into_tree(),
                    then_branch.to_labelled().into_tree(),
                    else_branch.to_labelled().into_tree(),
                ]).expression(Expression::Conditional)
            }
            Expr::Call(_, ref callee, ref arguments) => {
                Unlabelled::Tuple(vec![
                    callee.to_labelled().into_tree(),
                    Labelled::list(arguments).into_tree()
                ]).expression(Expression::Call)
            }
            Expr::New(_, ref callee, Some(ref arguments)) => {
                Unlabelled::Tuple(vec![
                    callee.to_labelled().into_tree(),
                    Labelled::list(arguments).into_tree()
                ]).expression(Expression::New)
            }
            Expr::New(_, ref callee, None) => {
                Unlabelled::Tuple(vec![
                    callee.to_labelled().into_tree(),
                    SerializeTree::empty()
                ]).expression(Expression::New)
            },
            // Note: We collapse `Dot` and `Brack` in `Member`.
            Expr::Dot(_, ref object, ref property) => {
                Unlabelled::Tuple(vec![
                    object.to_labelled().into_tree(),
                    Unlabelled::Atom(property.value).label(Kind::Name).into_tree(),
                ]).expression(Expression::Member)
            }
            Expr::Brack(_, ref object, ref property) => {
                Unlabelled::Tuple(vec![
                    object.to_labelled().into_tree(),
                    property.to_labelled().into_tree(),
                ]).expression(Expression::Member)
            }
            Expr::NewTarget(_) =>
                Unlabelled::Tuple(vec![])
                    .expression(Expression::NewTarget),
            Expr::True(_) =>
                Unlabelled::Tuple(vec![])
                    .expression(Expression::True),
            Expr::False(_) =>
                Unlabelled::Tuple(vec![])
                    .expression(Expression::False),
            Expr::Null(_) =>
                Unlabelled::Tuple(vec![])
                    .expression(Expression::Null),
            Expr::String(_, ref string) =>
                Unlabelled::Atom(string.value.clone())
                    .expression(Expression::String),
            Expr::RegExp(_, ref regexp) => {
                let flags : String = regexp.flags.iter().collect();
                Unlabelled::Tuple(vec![
                    Unlabelled::Atom(regexp.pattern.clone()).into_tree(),
                    Unlabelled::Atom(flags).into_tree()
                ]).expression(Expression::RegExp)
            },
        }
    }
}

impl ToLabelled for Patt<AssignTarget> { } // FIXME: Could be ToUnlabelled, I'm ok with that.

impl ToLabelled for Id { }

impl ToUnlabelled for Case { }

impl ToUnlabelled for Catch { }

impl ToLabelled for ForHead { }

impl ToLabelled for ForInHead { }

impl ToLabelled for ForOfHead { }

impl ToUnlabelled for PropPatt<Id> { }

impl ToUnlabelled for Fun { }

impl ToUnlabelled for Script { }

impl ToUnlabelled for Prop { }