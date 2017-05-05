
use atoms::*;
use varnum::*;

use easter::decl::*;
use easter::expr::*;
use easter::fun::*;
use easter::id::*;
use easter::obj::*;
use easter::patt::*;
use easter::prog::*;
use easter::punc::*;
use easter::stmt::*;

use std;
use std::io::Write;
use std::ops::Deref;


fn bytes_for_number(value: f64) -> Vec<SerializeTree> {
    assert!(std::mem::size_of_val(&value) == std::mem::size_of::<[u8;8]>());
    // FIXME: This makes assumptions on endianness.
    let bytes = unsafe { std::mem::transmute::<_, [u8;8]>(value) };
    bytes.iter()
        .cloned()
        .map(Unlabelled::RawByte)
        .map(SerializeTree::Unlabelled)
        .collect()
}


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
        VarInit,
        Get,
        Set,
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
    }

    impl Kind {
        pub fn get_spec_name(&self) -> String {
            use self::Kind::*;
            let str = match *self {
                Empty => "",
                FunDecl => "FunctionDeclaration",
                Name => "Identifier",
                Statement(ref x) => return x.get_spec_name(),
                VarDecl(ref x) => return x.get_spec_name(),
                BindingPattern(ref x) => return x.get_spec_name(),
                Expression(ref x) => return x.get_spec_name(),
            };
            str.to_string()
        }
    }

    impl Statement {
        pub fn get_spec_name(&self) -> String {
            use self::Statement::*;
            let str = match *self {
                Block => "BlockStatement",
                Expression => "ExpressionStatement",
                VariableDeclaration => "VariableDeclaration",
                IfThenElse => "IfStatement",
                Label => "LabeledStatement",
                Break => "BreakStatement",
                Cont => "ContinueStatement",
                With => "WithStatement",
                Switch => "SwitchStatement",
                Return => "ReturnStatement",
                Throw => "ThrowStatement",
                Try => "TryStatement",
                While => "WhileStatement",
                DoWhile => "DoWhileStatement",
                For => "ForStatement",
                ForIn => "ForInStatement",
                ForOf => "ForOfStatement",
                Debugger => "DebuggerStatement",
            };
            str.to_string()
        }
    }

    impl Variable {
        pub fn get_spec_name(&self) -> String {
            use self::Variable::*;
            let str = match *self {
                Var => "var",
                Let => "let",
                Const => "const",
                Get => "get",
                Set => "set",
                VarInit => "init",
            };
            str.to_string()
        }
    }

    impl BindingPattern {
        pub fn get_spec_name(&self) -> String {
            use self::BindingPattern::*;
            let str = match *self {
                Array => "ArrayPattern",
                Object => "ObjectPattern",
            };
            str.to_string()
        }
    }

    impl Expression {
        pub fn get_spec_name(&self) -> String {
            use self::Expression::*;
            let str = match *self {
                This => "ThisExpression",
                Array => "ArrayExpression",
                Object => "ObjectExpression",
                Function => "FunctionExpression",
                Sequence => "SequenceExpression",
                UnaryMinus => "Unary-",
                UnaryPlus => "Unary+",
                UnaryNot => "~",
                UnaryBitNot => "~",
                UnaryTypeof => "typeof",
                UnaryVoid => "void",
                UnaryDelete => "delete",
                Eq => "==",
                NEq => "!=",
                StrictEq => "===",
                StrictNEq => "!==",
                Lt => "<",
                LEq => "<=",
                Gt => ">",
                GEq => ">=",
                LShift => "<<",
                RShift => ">>",
                URShift => ">>>",
                Plus => "+",
                Minus => "-",
                Times => "*",
                Div => "/",
                Mod => "%",
                BitOr => "|",
                BitXor => "^",
                BitAnd => "&",
                In => "in",
                Instanceof => "instanceof",
                LogOr => "||",
                LogAnd => "&&",
                PreInc => "++_",
                PostInc => "_++",
                PreDec => "--_",
                PostDec => "_--",
                Assign => "=",
                PlusEq => "+=",
                MinusEq => "-=",
                TimesEq => "*=",
                DivEq => "/=",
                ModEq => "%=",
                LShiftEq => "<<=",
                RShiftEq => ">>=",
                URShiftEq => ">>>=",
                BitOrEq => "|=",
                BitXorEq => "^=",
                BitAndEq => "&=",
                Conditional => "ConditionalExpression",
                Call => "CallExpression",
                New => "NewExpression",
                Member => "MemberExpression",
                NewTarget => "NewTargetExpression", // FIXME: No clue what this is.
                True => "true",
                False => "false",
                Null => "null",
                Number => "NumberLiteral",
                RegExp => "RegexpLiteral",
                String => "StringLiteral",
            };
            str.to_string()
        }
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

    fn walk_tree<T>(&self, f: &mut T) where T: FnMut(&SerializeTree) {
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

    fn into_unlabelled(self) -> Unlabelled {
        Unlabelled::Tuple(vec![SerializeTree::Labelled(self)])
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

    fn walk<T>(&self, f: &mut T) where T: FnMut(&SerializeTree) {
        f(self);
        match *self {
            SerializeTree::Unlabelled(ref tree) |
            SerializeTree::Labelled(Labelled { kind: _, tree: ref tree }) =>
                tree.walk_tree(f)
        }
    }

    fn walk_unlabelled<T>(&self, f: &mut T) where T: FnMut(&Unlabelled) {
        self.walk(&mut |tree| {
            match *self {
                SerializeTree::Unlabelled(ref tree) |
                SerializeTree::Labelled(Labelled { kind: _, tree: ref tree }) =>
                    f(tree)
            }
        })
    }

    fn walk_labelled<T>(&self, f: &mut T) where T: FnMut(&Labelled) {
        self.walk(&mut |tree| {
            if let SerializeTree::Labelled(ref tree) = *tree {
                f(tree)
            }
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

pub fn compile<T>(ast: &Script, out: &mut T) -> Result<usize, std::io::Error> where T: Write {
    // 1. Generate tree.
    let tree = SerializeTree::Unlabelled(ast.to_naked());
        // FIXME: Before any kind of release, we'd need to add the following features:
        // - collecting free variables and annotating functions with them
        // - collecting immediate uses of `eval` and annotating functions with them

    // Total bytes written.
    let mut bytes = 0;

    // 2. Collect atoms into an atoms table.
    let mut atoms = AtomsTableInitializer::new();
    tree.walk_unlabelled(&mut |item| {
        if let Unlabelled::Atom(ref s) = *item {
            atoms.add(s.clone())
        }
    });

    // 3. Write atoms table (first the lengths, then the table)
    let atoms = atoms.compile();
    bytes += atoms.write(out)?;

    // 4. Collect kinds into an atoms table.
    let mut kinds = AtomsTableInitializer::new();
    tree.walk_labelled(&mut |item| {
        kinds.add(item.kind.get_spec_name());
    });

    // 5. Write kinds table
    let kinds = kinds.compile();
    bytes += kinds.write(out)?;

    // FIXME: 6. Write `tree`, substituting
    // kinds to `kinds` and atoms to `atoms`.
    unimplemented!();
    Ok(bytes)
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
                    id.to_naked().into_tree(),
                    statement.to_labelled().into_tree()
                ]).statement(Statement::Label),
            Break(_, ref id, _) =>
                Unlabelled::option(id).into_tree().statement(Statement::Break),
            Cont(_, ref id, _) =>
                Unlabelled::option(id).into_tree().statement(Statement::Cont),
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
                    id.to_naked().label(Kind::Name).into_tree(),
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
                id.to_naked().label(Kind::Name),
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
            Expr::Id(ref id) => id.to_naked()
                    .label(Kind::Name),
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
                    Unlabelled::Atom(property.value.clone()).label(Kind::Name).into_tree(),
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
            Expr::Number(_, ref number) => {
                let tuple = bytes_for_number(number.value);
                Unlabelled::Tuple(tuple).expression(Expression::Number)
            }
        }
    }
}

impl ToUnlabelled for Id {
    fn to_naked(&self) -> Unlabelled {
        Unlabelled::Atom(self.name.clone().into_string())
    }
}

impl ToLabelled for Patt<AssignTarget> { } // FIXME: Could be ToUnlabelled, I'm ok with that.

impl ToUnlabelled for Case {
    fn to_naked(&self) -> Unlabelled {
        Unlabelled::Tuple(vec![
            Labelled::option(&self.test).into_tree(),
            Labelled::list(&self.body).into_tree()
        ])
    }
}

impl ToUnlabelled for Catch {
    fn to_naked(&self) -> Unlabelled {
        Unlabelled::Tuple(vec![
            self.param.to_labelled().into_tree(),
            Labelled::list(&self.body).into_tree()
        ])
    }
}

impl ToLabelled for ForHead {
    fn to_labelled(&self) -> Labelled {
        match *self {
            ForHead::Var(_, ref dtor) =>
                Labelled::list(dtor)
                    .label(Kind::VarDecl(Variable::Var)),
            ForHead::Let(_, ref dtor) =>
                Labelled::list(dtor)
                    .label(Kind::VarDecl(Variable::Let)),
            ForHead::Expr(_, ref expr) =>
                expr.to_labelled()
        }
    }
}

impl ToLabelled for ForInHead {
    fn to_labelled(&self) -> Labelled {
        match *self {
            ForInHead::VarInit(_, ref id, ref expr) =>
                Unlabelled::Tuple(vec![
                    id.to_naked().into_tree(),
                    expr.to_labelled().into_tree()
                ]).label(Kind::VarDecl(Variable::VarInit)),
            ForInHead::Var(_, ref pat) =>
                pat.to_labelled()
                    .into_tree()
                    .label(Kind::VarDecl(Variable::Var)),
            ForInHead::Let(_, ref pat) =>
                pat.to_labelled()
                    .into_tree()
                    .label(Kind::VarDecl(Variable::Let)),
            ForInHead::Expr(ref expr) =>
                expr.to_labelled()
        }
    }
}


impl ToLabelled for ForOfHead {
    fn to_labelled(&self) -> Labelled {
        match *self {
            ForOfHead::Var(_, ref pat) =>
                pat.to_labelled()
                    .into_tree()
                    .label(Kind::VarDecl(Variable::Var)),
            ForOfHead::Let(_, ref pat) =>
                pat.to_labelled()
                    .into_tree()
                    .label(Kind::VarDecl(Variable::Let)),
            ForOfHead::Expr(ref expr) =>
                expr.to_labelled()
        }
    }
}

impl ToLabelled for PropKey {
    fn to_labelled(&self) -> Labelled {
        match *self {
            // Afaict, that's `foo` in `{ foo: bar }`
            PropKey::Id(_, ref string) =>
                Unlabelled::Atom(string.clone())
                    .label(Kind::Name),
            // Afaict, that's `"foo"` in `{ "foo": bar }`
            PropKey::String(_, ref literal) =>
                Unlabelled::Atom(literal.value.clone())
                    .expression(Expression::String),
            PropKey::Number(_, ref literal) => {
                let tuple = bytes_for_number(literal.value);
                Unlabelled::Tuple(tuple)
                    .expression(Expression::Number)
            }
        }
    }
}

impl ToUnlabelled for Fun {
    fn to_naked(&self) -> Unlabelled {
        Unlabelled::Tuple(vec![
            Unlabelled::option(&self.id).into_tree(),
            Labelled::list(&self.params.list).into_tree(),
            Labelled::list(&self.body).into_tree(),
        ])
    }
}

impl ToUnlabelled for PropPatt<Id> {
    fn to_naked(&self) -> Unlabelled {
        Unlabelled::Tuple(vec![
            self.key.to_labelled().into_tree(),
            self.patt.to_labelled().into_tree()
        ])
    }
}


impl ToUnlabelled for Prop {
    fn to_naked(&self) -> Unlabelled {
        Unlabelled::Tuple(vec![
            self.key.to_labelled().into_tree(),
            self.val.to_labelled().into_tree()
        ])
    }
}

impl ToLabelled for PropVal {
    fn to_labelled(&self) -> Labelled {
        match *self {
            PropVal::Get(_, ref body) =>
                Labelled::list(body)
                    .label(Kind::VarDecl(Variable::Get)),
            PropVal::Set(_, ref pat, ref body) =>
                Unlabelled::Tuple(vec![
                    pat.to_labelled().into_tree(),
                    Labelled::list(body).into_tree(),
                ]).label(Kind::VarDecl(Variable::Set)),
            PropVal::Init(ref expr) =>
                expr.to_labelled()
        }
    }
}

impl ToUnlabelled for Script {
    fn to_naked(&self) -> Unlabelled {
        Labelled::list(&self.body)
    }
}
