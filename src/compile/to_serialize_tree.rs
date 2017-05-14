use atoms::*;
use compile::serialize_tree::*;
use compile::serialize_tree;
use compile::serialize_tree::Context;
use kind::*;
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
use std::collections::HashSet;
use std::io::Write;
use std::cell::RefCell;
use std::rc::Rc;

pub struct EnvNode {
    let_declarations: HashSet<String>,
    var_declarations: HashSet<String>,
    looks_like_direct_eval: bool,
    link: EnvLink
}
impl EnvNode {
    fn new(link: EnvLink) -> Self {
        EnvNode {
            let_declarations: HashSet::new(),
            var_declarations: HashSet::new(),
            looks_like_direct_eval: false,
            link
        }
    }
}
pub enum EnvLink {
    Toplevel,
    Function(Env),
    Block(Env)
}
#[derive(Clone)]
pub struct Env(Rc<RefCell<EnvNode>>);
impl Drop for Env {
    fn drop(&mut self) {
        use self::EnvLink::*;
        let mut borrow = self.0.borrow_mut();
        if let EnvNode {
            link: EnvLink::Block(ref mut parent),
            ref mut var_declarations,
            ..
        } = *borrow {
            if self.looks_like_direct_eval() {
                // Propagate `eval`, `var` to parent.
                parent.add_eval()
            }
            let ref mut parent_var_declarations = parent.0
                .borrow_mut()
                .var_declarations;
            for x in var_declarations.drain() {
                parent_var_declarations.insert(x);
            }
        }
    }
}
impl Env {
    fn new(link: EnvLink) -> Self {
        Env(Rc::new(RefCell::new(EnvNode::new(link))))
    }

    pub fn toplevel() -> Self {
        Env::new(EnvLink::Toplevel)
    }

    /// Enter a function. Every variable
    /// declaration local to the function
    /// (e.g. pattern, `let`, `var`, `const`,
    /// arguments) is dropped when we leave
    /// the function. Other declarations
    /// are unaffected.
    fn enter_function(&mut self) -> Env {
        Env::new(EnvLink::Function(self.clone()))
    }

    /// Enter a block. Every variable
    /// declaration local to the block
    /// (e.g. `let`) is dropped when we leave
    /// the block. Other declarations are
    /// unaffected.
    fn enter_block(&mut self) -> Self {
        Env::new(EnvLink::Block(self.clone()))
    }

    fn add_binding<'b>(&mut self, binding: &IdUsage<'b, Id>) {
        use self::IdUsageKind::*;
        match binding.kind {
            Let => {
                // Add to current block.
                self.0
                    .borrow_mut()
                    .let_declarations
                    .insert(binding.data.name.clone().into_string());
            },
            Var => {
                // Add to current block, then to parent.
                {
                    self.0
                        .borrow_mut()
                        .var_declarations
                        .insert(binding.data.name.clone().into_string());
                }
                use self::EnvLink::*;
                match self.0.borrow_mut().link {
                    Function(ref mut parent)
                    | Block(ref mut parent) =>
                        parent.add_binding(binding),
                    _ => {}
                }
            }
            // FIXME: Double-check, but it looks like other declarations are just ignored.
            _ => {}
        }
    }

    fn looks_like_direct_eval(&self) -> bool {
        let borrow = self.0.borrow();
        borrow.looks_like_direct_eval
        && !borrow.let_declarations.contains("eval")
        && !borrow.var_declarations.contains("eval")
    }


    /// Export the context for the block (both
    /// lexically declared names and var-declared names).
    fn to_block_naked(&self) -> Unlabelled<Kind> {
        let mut let_names : Vec<_> = self.0
            .borrow_mut()
            .let_declarations
            .iter()
            .cloned()
            .collect();
        let_names.sort();
        let mut var_names : Vec<_> = self.0
            .borrow_mut().
            var_declarations
            .iter()
            .cloned()
            .collect();
        var_names.sort();
        let let_list = Unlabelled::List(let_names
            .iter()
            .cloned()
            .map(Unlabelled::Atom)
            .map(Unlabelled::into_tree)
            .collect());
        let var_list = Unlabelled::List(let_names
            .iter()
            .cloned()
            .map(Unlabelled::Atom)
            .map(Unlabelled::into_tree)
            .collect());
        Unlabelled::Tuple(vec![
            let_list.into_tree(),
            var_list.into_tree()
        ])
    }

    /// Export the context for the function (both
    /// lexically declared names and var declared names).
    fn to_function_naked(&self) -> Unlabelled<Kind> {
        let keys = self.to_block_naked();
        // FIXME: I'm probably missing the case of `function() { function eval() {}; eval(...) }`
        // or `function() { let eval = () => {}; eval(...);}`
        Unlabelled::Tuple(vec![
            if self.looks_like_direct_eval() {
                Unlabelled::RawByte(0)
            } else {
                Unlabelled::RawByte(1)
            }.into_tree(),
            keys.into_tree()
        ])
    }

    /// Mark that we have been calling `eval(...)`.
    ///
    /// Note that this does NOT mean that we have a direct call to
    /// `eval`. We only find this out when we leave the block/function
    /// and we find out that `eval` has not been bound.
    fn add_eval(&mut self) {
        self.0.borrow_mut().looks_like_direct_eval = true
    }
}


impl<'a> serialize_tree::Context for Env {
}

fn bytes_for_number<K>(value: f64) -> Vec<SerializeTree<K>> {
    assert!(std::mem::size_of_val(&value) == std::mem::size_of::<[u8;8]>());
    // FIXME: This makes assumptions on endianness.
    let bytes = unsafe { std::mem::transmute::<_, [u8;8]>(value) };
    bytes.iter()
        .cloned()
        .map(Unlabelled::RawByte)
        .map(SerializeTree::Unlabelled)
        .collect()
}


impl<'a> ToLabelled<Kind, Env> for StmtListItem {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind>{
        match *self {
            StmtListItem::Decl(ref decl) => decl.to_labelled(env).into(),
            StmtListItem::Stmt(ref stmt) => stmt.to_labelled(env).into(),
        }
    }
}

impl<'a> ToLabelled<Kind, Env> for Decl {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind>{
        let Decl::Fun(ref fun) = *self;
        fun.to_naked(env)
            .label(Kind::FunDecl)
    }
}

impl<'a> ToLabelled<Kind, Env> for Stmt {
    fn to_labelled<'b>(&self, env: &'b mut Env) -> Labelled<Kind>{
        use easter::stmt::Stmt::*;
        match *self {
            Empty(_) =>
                Unlabelled::Tuple(vec![])
                    .label(Kind::Empty),
            Block(_, ref items) => {
                let mut env = env.enter_block();
                // 1. Compute the children (this may have side-effects
                // on `env`).
                let children = Labelled::list(items, &mut env).into_tree();
                // 2. Write annotated block.
                Unlabelled::Tuple(vec![
                    env.to_block_naked().into_tree(),
                    children
                ]).statement(Statement::Block)
                // 3. Auto-cleanup
            }
            Expr(_, ref expr, _) =>
                expr.to_labelled(env)
                    .into_tree()
                    .statement(Statement::Expression),
            Var(_, ref dtor, _) => {
                let list : Vec<_> = dtor.iter()
                    .map(IdUsage::var)
                    .collect();
                Labelled::list(&list, env)
                    .statement(Statement::VariableDeclaration)
            }
            If(_, ref condition, ref then_branch, ref else_branch) =>
                Unlabelled::Tuple(vec![
                    condition.to_labelled(env).into_tree(),
                    then_branch.to_labelled(env).into_tree(),
                    Labelled::option(else_branch, env).into_tree()
                ]).statement(Statement::IfThenElse),
            Label(_, ref id, ref statement) =>
                Unlabelled::Tuple(vec![
                    IdUsage::label(id).to_naked(env).into_tree(),
                    statement.to_labelled(env).into_tree()
                ]).statement(Statement::Label),
            Break(_, None, _) =>
                SerializeTree::empty()
                    .statement(Statement::Break),
            Break(_, Some(ref label), _) =>
                IdUsage::label(label)
                    .to_naked(env)
                    .into_tree()
                    .statement(Statement::Break),
            Cont(_, None, _) =>
                SerializeTree::empty()
                    .statement(Statement::Cont),
            Cont(_, Some(ref label), _) =>
                IdUsage::label(label)
                    .to_naked(env)
                    .into_tree()
                    .statement(Statement::Cont),
            With(_, ref expr, ref statement) =>
                Unlabelled::Tuple(vec![
                    expr.to_labelled(env).into_tree(),
                    statement.to_labelled(env).into_tree()
                ]).statement(Statement::With),
            Switch(_, ref expr, ref cases) =>
                // We use a pair (expr, list). We could have used a heterogenous
                // list instead, this wouldn't change the performance or size of
                // the file.
                Unlabelled::Tuple(vec![
                    expr.to_labelled(env).into_tree(),
                    Unlabelled::list(cases, env).into_tree(),
                ]).statement(Statement::Switch),
            Return(_, ref expr, _) =>
                Labelled::option(expr, env)
                    .into_tree()
                    .statement(Statement::Return),
            Throw(_, ref expr, _) =>
                expr.to_labelled(env)
                    .into_tree()
                    .statement(Statement::Throw),
            Try(_, ref list, ref catch, Some(ref finalizer)) =>
                Unlabelled::Tuple(vec![
                    Labelled::list(list, env).into_tree(),
                    Unlabelled::option(catch, env).into_tree(),
                    Labelled::list(finalizer, env).into_tree(),
                ]).statement(Statement::Try),
            Try(_, ref list, ref catch, None) =>
                Unlabelled::Tuple(vec![
                    Labelled::list(list, env).into_tree(),
                    Unlabelled::option(catch, env).into_tree(),
                    SerializeTree::empty()
                ]).statement(Statement::Try),
            While(_, ref expr, ref stmt) =>
                Unlabelled::Tuple(vec![
                    expr.to_labelled(env).into_tree(),
                    stmt.to_labelled(env).into_tree(),
                ]).statement(Statement::While),
            DoWhile(_, ref expr, ref stmt, _) =>
                Unlabelled::Tuple(vec![
                    expr.to_labelled(env).into_tree(),
                    stmt.to_labelled(env).into_tree(),
                ]).statement(Statement::DoWhile),
            For(_, ref head, ref cond, ref incr, ref stmt) =>
                Unlabelled::Tuple(vec![
                    Labelled::option(head, env).into_tree(),
                    Labelled::option(cond, env).into_tree(),
                    Labelled::option(incr, env).into_tree(),
                    stmt.to_labelled(env).into_tree()
                ]).statement(Statement::For),
            ForIn(_, ref head, ref expr, ref stmt) =>
                Unlabelled::Tuple(vec![
                    head.to_labelled(env).into_tree(),
                    expr.to_labelled(env).into_tree(),
                    stmt.to_labelled(env).into_tree()
                ]).statement(Statement::ForIn),
            ForOf(_, ref head, ref expr, ref stmt) =>
                Unlabelled::Tuple(vec![
                    head.to_labelled(env).into_tree(),
                    expr.to_labelled(env).into_tree(),
                    stmt.to_labelled(env).into_tree()
                ]).statement(Statement::ForOf),
            Debugger(_, _) => SerializeTree::empty().statement(Statement::Debugger)
        }
    }
}

/// A data structure used to represent uses of an id.
///
/// The `Context` uses this to determine whether the id represents
/// a declaration or a use, and whether it is lexically scoped.
struct IdUsage<'a, T> where T: 'a {
    kind: IdUsageKind,
    data: &'a T
}
#[derive(Clone)]
enum IdUsageKind {
    /// `var foo;`
    Var,
    /// `let foo;`
    Let,
    /// `foo:` or `break foo` or `continue foo`
    Label,
    /// `foo` (in an expression)
    Usage,
    /// `function(foo) {}`
    Argument,
    /// `function foo() {}`
    FunDecl
}
impl<'a, T> IdUsage<'a, T> {
    fn propagate<U>(&self, data: &'a U) -> IdUsage<'a, U> where U: 'a {
        IdUsage {
            data,
            kind: self.kind.clone()
        }
    }
    fn data(&self) -> &'a T {
        self.data
    }
    fn lex(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::Let,
            data
        }
    }
    fn var(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::Var,
            data
        }
    }
    fn label(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::Label,
            data
        }
    }
    fn usage(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::Usage,
            data
        }
    }
    fn arg(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::Argument,
            data
        }
    }
    fn fun_decl(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::FunDecl,
            data
        }
    }
}

impl<'a, 'b> ToLabelled<Kind, Env> for IdUsage<'b, Dtor>  {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind> {
// Note: Here, there's a divergence between Esprima and Esprit.
// Trying to follow Esprima. When parsing to Esprit, we'll use the label of `id`/`pat` to
// differentiate between Simple and Compound. When parsing to Esprima, it's actually the same node.

        match *self.data {
            Dtor::Simple(_, ref id, ref expr) => {
                Unlabelled::Tuple(vec![
                    self.propagate(id).to_naked(env).label(Kind::Name).into_tree(),
                    Labelled::option(expr, env).into_tree()
                ]).label(Kind::VarDecl(Variable::Var))
            }
            Dtor::Compound(_, ref pat, ref expr) => {
                Unlabelled::Tuple(vec![
                    self.propagate(pat).to_labelled(env).into_tree(),
                    expr.to_labelled(env).into_tree()
                ]).label(Kind::VarDecl(Variable::Var))
            }
        }
    }
}

impl<'a, 'b> ToLabelled<Kind, Env> for IdUsage<'b, CompoundPatt<Id>> {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind> {
        match *self.data() {
            CompoundPatt::Arr(_, ref patterns) => {
                let mut list = Vec::with_capacity(patterns.len());
                for pat in patterns {
                    let item = match *pat {
                        None => SerializeTree::empty(),
                        Some(ref pat) => self.propagate(pat)
                            .to_labelled(env)
                            .into_tree()
                    };
                    list.push(item)
                }
                Unlabelled::List(list)
                    .label(Kind::BindingPattern(BindingPattern::Array))
            }
            CompoundPatt::Obj(_, ref patterns) => {
                let patterns : Vec<_> = patterns.iter()
                    .map(|x| self.propagate(x))
                    .collect();
                Unlabelled::list(&patterns, env)
                    .label(Kind::BindingPattern(BindingPattern::Object))
            }
        }
    }
}

impl<'a> ToLabelled<Kind, Env> for CompoundPatt<AssignTarget> {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind> {
        match *self {
            CompoundPatt::Arr(_, ref patterns) => {
                let mut list = Vec::with_capacity(patterns.len());
                for pat in patterns {
                    list.push(SerializeTree::Labelled(Labelled::option(pat, env)))
                }
                Unlabelled::List(list)
                    .label(Kind::BindingPattern(BindingPattern::Array))
            }
            CompoundPatt::Obj(_, ref patterns) => {
                Unlabelled::list(patterns, env)
                    .label(Kind::BindingPattern(BindingPattern::Object))
            }
        }
    }
}

impl<'a, 'b> ToLabelled<Kind, Env> for IdUsage<'b, Patt<Id>> {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind> {
        match *self.data() {
            Patt::Simple(ref id) => {
                self.propagate(id).to_naked(env).label(Kind::Name)
            }
            Patt::Compound(ref compound) =>
                self.propagate(compound).to_labelled(env)
        }
    }
}

impl<'a> ToLabelled<Kind, Env> for Patt<AssignTarget> {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind> {
        match *self {
            Patt::Simple(ref target) => {
                target.to_labelled(env)
            }
            Patt::Compound(ref compound) =>
                compound.to_labelled(env)
        }
    }
}

impl<'a> ToLabelled<Kind, Env> for Expr {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind>{
        match *self {
            Expr::This(_) =>
                Unlabelled::Tuple(vec![])
                    .expression(Expression::This),
            Expr::Id(ref id) =>
                IdUsage::usage(id)
                    .to_naked(env)
                    .label(Kind::Name),
            Expr::Arr(_, ref values) => {
                let mut list = Vec::with_capacity(values.len());
                for value in values {
                    list.push(Labelled::option(value, env).into_tree())
                }
                Unlabelled::List(list)
                    .expression(Expression::Array)
            }
            Expr::Obj(_, ref properties) =>
                Unlabelled::list(properties, env)
                    .expression(Expression::Object),
            Expr::Fun(ref fun) =>
                fun.to_naked(env)
                    .expression(Expression::Function),
            Expr::Seq(_, ref expr) =>
                Labelled::list(expr, env)
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
                expr.to_labelled(env)
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
                    left.to_labelled(env).into_tree(),
                    right.to_labelled(env).into_tree()
                ]).expression(kind)
            }
            Expr::Logop(_, ref op, ref left, ref right) => {
                let kind = match op.tag {
                    LogopTag::Or => Expression::LogOr,
                    LogopTag::And => Expression::LogAnd,
                };
                Unlabelled::Tuple(vec![
                    left.to_labelled(env).into_tree(),
                    right.to_labelled(env).into_tree()
                ]).expression(kind)
            }
            Expr::PreInc(_, ref expr) => {
                expr.to_labelled(env)
                    .into_tree()
                    .expression(Expression::PreInc)
            }
            Expr::PostInc(_, ref expr) => {
                expr.to_labelled(env)
                    .into_tree()
                    .expression(Expression::PostInc)
            }
            Expr::PreDec(_, ref expr) => {
                expr.to_labelled(env)
                    .into_tree()
                    .expression(Expression::PreDec)
            }
            Expr::PostDec(_, ref expr) => {
                expr.to_labelled(env)
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
                    pat.to_labelled(env).into_tree(),
                    expr.to_labelled(env).into_tree(),
                ]).expression(kind)
            }
            Expr::Cond(_, ref cond, ref then_branch, ref else_branch) => {
                Unlabelled::Tuple(vec![
                    cond.to_labelled(env).into_tree(),
                    then_branch.to_labelled(env).into_tree(),
                    else_branch.to_labelled(env).into_tree(),
                ]).expression(Expression::Conditional)
            }
            Expr::Call(_, ref callee, ref arguments) => {
                // Detect direct calls to `eval`.
                use easter::expr::Expr::*;
                if let Id(ref id) = *callee.as_ref() {
                    use joker::word::*;
                    if let Name::Atom(Atom::Eval) = id.name {
                        env.add_eval();
                    }
                };
                Unlabelled::Tuple(vec![
                    callee.to_labelled(env).into_tree(),
                    Labelled::list(arguments, env).into_tree()
                ]).expression(Expression::Call)
            }
            Expr::New(_, ref callee, Some(ref arguments)) => {
                Unlabelled::Tuple(vec![
                    callee.to_labelled(env).into_tree(),
                    Labelled::list(arguments, env).into_tree()
                ]).expression(Expression::New)
            }
            Expr::New(_, ref callee, None) => {
                Unlabelled::Tuple(vec![
                    callee.to_labelled(env).into_tree(),
                    SerializeTree::empty()
                ]).expression(Expression::New)
            },
            // Note: We collapse `Dot` and `Brack` in `Member`.
            Expr::Dot(_, ref object, ref property) => {
                Unlabelled::Tuple(vec![
                    object.to_labelled(env).into_tree(),
                    Unlabelled::Atom(property.value.clone()).label(Kind::Name).into_tree(),
                ]).expression(Expression::Member)
            }
            Expr::Brack(_, ref object, ref property) => {
                Unlabelled::Tuple(vec![
                    object.to_labelled(env).into_tree(),
                    property.to_labelled(env).into_tree(),
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

impl<'a> ToUnlabelled<Kind, Env> for DotKey {
    fn to_naked(&self, env: &mut Env) -> Unlabelled<Kind> {
        Unlabelled::Atom(self.value.clone())
    }
}

impl<'a, 'b> ToUnlabelled<Kind, Env> for IdUsage<'b, Id> {
    fn to_naked(&self, env: &mut Env) -> Unlabelled<Kind> {
        env.add_binding(self);
        Unlabelled::Atom(self.data().name.clone().into_string())
    }
}

impl<'a> ToLabelled<Kind, Env> for AssignTarget {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind> {
        match *self {
            AssignTarget::Id(ref id) =>
                IdUsage::usage(id).to_naked(env)
                    .label(Kind::Name),
            // Note: We collapse `Dot` and `Brack` in `Member`.
            AssignTarget::Dot(_, ref object, ref property) => {
                Unlabelled::Tuple(vec![
                    object.to_labelled(env).into_tree(),
                    property.to_naked(env).into_tree()
                ]).expression(Expression::Member)
            }
            AssignTarget::Brack(_, ref object, ref property) => {
                Unlabelled::Tuple(vec![
                    object.to_labelled(env).into_tree(),
                    property.to_labelled(env).into_tree()
                ]).expression(Expression::Member)
            }
        }
    }
}

impl<'a> ToUnlabelled<Kind, Env> for Case {
    fn to_naked(&self, env: &mut Env) -> Unlabelled<Kind> {
        Unlabelled::Tuple(vec![
            Labelled::option(&self.test, env).into_tree(),
            Labelled::list(&self.body, env).into_tree()
        ])
    }
}

impl<'a> ToUnlabelled<Kind, Env> for Catch {
    fn to_naked(&self, env: &mut Env) -> Unlabelled<Kind> {
        Unlabelled::Tuple(vec![
            // If I read correctly the specifications (13.15.5,
            // 13.15.6), the variables of `self.param` are entirely
            // ignored.
            IdUsage::usage(&self.param).to_labelled(env).into_tree(),
            Labelled::list(&self.body, env).into_tree()
        ])
    }
}

impl<'a> ToLabelled<Kind, Env> for ForHead {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind>{
        match *self {
            ForHead::Var(_, ref dtor) => {
                // All the variables are `var`, so wrap everything
                // in a `Var`.
                let list : Vec<_> = dtor.iter().map(IdUsage::var).collect();
                Labelled::list(&list, env)
                    .label(Kind::VarDecl(Variable::Var))
            }
            ForHead::Let(_, ref dtor) => {
                // All the variables are `let`, so wrap everything
                // in a `Let`.
                let list : Vec<_> = dtor.iter().map(IdUsage::lex).collect();
                Labelled::list(&list, env)
                    .label(Kind::VarDecl(Variable::Let))
            }
            ForHead::Expr(_, ref expr) =>
                expr.to_labelled(env)
        }
    }
}

impl<'a> ToLabelled<Kind, Env> for ForInHead {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind>{
        match *self {
            ForInHead::VarInit(_, ref id, ref expr) => {
                Unlabelled::Tuple(vec![
                    IdUsage::var(id).to_naked(env).into_tree(),
                    expr.to_labelled(env).into_tree()
                ]).label(Kind::VarDecl(Variable::VarInit))
            }
            ForInHead::Var(_, ref pat) =>
                IdUsage::var(pat).to_labelled(env)
                    .into_tree()
                    .label(Kind::VarDecl(Variable::Var)),
            ForInHead::Let(_, ref pat) =>
                IdUsage::lex(pat).to_labelled(env)
                    .into_tree()
                    .label(Kind::VarDecl(Variable::Let)),
            ForInHead::Expr(ref expr) =>
                expr.to_labelled(env)
        }
    }
}


impl<'a> ToLabelled<Kind, Env> for ForOfHead {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind>{
        match *self {
            ForOfHead::Var(_, ref pat) =>
                IdUsage::var(pat).to_labelled(env)
                    .into_tree()
                    .label(Kind::VarDecl(Variable::Var)),
            ForOfHead::Let(_, ref pat) =>
                IdUsage::lex(pat).to_labelled(env)
                    .into_tree()
                    .label(Kind::VarDecl(Variable::Let)),
            ForOfHead::Expr(ref expr) =>
                expr.to_labelled(env)
        }
    }
}

impl<'a> ToLabelled<Kind, Env> for PropKey {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind>{
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

impl<'a> ToUnlabelled<Kind, Env> for Fun {
    fn to_naked<'b>(&self, env: &'b mut Env) -> Unlabelled<Kind> {
        let mut env = env.enter_function();
        // 1. Evaluate the children.
        let id = match self.id {
            None => SerializeTree::empty(),
            Some(ref id) => IdUsage::fun_decl(id)
                .to_naked(&mut env)
                .into_tree()
        };
        let mut list = Vec::with_capacity(self.params.list.len());
        for param in &self.params.list {
            list.push(IdUsage::arg(param)
                    .to_labelled(&mut env)
                    .into_tree())
        }
        let params = Unlabelled::List(list).into_tree();
        let body = Labelled::list(&self.body, &mut env).into_tree();
        // 2. Deduce annotation (must be done AFTER evaluating
        // the children, as there may be side-effects on `env`).
        Unlabelled::Tuple(vec![
            env.to_function_naked().into_tree(),
            id,
            params,
            body
        ])
        // 3. Auto-cleanup `env`.
    }
}

impl<'a> ToUnlabelled<Kind, Env> for PropPatt<AssignTarget> {
    fn to_naked(&self, env: &mut Env) -> Unlabelled<Kind> {
        Unlabelled::Tuple(vec![
            self.key.to_labelled(env).into_tree(),
            self.patt.to_labelled(env).into_tree()
        ])
    }
}

impl<'a, 'b> ToUnlabelled<Kind, Env> for IdUsage<'b, PropPatt<Id>> {
    fn to_naked(&self, env: &mut Env) -> Unlabelled<Kind> {
        Unlabelled::Tuple(vec![
            self.data().key.to_labelled(env).into_tree(),
            self.propagate(&self.data().patt).to_labelled(env).into_tree()
        ])
    }
}

impl<'a> ToUnlabelled<Kind, Env> for Prop {
    fn to_naked(&self, env: &mut Env) -> Unlabelled<Kind> {
        Unlabelled::Tuple(vec![
            self.key.to_labelled(env).into_tree(),
            self.val.to_labelled(env).into_tree()
        ])
    }
}

impl<'a> ToLabelled<Kind, Env> for PropVal {
    fn to_labelled(&self, env: &mut Env) -> Labelled<Kind>{
        match *self {
            PropVal::Get(_, ref body) =>
                Labelled::list(body, env)
                    .label(Kind::VarDecl(Variable::Get)),
            PropVal::Set(_, ref pat, ref body) =>
                Unlabelled::Tuple(vec![
                    IdUsage::arg(pat).to_labelled(env)
                        .into_tree(),
                    Labelled::list(body, env).into_tree(),
                ]).label(Kind::VarDecl(Variable::Set)),
            PropVal::Init(ref expr) =>
                expr.to_labelled(env)
        }
    }
}

impl<'a> ToUnlabelled<Kind, Env> for Script {
    fn to_naked(&self, env: &mut Env) -> Unlabelled<Kind> {
        Labelled::list(&self.body, env)
    }
}

impl Unlabelled<Kind> {
    fn statement(self, kind: Statement) -> Labelled<Kind> {
        self.label(Kind::Statement(kind))
    }
    fn expression(self, kind: Expression) -> Labelled<Kind> {
        self.label(Kind::Expression(kind))
    }

    // Note: We explicitly use `list` rather than deriving `ToUnlabelled`
    // for `Vec<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn list<'a, T>(items: &[T], env: &mut Env) -> Self where T: ToUnlabelled<Kind, Env> {
        let mut list = Vec::with_capacity(items.len());
        for item in items {
            list.push(item.to_naked(env).into_tree());
        }
        Unlabelled::List(list)
    }

    // Note: We explicitly use `option` rather than deriving `ToUnlabelled`
    // for `Option<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn option<'a, T>(item: &Option<T>, env: &mut Env) -> Self where T: ToUnlabelled<Kind, Env> {
        match *item {
            None => Unlabelled::Tuple(vec![]),
            Some(ref some) => some.to_naked(env)
        }
    }
}

impl Labelled<Kind> {
    // Note: We explicitly use `list` rather than deriving `ToSerializeTree`
    // for `Vec<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn list<'a, T>(items: &[T], env: &mut Env) -> Unlabelled<Kind> where T: ToLabelled<Kind, Env> {
        let mut list = Vec::with_capacity(items.len());
        for item in items {
            list.push(item.to_labelled(env).into_tree());
        }
        Unlabelled::List(list)
    }
    // Note: We explicitly use `option` rather than deriving `ToSerializeTree`
    // for `Option<T>` to avoid ambiguous magic code and to make porting to
    // other languages simpler.
    fn option<'a, T>(item: &Option<T>, env: &mut Env) -> Labelled<Kind> where T: ToLabelled<Kind, Env> {
        match *item {
            None => Unlabelled::Tuple(vec![]).label(Kind::default()),
            Some(ref some) => some.to_labelled(env)
        }
    }
}

impl SerializeTree<Kind> {
    fn statement(self, kind: Statement) -> Labelled<Kind> {
        self.label(Kind::Statement(kind))
    }
    fn expression(self, kind: Expression) -> Labelled<Kind> {
        self.label(Kind::Expression(kind))
    }
}
