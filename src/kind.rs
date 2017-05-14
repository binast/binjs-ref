/// Labels to differentiate AST nodes.
// FIXME: There is a size bonus if we can make sure that all the commonly used instances
// of `Kind` are fewer than 128, as we can fit this in a single VarNum byte. If this is not
// the case, consider using several pools (e.g. one for Statement, one for VarDecl, ...).
// This is more accident-prone.
use atoms::ToBytes;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Kind {
    Empty,
    Statement(Statement),
    VarDecl(Variable),
    BindingPattern(BindingPattern),
    Expression(Expression),
    FunDecl,
    Name,
}
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum BindingPattern {
    Array,
    Object,
}
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Variable {
    Var,
    Let,
    // Const,
    VarInit,
    Get,
    Set,
}
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
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
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
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

impl Default for Kind {
    fn default() -> Kind {
        Kind::Empty
    }
}

impl ToBytes for str {
    fn to_bytes(&self) -> Vec<u8> {
        self.bytes().collect()
    }
}

impl ToBytes for Kind {
    fn to_bytes(&self) -> Vec<u8> {
        use self::Kind::*;
        let str = match *self {
            Empty => "",
            FunDecl => "FunctionDeclaration",
            Name => "Identifier",
            Statement(ref x) => return x.to_bytes(),
            VarDecl(ref x) => return x.to_bytes(),
            BindingPattern(ref x) => return x.to_bytes(),
            Expression(ref x) => return x.to_bytes(),
        };
        str.to_bytes()
    }
}

impl ToBytes for Statement {
    fn to_bytes(&self) -> Vec<u8> {
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
        str.to_bytes()
    }
}

impl ToBytes for Variable {
    fn to_bytes(&self) -> Vec<u8> {
        use self::Variable::*;
        let str = match *self {
            Var => "var",
            Let => "let",
            // Const => "const",
            Get => "get",
            Set => "set",
            VarInit => "init",
        };
        str.to_bytes()
    }
}

impl ToBytes for BindingPattern {
    fn to_bytes(&self) -> Vec<u8> {
        use self::BindingPattern::*;
        let str = match *self {
            Array => "ArrayPattern",
            Object => "ObjectPattern",
        };
        str.to_bytes()
    }
}

impl ToBytes for Expression {
    fn to_bytes(&self) -> Vec<u8> {
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
        str.to_bytes()
    }
}

/// A data structure used to represent uses of an id.
///
/// The `Context` uses this to determine whether the id represents
/// a declaration or a use, and whether it is lexically scoped.
pub struct IdUsage<'a, T> where T: 'a {
    kind: IdUsageKind,
    data: &'a T
}
#[derive(Clone)]
pub enum IdUsageKind {
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
    pub fn propagate<U>(&self, data: &'a U) -> IdUsage<'a, U> where U: 'a {
        IdUsage {
            data,
            kind: self.kind.clone()
        }
    }
    pub fn data(&self) -> &'a T {
        self.data
    }
    pub fn lex(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::Let,
            data
        }
    }
    pub fn var(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::Var,
            data
        }
    }
    pub fn label(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::Label,
            data
        }
    }
    pub fn usage(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::Usage,
            data
        }
    }
    pub fn arg(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::Argument,
            data
        }
    }
    pub fn fun_decl(data: &'a T) -> Self {
        IdUsage {
            kind: IdUsageKind::FunDecl,
            data
        }
    }
    pub fn kind(&self) -> IdUsageKind {
        self.kind.clone()
    }
}
