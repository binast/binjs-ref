/// Labels to differentiate AST nodes.
// FIXME: There is a size bonus if we can make sure that all the commonly used instances
// of `Kind` are fewer than 128, as we can fit this in a single VarNum byte. If this is not
// the case, consider using several pools (e.g. one for Statement, one for VarDecl, ...).
// This is more accident-prone.
use atoms::{ FromBytes, ToBytes };

use std;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Kind {
    Empty,
    Statement(Statement),
    VarDecl(Variable),
    BindingPattern(BindingPattern),
    Expression(Expression),
    FunDecl,
    Name,
}
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum BindingPattern {
    Array,
    Object,
}
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Variable {
    Var,
    Let,
    // Const,
    VarInit,
    Get,
    Set,
}
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
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
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
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
impl FromBytes for Kind {
    fn from_bytes(bytes: &[u8]) -> Result<Self, std::io::Error> {
        Self::from_str(&String::from_bytes(bytes)?)
    }
}
impl Kind {
    fn from_str(string: &str) -> Result<Self, std::io::Error> {
        use self::Kind::*;
        match string {
            "" => return Ok(Empty),
            "FunctionDeclaration" => return Ok(FunDecl),
            "Identifier" => return Ok(Name),
            _ => {}
        }
        self::Statement::from_str(string).map(Statement)
            .or_else(|_| Variable::from_str(string).map(VarDecl))
            .or_else(|_| self::BindingPattern::from_str(string).map(BindingPattern))
            .or_else(|_| self::Expression::from_str(string).map(Expression))
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
impl Statement {
    fn from_str(str: &str) -> Result<Self, std::io::Error> {
        use self::Statement::*;
        match str {
            "BlockStatement" => Ok(Block),
            "ExpressionStatement" => Ok(Expression),
            "VariableDeclaration" => Ok(VariableDeclaration),
            "IfStatement" => Ok(IfThenElse),
            "LabeledStatement" => Ok(Label),
            "BreakStatement" => Ok(Break),
            "ContinueStatement" => Ok(Cont),
            "WithStatement" => Ok(With),
            "SwitchStatement" => Ok(Switch),
            "ReturnStatement" => Ok(Return),
            "ThrowStatement" => Ok(Throw),
            "TryStatement" => Ok(Try),
            "WhileStatement" => Ok(While),
            "DoWhileStatement" => Ok(DoWhile),
            "ForStatement" => Ok(For),
            "ForInStatement" => Ok(ForIn),
            "ForOfStatement" => Ok(ForOf),
            "DebuggerStatement" => Ok(Debugger),
            _ => Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Not a Statement"))
        }
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
impl Variable {
    fn from_str(str: &str) -> Result<Self, std::io::Error> {
        use self::Variable::*;
        match str {
            "var" => Ok(Var),
            "let" => Ok(Let),
            // "const" => Ok(Const),
            "get" => Ok(Get),
            "set" => Ok(Set),
            "init" => Ok(VarInit),
            _ => Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Not a Variable"))
        }
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
impl BindingPattern {
    fn from_str(str: &str) -> Result<Self, std::io::Error> {
        use self::BindingPattern::*;
        match str {
            "ArrayPattern" => Ok(Array),
            "ObjectPattern" => Ok(Object),
            _ => Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Not a BindingPattern"))
        }
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
            UnaryBitNot => "Unary~",
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
impl Expression {
    fn from_str(str: &str) -> Result<Self, std::io::Error> {
        use self::Expression::*;
        match str {
            "ThisExpression" => Ok(This),
            "ArrayExpression" => Ok(Array),
            "ObjectExpression" => Ok(Object),
            "FunctionExpression" => Ok(Function),
            "SequenceExpression" => Ok(Sequence),
            "Unary-" => Ok(UnaryMinus),
            "Unary+" => Ok(UnaryPlus),
            "~" => Ok(UnaryNot),
            "Unary~" => Ok(UnaryBitNot),
            "typeof" => Ok(UnaryTypeof),
            "void" => Ok(UnaryVoid),
            "delete" => Ok(UnaryDelete),
            "==" => Ok(Eq),
            "!=" => Ok(NEq),
            "===" => Ok(StrictEq),
            "!==" => Ok(StrictNEq),
            "<" => Ok(Lt),
            "<=" => Ok(LEq),
            ">" => Ok(Gt),
            ">=" => Ok(GEq),
            "<<" => Ok(LShift),
            ">>" => Ok(RShift),
            ">>>" => Ok(URShift),
            "+" => Ok(Plus),
            "-" => Ok(Minus),
            "*" => Ok(Times),
            "/" => Ok(Div),
            "%" => Ok(Mod),
            "|" => Ok(BitOr),
            "^" => Ok(BitXor),
            "&" => Ok(BitAnd),
            "in" => Ok(In),
            "instanceof" => Ok(Instanceof),
            "||" => Ok(LogOr),
            "&&" => Ok(LogAnd),
            "++_" => Ok(PreInc),
            "_++" => Ok(PostInc),
            "--_" => Ok(PreDec),
            "_--" => Ok(PostDec),
            "=" => Ok(Assign),
            "+=" => Ok(PlusEq),
            "-=" => Ok(MinusEq),
            "*=" => Ok(TimesEq),
            "/=" => Ok(DivEq),
            "%=" => Ok(ModEq),
            "<<=" => Ok(LShiftEq),
            ">>=" => Ok(RShiftEq),
            ">>>=" => Ok(URShiftEq),
            "|=" => Ok(BitOrEq),
            "^=" => Ok(BitXorEq),
            "&=" => Ok(BitAndEq),
            "ConditionalExpression" => Ok(Conditional),
            "CallExpression" => Ok(Call),
            "NewExpression" => Ok(New),
            "MemberExpression" => Ok(Member),
            "NewTargetExpression" => Ok(NewTarget), // FIXME: No clue what this is.
            "true" => Ok(True),
            "false" => Ok(False),
            "null" => Ok(Null),
            "NumberLiteral" => Ok(Number),
            "RegexpLiteral" => Ok(RegExp),
            "StringLiteral" => Ok(String),
            _ => Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "Not a Expression"))
        }
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
