// # Documentation of the JavaScript AST, level ES5


 // # Interfaces.
 //
 // The order of fields matters: if an interface `Foo` defines field `a` before field `b`,
 // any implementation of the format MUST encode the contents of `a` before the contents of `b`
 // The order of fields between subinterfaces and superinterfaces is not specified, as long
 // as the above is respected at each level

```typescript
 interface ArrayExpression :< Expression {
    ?elements: Expression | ElisionExpression; ; // Defaults to `[]`
 }

 interface AssignmentExpression :< Expression {
    operator: AssignmentOperator; 
    left: Pattern | Expression; 
    right: Expression; 
 }

 interface BINJS:Scope  {
    ?BINJS:VarDeclaredNames: string; ; // Defaults to `[]`
    ?BINJS:LetDeclaredNames: string; ; // Defaults to `[]`
    ?BINJS:ConstDeclaredNames: string; ; // Defaults to `[]`
    ?BINJS:CapturedNames: string; ; // Defaults to `[]`
    ?BINJS:HasDirectEval: bool; // Defaults to `false`
 }

 interface BinaryExpression :< Expression {
    operator: BinaryOperator; 
    left: Expression; 
    right: Expression; 
 }

 interface BlockStatement :< Statement {
    ?body: Statement | FunctionDeclaration; ; // Defaults to `[]`
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
    ?directives: Directive; ; // Defaults to `[]`
 }

 interface BooleanLiteral :< Literal {
    ?value: bool; // Defaults to `false`
 }

 interface BracketExpression :< Expression, Pattern {
    object: Expression; 
    property: Expression; 
 }

 interface BracketObjectGetter :< BracketObjectMember, Function {
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern; ; // Defaults to `[]`
    body: BlockStatement; 
    key: Expression; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface BracketObjectMember  {
    key: Expression; 
 }

 interface BracketObjectMethod :< BracketObjectMember, Function {
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern; ; // Defaults to `[]`
    body: BlockStatement; 
    key: Expression; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface BracketObjectProperty :< BracketObjectMember {
    key: Expression; 
    value: Expression; 
 }

 interface BracketObjectSetter :< ObjectMember, Function {
    key: Identifier | Literal; 
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern; ; // Defaults to `[]`
    body: BlockStatement; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface BreakStatement :< Statement {
    ?label: Identifier; // Defaults to `null`
 }

 interface CallExpression :< Expression {
    callee: Expression; 
    ?arguments: Expression; ; // Defaults to `[]`
 }

 interface CatchClause  {
    param: Pattern; 
    body: BlockStatement; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface ConditionalExpression :< Expression {
    test: Expression; 
    alternate: Expression; 
    consequent: Expression; 
 }

 interface ContinueStatement :< Statement {
    ?label: Identifier; // Defaults to `null`
 }

 interface DebuggerStatement :< Statement {
 }

 interface Declaration :< Statement {
 }

 interface Directive  {
    value: DirectiveLiteral; 
 }

 interface DirectiveLiteral  {
    value: string; 
 }

 interface DoWhileStatement :< Statement {
    body: Statement; 
    test: Expression; 
 }

 interface DotExpression :< Expression, Pattern {
    object: Expression; 
    property: Identifier; 
 }

 interface ElisionExpression  {
 }

 interface EmptyStatement :< Statement {
 }

 interface Expression  {
 }

 interface ExpressionStatement :< Statement {
    expression: Expression; 
 }

 interface ForInStatement :< Statement {
    left: VariableDeclaration | Pattern; 
    right: Expression; 
    body: Statement; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface ForStatement :< Statement {
    ?init: VariableDeclaration | Expression; // Defaults to `null`
    ?test: Expression; // Defaults to `null`
    ?update: Expression; // Defaults to `null`
    body: Statement; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface Function  {
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern; ; // Defaults to `[]`
    body: BlockStatement; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface FunctionDeclaration :< Function {
    id: Identifier; 
    ?params: Pattern; ; // Defaults to `[]`
    body: BlockStatement; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface FunctionExpression :< Expression, Function {
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern; ; // Defaults to `[]`
    body: BlockStatement; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface Identifier :< Expression, Pattern {
    name: string; 
 }

 interface IfStatement :< Statement {
    test: Expression; 
    consequent: Statement; 
    ?alternate: Statement; // Defaults to `null`
 }

 interface LabeledStatement :< Statement {
    label: Identifier; 
    body: Statement; 
 }

 interface Literal :< Expression {
 }

 interface LogicalExpression :< Expression {
    operator: LogicalOperator; 
    left: Expression; 
    right: Expression; 
 }

 interface NewExpression :< Expression {
    callee: Expression; 
    ?arguments: Expression; ; // Defaults to `[]`
 }

 interface NullLiteral :< Literal {
 }

 interface NumericLiteral :< Literal {
    ?value: number; // Defaults to `0`
 }

 interface ObjectExpression :< Expression {
    ?properties: ObjectProperty | ObjectMethod | ObjectGetter | ObjectSetter | BracketObjectProperty | BracketObjectMethod | BracketObjectGetter | BracketObjectSetter; ; // Defaults to `[]`
 }

 interface ObjectGetter :< ObjectMember, Function {
    key: Identifier | Literal; 
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern; ; // Defaults to `[]`
    body: BlockStatement; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface ObjectMember  {
    key: Identifier | Literal; 
 }

 interface ObjectMethod :< ObjectMember, Function {
    key: Identifier | Literal; 
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern; ; // Defaults to `[]`
    body: BlockStatement; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface ObjectProperty :< ObjectMember {
    value: Expression; 
    key: Identifier | Literal; 
 }

 interface ObjectSetter :< ObjectMember, Function {
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern; ; // Defaults to `[]`
    body: BlockStatement; 
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
    key: Identifier | Literal; 
 }

 interface Pattern  {
 }

 // Root of the AST.
 interface Program  {
    ?body: Statement | FunctionDeclaration; ; // Defaults to `[]`
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
    ?directives: Directive; ; // Defaults to `[]`
 }

 interface RegExpLiteral :< Literal {
    pattern: string; 
    flags: string; 
 }

 interface ReturnStatement :< Statement {
    ?argument: Expression; // Defaults to `null`
 }

 interface SequenceExpression :< Expression {
    ?expressions: Expression; ; // Defaults to `[]`
 }

 interface Statement  {
 }

 interface StringLiteral :< Literal {
    value: string; 
 }

 interface SwitchCase  {
    ?test: Expression; // Defaults to `null`
    ?consequent: Statement | FunctionDeclaration; ; // Defaults to `[]`
 }

 interface SwitchStatement :< Statement {
    discriminant: Expression; 
    ?cases: SwitchCase; ; // Defaults to `[]`
 }

 interface ThisExpression :< Expression {
 }

 interface ThrowStatement :< Statement {
    argument: Expression; 
 }

 interface TryStatement :< Statement {
    block: BlockStatement; 
    ?handler: CatchClause; // Defaults to `null`
    ?finalizer: BlockStatement; // Defaults to `null`
 }

 interface UnaryExpression :< Expression {
    operator: UnaryOperator; 
    ?prefix: bool; // Defaults to `false`
    argument: Expression; 
 }

 interface UpdateExpression :< Expression {
    operator: UpdateOperator; 
    argument: Expression; 
    ?prefix: bool; // Defaults to `false`
 }

 interface VariableDeclaration :< Declaration {
    declarations: VariableDeclarator;  /* Non-empty */; 
    kind: VariableKind; 
 }

 interface VariableDeclarator  {
    id: Pattern; 
    ?init: Expression; // Defaults to `null`
 }

 interface WhileStatement :< Statement {
    test: Expression; 
    body: Statement; 
 }

 interface WithStatement :< Statement {
    object: Expression; 
    body: Statement; 
 }



 // # Enums.
 //
 // The order of enum values does NOT matter.
enum AssignmentOperator {
    "=",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",
    "<<=",
    ">>=",
    ">>>=",
    "|=",
    "^=",
    "&=",
}

enum BinaryOperator {
    "==",
    "!=",
    "===",
    "!==",
    "<",
    "<=",
    ">",
    ">=",
    "<<",
    ">>",
    ">>>",
    "+",
    "-",
    "*",
    "/",
    "%",
    "|",
    "^",
    "&",
    "in",
    "instanceof",
}

enum LogicalOperator {
    "||",
    "&&",
}

enum UnaryOperator {
    "-",
    "+",
    "!",
    "~",
    "typeof",
    "void",
    "delete",
}

enum UpdateOperator {
    "++",
    "--",
}

enum VariableKind {
    "let",
    "var",
    "const",
}


david binjs 19:44$ cargo build --example ast-doc && ./target/debug/examples/ast-doc cargo install cargo-external-doc^C
david binjs 19:49$ cargo install cargo-external-doc
    Updating registry `https://github.com/rust-lang/crates.io-index`
 Downloading cargo-external-doc v0.1.0
  Installing cargo-external-doc v0.1.0
 Downloading handlebars v0.22.0
 Downloading tempfile v2.2.0
 Downloading quick-error v1.2.1
 Downloading pest v0.3.3
   Compiling pest v0.3.3
   Compiling libc v0.2.32
   Compiling dtoa v0.2.2
   Compiling itoa v0.1.1
   Compiling serde v0.8.23
   Compiling log v0.3.8
   Compiling quick-error v1.2.1
   Compiling rustc-serialize v0.3.24
   Compiling num-traits v0.1.40
   Compiling rand v0.3.17
   Compiling tempfile v2.2.0
   Compiling serde_json v0.8.6
   Compiling handlebars v0.22.0
   Compiling cargo-external-doc v0.1.0
    Finished release [optimized] target(s) in 38.57 secs
  Installing /Users/david/.cargo/bin/cargo-external-doc
david binjs 19:50$ cargo build --example ast-doc && ./target/debug/examples/ast-doc 
   Compiling binjs v0.2.0 (file:///Users/david/Documents/Code/binjs)
    Finished dev [unoptimized + debuginfo] target(s) in 17.62 secs
 // # Documentation of the JavaScript AST, level ES5


 // # Interfaces.
 //
 // The order of fields matters: if an interface `Foo` defines field `a` before field `b`,
 // any implementation of the format MUST encode the contents of `a` before the contents of `b`
 // The order of fields between subinterfaces and superinterfaces is not specified, as long
 // as the above is respected at each level
 interface ArrayExpression :< Expression {
    ?elements: Expression | ElisionExpression;; // Defaults to `[]`
 }

 interface AssignmentExpression :< Expression {
    operator: AssignmentOperator;
    left: Pattern | Expression;
    right: Expression;
 }

 interface BINJS:Scope  {
    ?BINJS:VarDeclaredNames: string;; // Defaults to `[]`
    ?BINJS:LetDeclaredNames: string;; // Defaults to `[]`
    ?BINJS:ConstDeclaredNames: string;; // Defaults to `[]`
    ?BINJS:CapturedNames: string;; // Defaults to `[]`
    ?BINJS:HasDirectEval: bool; // Defaults to `false`
 }

 interface BinaryExpression :< Expression {
    operator: BinaryOperator;
    left: Expression;
    right: Expression;
 }

 interface BlockStatement :< Statement {
    ?body: Statement | FunctionDeclaration;; // Defaults to `[]`
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
    ?directives: Directive;; // Defaults to `[]`
 }

 interface BooleanLiteral :< Literal {
    ?value: bool; // Defaults to `false`
 }

 interface BracketExpression :< Expression, Pattern {
    object: Expression;
    property: Expression;
 }

 interface BracketObjectGetter :< BracketObjectMember, Function {
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern;; // Defaults to `[]`
    body: BlockStatement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
    key: Expression;
 }

 interface BracketObjectMember  {
    key: Expression;
 }

 interface BracketObjectMethod :< BracketObjectMember, Function {
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern;; // Defaults to `[]`
    key: Expression;
    body: BlockStatement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface BracketObjectProperty :< BracketObjectMember {
    value: Expression;
    key: Expression;
 }

 interface BracketObjectSetter :< ObjectMember, Function {
    ?id: Identifier; // Defaults to `null`
    key: Identifier | Literal;
    ?params: Pattern;; // Defaults to `[]`
    body: BlockStatement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface BreakStatement :< Statement {
    ?label: Identifier; // Defaults to `null`
 }

 interface CallExpression :< Expression {
    callee: Expression;
    ?arguments: Expression;; // Defaults to `[]`
 }

 interface CatchClause  {
    param: Pattern;
    body: BlockStatement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface ConditionalExpression :< Expression {
    test: Expression;
    alternate: Expression;
    consequent: Expression;
 }

 interface ContinueStatement :< Statement {
    ?label: Identifier; // Defaults to `null`
 }

 interface DebuggerStatement :< Statement {
 }

 interface Declaration :< Statement {
 }

 interface Directive  {
    value: DirectiveLiteral;
 }

 interface DirectiveLiteral  {
    value: string;
 }

 interface DoWhileStatement :< Statement {
    body: Statement;
    test: Expression;
 }

 interface DotExpression :< Expression, Pattern {
    object: Expression;
    property: Identifier;
 }

 interface ElisionExpression  {
 }

 interface EmptyStatement :< Statement {
 }

 interface Expression  {
 }

 interface ExpressionStatement :< Statement {
    expression: Expression;
 }

 interface ForInStatement :< Statement {
    left: VariableDeclaration | Pattern;
    right: Expression;
    body: Statement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface ForStatement :< Statement {
    ?init: VariableDeclaration | Expression; // Defaults to `null`
    ?test: Expression; // Defaults to `null`
    ?update: Expression; // Defaults to `null`
    body: Statement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface Function  {
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern;; // Defaults to `[]`
    body: BlockStatement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface FunctionDeclaration :< Function {
    id: Identifier;
    ?params: Pattern;; // Defaults to `[]`
    body: BlockStatement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface FunctionExpression :< Expression, Function {
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern;; // Defaults to `[]`
    body: BlockStatement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface Identifier :< Expression, Pattern {
    name: string;
 }

 interface IfStatement :< Statement {
    test: Expression;
    consequent: Statement;
    ?alternate: Statement; // Defaults to `null`
 }

 interface LabeledStatement :< Statement {
    label: Identifier;
    body: Statement;
 }

 interface Literal :< Expression {
 }

 interface LogicalExpression :< Expression {
    operator: LogicalOperator;
    left: Expression;
    right: Expression;
 }

 interface NewExpression :< Expression {
    callee: Expression;
    ?arguments: Expression;; // Defaults to `[]`
 }

 interface NullLiteral :< Literal {
 }

 interface NumericLiteral :< Literal {
    ?value: number; // Defaults to `0`
 }

 interface ObjectExpression :< Expression {
    ?properties: ObjectProperty | ObjectMethod | ObjectGetter | ObjectSetter | BracketObjectProperty | BracketObjectMethod | BracketObjectGetter | BracketObjectSetter;; // Defaults to `[]`
 }

 interface ObjectGetter :< ObjectMember, Function {
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern;; // Defaults to `[]`
    body: BlockStatement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
    key: Identifier | Literal;
 }

 interface ObjectMember  {
    key: Identifier | Literal;
 }

 interface ObjectMethod :< ObjectMember, Function {
    key: Identifier | Literal;
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern;; // Defaults to `[]`
    body: BlockStatement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface ObjectProperty :< ObjectMember {
    key: Identifier | Literal;
    value: Expression;
 }

 interface ObjectSetter :< ObjectMember, Function {
    key: Identifier | Literal;
    ?id: Identifier; // Defaults to `null`
    ?params: Pattern;; // Defaults to `[]`
    body: BlockStatement;
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
 }

 interface Pattern  {
 }

 // Root of the AST.
 interface Program  {
    ?body: Statement | FunctionDeclaration;; // Defaults to `[]`
    ?BINJS:Scope: BINJS:Scope; // Defaults to `null`
    ?directives: Directive;; // Defaults to `[]`
 }

 interface RegExpLiteral :< Literal {
    pattern: string;
    flags: string;
 }

 interface ReturnStatement :< Statement {
    ?argument: Expression; // Defaults to `null`
 }

 interface SequenceExpression :< Expression {
    ?expressions: Expression;; // Defaults to `[]`
 }

 interface Statement  {
 }

 interface StringLiteral :< Literal {
    value: string;
 }

 interface SwitchCase  {
    ?test: Expression; // Defaults to `null`
    ?consequent: Statement | FunctionDeclaration;; // Defaults to `[]`
 }

 interface SwitchStatement :< Statement {
    discriminant: Expression;
    ?cases: SwitchCase;; // Defaults to `[]`
 }

 interface ThisExpression :< Expression {
 }

 interface ThrowStatement :< Statement {
    argument: Expression;
 }

 interface TryStatement :< Statement {
    block: BlockStatement;
    ?handler: CatchClause; // Defaults to `null`
    ?finalizer: BlockStatement; // Defaults to `null`
 }

 interface UnaryExpression :< Expression {
    operator: UnaryOperator;
    ?prefix: bool; // Defaults to `false`
    argument: Expression;
 }

 interface UpdateExpression :< Expression {
    operator: UpdateOperator;
    argument: Expression;
    ?prefix: bool; // Defaults to `false`
 }

 interface VariableDeclaration :< Declaration {
    declarations: VariableDeclarator; /* Non-empty */;
    kind: VariableKind;
 }

 interface VariableDeclarator  {
    id: Pattern;
    ?init: Expression; // Defaults to `null`
 }

 interface WhileStatement :< Statement {
    test: Expression;
    body: Statement;
 }

 interface WithStatement :< Statement {
    object: Expression;
    body: Statement;
 }



 // # Enums.
 //
 // The order of enum values does NOT matter.
enum AssignmentOperator {
    "=",
    "+=",
    "-=",
    "*=",
    "/=",
    "%=",
    "<<=",
    ">>=",
    ">>>=",
    "|=",
    "^=",
    "&=",
}

enum BinaryOperator {
    "==",
    "!=",
    "===",
    "!==",
    "<",
    "<=",
    ">",
    ">=",
    "<<",
    ">>",
    ">>>",
    "+",
    "-",
    "*",
    "/",
    "%",
    "|",
    "^",
    "&",
    "in",
    "instanceof",
}

enum LogicalOperator {
    "||",
    "&&",
}

enum UnaryOperator {
    "-",
    "+",
    "!",
    "~",
    "typeof",
    "void",
    "delete",
}

enum UpdateOperator {
    "++",
    "--",
}

enum VariableKind {
    "let",
    "var",
    "const",
}
```
