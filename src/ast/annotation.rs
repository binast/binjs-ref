// FIXME: Too much copy&paste in this file.

use ast::grammar::{ ASTError, FieldName, Kind, Syntax };
use ast::library::{ BINJS_VAR_NAME, BINJS_LEX_NAME, BINJS_DIRECT_EVAL, BINJS_CAPTURED_NAME };
use util::{ Dispose, JSONGetter };

use serde_json;
use serde_json::Value as JSON;

use std;
use std::cell::{ RefCell, Ref };
use std::collections::HashSet;
use std::rc::Rc;

type Object = serde_json::Map<String, JSON>;

/// The position currently being examined.
#[derive(Clone)]
pub struct Position {
    /// The kind of object currently being examined.
    kind: Kind,

    /// If a field is being examined, its name.
    field: Option<FieldName>
}
impl Position {
    pub fn new(kind: &Kind, field: Option<&FieldName>) -> Self {
        Position {
            kind: kind.clone(),
            field: field.map(FieldName::clone)
        }
    }
    pub fn field(&self) -> &Option<FieldName> {
        &self.field
    }

    /// Get the name of the current field, if any.
    pub fn field_str(&self) -> Option<&str> {
        match self.field {
            None => None,
            Some(ref x) => Some(x.to_str())
        }
    }
    pub fn kind(&self) -> &Kind {
        &self.kind
    }
}

/// Storage for information collected while annotating an AST.
pub struct Context<'a, T> where ContextContents<'a, T>: Dispose {
    /// The current position.
    ///
    /// Copied here to avoid some issues with borrowing. Immutable.
    position: Position,

    /// A shared pointer towards the information collected so far.
    ///
    /// This pointer is shared with children whenever we `enter_field`
    /// or `enter_obj` a new
    contents: Rc<RefCell<ContextContents<'a, T>>>,
}

impl<'a, T> Drop for Context<'a, T> where ContextContents<'a, T>: Dispose {
    fn drop(&mut self) {
        self.contents.borrow_mut().dispose()
    }
}

impl<'a, T> Context<'a, T> where T: Default, ContextContents<'a, T>: Dispose {
    pub fn new(syntax: &'a Syntax) -> Self {
        let contents = ContextContents::new(syntax);
        let position = contents.position.clone();
        Context {
            position,
            contents: Rc::new(RefCell::new(contents)),
        }
    }
    pub fn enter_field(&mut self, field: &str) -> Result<Self, ASTError> {
        let position = {
            let grammar = self.contents.borrow()
                .grammar;
            let ref kind = self.contents.borrow().position.kind;
            let field = grammar.get_field_name(field)
                    .ok_or_else(|| ASTError::InvalidField(field.to_string()))?;
            Position::new(kind, Some(field))
        };
        Ok(Context {
            position: position.clone(),
            contents: Rc::new(RefCell::new(
                ContextContents {
                    position,
                    parent: Some(self.contents.clone()),
                    ..ContextContents::new(self.contents.borrow().grammar)
                }
            )),
        })
    }
    pub fn enter_obj(&mut self, kind: &str) -> Result<Self, ASTError> {
        let position = {
            let grammar = self.contents.borrow()
                .grammar;
            let kind = grammar.get_kind(kind)
                .ok_or_else(|| ASTError::InvalidKind(kind.to_string()))?;
            Position::new(&kind, None)
        };
        Ok(Context {
            position: position.clone(),
            contents: Rc::new(RefCell::new(
                ContextContents {
                    position,
                    parent: Some(self.contents.clone()),
                    ..ContextContents::new(self.contents.borrow().grammar)
                }
            )),
        })
    }
    pub fn kind_str(&self) -> &str {
        self.position.kind().to_str()
    }
    pub fn contents(&self) -> Ref<ContextContents<'a, T>> {
        self.contents.borrow()
    }
    pub fn parent(&self) -> Option<Rc<RefCell<ContextContents<'a, T>>>> {
        self.contents.borrow().parent.clone()
    }
}

pub struct ContextContents<'a, T> {
    data: T,
    position: Position,
    grammar: &'a Syntax,
    parent: Option<Rc<RefCell<ContextContents<'a, T>>>>,
}

impl<'a, T> ContextContents<'a, T> where T: Default {
    /// Create a new ContextContents containing no data, no parent,
    /// at the default position.
    fn new(syntax: &'a Syntax) -> Self {
        let root = syntax.get_root()
            .kind()
            .expect("Could not extract kind of syntax root");
        ContextContents {
            grammar: syntax,
            position: Position::new(&root, None),
            parent: None,
            data: Default::default()
        }
    }
}
impl<'a, T> ContextContents<'a, T> {
    pub fn field_str(&self) -> Option<&str> {
        self.position.field_str()
    }
    pub fn kind_str(&self) -> &str {
        self.position.kind.to_str()
    }
    pub fn parent(&self) -> Option<Rc<RefCell<ContextContents<'a, T>>>> {
        self.parent.clone()
    }
}

/// The contents of a context used to annotate use of variables.
#[derive(Default)]
pub struct RefContents {
    bound: HashSet<String>,
    free:  HashSet<String>,

    /// `true` if we have detected a call `eval("foo")` anywhere in the subtree
    /// (including subfunctions), where `eval` is a free name.
    has_direct_eval: bool
}


impl<'a> ContextContents<'a, RefContents> {
    pub fn add_free_name(&mut self, name: &str) {
        self.data.free.insert(name.to_string());
    }
    pub fn add_bound_name(&mut self, name: &str) {
        self.data.bound.insert(name.to_string());
    }
    pub fn add_direct_eval(&mut self) {
        self.data.has_direct_eval = true;
    }

    fn is_bound(&self, name: &str) -> bool {
        if self.data.bound.contains(name) {
            return true;
        }
        if let Some(ref parent) = self.parent {
            return parent.borrow().is_bound(name);
        }
        false
    }

    fn captured_names(&self) -> Vec<String> {
        let mut result = vec![];
        for name in &self.data.free {
            if !self.data.bound.contains(name) {
                result.push(name.to_string())
            }
        }
        result
    }
}

impl<'a> Dispose for ContextContents<'a, RefContents> {
    fn dispose(&mut self) {
        if let Some(ref mut parent) = self.parent {
            let mut parent = parent.borrow_mut();
            parent.data.has_direct_eval |= self.data.has_direct_eval;
            // Drop `bound`, keep `free`... unless the name is bound.
            let mut bound = HashSet::new();
            std::mem::swap(&mut self.data.bound, &mut bound); // Swap to avoid borrow issues.
            for free in self.data.free.drain() {
                if !bound.contains(&free) {
                    parent.data.free.insert(free);
                }
            }
        }
    }
}




impl<'a> Context<'a, RefContents> {
    pub fn add_direct_eval(&mut self) {
        self.contents.borrow_mut().data.has_direct_eval = true;
    }
    pub fn add_free_name(&mut self, name: &str) {
        self.contents.borrow_mut().data.free.insert(name.to_string());
    }
    pub fn add_bound_name(&mut self, name: &str) {
        self.contents.borrow_mut().data.bound.insert(name.to_string());
    }

    /// Check whether the name is bound somewhere on the stack.
    pub fn is_bound(&self, name: &str) -> bool {
        let borrow = self.contents.borrow();
        borrow.is_bound(name)
    }

    pub fn store(&self, object: &mut Object) {
        println!("RefContext::store() {:?}", object.get_string("type", "").unwrap());
        // Make sure that we didn't forget the object during the previous pass.
        assert!(object.contains_key(BINJS_VAR_NAME));
        assert!(object.contains_key(BINJS_LEX_NAME));

        let borrow = self.contents.borrow();
        let mut captured_names = borrow.captured_names();
        captured_names.sort();
        assert!(object
            .insert(BINJS_CAPTURED_NAME.to_string(), json!(captured_names))
            .is_none(),
            "This node already has a field {}", BINJS_CAPTURED_NAME);

        assert!(object
            .insert(BINJS_DIRECT_EVAL.to_string(), json!(borrow.data.has_direct_eval))
            .is_none(),
            "This node already has a field {}", BINJS_DIRECT_EVAL);
    }
    pub fn load(&mut self, object: &Object) {
        let mut borrow = self.contents.borrow_mut();

        let var_decl_names = object.get_array(BINJS_VAR_NAME, "Repository of VarDecl bindings")
            .expect("Could not fetch VarDecl repository");
        for item in var_decl_names {
            let item = item.as_str()
                .expect("Item should be a string")
                .to_string();
            borrow.data.bound.insert(item);
        }

        let lex_names = object.get_array(BINJS_LEX_NAME, "Repository of LexDecl bindings")
            .expect("Could not fetch LexDecl repository");
        for item in lex_names {
            let item = item.as_str()
                .expect("Item should be a string")
                .to_string();
            borrow.data.bound.insert(item);
        }
    }
}
#[derive(Clone)]
pub enum DeclPosition {
    Function,
    FunctionArguments,
    Block,
    VarDecl,
    LexDecl,
    Expression,
    Callee,
    Other
}
impl Default for DeclPosition {
    fn default() -> Self {
        DeclPosition::other("uninitialized")
    }
}
impl DeclPosition {
    pub fn other(_: &str) -> Self {
        DeclPosition::Other
    }
}
#[derive(Default)]
pub struct DeclContents {
    lex_names: HashSet<String>,
    var_names: HashSet<String>,
    scope_kind: ScopeKind,
}

#[derive(Clone, Copy)]
pub enum ScopeKind {
    VarDecl,
    LexDecl,
    Nothing
}
impl Default for ScopeKind {
    fn default() -> Self {
        ScopeKind::Nothing
    }
}
impl<'a> ContextContents<'a, DeclContents> {
    pub fn is_lex_bound(&self, name: &str) -> bool {
        if self.data.lex_names.contains(name) {
            return true;
        }
        if let Some(ref parent) = self.parent {
            return parent.borrow().is_lex_bound(name);
        }
        false
    }
    pub fn scope_kind(&self) -> ScopeKind {
        if let ScopeKind::Nothing = self.data.scope_kind {
            if let Some(ref parent) = self.parent {
                return parent.borrow().scope_kind()
            }
            return ScopeKind::Nothing
        }
        return self.data.scope_kind
    }
    pub fn add_var_name(&mut self, name: &str) {
        self.data.var_names.insert(name.to_string());
    }
    pub fn add_lex_name(&mut self, name: &str) {
        self.data.lex_names.insert(name.to_string());
    }
}

impl<'a> Dispose for ContextContents<'a, DeclContents> {
    /// Ensure that whenever we leave a stack, we propagate
    /// all the necessary bindings to the parent.
    fn dispose(&mut self) {
        if let Some(ref parent) = self.parent {
            let mut parent = parent.borrow_mut();
            // Propagate everything.
            parent.data.var_names.extend(self.data.var_names.drain());
            parent.data.lex_names.extend(self.data.lex_names.drain());
        }
    }
}

impl<'a> Context<'a, DeclContents> {
    pub fn add_var_name(&mut self, name: &str) {
        self.contents.borrow_mut().add_var_name(name)
    }
    pub fn add_lex_name(&mut self, name: &str) {
        self.contents.borrow_mut().add_lex_name(name);
    }
    /// Check whether the name is lexically bound somewhere on the stack.
    pub fn is_lex_bound(&self, name: &str) -> bool {
        let borrow = self.contents.borrow();
        borrow.is_lex_bound(name)
    }
    pub fn scope_kind(&self) -> ScopeKind {
        self.contents.borrow().scope_kind()
    }
    pub fn set_scope_kind(&mut self, kind: ScopeKind) {
        self.contents.borrow_mut().data.scope_kind = kind
    }
    pub fn clear_var_names(&mut self) {
        self.contents.borrow_mut()
            .data
            .var_names
            .clear();
    }
    pub fn clear_lex_names(&mut self) {
        self.contents.borrow_mut()
            .data
            .lex_names
            .clear();
    }

    /// Store scope information in a node.
    ///
    /// This makes sense only if the node implements `binjs`, otherwise all data
    /// will be stripped while encoding.
    pub fn store(&self, object: &mut Object) {
        println!("DeclContext::store() {}", object.get_string("type", "").unwrap());
        assert!(!object.contains_key(BINJS_CAPTURED_NAME));
        assert!(!object.contains_key(BINJS_DIRECT_EVAL));

        let borrow = self.contents.borrow();

        let mut var_decl_names: Vec<_> = borrow.data.var_names.iter().collect();
        var_decl_names.sort();
        assert!(object
            .insert(BINJS_VAR_NAME.to_string(), json!(var_decl_names))
            .is_none(),
            "This node already has a field {}", BINJS_VAR_NAME);

        let mut lexically_declared_names: Vec<_> = borrow.data.lex_names.iter().collect();
        lexically_declared_names.sort();
        assert!(object
            .insert(BINJS_LEX_NAME.to_string(), json!(lexically_declared_names))
            .is_none(),
            "This node already has a field {}", BINJS_LEX_NAME);
    }
}


pub trait Annotator {
    fn name(&self) -> String;

    /// At the end of this pass:
    /// - LexicallyDeclaredNames is correct;
    /// - VarDeclaredNames is correct;
    fn process_declarations(&self, me: &Annotator, ctx: &mut Context<DeclContents>, object: &mut Object) -> Result<(), ASTError>;
    fn process_declarations_aux(&self, me: &Annotator, ctx: &mut Context<DeclContents>, tree: &mut JSON) -> Result<(), ASTError> {
        // Only process object nodes.
        match *tree {
            JSON::Array(ref mut array) => {
                println!("process_declarations: array");
                for tree in array.iter_mut() {
                    me.process_declarations_aux(me, ctx, tree)?
                }
            }
            JSON::Object(ref mut object) => {
                println!("process_declarations: object");
                if let Ok(kind) = object.get_string("type", "Field `type`")
                    .map(str::to_string)
                {
                    let mut ctx = ctx.enter_obj(&kind)?;
                    me.process_declarations(me, &mut ctx, object)?
                } else {
                    println!("process_declarations: No type, not looking at {:?}", object);
                }
            }
            _ => {
                println!("process_declarations: Not object/array, not looking at {:?}", tree);
            }
        }
        Ok(())
    }

    fn process_declarations_field(&self, me: &Annotator, ctx: &mut Context<DeclContents>, object: &mut Object, key: &str) -> Result<(), ASTError> {
        if let Some(ref mut json) = object.get_mut(key) {
            return me.process_declarations_aux(me, ctx, json)
        }
        Err(ASTError::InvalidValue {
            got: serde_json::to_string(object).unwrap(),
            expected: format!("Field `{}`", key)
        })
    }

    /// At the START of this pass:
    /// - LexicallyDeclaredNames MUST BE correct;
    /// - VarDeclaredNames MUST BE correct;
    ///
    /// At the END of this pass:
    /// - LexicallyDeclaredNames is correct;
    /// - VarDeclaredNames is correct;
    /// - CapturedNames is correct;
    /// - HasDirectEval is correct;
    fn process_references(&self, me: &Annotator, ctx: &mut Context<RefContents>, object: &mut Object) -> Result<(), ASTError>;
    fn process_references_aux(&self, me: &Annotator, ctx: &mut Context<RefContents>, tree: &mut JSON) -> Result<(), ASTError> {
        // Only process object nodes and array.
        match *tree {
            JSON::Array(ref mut array) => {
                println!("process_references: array");
                for tree in array.iter_mut() {
                    me.process_references_aux(me, ctx, tree)?
                }
            }
            JSON::Object(ref mut object) => {
                println!("process_references: object");
                if let Ok(kind) = object.get_string("type", "Field `type`")
                    .map(str::to_string)
                {
                    let mut ctx = ctx.enter_obj(&kind)?;
                    me.process_references(me, &mut ctx, object)?
                } else {
                    println!("process_references: No type, not looking at {:?}", object);
                }
            }
            _ => {
                println!("process_references: Not object/array, not looking at {:?}", tree);
            }
        }
        Ok(())
    }

    fn process_references_field(&self, me: &Annotator, ctx: &mut Context<RefContents>, object: &mut Object, key: &str) -> Result<(), ASTError> {
        if let Some(ref mut json) = object.get_mut(key) {
            return me.process_references_aux(me, ctx, json)
        }
        Err(ASTError::InvalidValue {
            got: serde_json::to_string(object).unwrap(),
            expected: format!("Field `{}`", key)
        })
    }
}