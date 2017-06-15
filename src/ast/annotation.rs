use ast::grammar::ASTError;
use ast::library::{ BINJS_VAR_NAME, BINJS_LEX_NAME, BINJS_DIRECT_EVAL, BINJS_CAPTURED_NAME };
use util::JSONGetter;

use serde_json;
use serde_json::Value as JSON;

use std;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

type Object = serde_json::Map<String, JSON>;

pub enum RefPosition {
    FunctionArguments,
    Callee,
    Other,
}
impl Default for RefPosition {
    fn default() -> Self {
        RefPosition::other("uninitialized")
    }
}
impl RefPosition {
    pub fn other(_: &str) -> Self {
        RefPosition::Other
    }
}

#[derive(Default)]
struct RefContextContents {
    bound: HashSet<String>,
    used: HashSet<String>,
    has_direct_eval: bool,
    position: RefPosition,
    parent: Option<Rc<RefCell<RefContextContents>>>,
}
impl RefContextContents {
    fn is_bound(&self, name: &str) -> bool {
        if self.bound.contains(name) {
            return true;
        }
        if let Some(ref parent) = self.parent {
            return parent.borrow().is_bound(name);
        }
        false
    }
    fn captured_names(&self) -> Vec<String> {
        let mut result = vec![];
        for name in &self.used {
            if !self.bound.contains(name) {
                result.push(name.to_string())
            }
        }
        result
    }
    fn dispose(&mut self) {
        if let Some(ref mut parent) = self.parent {
            let mut parent = parent.borrow_mut();
            parent.has_direct_eval |= self.has_direct_eval;
            match self.position {
                RefPosition::FunctionArguments => {
                    // Propagate both `bound` and `used`.
                    parent.bound.extend(self.bound.drain());
                    parent.used.extend(self.used.drain());
                }
                _ => {
                    // Drop `bound`, keep `used`... unless the name is bound.
                    let mut bound = HashSet::new();
                    std::mem::swap(&mut self.bound, &mut bound); // Swap to avoid borrow issues.
                    for used in self.used.drain() {
                        if !bound.contains(&used) {
                            parent.used.insert(used);
                        }
                    }
                }
            }
        }
    }
}

pub struct RefContext {
    contents: Rc<RefCell<RefContextContents>>,
}
impl Drop for RefContext {
    /// Ensure that whenever we leave a stack frame, we propagate
    /// all the necessary bindings to the parent.
    fn drop(&mut self) {
        self.contents.borrow_mut().dispose();
    }
}

// FIXME: add_direct_eval crosses function boundaries
// However, it MUST NOT propagate laterally.
impl RefContext {
    pub fn new() -> Self {
        RefContext {
            contents: Rc::new(RefCell::new(
                RefContextContents {
                    position: RefPosition::other("Toplevel"),
                    ..RefContextContents::default()
                }
            )),
        }
    }
    pub fn enter(&mut self, position: RefPosition) -> Self {
        RefContext {
            contents: Rc::new(RefCell::new(
                RefContextContents {
                    position,
                    parent: Some(self.contents.clone()),
                    ..RefContextContents::default()
                }
            )),
        }
    }
    pub fn add_identifier(&mut self, name: &str) {
        let name = name.to_string();
        let mut borrow = self.contents.borrow_mut();
        let set = match borrow.position {
            RefPosition::FunctionArguments =>
                &mut borrow.bound,
            RefPosition::Callee => {
                if &name == "eval" && !self.is_bound("eval") {
                    borrow.has_direct_eval = true;
                }
                &mut borrow.used
            }
            _ => &mut borrow.used
        };
        set.insert(name.to_string());
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
            .insert(BINJS_DIRECT_EVAL.to_string(), json!(borrow.has_direct_eval))
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
            borrow.bound.insert(item);
        }

        let lex_names = object.get_array(BINJS_LEX_NAME, "Repository of LexDecl bindings")
            .expect("Could not fetch LexDecl repository");
        for item in lex_names {
            let item = item.as_str()
                .expect("Item should be a string")
                .to_string();
            borrow.bound.insert(item);
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
struct DeclContextContents {
    lex_names: HashSet<String>,
    var_names: HashSet<String>,
    position: DeclPosition,
}

/// An analysis context containing top-down information
/// on variable declarations.
///
/// Contexts are organised as a stack.
pub struct DeclContext {
    contents: Rc<RefCell<DeclContextContents>>,

    /// The parent, i.e. the context below in the stack.
    parent: Option<Rc<RefCell<DeclContextContents>>>,
}

impl Drop for DeclContext {
    /// Ensure that whenever we leave a stack, we propagate
    /// all the necessary bindings to the parent.
    fn drop(&mut self) {
        if let Some(ref parent) = self.parent {
            let mut borrow = self.contents.borrow_mut();
            let mut parent = parent.borrow_mut();
            match borrow.position {
                DeclPosition::Block => {
                    // Propagate var_names, drop lex_names.
                    parent.var_names.extend(borrow.var_names.drain());
                }
                DeclPosition::Function => {
                    // Drop everything.
                }
                _ => {
                    // Propagate everything.
                    parent.var_names.extend(borrow.var_names.drain());
                    parent.lex_names.extend(borrow.lex_names.drain());
                }
            }
        }
    }
}

impl DeclContext {
    /// Create an empty `DeclContext`.
    pub fn new() -> Self {
        DeclContext {
            contents: Rc::new(RefCell::new(DeclContextContents {
                position: DeclPosition::other("Toplevel"),
                ..DeclContextContents::default()
            })),
            parent: None
        }
    }
    pub fn position(&self) -> DeclPosition {
        self.contents.borrow().position.clone()
    }

    /// Add an untreated identifier. It still needs to be classified as `var`, `let`, etc – or possibly nothing.
    pub fn add_identifier(&mut self, name: &str) -> Result<(), ASTError> {
        let name = name.to_string();
        let mut borrow = self.contents.borrow_mut();
        let set = match borrow.position {
            DeclPosition::LexDecl =>
                Some(&mut borrow.lex_names),
            DeclPosition::VarDecl =>
                Some(&mut borrow.var_names),
            _ => None
        };
        if let Some(set) = set {
            set.insert(name);
        }
        Ok(())
    }
    pub fn enter(&mut self, position: DeclPosition) -> Self {
        DeclContext {
            contents: Rc::new(RefCell::new(DeclContextContents {
                position,
                ..DeclContextContents::default()
            })),
            parent: Some(self.contents.clone()),
        }
    }

    /// Store scope information in a node.
    ///
    /// This makes sense only if the node implements `binjs`, otherwise all data
    /// will be stripped while encoding.
    pub fn store(&self, object: &mut Object) {
        println!("DeclContext::store() {}", object.get_string("type", "").unwrap());
        let borrow = self.contents.borrow();

        let mut var_decl_names: Vec<_> = borrow.var_names.iter().collect();
        var_decl_names.sort();
        assert!(object
            .insert(BINJS_VAR_NAME.to_string(), json!(var_decl_names))
            .is_none(),
            "This node already has a field {}", BINJS_VAR_NAME);

        let mut lexically_declared_names: Vec<_> = borrow.lex_names.iter().collect();
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
    /// - CapturedNames **may contain false positives**;
    /// - HasDirectEval **may contain false positives**.
    fn process_declarations(&self, me: &Annotator, ctx: &mut DeclContext, tree: &mut JSON) -> Result<(), ASTError> {
        // Only process object nodes.
        match *tree {
            JSON::Array(ref mut array) => {
                println!("process_declarations: array");
                for tree in array.iter_mut() {
                    me.process_declarations(me, ctx, tree)?
                }
            }
            JSON::Object(ref mut object) => {
                println!("process_declarations: object");
                if let Ok(kind) = object.get_string("type", "Field `type`")
                    .map(str::to_string)
                {
                    me.process_declarations_obj(me, ctx, object, &kind)?
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

    fn process_declarations_field(&self, me: &Annotator, ctx: &mut DeclContext, object: &mut Object, key: &str) -> Result<(), ASTError> {
        if let Some(ref mut json) = object.get_mut(key) {
            return me.process_declarations(me, ctx, json)
        }
        Err(ASTError::InvalidValue {
            got: serde_json::to_string(object).unwrap(),
            expected: format!("Field `{}`", key)
        })
    }

    /// At the START of this pass:
    /// - LexicallyDeclaredNames MUST BE correct;
    /// - VarDeclaredNames MUST BE correct;
    /// - CapturedNames MAY contain false positives but MUST NOT contain false negatives;
    /// - HasDirectEval MAY contain false positives but MUST NOT contain false negatives.
    ///
    /// At the END of this pass:
    /// - LexicallyDeclaredNames is correct;
    /// - VarDeclaredNames is correct;
    /// - CapturedNames is correct;
    /// - HasDirectEval is correct;
    fn process_references(&self, me: &Annotator, ctx: &mut RefContext, tree: &mut JSON) -> Result<(), ASTError> {
        // Only process object nodes and array.
        match *tree {
            JSON::Array(ref mut array) => {
                println!("process_references: array");
                for tree in array.iter_mut() {
                    me.process_references(me, ctx, tree)?
                }
            }
            JSON::Object(ref mut object) => {
                println!("process_references: object");
                if let Ok(kind) = object.get_string("type", "Field `type`")
                    .map(str::to_string) {
                        me.process_references_obj(me, ctx, object, &kind)?
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

    fn process_references_field(&self, me: &Annotator, ctx: &mut RefContext, object: &mut Object, key: &str) -> Result<(), ASTError> {
        if let Some(ref mut json) = object.get_mut(key) {
            return me.process_references(me, ctx, json)
        }
        Err(ASTError::InvalidValue {
            got: serde_json::to_string(object).unwrap(),
            expected: format!("Field `{}`", key)
        })
    }

    fn process_references_obj(&self, me: &Annotator, ctx: &mut RefContext, object: &mut Object, kind: &str) -> Result<(), ASTError>;
    fn process_declarations_obj(&self, me: &Annotator, ctx: &mut DeclContext, object: &mut Object, kind: &str) -> Result<(), ASTError>;
}