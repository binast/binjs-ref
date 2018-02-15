//! A library of specifications for versions of JavaScript.
//!
//! For a human-readable documentation of the latest version of the library:
//! - build the examples
//!    `cargo build --examples`
//! - run example ast-doc
//!    `./target/debug/examples/ast-doc`.
//!

use ast::grammar::*;
use util::{ FromJSON, ToJSON };
use json::JsonValue as JSON;

use std;
use std::cell::RefCell;
use std::collections::{  HashSet };

pub static TOPLEVEL_SCOPE_NAME: &'static str = "AssertedTopLevelScope";
pub static BLOCK_SCOPE_NAME: &'static str = "AssertedBlockScope";
pub static SCOPE_FIELD: &'static str = "scope";
pub static BINJS_VAR_NAME: &'static str = "varDeclaredNames";
pub static BINJS_LET_NAME: &'static str = "lexicallyDeclaredNames";
pub static BINJS_CAPTURED_NAME: &'static str = "capturedNames";
pub static BINJS_DIRECT_EVAL: &'static str = "hasDirectEval";

/// The set of features requested for a syntax.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Level {
    /// Empty syntax, for testing purposes.
    Minimal,
    /// All the features for ES6.
    ES6,
    /// All the features of the latest version of JavaScript.
    Latest,
    // FIXME: More levels to be implemented.
}
impl std::fmt::Display for Level {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            Level::Minimal => write!(fmt, "Minimal"),
            Level::ES6 | Level::Latest => write!(fmt, "ES6"),
        }
    }
}


/// Special nodes used by BINJS. Not visible at source level.
fn setup_binjs(_: &mut SyntaxBuilder) -> Box<RefCell<Annotator>> {
    struct BaseAnnotator;
    impl Annotator for BaseAnnotator {
        fn annotate(&mut self, _: &mut JSON) {
            // Nothing to do.
        }
    }

    Box::new(RefCell::new(BaseAnnotator))
}

fn setup_es6(syntax: &mut SyntaxBuilder) -> Box<RefCell<Annotator>> {
    use ast::library_es6_generated;
    use ast::library_es6_generated::ast::*;
    let _names = library_es6_generated::Library::new(syntax) ;

    #[derive(Debug, PartialEq, Eq)]
    enum BindingKind {
        Var,
        Lex,
        Param,
        Implicit,
    }

    #[derive(Default)]
    struct AnnotationVisitor {
        // The following are stacks.
        var_names_stack: Vec<HashSet<String>>,
        lex_names_stack: Vec<HashSet<String>>,
        param_names_stack: Vec<HashSet<String>>,
        binding_kind_stack: Vec<BindingKind>,
        apparent_direct_eval_stack: Vec<bool>,
        free_names_in_function_stack: Vec<HashSet<String>>,

        // Whenever we pop from `free_names_in_function_stack`,
        // we transfer everything here.
        free_names_in_nested_functions: HashSet<String>,
    }
    impl AnnotationVisitor {
        fn pop_captured_names(&mut self, bindings: &[&HashSet<String>]) -> Vec<String> {
            let mut captured_names = vec![];
            for binding in bindings {
                for name in *binding {
                    if self.free_names_in_nested_functions.remove(name) {
                        // Names that appear in both `bindings` and in `free_names_in_nested_functions`
                        // are names that are declared in the current scope and captured in a nested
                        // function.
                        captured_names.push(name.clone());
                    }
                    // Also, cleanup free names.
                    self.free_names_in_function_stack.last_mut()
                        .unwrap()
                        .remove(name);
                }
            }

            captured_names.sort();
            captured_names
        }

        fn push_free_names(&mut self) {
            self.free_names_in_function_stack.push(HashSet::new());
        }
        fn pop_free_names(&mut self, bindings: &[&HashSet<String>]) {
            let mut free_names_in_current_function = self.free_names_in_function_stack.pop().unwrap();
            for name in free_names_in_current_function.drain() {
                let is_bound = bindings.iter()
                    .find(|container| container.contains(&name))
                    .is_some();
                if !is_bound {
                    self.free_names_in_nested_functions.insert(name);
                }
            }
        }

        fn push_direct_eval(&mut self) {
            // So far, we haven't spotted any direct eval.
            self.apparent_direct_eval_stack.push(false);
        }
        fn pop_direct_eval(&mut self) -> bool {
            let spotted_direct_eval = self.apparent_direct_eval_stack.pop().unwrap();
            if spotted_direct_eval {
                // If we have spotted a direct eval, well, the parents also have
                // a direct eval. Note that we will perform a second pass to
                // remove erroneous direct evals if we find out that name `eval`
                // was actually bound at some point.
                if let Some(parent) = self.apparent_direct_eval_stack.last_mut() {
                    *parent = true;
                }
            }
            spotted_direct_eval
        }

        fn push_block_scope(&mut self, _path: &ASTPath) {
            self.lex_names_stack.push(HashSet::new());
            self.push_direct_eval();
        }
        fn pop_block_scope(&mut self, path: &ASTPath) -> Option<AssertedBlockScope> {
            debug!(target: "annotating", "pop_block_scope at {:?}", path);
            let mut lex_names = self.lex_names_stack.pop().unwrap();

            let captured_names = self.pop_captured_names(&[&lex_names]);
            let mut lex_names : Vec<_> = lex_names.drain().collect();
            lex_names.sort();

            let has_direct_eval = self.pop_direct_eval();
            if lex_names.len() > 0 || has_direct_eval /* implied || captured_var_names.len() > 0 */ {
                Some(AssertedBlockScope {
                    lexically_declared_names: lex_names,
                    captured_names,
                    has_direct_eval
                })
            } else {
                None
            }
        }

        fn push_var_scope(&mut self, _path: &ASTPath) {
            self.var_names_stack.push(HashSet::new());
            self.lex_names_stack.push(HashSet::new());
            self.push_free_names();
            self.push_direct_eval();
        }
        fn pop_var_scope(&mut self, path: &ASTPath, function_name: Option<&String>) -> Option<AssertedVarScope> {
            debug!(target: "annotating", "pop_var_scope at {:?}", path);
            let mut var_names = self.var_names_stack.pop().unwrap();
            let mut lex_names = self.lex_names_stack.pop().unwrap();

            let function_name = {
                let mut set = HashSet::new();
                if let Some(name) = function_name {
                    set.insert(name.clone());
                }
                set
            };
            let captured_names = self.pop_captured_names(&[&var_names, &lex_names, &function_name]);
            self.pop_free_names(&[&var_names, &lex_names, &function_name]);

            let mut var_names : Vec<_> = var_names.drain().collect();
            var_names.sort();
            let mut lex_names : Vec<_> = lex_names.drain().collect();
            lex_names.sort();

            let has_direct_eval = self.pop_direct_eval();

            if var_names.len() > 0 || lex_names.len() > 0 || has_direct_eval /* implied || captured_var_names.len() > 0 */ {
                Some(AssertedVarScope {
                    lexically_declared_names: lex_names,
                    var_declared_names: var_names,
                    captured_names,
                    has_direct_eval
                })
            } else {
                None
            }
        }

        fn push_param_scope(&mut self, _path: &ASTPath) {
            self.param_names_stack.push(HashSet::new());
            self.push_direct_eval();
        }
        fn pop_param_scope(&mut self, path: &ASTPath) -> Option<AssertedParameterScope> {
            debug!(target: "annotating", "pop_param_scope at {:?}", path);
            let mut param_names = self.param_names_stack.pop().unwrap();
            let captured_names = self.pop_captured_names(&[&param_names]);

            let has_direct_eval = self.pop_direct_eval();
            if param_names.len() > 0 || has_direct_eval /* implied || captured_names.len() > 0 */ {
                let mut param_names : Vec<_> = param_names.drain().collect();
                param_names.sort();
                Some(AssertedParameterScope {
                    parameter_names: param_names,
                    captured_names,
                    has_direct_eval
                })
            } else {
                None
            }
        }
    }

    impl Visitor<()> for AnnotationVisitor {
        // Identifiers

        fn exit_call_expression(&mut self, _path: &ASTPath, node: &mut CallExpression) -> Result<(), ()> {
            if let ExpressionOrSuper::IdentifierExpression(box ref id) = node.callee {
                if id.name == "eval" {
                    *self.apparent_direct_eval_stack.last_mut()
                        .unwrap() = true;
                }
            }
            Ok(())
        }

        fn exit_identifier_expression(&mut self, _path: &ASTPath, node: &mut IdentifierExpression) -> Result<(), ()> {
            self.free_names_in_function_stack.last_mut()
                .unwrap()
                .insert(node.name.clone());
            Ok(())
        }
        

        fn exit_binding_identifier(&mut self, path: &ASTPath, node: &mut BindingIdentifier) -> Result<(), ()> {
            if let Some(&ASTPathItem { interface: ASTNode::FunctionDeclaration, field: ASTField::Name}) = path.get(0) {
                // The name of a FunctionDeclaration is quite special and is handled in `exit_function_declaration`.
                return Ok(())
            }
            debug!(target: "annotating", "exit_binding identifier – marking {name} at {path:?}",
                name = node.name,
                path = path);
            match *self.binding_kind_stack.last().unwrap() {
                BindingKind::Var => {
                    self.var_names_stack.last_mut()
                        .unwrap()
                        .insert(node.name.clone());
                }
                BindingKind::Lex => {
                    self.lex_names_stack.last_mut()
                        .unwrap()
                        .insert(node.name.clone());
                }
                BindingKind::Param => {
                    self.param_names_stack.last_mut()
                        .unwrap()
                        .insert(node.name.clone());
                }
                BindingKind::Implicit => { /* Nothing to do */ }
            }
            Ok(())
        }


        // Blocks

        fn enter_block(&mut self, path: &ASTPath, _node: &mut Block) -> Result<(), ()> {
            self.push_block_scope(path);
            Ok(())
        }

        fn exit_block(&mut self, path: &ASTPath, node: &mut Block) -> Result<(), ()> {
            node.scope = self.pop_block_scope(path);
            Ok(())
        }

        fn enter_script(&mut self, path: &ASTPath, _node: &mut Script) -> Result<(), ()> {
            self.push_var_scope(path);
            Ok(())
        }
        fn exit_script(&mut self, path: &ASTPath, node: &mut Script) -> Result<(), ()> {
            node.scope = self.pop_var_scope(path, None);
            Ok(())
        }

        fn enter_module(&mut self, path: &ASTPath, _node: &mut Module) -> Result<(), ()> {
            self.push_var_scope(path);
            Ok(())
        }
        fn exit_module(&mut self, path: &ASTPath, node: &mut Module) -> Result<(), ()> {
            node.scope = self.pop_var_scope(path, None);
            Ok(())
        }

        // Try/Catch
        fn enter_catch_clause(&mut self, _path: &ASTPath, _node: &mut CatchClause) -> Result<(), ()> {
            self.binding_kind_stack.push(BindingKind::Implicit);
            Ok(())
        }
        fn exit_catch_clause(&mut self, _path: &ASTPath, _node: &mut CatchClause) -> Result<(), ()> {
            assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Implicit));
            Ok(())
        }

        // Explicit variable declarations

        fn enter_for_in_of_binding(&mut self, _path: &ASTPath, node: &mut ForInOfBinding) -> Result<(), ()> {
            let kind = match node.kind {
                VariableDeclarationKind::Let | VariableDeclarationKind::Const => BindingKind::Lex,
                VariableDeclarationKind::Var => BindingKind::Var,
            };
            self.binding_kind_stack.push(kind);
            Ok(())
        }
        fn exit_for_in_of_binding(&mut self, _path: &ASTPath, node: &mut ForInOfBinding) -> Result<(), ()> {
            let kind = match node.kind {
                VariableDeclarationKind::Let | VariableDeclarationKind::Const => BindingKind::Lex,
                VariableDeclarationKind::Var => BindingKind::Var,
            };
            assert_eq!(self.binding_kind_stack.pop().unwrap(), kind);
            Ok(())
        }

        fn enter_variable_declaration(&mut self, _path: &ASTPath, node: &mut VariableDeclaration) -> Result<(), ()> {
            let kind = match node.kind {
                VariableDeclarationKind::Let | VariableDeclarationKind::Const => BindingKind::Lex,
                VariableDeclarationKind::Var => BindingKind::Var,
            };
            self.binding_kind_stack.push(kind);
            Ok(())
        }
        fn exit_variable_declaration(&mut self, _path: &ASTPath, node: &mut VariableDeclaration) -> Result<(), ()> {
            let kind = match node.kind {
                VariableDeclarationKind::Let | VariableDeclarationKind::Const => BindingKind::Lex,
                VariableDeclarationKind::Var => BindingKind::Var,
            };
            assert_eq!(self.binding_kind_stack.pop().unwrap(), kind);
            Ok(())
        }

        // Functions, methods, arguments.
        fn enter_setter(&mut self, path: &ASTPath, _node: &mut Setter) -> Result<(), ()> {
            self.binding_kind_stack.push(BindingKind::Param);
            self.push_param_scope(path);
            self.push_var_scope(path);
            Ok(())
        }
        fn exit_setter(&mut self, path: &ASTPath, node: &mut Setter) -> Result<(), ()> {
            assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));
            // If the setter has a name, it's not a free name.
            let name = if let PropertyName::LiteralPropertyName(box ref name) = node.name {
                Some(&name.value)
            } else {
                None
            };

            // Commit parameter scope and var scope.
            node.parameter_scope = self.pop_param_scope(path);
            node.body_scope = self.pop_var_scope(path, name);

            Ok(())
        }

        fn enter_getter(&mut self, path: &ASTPath, _node: &mut Getter) -> Result<(), ()> {
            self.push_var_scope(path);
            Ok(())
        }

        fn exit_getter(&mut self, path: &ASTPath, node: &mut Getter) -> Result<(), ()> {
            // If the getter has a name, it's not a free name.
            let name = if let PropertyName::LiteralPropertyName(box ref name) = node.name {
                Some(&name.value)
            } else {
                None
            };

            node.body_scope = self.pop_var_scope(path, name);

            Ok(())
        }

        fn enter_method(&mut self, path: &ASTPath, _node: &mut Method) -> Result<(), ()> {
            self.binding_kind_stack.push(BindingKind::Param);
            self.push_var_scope(path);
            self.push_param_scope(path);
            Ok(())
        }
        fn exit_method(&mut self, path: &ASTPath, node: &mut Method) -> Result<(), ()> {
            assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

            // If the method has a name, it's not a free name.
            let name = if let PropertyName::LiteralPropertyName(box ref name) = node.name {
                Some(&name.value)
            } else {
                None
            };

            // Commit parameter scope and var scope.
            node.parameter_scope = self.pop_param_scope(path);
            node.body_scope = self.pop_var_scope(path, name);

            Ok(())
        }

        fn enter_arrow_expression(&mut self, path: &ASTPath, _node: &mut ArrowExpression) -> Result<(), ()> {
            self.binding_kind_stack.push(BindingKind::Param);
            self.push_var_scope(path);
            self.push_param_scope(path);
            Ok(())
        }
        fn exit_arrow_expression(&mut self, path: &ASTPath, node: &mut ArrowExpression) -> Result<(), ()> {
            assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

            // Commit parameter scope and var scope.
            node.parameter_scope = self.pop_param_scope(path);
            node.body_scope = self.pop_var_scope(path, None);

            Ok(())
        }

        fn enter_function_expression(&mut self, path: &ASTPath, _node: &mut FunctionExpression) -> Result<(), ()> {
            self.binding_kind_stack.push(BindingKind::Param);
            self.push_var_scope(path);
            self.push_param_scope(path);
            Ok(())
        }
        fn exit_function_expression(&mut self, path: &ASTPath, node: &mut FunctionExpression) -> Result<(), ()> {
            assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

            // If the function has a name, it's not a free name
            let name = if let Some(ref name) = node.name {
                Some(&name.name)
            } else {
                None
            };

            // Commit parameter scope and var scope.
            node.parameter_scope = self.pop_param_scope(path);
            node.body_scope = self.pop_var_scope(path, name);

            Ok(())
        }

        fn enter_function_declaration(&mut self, path: &ASTPath, _node: &mut FunctionDeclaration) -> Result<(), ()> {
            self.binding_kind_stack.push(BindingKind::Param);
            self.push_var_scope(path);
            self.push_param_scope(path);
            Ok(())
        }
        fn exit_function_declaration(&mut self, path: &ASTPath, node: &mut FunctionDeclaration) -> Result<(), ()> {
            debug!(target: "annotating", "exit_function_declaration {} at {:?}", node.name.name, path);
            assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

            // If a name declaration was specified, remove it from `unknown`.
            let ref name = node.name.name;

            // Commit parameter scope and var scope.
            node.parameter_scope = self.pop_param_scope(path);
            node.body_scope = self.pop_var_scope(path, Some(name));
            // Anything we do from this point affects the scope outside the function.

            // 1. If the declaration is at the toplevel, the name is declared as a `var`.
            // 2. If the declaration is in a function's toplevel block, the name is declared as a `var`.
            // 3. Otherwise, the name is declared as a `let`.
            let name = name.to_string();
            debug!(target: "annotating", "exit_function_declaration sees {} at {:?}", node.name.name, path.get(0));
            match path.get(0) {
                None => {
                    // Case 1.
                    debug!(target: "annotating", "exit_function_declaration says it's a var (case 1)");
                    self.var_names_stack.last_mut()
                        .unwrap()
                        .insert(name);
                }
                Some(&ASTPathItem { field: ASTField::Statements, interface: ASTNode::FunctionBody }) =>
                {
                    // Case 2.
                    debug!(target: "annotating", "exit_function_declaration says it's a var (case 2)");
                    self.var_names_stack.last_mut()
                        .unwrap()
                        .insert(name);
                }
                Some(_) => {
                    // Case 3.
                    debug!(target: "annotating", "exit_function_declaration says it's a lex (case 3)");
                    self.lex_names_stack.last_mut()
                        .unwrap()
                        .insert(name);
                }
            }
            Ok(())
        }
    }


    /// Perform a second pass to cleanup incorrect instances of `eval`.
    struct EvalCleanupAnnotator {
        /// `true` if name `eval` was bound at this level or higher in the tree.
        eval_bindings: Vec<bool>,
    }
    impl Visitor<()> for EvalCleanupAnnotator {
        // FIXME: Anything that has a scope (including CatchClause and its invisible scope) should push an `eval_bindings`.
        // on entering, pop it on exit.
        fn enter_function_declaration(&mut self, _path: &ASTPath, _node: &mut FunctionDeclaration) -> Result<(), ()> {
            // By default, adopt parent's behavior.
            // If necessary, reading the scope information will amend it.
            let has_eval_binding = *self.eval_bindings.last().unwrap();
            self.eval_bindings.push(has_eval_binding);
            Ok(())
        }
        fn exit_function_declaration(&mut self, _path: &ASTPath, _node: &mut FunctionDeclaration) -> Result<(), ()> {
            self.eval_bindings.pop().unwrap();
            Ok(())
        }
        fn enter_function_expression(&mut self, _path: &ASTPath, node: &mut FunctionExpression) -> Result<(), ()> {
            // By default, adopt parent's behavior.
            // Don't forget that the internal name of the function may mask `eval`.
            let mut has_eval_binding = *self.eval_bindings.last().unwrap();
            if let Some(ref name) = node.name {
                has_eval_binding = has_eval_binding || &name.name == "eval";
            }
            self.eval_bindings.push(has_eval_binding);
            // If necessary, reading the scope information will amend it.
            Ok(())
        }
        fn exit_function_expression(&mut self, _path: &ASTPath, _node: &mut FunctionExpression) -> Result<(), ()> {
            self.eval_bindings.pop().unwrap();
            Ok(())
        }
        fn enter_arrow_expression(&mut self, _path: &ASTPath, _node: &mut ArrowExpression) -> Result<(), ()> {
            // By default, adopt parent's behavior.
            // If necessary, reading the scope information will amend it.
            let has_eval_binding = *self.eval_bindings.last().unwrap();
            self.eval_bindings.push(has_eval_binding);
            Ok(())
        }
        fn exit_arrow_expression(&mut self, _path: &ASTPath, _node: &mut ArrowExpression) -> Result<(), ()> {
            self.eval_bindings.pop().unwrap();
            Ok(())
        }
        fn enter_getter(&mut self, _path: &ASTPath, node: &mut Getter) -> Result<(), ()> {
            // Don't forget that the internal name of the getter may mask `eval`.
            let mut has_eval_binding = *self.eval_bindings.last().unwrap();
            if let PropertyName::LiteralPropertyName(ref name) = node.name {
                has_eval_binding = has_eval_binding || &name.value == "eval";
            }
            // If necessary, reading the scope information will amend it.
            self.eval_bindings.push(has_eval_binding);
            Ok(())
        }
        fn exit_getter(&mut self, _path: &ASTPath, _node: &mut Getter) -> Result<(), ()> {
            self.eval_bindings.pop().unwrap();
            Ok(())
        }
        fn enter_setter(&mut self, _path: &ASTPath, node: &mut Setter) -> Result<(), ()> {
            // Don't forget that the internal name of the setter may mask `eval`.
            let mut has_eval_binding = *self.eval_bindings.last().unwrap();
            if let PropertyName::LiteralPropertyName(ref name) = node.name {
                has_eval_binding = has_eval_binding || &name.value == "eval";
            }
            // If necessary, reading the scope information will amend it.
            self.eval_bindings.push(has_eval_binding);
            Ok(())
        }
        fn exit_setter(&mut self, _path: &ASTPath, _node: &mut Setter) -> Result<(), ()> {
            self.eval_bindings.pop().unwrap();
            Ok(())
        }
        fn enter_method(&mut self, _path: &ASTPath, node: &mut Method) -> Result<(), ()> {
            // Don't forget that the internal name of the method may mask `eval`.
            let mut has_eval_binding = *self.eval_bindings.last().unwrap();
            if let PropertyName::LiteralPropertyName(ref name) = node.name {
                has_eval_binding = has_eval_binding || &name.value == "eval";
            }
            // If necessary, reading the scope information will amend it.
            self.eval_bindings.push(has_eval_binding);
            Ok(())
        }
        fn exit_method(&mut self, _path: &ASTPath, _node: &mut Method) -> Result<(), ()> {
            self.eval_bindings.pop().unwrap();
            Ok(())
        }
        fn enter_catch_clause(&mut self, _path: &ASTPath, node: &mut CatchClause) -> Result<(), ()> {
            // Don't forget that the implicitly declared variable may mask `eval`.
            let mut has_eval_binding = *self.eval_bindings.last().unwrap();
            match node.binding {
                Binding::BindingIdentifier(ref binding) => {
                    has_eval_binding = has_eval_binding || &binding.name == "eval";
                }
                _ => unimplemented!() // FIXME: Patterns may also mask `eval`.
            }
            self.eval_bindings.push(has_eval_binding);
            Ok(())
        }
        fn exit_catch_clause(&mut self, _path: &ASTPath, _node: &mut CatchClause) -> Result<(), ()> {
            self.eval_bindings.pop().unwrap();
            Ok(())
        }



        // Update scopes themselves.
        fn exit_asserted_block_scope(&mut self, _path: &ASTPath, node: &mut AssertedBlockScope) -> Result<(), ()> {
            if node.lexically_declared_names.iter()
                .find(|e| *e == "eval")
                .is_some()
            {
                *self.eval_bindings.last_mut()
                    .unwrap() = true;
            }
            if *self.eval_bindings.last()
                .unwrap()
            {
                node.has_direct_eval = false;    
            }
            Ok(())
        }
        fn exit_asserted_var_scope(&mut self, _path: &ASTPath, node: &mut AssertedVarScope) -> Result<(), ()> {
            if node.lexically_declared_names.iter()
                .chain(node.var_declared_names.iter())
                .find(|e| *e == "eval")
                .is_some()
            {
                *self.eval_bindings.last_mut()
                    .unwrap() = true;
            }
            if *self.eval_bindings.last()
                .unwrap()
            {
                node.has_direct_eval = false;    
            }
            Ok(())
        }
        fn exit_asserted_parameter_scope(&mut self, _path: &ASTPath, node: &mut AssertedParameterScope) -> Result<(), ()> {
            if node.parameter_names.iter()
                .find(|e| *e == "eval")
                .is_some()
            {
                *self.eval_bindings.last_mut()
                    .unwrap() = true;
            }
            if *self.eval_bindings.last()
                .unwrap()
            {
                node.has_direct_eval = false;    
            }
            Ok(())
        }
    }

    impl Annotator for AnnotationVisitor {
        fn annotate(&mut self, ast: &mut JSON) {
            let mut script = Script::import(ast)
                .expect("Invalid script"); // FIXME: Error values would be nicer.
            script.walk(&mut ASTPath::new(), self)
                .expect("Could not walk script");

            let mut cleanup = EvalCleanupAnnotator {
                eval_bindings: vec![false]
            };
            script.walk(&mut ASTPath::new(), &mut cleanup)
                .expect("Could not walk script for eval cleanup");

            *ast = script.export();
        }
    }

    Box::new(RefCell::new(AnnotationVisitor::default()))
}


/// Construct a syntax for a specific version of JavaScript.
pub fn syntax(level: Level) -> Syntax {
    let mut builder = SyntaxBuilder::new();

    let root    = builder.node_name("Script");
    let null    = builder.node_name("_Null");

    let base_annotator = setup_binjs(&mut builder);

    let annotator = match level {
        Level::Minimal => {
            builder.add_interface(&root).unwrap();
            base_annotator
        }
        Level::ES6
        | Level::Latest => {
            setup_es6(&mut builder)
        }
    };
    builder.into_syntax(SyntaxOptions {
        root: &root,
        null: &null,
        annotator
    })
}



#[test]
fn test_syntax() {
    syntax(Level::Minimal);
    syntax(Level::ES6);
    syntax(Level::Latest);
}
