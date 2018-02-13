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
    // FIXME: Perform `hasDirectEval` cleanup if we realize that `eval` is bound.
    struct AnnotationVisitor {
        var_names: HashSet<String>,
        lex_names: HashSet<String>,
        param_names: HashSet<String>,
        free_names_in_current_function: HashSet<String>,
        free_names_in_nested_function: HashSet<String>,
        binding_kind_stack: Vec<BindingKind>,
    }
    impl AnnotationVisitor {
        fn new() -> Self {
            Self {
                var_names: HashSet::new(),
                lex_names: HashSet::new(),
                param_names: HashSet::new(),
                free_names_in_current_function: HashSet::new(),
                free_names_in_nested_function: HashSet::new(),
                binding_kind_stack: vec![],
            }
        }
        fn pop_captured_names(&mut self, bindings: &[&HashSet<String>]) -> Vec<String> {
            // FIXME: It might be possible to optimize this by reverting the lookup,
            // i.e. lookup in `bindings` first and in `free_names_in_nested_function` later.
            let mut captured_names = vec![];
            for name in &self.free_names_in_nested_function {
                for binding in bindings {
                    if binding.contains(name) {
                        captured_names.push(name.to_string());
                    }
                }
            }
            for name in &captured_names {
                self.free_names_in_nested_function.remove(name);
            }
            captured_names.sort();
            captured_names
        }

        fn pop_block_scope(&mut self) -> Option<AssertedBlockScope> {
            let mut lex_names = HashSet::new();
            std::mem::swap(&mut self.lex_names, &mut lex_names);

            let captured_names = self.pop_captured_names(&[&lex_names]);
            let mut lex_names : Vec<_> = lex_names.drain().collect();
            lex_names.sort();

            let has_direct_eval = self.free_names_in_current_function.contains("eval")
                                || self.free_names_in_nested_function.contains("eval");
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

        /// Return the current AssertedVarScope, cleaning up `var_names`, `lex_names` and `free_names_in_nested_function`.
        ///
        /// Invariant: `var_names` and `lex_names` are empty at the end.
        fn pop_var_scope(&mut self) -> Option<AssertedVarScope> {
            let mut var_names = HashSet::new();
            std::mem::swap(&mut self.var_names, &mut var_names);

            let mut lex_names = HashSet::new();
            std::mem::swap(&mut self.lex_names, &mut lex_names);

            let captured_names = self.pop_captured_names(&[&var_names, &lex_names]);

            let mut var_names : Vec<_> = var_names.drain().collect();
            var_names.sort();
            let mut lex_names : Vec<_> = lex_names.drain().collect();
            lex_names.sort();

            let has_direct_eval = self.free_names_in_current_function.contains("eval")
                                || self.free_names_in_nested_function.contains("eval");
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
        /// Return the current param scope, cleaning up `param_names` and `free_names_in_nested_function`.
        ///
        /// Invariant: `param_names` is empty at the end.
        fn pop_param_scope(&mut self) -> Option<AssertedParameterScope> {
            if self.param_names.len() == 0 {
                return None;
            }
            let mut param_names = HashSet::new();
            std::mem::swap(&mut self.param_names, &mut param_names);
            let captured_names = self.pop_captured_names(&[&param_names]);

            let has_direct_eval = self.free_names_in_current_function.contains("eval")
                                || self.free_names_in_nested_function.contains("eval");
            if self.param_names.len() > 0 || has_direct_eval /* implied || captured_names.len() > 0 */ {
                let mut param_names : Vec<_> = self.param_names.drain().collect();
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

        fn exit_identifier_expression(&mut self, _path: &ASTPath, node: &mut IdentifierExpression) -> Result<(), ()> {
            self.free_names_in_current_function.insert(node.name.clone());
            Ok(())
        }
        

        fn exit_binding_identifier(&mut self, _path: &ASTPath, node: &mut BindingIdentifier) -> Result<(), ()> {
            debug!(target: "annotating", "exit_binding identifier – marking {name} at {path:?}",
                name = node.name,
                path = _path);
            let scope = match *self.binding_kind_stack.last().unwrap() {
                BindingKind::Var => Some(&mut self.var_names),
                BindingKind::Lex => Some(&mut self.lex_names),
                BindingKind::Param => Some(&mut self.param_names),
                BindingKind::Implicit => None,
            };
            if let Some(scope) = scope {
                scope.insert(node.name.clone());
            }
            Ok(())
        }


        // Blocks

        fn exit_block(&mut self, _path: &ASTPath, node: &mut Block) -> Result<(), ()> {
            node.scope = self.pop_block_scope();
            Ok(())
        }

        fn exit_script(&mut self, _path: &ASTPath, node: &mut Script) -> Result<(), ()> {
            node.scope = self.pop_var_scope();
            // FIXME: We may need to promote free names to `var` names.
            Ok(())
        }

        fn exit_module(&mut self, _path: &ASTPath, node: &mut Module) -> Result<(), ()> {
            node.scope = self.pop_var_scope();
            // FIXME: We may need to promote free names to `var` names.
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
        fn enter_setter(&mut self, _path: &ASTPath, _node: &mut Setter) -> Result<(), ()> {
            self.binding_kind_stack.push(BindingKind::Param);
            Ok(())
        }
        fn exit_setter(&mut self, _path: &ASTPath, node: &mut Setter) -> Result<(), ()> {
            assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));
            // If a name declaration was specified, remove it from `unknown`.
            if let PropertyName::LiteralPropertyName(ref name) = node.name {
                self.free_names_in_current_function.remove(&name.value);
                self.free_names_in_nested_function.remove(&name.value);
            }

            // Commit parameter scope and var scope.
            node.body_scope = self.pop_var_scope();
            node.parameter_scope = self.pop_param_scope();
            // Anything we do from this point affects the scope outside the function.

            // Empty free_names_in_current_function in free_names_in_nested_function.
            self.free_names_in_nested_function.extend(self.free_names_in_current_function.drain());

            Ok(())
        }

        fn exit_getter(&mut self, _path: &ASTPath, node: &mut Getter) -> Result<(), ()> {
            // If a name declaration was specified, remove it from `unknown`.
            if let PropertyName::LiteralPropertyName(ref name) = node.name {
                self.free_names_in_current_function.remove(&name.value);
                self.free_names_in_nested_function.remove(&name.value);
            }

            // Commit parameter scope and var scope.
            node.body_scope = self.pop_var_scope();
            // Anything we do from this point affects the scope outside the function.

            // Empty free_names_in_current_function in free_names_in_nested_function.
            self.free_names_in_nested_function.extend(self.free_names_in_current_function.drain());

            Ok(())
        }

        fn enter_method(&mut self, _path: &ASTPath, _node: &mut Method) -> Result<(), ()> {
            self.binding_kind_stack.push(BindingKind::Param);
            Ok(())
        }
        fn exit_method(&mut self, _path: &ASTPath, node: &mut Method) -> Result<(), ()> {
            assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

            // If a name declaration was specified, remove it from `unknown`.
            if let PropertyName::LiteralPropertyName(ref name) = node.name {
                self.free_names_in_current_function.remove(&name.value);
                self.free_names_in_nested_function.remove(&name.value);
            }

            // Commit parameter scope and var scope.
            node.parameter_scope = self.pop_param_scope();
            node.body_scope = self.pop_var_scope();
            // Anything we do from this point affects the scope outside the function.

            // Empty free_names_in_current_function in free_names_in_nested_function.
            self.free_names_in_nested_function.extend(self.free_names_in_current_function.drain());

            Ok(())
        }

        fn enter_arrow_expression(&mut self, _path: &ASTPath, _node: &mut ArrowExpression) -> Result<(), ()> {
            self.binding_kind_stack.push(BindingKind::Param);
            Ok(())
        }
        fn exit_arrow_expression(&mut self, _path: &ASTPath, node: &mut ArrowExpression) -> Result<(), ()> {
            assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

            // Commit parameter scope and var scope.
            node.parameter_scope = self.pop_param_scope();
            node.body_scope = self.pop_var_scope();
            // Anything we do from this point affects the scope outside the function.

            // Empty free_names_in_current_function in free_names_in_nested_function.
            self.free_names_in_nested_function.extend(self.free_names_in_current_function.drain());

            Ok(())
        }

        fn enter_function_expression(&mut self, _path: &ASTPath, _node: &mut FunctionExpression) -> Result<(), ()> {
            self.binding_kind_stack.push(BindingKind::Param);
            Ok(())
        }
        fn exit_function_expression(&mut self, _path: &ASTPath, node: &mut FunctionExpression) -> Result<(), ()> {
            assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

            // If a name declaration was specified, remove it from `unknown`.
            if let Some(ref name) = node.name {
                self.free_names_in_current_function.remove(&name.name);
                self.free_names_in_nested_function.remove(&name.name);
            }

            // Commit parameter scope and var scope.
            node.parameter_scope = self.pop_param_scope();
            node.body_scope = self.pop_var_scope();
            // Anything we do from this point affects the scope outside the function.

            // Empty free_names_in_current_function in free_names_in_nested_function.
            self.free_names_in_nested_function.extend(self.free_names_in_current_function.drain());

            Ok(())
        }

        fn enter_function_declaration(&mut self, _path: &ASTPath, _node: &mut FunctionDeclaration) -> Result<(), ()> {
            self.binding_kind_stack.push(BindingKind::Param);
            Ok(())
        }
        fn exit_function_declaration(&mut self, path: &ASTPath, node: &mut FunctionDeclaration) -> Result<(), ()> {
            assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

            // If a name declaration was specified, remove it from `unknown`.
            let mut id_was_captured = false;
            let ref id = node.name.name;

            if self.free_names_in_current_function.remove(id) {
                id_was_captured = true;
            }
            if self.free_names_in_nested_function.remove(id) {
                id_was_captured = true;
            }

            // Commit parameter scope and var scope.
            node.parameter_scope = self.pop_param_scope();
            node.body_scope = self.pop_var_scope();
            // Anything we do from this point affects the scope outside the function.

            // Empty free_names_in_current_function in free_names_in_nested_function.
            self.free_names_in_nested_function.extend(self.free_names_in_current_function.drain());

            // If the id was captured, reintroduce it as captured in a subfunction.
            if id_was_captured {
                self.free_names_in_nested_function.insert(id.clone());
            }

            // 1. If the declaration is at the toplevel, the name is declared as a `var`.
            // 2. If the declaration is in a function's toplevel block, the name is declared as a `var`.
            // 3. Otherwise, the name is declared as a `let`.
            let id = id.to_string();
            match path.get(1) {
                None => {
                    // Case 1.
                    self.var_names.insert(id);
                }
                Some(&ASTPathItem { field: ASTField::Statements, interface: ASTNode::FunctionBody }) =>
                {
                    // Case 2.
                    self.var_names.insert(id);
                }
                Some(_) => {
                    // Case 3.
                    self.lex_names.insert(id);
                }
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
            *ast = script.export();
        }
    }

    Box::new(RefCell::new(AnnotationVisitor::new()))
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
