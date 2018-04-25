use ast::*;
use binjs_shared::{ FromJSON, ToJSON, VisitMe };

use std::collections::{  HashSet };

use itertools::Itertools;
use json::JsonValue as JSON;

#[derive(Debug, PartialEq, Eq)]
enum BindingKind {
    Var,
    Lex,
    Param,
    Implicit,
}

#[derive(Default)]
pub struct AnnotationVisitor {
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

    fn push_block_scope(&mut self, _path: &Path) {
        self.lex_names_stack.push(HashSet::new());
        self.push_direct_eval();
    }
    fn pop_block_scope(&mut self, path: &Path) -> Option<AssertedBlockScope> {
        debug!(target: "annotating", "pop_block_scope at {:?}", path);
        let lex_names = self.lex_names_stack.pop().unwrap();

        let captured_names = self.pop_captured_names(&[&lex_names]);
        let lex_names : Vec<_> = lex_names.into_iter()
            .sorted();

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

    fn push_var_scope(&mut self, _path: &Path) {
        self.var_names_stack.push(HashSet::new());
        self.lex_names_stack.push(HashSet::new());
        self.push_free_names();
        self.push_direct_eval();
    }
    fn pop_var_scope(&mut self, path: &Path) -> Option<AssertedVarScope> {
        debug!(target: "annotating", "pop_var_scope at {:?}", path);
        let var_names = self.var_names_stack.pop().unwrap();
        let lex_names = self.lex_names_stack.pop().unwrap();

        // Check that a name isn't defined twice in the same scope.
        for name in var_names.intersection(&lex_names) {
            panic!("This name is both lex-bound and var-bound: {}", name);
        }

        let captured_names = self.pop_captured_names(&[&var_names, &lex_names]);
        self.pop_free_names(&[&var_names, &lex_names]);

        let var_names : Vec<_> = var_names.into_iter()
            .sorted();
        let lex_names : Vec<_> = lex_names.into_iter()
            .sorted();


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

    fn push_param_scope(&mut self, _path: &Path) {
        self.param_names_stack.push(HashSet::new());
        self.push_direct_eval();
    }
    fn pop_param_scope(&mut self, path: &Path) -> Option<AssertedParameterScope> {
        debug!(target: "annotating", "pop_param_scope at {:?}", path);
        let mut param_names = self.param_names_stack.pop().unwrap();
        let captured_names = self.pop_captured_names(&[&param_names]);

        // In the case of `function foo(j) {var j;}`, the `var j` is not the true declaration.
        // Remove it from parameters.
        for name in &param_names {
            self.var_names_stack.last_mut()
                .unwrap()
                .remove(name);
        }

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

    fn exit_call_expression(&mut self, _path: &Path, node: &mut CallExpression) -> Result<Option<CallExpression>, ()> {
        if let ExpressionOrSuper::IdentifierExpression(box ref id) = node.callee {
            if id.name == "eval" {
                *self.apparent_direct_eval_stack.last_mut()
                    .unwrap() = true;
            }
        }
        Ok(None)
    }

    fn exit_identifier_expression(&mut self, _path: &Path, node: &mut IdentifierExpression) -> Result<Option<IdentifierExpression>, ()> {
        self.free_names_in_function_stack.last_mut()
            .unwrap()
            .insert(node.name.clone());
        Ok(None)
    }

    fn exit_assignment_target_identifier(&mut self, _path: &Path, node: &mut AssignmentTargetIdentifier) -> Result<Option<AssignmentTargetIdentifier>, ()> {
        self.free_names_in_function_stack.last_mut()
            .unwrap()
            .insert(node.name.clone());
        Ok(None)
    }

    fn exit_binding_identifier(&mut self, path: &Path, node: &mut BindingIdentifier) -> Result<Option<BindingIdentifier>, ()> {
        match path.get(0) {
            Some(&PathItem { interface: ASTNode::EagerFunctionDeclaration, field: ASTField::Name})
            | Some(&PathItem { interface: ASTNode::EagerFunctionExpression, field: ASTField::Name})
            | Some(&PathItem { interface: ASTNode::EagerMethod, field: ASTField::Name})
            | Some(&PathItem { interface: ASTNode::EagerGetter, field: ASTField::Name})
            | Some(&PathItem { interface: ASTNode::EagerSetter, field: ASTField::Name})
            => {
                // Function names are special.
                // They are handled in the respective `exit_*` methods.
                return Ok(None)
            }
            _ => {}
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
        Ok(None)
    }


    // Blocks

    fn enter_block(&mut self, path: &Path, _node: &mut Block) -> Result<VisitMe<()>, ()> {
        self.push_block_scope(path);
        Ok(VisitMe::HoldThis(()))
    }

    fn exit_block(&mut self, path: &Path, node: &mut Block) -> Result<Option<Block>, ()> {
        node.scope = self.pop_block_scope(path);
        Ok(None)
    }

    fn enter_script(&mut self, path: &Path, _node: &mut Script) -> Result<VisitMe<()>, ()> {
        self.push_var_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_script(&mut self, path: &Path, node: &mut Script) -> Result<Option<Script>, ()> {
        node.scope = self.pop_var_scope(path);
        Ok(None)
    }

    fn enter_module(&mut self, path: &Path, _node: &mut Module) -> Result<VisitMe<()>, ()> {
        self.push_var_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_module(&mut self, path: &Path, node: &mut Module) -> Result<Option<Module>, ()> {
        node.scope = self.pop_var_scope(path);
        Ok(None)
    }

    // Try/Catch
    fn enter_catch_clause(&mut self, _path: &Path, _node: &mut CatchClause) -> Result<VisitMe<()>, ()> {
        self.binding_kind_stack.push(BindingKind::Implicit);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_catch_clause(&mut self, _path: &Path, _node: &mut CatchClause) -> Result<Option<CatchClause>, ()> {
        assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Implicit));
        Ok(None)
    }

    // Explicit variable declarations

    fn enter_for_in_of_binding(&mut self, _path: &Path, node: &mut ForInOfBinding) -> Result<VisitMe<()>, ()> {
        let kind = match node.kind {
            VariableDeclarationKind::Let | VariableDeclarationKind::Const => BindingKind::Lex,
            VariableDeclarationKind::Var => BindingKind::Var,
        };
        self.binding_kind_stack.push(kind);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_for_in_of_binding(&mut self, _path: &Path, node: &mut ForInOfBinding) -> Result<Option<ForInOfBinding>, ()> {
        let kind = match node.kind {
            VariableDeclarationKind::Let | VariableDeclarationKind::Const => BindingKind::Lex,
            VariableDeclarationKind::Var => BindingKind::Var,
        };
        assert_eq!(self.binding_kind_stack.pop().unwrap(), kind);
        Ok(None)
    }

    fn enter_variable_declaration(&mut self, _path: &Path, node: &mut VariableDeclaration) -> Result<VisitMe<()>, ()> {
        let kind = match node.kind {
            VariableDeclarationKind::Let | VariableDeclarationKind::Const => BindingKind::Lex,
            VariableDeclarationKind::Var => BindingKind::Var,
        };
        self.binding_kind_stack.push(kind);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_variable_declaration(&mut self, _path: &Path, node: &mut VariableDeclaration) -> Result<Option<VariableDeclaration>, ()> {
        let kind = match node.kind {
            VariableDeclarationKind::Let | VariableDeclarationKind::Const => BindingKind::Lex,
            VariableDeclarationKind::Var => BindingKind::Var,
        };
        assert_eq!(self.binding_kind_stack.pop().unwrap(), kind);
        Ok(None)
    }

    // Functions, methods, arguments.
    fn enter_eager_setter(&mut self, path: &Path, _node: &mut EagerSetter) -> Result<VisitMe<()>, ()> {
        self.binding_kind_stack.push(BindingKind::Param);
        self.push_param_scope(path);
        self.push_var_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_setter(&mut self, path: &Path, node: &mut EagerSetter) -> Result<Option<EagerSetter>, ()> {
        assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));
        // Commit parameter scope and var scope.
        node.parameter_scope = self.pop_param_scope(path);
        node.body_scope = self.pop_var_scope(path);

        Ok(None)
    }

    fn enter_eager_getter(&mut self, path: &Path, _node: &mut EagerGetter) -> Result<VisitMe<()>, ()> {
        self.push_var_scope(path);
        Ok(VisitMe::HoldThis(()))
    }

    fn exit_eager_getter(&mut self, path: &Path, node: &mut EagerGetter) -> Result<Option<EagerGetter>, ()> {
        node.body_scope = self.pop_var_scope(path);

        Ok(None)
    }

    fn enter_eager_method(&mut self, path: &Path, _node: &mut EagerMethod) -> Result<VisitMe<()>, ()> {
        self.binding_kind_stack.push(BindingKind::Param);
        self.push_var_scope(path);
        self.push_param_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_method(&mut self, path: &Path, node: &mut EagerMethod) -> Result<Option<EagerMethod>, ()> {
        assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

        // Commit parameter scope and var scope.
        node.parameter_scope = self.pop_param_scope(path);
        node.body_scope = self.pop_var_scope(path);

        Ok(None)
    }

    fn enter_eager_arrow_expression(&mut self, path: &Path, _node: &mut EagerArrowExpression) -> Result<VisitMe<()>, ()> {
        self.binding_kind_stack.push(BindingKind::Param);
        self.push_var_scope(path);
        self.push_param_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_arrow_expression(&mut self, path: &Path, node: &mut EagerArrowExpression) -> Result<Option<EagerArrowExpression>, ()> {
        assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

        // Commit parameter scope and var scope.
        node.parameter_scope = self.pop_param_scope(path);
        node.body_scope = self.pop_var_scope(path);

        Ok(None)
    }

    fn enter_eager_function_expression(&mut self, path: &Path, _node: &mut EagerFunctionExpression) -> Result<VisitMe<()>, ()> {
        self.binding_kind_stack.push(BindingKind::Param);
        self.push_var_scope(path);
        self.push_param_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_function_expression(&mut self, path: &Path, node: &mut EagerFunctionExpression) -> Result<Option<EagerFunctionExpression>, ()> {
        assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

        // If the function has a name, it's a parameter.
        if let Some(ref name) = node.name {
            self.param_names_stack.last_mut()
                .unwrap()
                .insert(name.name.clone());
        }

        // Commit parameter scope and var scope.
        node.parameter_scope = self.pop_param_scope(path);
        node.body_scope = self.pop_var_scope(path);

        Ok(None)
    }

    fn enter_eager_function_declaration(&mut self, path: &Path, _node: &mut EagerFunctionDeclaration) -> Result<VisitMe<()>, ()> {
        self.binding_kind_stack.push(BindingKind::Param);
        self.push_var_scope(path);
        self.push_param_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_function_declaration(&mut self, path: &Path, node: &mut EagerFunctionDeclaration) -> Result<Option<EagerFunctionDeclaration>, ()> {
        debug!(target: "annotating", "exit_function_declaration {} at {:?}", node.name.name, path);
        assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Param));

        // If a name declaration was specified, remove it from `unknown`.
        let ref name = node.name.name;

        // Commit parameter scope and var scope. The function's name is not actually bound in the function; the outer var binding is used.
        node.parameter_scope = self.pop_param_scope(path);
        node.body_scope = self.pop_var_scope(path);
        // Anything we do from this point affects the scope outside the function.

        // 1. If the declaration is at the toplevel, the name is declared as a `var`.
        // 2. If the declaration is in a function's toplevel block, the name is declared as a `var`.
        // 3. Otherwise, the name is declared as a `let`.
        let name = name.to_string();
        debug!(target: "annotating", "exit_function_declaration sees {} at {:?}", node.name.name, path.get(0));
        match path.get(0).expect("Impossible AST walk") {
            &PathItem { field: ASTField::Statements, interface: ASTNode::Script } |
            &PathItem { field: ASTField::Statements, interface: ASTNode::Module } => {
                // Case 1.
                debug!(target: "annotating", "exit_function_declaration says it's a var (case 1)");
                self.var_names_stack.last_mut()
                    .unwrap()
                    .insert(name);
            }
            &PathItem { field: ASTField::Statements, interface: ASTNode::FunctionBody } =>
            {
                // Case 2.
                debug!(target: "annotating", "exit_function_declaration says it's a var (case 2)");
                self.var_names_stack.last_mut()
                    .unwrap()
                    .insert(name);
            }
            _ => {
                // Case 3.
                debug!(target: "annotating", "exit_function_declaration says it's a lex (case 3)");
                self.lex_names_stack.last_mut()
                    .unwrap()
                    .insert(name);
            }
        }
        Ok(None)
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
    fn enter_eager_function_declaration(&mut self, _path: &Path, _node: &mut EagerFunctionDeclaration) -> Result<VisitMe<()>, ()> {
        // By default, adopt parent's behavior.
        // If necessary, reading the scope information will amend it.
        let has_eval_binding = *self.eval_bindings.last().unwrap();
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_function_declaration(&mut self, _path: &Path, _node: &mut EagerFunctionDeclaration) -> Result<Option<EagerFunctionDeclaration>, ()> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_eager_function_expression(&mut self, _path: &Path, node: &mut EagerFunctionExpression) -> Result<VisitMe<()>, ()> {
        // By default, adopt parent's behavior.
        // Don't forget that the internal name of the function may mask `eval`.
        let mut has_eval_binding = *self.eval_bindings.last().unwrap();
        if let Some(ref name) = node.name {
            has_eval_binding = has_eval_binding || &name.name == "eval";
        }
        self.eval_bindings.push(has_eval_binding);
        // If necessary, reading the scope information will amend it.
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_function_expression(&mut self, _path: &Path, _node: &mut EagerFunctionExpression) -> Result<Option<EagerFunctionExpression>, ()> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_eager_arrow_expression(&mut self, _path: &Path, _node: &mut EagerArrowExpression) -> Result<VisitMe<()>, ()> {
        // By default, adopt parent's behavior.
        // If necessary, reading the scope information will amend it.
        let has_eval_binding = *self.eval_bindings.last().unwrap();
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_arrow_expression(&mut self, _path: &Path, _node: &mut EagerArrowExpression) -> Result<Option<EagerArrowExpression>, ()> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_eager_getter(&mut self, _path: &Path, node: &mut EagerGetter) -> Result<VisitMe<()>, ()> {
        // Don't forget that the internal name of the getter may mask `eval`.
        let mut has_eval_binding = *self.eval_bindings.last().unwrap();
        if let PropertyName::LiteralPropertyName(ref name) = node.name {
            has_eval_binding = has_eval_binding || &name.value == "eval";
        }
        // If necessary, reading the scope information will amend it.
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_getter(&mut self, _path: &Path, _node: &mut EagerGetter) -> Result<Option<EagerGetter>, ()> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_eager_setter(&mut self, _path: &Path, node: &mut EagerSetter) -> Result<VisitMe<()>, ()> {
        // Don't forget that the internal name of the setter may mask `eval`.
        let mut has_eval_binding = *self.eval_bindings.last().unwrap();
        if let PropertyName::LiteralPropertyName(ref name) = node.name {
            has_eval_binding = has_eval_binding || &name.value == "eval";
        }
        // If necessary, reading the scope information will amend it.
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_setter(&mut self, _path: &Path, _node: &mut EagerSetter) -> Result<Option<EagerSetter>, ()> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_eager_method(&mut self, _path: &Path, node: &mut EagerMethod) -> Result<VisitMe<()>, ()> {
        // Don't forget that the internal name of the method may mask `eval`.
        let mut has_eval_binding = *self.eval_bindings.last().unwrap();
        if let PropertyName::LiteralPropertyName(ref name) = node.name {
            has_eval_binding = has_eval_binding || &name.value == "eval";
        }
        // If necessary, reading the scope information will amend it.
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_method(&mut self, _path: &Path, _node: &mut EagerMethod) -> Result<Option<EagerMethod>, ()> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_catch_clause(&mut self, _path: &Path, node: &mut CatchClause) -> Result<VisitMe<()>, ()> {
        // Don't forget that the implicitly declared variable may mask `eval`.
        let mut has_eval_binding = *self.eval_bindings.last().unwrap();
        match node.binding {
            Binding::BindingIdentifier(ref binding) => {
                has_eval_binding = has_eval_binding || &binding.name == "eval";
            }
            _ => unimplemented!() // FIXME: Patterns may also mask `eval`.
        }
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_catch_clause(&mut self, _path: &Path, _node: &mut CatchClause) -> Result<Option<CatchClause>, ()> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }



    // Update scopes themselves.
    fn exit_asserted_block_scope(&mut self, _path: &Path, node: &mut AssertedBlockScope) -> Result<Option<AssertedBlockScope>, ()> {
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
        Ok(None)
    }
    fn exit_asserted_var_scope(&mut self, _path: &Path, node: &mut AssertedVarScope) -> Result<Option<AssertedVarScope>, ()> {
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
        Ok(None)
    }
    fn exit_asserted_parameter_scope(&mut self, _path: &Path, node: &mut AssertedParameterScope) -> Result<Option<AssertedParameterScope>, ()> {
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
        Ok(None)
    }
}

impl AnnotationVisitor {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn annotate_script(&mut self, script: &mut Script) {
        // Annotate.

        // At this stage, we may have false positives for `hasDirectEval`.
        script.walk(&mut Path::new(), self)
            .expect("Could not walk script");

        // Cleanup false positives for `hasDirectEval`.
        let mut cleanup = EvalCleanupAnnotator {
            eval_bindings: vec![false]
        };
        script.walk(&mut Path::new(), &mut cleanup)
            .expect("Could not walk script for eval cleanup");
    }
    pub fn annotate(&mut self, ast: &mut JSON) {
        // Import script
        let mut script = Script::import(ast)
            .expect("Invalid script"); // FIXME: Error values would be nicer.

        self.annotate_script(&mut script);

        // Reexport the AST to JSON.
        *ast = script.export();
    }
}
