use ast::*;
use binjs_shared::{IdentifierName, VisitMe};

use std::collections::{HashMap, HashSet};

use itertools::Itertools;

type EnterResult = Result<VisitMe<()>, ScopeError>;
type ExitResult<T> = Result<Option<T>, ScopeError>;

#[derive(Debug)]
pub enum ScopeError {
    /// While analyzing scopes, we exit a binding identifier but there is no binding kind.
    MissingBindingKind,

    /// This name is defined twice in the same scope
    NameIsDefinedTwiceInTheSameScope {
        /// If `true`, the name is a const lexical name.
        const_lexical: bool,
        name: IdentifierName,
    },
}

#[derive(Debug, PartialEq, Eq)]
enum BindingKind {
    Var,
    NonConstLexical,
    ConstLexical,

    // Positional parameter.
    //   * Simple parameter
    //   * Default parameter without destructuring
    PositionalParam {
        // The depth of the path when entering PositionalParameter.
        // Used to check matching binding when leaving.
        depth: usize,

        // The perameter index.
        index: u32,
    },

    // Rest parameter.
    // This is pushed onto the stack whenever it's inside parameter, for the
    // following 2 reasons.
    //   * To handle rest parameter binding correctly,
    //     which doesn't have any enclosing structure
    //   * To mark it's inside parameter, see is_positional_parameter and
    //     is_destructuring_parameter
    RestParam,

    // Destructuring parameter, including default parameter with destructuring.
    DestructuringParam {
        // The depth of the path when entering destructuring parameter,
        // which is ObjectBinding or ArrayBinding directly under parameter
        // or default parameter.
        // Used to check matching binding when leaving.
        depth: usize,
    },
    Bound,
}

struct VarAndLexNames {
    var_names: HashSet<IdentifierName>,
    non_const_lexical_names: HashSet<IdentifierName>,
    const_lexical_names: HashSet<IdentifierName>,
}

enum ParamKind {
    Positional { index: u32, name: IdentifierName },
    Destructuring { name: IdentifierName },
    Rest { name: IdentifierName },
}

pub struct AnnotationVisitor {
    // The following are stacks.
    var_names_stack: Vec<HashSet<IdentifierName>>,
    non_const_lexical_names_stack: Vec<HashSet<IdentifierName>>,
    const_lexical_names_stack: Vec<HashSet<IdentifierName>>,
    params_stack: Vec<Vec<ParamKind>>,

    // The current parameter index for each (nested) function, which is used
    // by PositionalParameter.
    // An item (0) is pushed when entering function, and popped when leaving.
    // The item is incremented when leaving parameter.
    param_indices_stack: Vec<u32>,

    bound_names_stack: Vec<HashSet<IdentifierName>>,
    binding_kind_stack: Vec<BindingKind>,
    apparent_direct_eval_stack: Vec<bool>,
    function_expression_name_stack: Vec<Option<BindingIdentifier>>,

    // 'true' if the free name has already crossed a function boundary
    // 'false' until then.
    free_names_in_block_stack: Vec<HashMap<IdentifierName, bool>>,

    /// A shared reference to `this`.
    this_reference: IdentifierName,
    this_declaration: IdentifierName,
}
impl AnnotationVisitor {
    pub fn new() -> Self {
        let this_reference = IdentifierName::from_str("this");
        Self {
            var_names_stack: Vec::new(),
            non_const_lexical_names_stack: Vec::new(),
            const_lexical_names_stack: Vec::new(),
            params_stack: Vec::new(),
            param_indices_stack: Vec::new(),
            bound_names_stack: Vec::new(),
            binding_kind_stack: Vec::new(),
            apparent_direct_eval_stack: Vec::new(),
            function_expression_name_stack: Vec::new(),
            free_names_in_block_stack: Vec::new(),
            this_reference: this_reference.clone(),
            this_declaration: this_reference,
        }
    }
    fn pop_captured_names(
        &mut self,
        bindings: &[&HashSet<IdentifierName>],
    ) -> HashSet<IdentifierName> {
        let mut captured_names = HashSet::new();
        let my_free_names = self.free_names_in_block_stack.last_mut().unwrap();
        for binding in bindings {
            for name in *binding {
                if let Some(cross_function) = my_free_names.remove(name) {
                    // Free names across nested function boundaries are closed.
                    debug!(target: "annotating", "found captured name {:?}", name);
                    if cross_function {
                        captured_names.insert(name.clone());
                    }
                }
            }
        }

        captured_names
    }

    fn push_free_names(&mut self) {
        self.free_names_in_block_stack.push(HashMap::new());
    }
    fn pop_free_names(
        &mut self,
        bindings: &[&HashSet<IdentifierName>],
        is_leaving_function_scope: bool,
    ) {
        let mut free_names_in_current_block = self.free_names_in_block_stack.pop().unwrap();
        for (name, old_cross_function) in free_names_in_current_block.drain() {
            let is_bound = bindings
                .iter()
                .find(|container| container.contains(&name))
                .is_some();
            if !is_bound {
                // Propagate free names up to the enclosing scope, for further analysis.
                // Actively propagate the closure flag as we go. It could have been set by
                //   A nested scope: old_cross_function
                //   This scope: is_leaving_function_scope
                //   Or, it could have already been in the parent scope from a sibling block.
                // Or everything together, so we don't forget if the binding was closed over.
                if let Some(mut parent_free) = self.free_names_in_block_stack.last_mut() {
                    let my_contribution = old_cross_function || is_leaving_function_scope;
                    parent_free
                        .entry(name)
                        .and_modify(|p| *p = *p || my_contribution)
                        .or_insert(my_contribution);
                }
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

    fn push_block_scope(&mut self, _path: &WalkPath) {
        self.non_const_lexical_names_stack.push(HashSet::new());
        self.const_lexical_names_stack.push(HashSet::new());
        self.push_free_names();
        self.push_direct_eval();
    }
    fn pop_block_scope(&mut self, path: &WalkPath) -> AssertedBlockScope {
        debug!(target: "annotating", "pop_block_scope at {:?}", path);
        let non_const_lexical_names = self.non_const_lexical_names_stack.pop().unwrap();
        let const_lexical_names = self.const_lexical_names_stack.pop().unwrap();

        debug!(target: "annotating", "pop_non_const_scope lex {:?}", non_const_lexical_names);
        debug!(target: "annotating", "pop_const_scope lex {:?}", const_lexical_names);

        let captured_names =
            self.pop_captured_names(&[&non_const_lexical_names, &const_lexical_names]);
        self.pop_free_names(
            &[&non_const_lexical_names, &const_lexical_names],
            /* is_leaving_function_scope = */ false,
        );

        let mut declared_names = vec![];
        for name in non_const_lexical_names.into_iter().sorted() {
            let is_captured = captured_names.contains(&name);
            declared_names.push(AssertedDeclaredName {
                name,
                kind: AssertedDeclaredKind::NonConstLexical,
                is_captured,
            })
        }
        for name in const_lexical_names.into_iter().sorted() {
            let is_captured = captured_names.contains(&name);
            declared_names.push(AssertedDeclaredName {
                name,
                kind: AssertedDeclaredKind::ConstLexical,
                is_captured,
            })
        }

        debug_assert_eq!(
            {
                let as_set: HashSet<_> = declared_names.iter().map(|x| &x.name).collect();
                as_set.len()
            },
            declared_names.len(),
            "Duplicate declared names"
        );

        let has_direct_eval = self.pop_direct_eval();
        AssertedBlockScope {
            declared_names,
            has_direct_eval,
        }
    }

    fn push_incomplete_var_scope(&mut self, _path: &WalkPath) {
        self.var_names_stack.push(HashSet::new());
        self.non_const_lexical_names_stack.push(HashSet::new());
        self.const_lexical_names_stack.push(HashSet::new());
    }
    fn push_var_scope(&mut self, path: &WalkPath) {
        debug!(target: "annotating", "push_var_scope at {:?}", path);
        self.push_incomplete_var_scope(path);
        self.push_direct_eval();
        self.push_free_names();
    }
    fn pop_incomplete_var_scope(&mut self, path: &WalkPath) -> Result<VarAndLexNames, ScopeError> {
        debug!(target: "annotating", "pop_incomplete_var_scope at {:?}", path);
        let var_names = self.var_names_stack.pop().unwrap();
        let non_const_lexical_names = self.non_const_lexical_names_stack.pop().unwrap();
        let const_lexical_names = self.const_lexical_names_stack.pop().unwrap();

        debug!(target: "annotating", "pop_incomplete_var_scope var {:?}", var_names);
        debug!(target: "annotating", "pop_incomplete_var_scope non_const {:?}", non_const_lexical_names);
        debug!(target: "annotating", "pop_incomplete_var_scope const {:?}", const_lexical_names);

        // Check that a name isn't defined twice in the same scope.
        for name in var_names.intersection(&non_const_lexical_names) {
            return Err(ScopeError::NameIsDefinedTwiceInTheSameScope {
                const_lexical: false,
                name: name.clone(),
            });
        }
        for name in var_names.intersection(&const_lexical_names) {
            return Err(ScopeError::NameIsDefinedTwiceInTheSameScope {
                const_lexical: true,
                name: name.clone(),
            });
        }
        Ok(VarAndLexNames {
            var_names,
            non_const_lexical_names,
            const_lexical_names,
        })
    }
    fn pop_var_and_lex_declared_names(
        &mut self,
        path: &WalkPath,
    ) -> Result<Vec<AssertedDeclaredName>, ScopeError> {
        let VarAndLexNames {
            var_names,
            non_const_lexical_names,
            const_lexical_names,
        } = self.pop_incomplete_var_scope(path)?;
        let captured_names =
            self.pop_captured_names(&[&var_names, &non_const_lexical_names, &const_lexical_names]);
        self.pop_free_names(
            &[&var_names, &non_const_lexical_names, &const_lexical_names],
            /* is_leaving_function_scope = */ true,
        );

        let mut declared_names = vec![];
        for name in var_names.into_iter().sorted() {
            let is_captured = captured_names.contains(&name);
            declared_names.push(AssertedDeclaredName {
                name,
                kind: AssertedDeclaredKind::Var,
                is_captured,
            })
        }
        for name in non_const_lexical_names.into_iter().sorted() {
            let is_captured = captured_names.contains(&name);
            declared_names.push(AssertedDeclaredName {
                name,
                kind: AssertedDeclaredKind::NonConstLexical,
                is_captured,
            })
        }
        for name in const_lexical_names.into_iter().sorted() {
            let is_captured = captured_names.contains(&name);
            declared_names.push(AssertedDeclaredName {
                name,
                kind: AssertedDeclaredKind::ConstLexical,
                is_captured,
            })
        }

        debug_assert_eq!(
            {
                let as_set: HashSet<_> = declared_names.iter().map(|x| &x.name).collect();
                as_set.len()
            },
            declared_names.len(),
            "Duplicate declared names"
        );

        Ok(declared_names)
    }

    fn pop_var_scope(&mut self, path: &WalkPath) -> Result<AssertedVarScope, ScopeError> {
        let declared_names = self.pop_var_and_lex_declared_names(path)?;
        let has_direct_eval = self.pop_direct_eval();

        Ok(AssertedVarScope {
            declared_names,
            has_direct_eval,
        })
    }
    fn pop_script_global_scope(
        &mut self,
        path: &WalkPath,
    ) -> Result<AssertedScriptGlobalScope, ScopeError> {
        let declared_names = self.pop_var_and_lex_declared_names(path)?;
        let has_direct_eval = self.pop_direct_eval();

        Ok(AssertedScriptGlobalScope {
            declared_names,
            has_direct_eval,
        })
    }

    fn push_this_captured(&mut self) {
        self.push_free_names();
    }
    fn pop_this_captured(&mut self) -> bool {
        let mut this_names = HashSet::new();
        this_names.insert(self.this_declaration.clone());
        let captured_names = self.pop_captured_names(&[&this_names]);
        self.pop_free_names(&[&this_names], /* is_leaving_function_scope = */ false);
        captured_names.contains(&self.this_declaration)
    }

    fn push_function_name_captured(&mut self) {
        self.push_free_names();
    }
    fn pop_function_name_captured(&mut self, name: IdentifierName) -> bool {
        let mut names = HashSet::new();
        names.insert(name.clone());
        let captured_names = self.pop_captured_names(&[&names]);
        self.pop_free_names(&[&names], /* is_leaving_function_scope = */ false);
        captured_names.contains(&name)
    }

    fn push_param_scope(&mut self, _path: &WalkPath) {
        debug!(target: "annotating", "push_param_scope at {:?}", _path);
        self.params_stack.push(vec![]);
        self.param_indices_stack.push(0);
        self.push_free_names();
        self.push_direct_eval();
    }
    fn pop_param_scope(
        &mut self,
        path: &WalkPath,
        parameter_scope: &AssertedParameterScope,
    ) -> Result<AssertedParameterScope, ScopeError> {
        debug!(target: "annotating", "pop_param_scope at {:?}", path);

        fn to_declaration(param: &ParamKind) -> IdentifierName {
            match param {
                ParamKind::Positional { index: _, name } => name.clone(),
                ParamKind::Destructuring { name } => name.clone(),
                ParamKind::Rest { name } => name.clone(),
            }
        };

        let params = self.params_stack.pop().unwrap();
        self.param_indices_stack.pop();
        let names = params.as_slice().into_iter().map(to_declaration).collect();
        let captured_names = self.pop_captured_names(&[&names]);
        self.pop_free_names(&[&names], /* is_leaving_function_scope = */ false);

        // In the case of `function foo(j) {var j;}`, the `var j` is not the true declaration.
        // Remove it from parameters.
        for name in &names {
            if self.var_names_stack.last_mut().unwrap().remove(name) {
                debug!(target: "annotating", "pop_param_scope removing {:?}", name);
            }
        }

        let len = params.len();
        let mut param_names = Vec::with_capacity(len);
        for param in params.into_iter() {
            let is_captured = captured_names.contains(&to_declaration(&param));
            param_names.push(match param {
                ParamKind::Positional { index, name } => {
                    AssertedMaybePositionalParameterName::AssertedPositionalParameterName(Box::new(
                        AssertedPositionalParameterName {
                            index,
                            name,
                            is_captured,
                        },
                    ))
                }
                ParamKind::Destructuring { name } => {
                    AssertedMaybePositionalParameterName::AssertedParameterName(Box::new(
                        AssertedParameterName { name, is_captured },
                    ))
                }
                ParamKind::Rest { name } => {
                    AssertedMaybePositionalParameterName::AssertedRestParameterName(Box::new(
                        AssertedRestParameterName { name, is_captured },
                    ))
                }
            })
        }

        let has_direct_eval = self.pop_direct_eval();

        debug_assert_eq!(
            {
                let as_set: HashSet<_> = param_names
                    .iter()
                    .map(|x| {
                        use ast::AssertedMaybePositionalParameterName::*;
                        match *x {
                            AssertedPositionalParameterName(ref b) => &b.name,
                            AssertedParameterName(ref b) => &b.name,
                            AssertedRestParameterName(ref b) => &b.name,
                            BinASTStolen => panic!(),
                        }
                    })
                    .collect();
                as_set.len()
            },
            len,
            "Duplicate param names"
        );

        Ok(AssertedParameterScope {
            param_names,
            has_direct_eval,
            is_simple_parameter_list: parameter_scope.is_simple_parameter_list,
        })
    }
    fn push_bound_names_scope(&mut self, _path: &WalkPath) {
        debug!(target: "annotating", "push_bound_names_scope at {:?}", _path);
        self.bound_names_stack.push(HashSet::new());
        self.push_free_names();
        self.push_direct_eval();
    }
    fn pop_bound_names_scope(&mut self, path: &WalkPath) -> AssertedBoundNamesScope {
        debug!(target: "annotating", "pop_bound_names_scope at {:?}", path);

        let names = self.bound_names_stack.pop().unwrap();
        let captured_names = self.pop_captured_names(&[&names]);
        self.pop_free_names(&[&names], /* is_leaving_function_scope = */ false);

        let mut bound_names = Vec::with_capacity(names.len());
        // The order of the bound names is not defined per spec.
        // Since `names` is HashSet which doesn't keep the order of appearance,
        // we sort it alphabetically.
        for name in names.into_iter().sorted() {
            let is_captured = captured_names.contains(&name);
            bound_names.push(AssertedBoundName { name, is_captured })
        }

        debug_assert_eq!(
            {
                let as_set: HashSet<_> = bound_names.iter().map(|x| &x.name).collect();
                as_set.len()
            },
            bound_names.len(),
            "Duplicate declared names"
        );

        let has_direct_eval = self.pop_direct_eval();
        AssertedBoundNamesScope {
            bound_names,
            has_direct_eval,
        }
    }
    fn function_expression_name(&self) -> Option<IdentifierName> {
        match self.function_expression_name_stack.last().unwrap() {
            Some(identifier) => Some(identifier.name.clone()),
            _ => None,
        }
    }
    fn push_function_expression_name(&mut self, name: Option<BindingIdentifier>) {
        self.function_expression_name_stack.push(name);
    }
    fn pop_function_expression_name(&mut self) {
        self.function_expression_name_stack.pop();
    }

    fn exit_lazy_or_eager_function_declaration<T>(
        &mut self,
        path: &WalkPath,
        name: &BindingIdentifier,
    ) -> ExitResult<T> {
        // If a name declaration was specified, remove it from `unknown`.
        let name = name.name.clone();

        // Var scope is already committed in exit_function_or_method_contents.
        // The function's name is not actually bound in the function; the outer var binding is used.
        // Anything we do from this point affects the scope outside the function.

        // 1. If the declaration is at the toplevel, the name is declared as a `var`.
        // 2. If the declaration is in a function's toplevel block, the name is declared as a `var`.
        // 3. Otherwise, the name is declared as a `let`.
        debug!(target: "annotating", "exit_lazy_or_eager_function_declaration sees {:?} at {:?}", name, path.get(0));
        match path.get(0).expect("Impossible AST walk") {
            &WalkPathItem {
                field: ASTField::Statements,
                interface: ASTNode::Script,
            }
            | &WalkPathItem {
                field: ASTField::Statements,
                interface: ASTNode::Module,
            } => {
                // Case 1.
                debug!(target: "annotating", "exit_lazy_or_eager_function_declaration says it's a var (case 1)");
                self.var_names_stack.last_mut().unwrap().insert(name);
            }
            &WalkPathItem {
                field: ASTField::Body,
                interface: ASTNode::GetterContents,
            }
            | &WalkPathItem {
                field: ASTField::Body,
                interface: ASTNode::SetterContents,
            }
            | &WalkPathItem {
                field: ASTField::Body,
                interface: ASTNode::ArrowExpressionContentsWithFunctionBody,
            }
            | &WalkPathItem {
                field: ASTField::Body,
                interface: ASTNode::ArrowExpressionContentsWithExpression,
            }
            | &WalkPathItem {
                field: ASTField::Body,
                interface: ASTNode::FunctionExpressionContents,
            }
            | &WalkPathItem {
                field: ASTField::Body,
                interface: ASTNode::FunctionOrMethodContents,
            } => {
                // Case 2.
                debug!(target: "annotating", "exit_eager_function_declaration says it's a var (case 2)");
                self.var_names_stack.last_mut().unwrap().insert(name);
            }
            _ => {
                // Case 3.
                debug!(target: "annotating", "exit_lazy_or_eager_function_declaration says it's a non const lexical (case 3)");
                self.non_const_lexical_names_stack
                    .last_mut()
                    .unwrap()
                    .insert(name);
            }
        }
        Ok(None)
    }
}

fn is_positional_parameter(visitor: &AnnotationVisitor, path: &WalkPath) -> bool {
    match visitor.binding_kind_stack.last() {
        // BindingKind::RestParam should be on the binding kind stack whenever
        // it's inside parameter, even if it's setter which cannot have rest.
        Some(BindingKind::RestParam) => {}
        _ => {
            return false;
        }
    };
    match path.get(0) {
        Some(&WalkPathItem {
            interface: ASTNode::BindingWithInitializer,
            field: ASTField::Binding,
        }) => match path.get(1) {
            Some(&WalkPathItem {
                interface: ASTNode::SetterContents,
                field: ASTField::Param,
            })
            | Some(&WalkPathItem {
                interface: ASTNode::FormalParameters,
                field: ASTField::Items,
            }) => true,
            _ => false,
        },
        Some(&WalkPathItem {
            interface: ASTNode::SetterContents,
            field: ASTField::Param,
        })
        | Some(&WalkPathItem {
            interface: ASTNode::FormalParameters,
            field: ASTField::Items,
        }) => true,
        _ => false,
    }
}
fn maybe_enter_positional_parameter(visitor: &mut AnnotationVisitor, path: &WalkPath) {
    if !is_positional_parameter(visitor, path) {
        return;
    }

    // `is_positional_parameter` above already checked this is inside function.
    // `param_indices_stack` should contain an item for the function.
    let i = *visitor.param_indices_stack.last().unwrap();
    visitor
        .binding_kind_stack
        .push(BindingKind::PositionalParam {
            depth: path.len(),
            index: i,
        });
}
fn maybe_exit_positional_parameter(visitor: &mut AnnotationVisitor, path: &WalkPath) {
    if visitor.param_indices_stack.is_empty() {
        return;
    }

    // We can unwrap because `param_indices_stack` is not empty.
    let i = *visitor.param_indices_stack.last().unwrap();
    match visitor.binding_kind_stack.last() {
        Some(BindingKind::PositionalParam { depth, index })
            if *depth == path.len() && *index == i => {}
        _ => {
            return;
        }
    }

    *(visitor.param_indices_stack.last_mut().unwrap()) = i + 1;
    visitor.binding_kind_stack.pop();
}

fn is_destructuring_parameter(visitor: &AnnotationVisitor, path: &WalkPath) -> bool {
    match visitor.binding_kind_stack.last() {
        // See is_positional_parameter.
        Some(BindingKind::RestParam) => {}
        _ => {
            return false;
        }
    };
    match path.get(0) {
        Some(&WalkPathItem {
            interface: ASTNode::BindingWithInitializer,
            field: ASTField::Binding,
        }) => match path.get(1) {
            Some(&WalkPathItem {
                interface: ASTNode::SetterContents,
                field: ASTField::Param,
            })
            | Some(&WalkPathItem {
                interface: ASTNode::FormalParameters,
                field: ASTField::Items,
            }) => true,
            _ => false,
        },
        Some(&WalkPathItem {
            interface: ASTNode::SetterContents,
            field: ASTField::Param,
        })
        | Some(&WalkPathItem {
            interface: ASTNode::FormalParameters,
            field: ASTField::Items,
        }) => true,
        _ => false,
    }
}
fn maybe_enter_destructuring_parameter(visitor: &mut AnnotationVisitor, path: &WalkPath) {
    if !is_destructuring_parameter(visitor, path) {
        return;
    }

    visitor
        .binding_kind_stack
        .push(BindingKind::DestructuringParam { depth: path.len() });
}
fn maybe_exit_destructuring_parameter(visitor: &mut AnnotationVisitor, path: &WalkPath) {
    match visitor.binding_kind_stack.last() {
        Some(BindingKind::DestructuringParam { depth }) if *depth == path.len() => {}
        _ => {
            return;
        }
    }

    *(visitor.param_indices_stack.last_mut().unwrap()) += 1;
    visitor.binding_kind_stack.pop();
}

impl Visitor<ScopeError> for AnnotationVisitor {
    // Identifiers

    fn exit_call_expression(
        &mut self,
        _path: &WalkPath,
        node: &mut CallExpression,
    ) -> ExitResult<CallExpression> {
        if let ExpressionOrSuper::IdentifierExpression(ref id) = node.callee {
            if id.name == "eval" {
                *self.apparent_direct_eval_stack.last_mut().unwrap() = true;
            }
        }
        Ok(None)
    }

    fn exit_identifier_expression(
        &mut self,
        _path: &WalkPath,
        node: &mut IdentifierExpression,
    ) -> ExitResult<IdentifierExpression> {
        debug!(target: "annotating", "exit_identifier_expression {:?} at {:?}", node.name, _path);
        let names = self.free_names_in_block_stack.last_mut().unwrap();
        if !names.contains_key(&node.name) {
            names.insert(node.name.clone(), false);
        }
        Ok(None)
    }
    fn exit_this_expression(
        &mut self,
        _path: &WalkPath,
        _node: &mut ThisExpression,
    ) -> ExitResult<ThisExpression> {
        debug!(target: "annotating", "exit_this_expression at {:?}", _path);
        let names = self.free_names_in_block_stack.last_mut().unwrap();
        if !names.contains_key(&self.this_reference) {
            names.insert(self.this_reference.clone(), false);
        }
        Ok(None)
    }

    fn exit_assignment_target_identifier(
        &mut self,
        _path: &WalkPath,
        node: &mut AssignmentTargetIdentifier,
    ) -> ExitResult<AssignmentTargetIdentifier> {
        let names = self.free_names_in_block_stack.last_mut().unwrap();
        if !names.contains_key(&node.name) {
            names.insert(node.name.clone(), false);
        }
        Ok(None)
    }

    fn enter_binding_identifier(
        &mut self,
        path: &WalkPath,
        _node: &mut BindingIdentifier,
    ) -> EnterResult {
        maybe_enter_positional_parameter(self, path);
        Ok(VisitMe::HoldThis(()))
    }

    fn exit_binding_identifier(
        &mut self,
        path: &WalkPath,
        node: &mut BindingIdentifier,
    ) -> ExitResult<BindingIdentifier> {
        match path.get(0) {
            Some(&WalkPathItem {
                interface: ASTNode::EagerFunctionDeclaration,
                field: ASTField::Name,
            })
            | Some(&WalkPathItem {
                interface: ASTNode::LazyFunctionDeclaration,
                field: ASTField::Name,
            })
            | Some(&WalkPathItem {
                interface: ASTNode::EagerFunctionExpression,
                field: ASTField::Name,
            })
            | Some(&WalkPathItem {
                interface: ASTNode::LazyFunctionExpression,
                field: ASTField::Name,
            })
            | Some(&WalkPathItem {
                interface: ASTNode::EagerMethod,
                field: ASTField::Name,
            })
            | Some(&WalkPathItem {
                interface: ASTNode::LazyMethod,
                field: ASTField::Name,
            })
            | Some(&WalkPathItem {
                interface: ASTNode::EagerGetter,
                field: ASTField::Name,
            })
            | Some(&WalkPathItem {
                interface: ASTNode::LazyGetter,
                field: ASTField::Name,
            })
            | Some(&WalkPathItem {
                interface: ASTNode::EagerSetter,
                field: ASTField::Name,
            })
            | Some(&WalkPathItem {
                interface: ASTNode::LazySetter,
                field: ASTField::Name,
            }) => {
                // Function names are special.
                // They are handled in the respective `exit_*` methods.
                return Ok(None);
            }
            _ => {}
        }
        debug!(target: "annotating", "exit_binding_identifier – marking {name:?} as {kind:?} at {path:?}",
                name = node.name,
                kind = self.binding_kind_stack.last(),
                path = path);
        if self.binding_kind_stack.last().is_none() {
            return Err(ScopeError::MissingBindingKind);
        }
        let declaration = node.name.clone();
        match *self.binding_kind_stack.last().unwrap() {
            BindingKind::Var => {
                self.var_names_stack.last_mut().unwrap().insert(declaration);
            }
            BindingKind::NonConstLexical => {
                self.non_const_lexical_names_stack
                    .last_mut()
                    .unwrap()
                    .insert(declaration);
            }
            BindingKind::ConstLexical => {
                self.const_lexical_names_stack
                    .last_mut()
                    .unwrap()
                    .insert(declaration);
            }
            BindingKind::PositionalParam { depth: _, index } => {
                self.params_stack
                    .last_mut()
                    .unwrap()
                    .push(ParamKind::Positional {
                        index,
                        name: declaration,
                    });
            }
            BindingKind::RestParam => {
                self.params_stack
                    .last_mut()
                    .unwrap()
                    .push(ParamKind::Rest { name: declaration });
            }
            BindingKind::DestructuringParam { depth: _ } => {
                self.params_stack
                    .last_mut()
                    .unwrap()
                    .push(ParamKind::Destructuring { name: declaration });
            }
            BindingKind::Bound => {
                self.bound_names_stack
                    .last_mut()
                    .unwrap()
                    .insert(declaration);
            }
        }
        maybe_exit_positional_parameter(self, path);
        Ok(None)
    }

    // Blocks

    fn enter_block(&mut self, path: &WalkPath, _node: &mut Block) -> EnterResult {
        self.push_block_scope(path);
        Ok(VisitMe::HoldThis(()))
    }

    fn exit_block(&mut self, path: &WalkPath, node: &mut Block) -> ExitResult<Block> {
        node.scope = self.pop_block_scope(path);
        Ok(None)
    }

    fn enter_script(&mut self, path: &WalkPath, _node: &mut Script) -> EnterResult {
        self.push_var_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_script(&mut self, path: &WalkPath, node: &mut Script) -> ExitResult<Script> {
        node.scope = self.pop_script_global_scope(path)?;
        Ok(None)
    }

    fn enter_module(&mut self, path: &WalkPath, _node: &mut Module) -> EnterResult {
        self.push_var_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_module(&mut self, path: &WalkPath, node: &mut Module) -> ExitResult<Module> {
        node.scope = self.pop_var_scope(path)?;
        Ok(None)
    }

    // Try/Catch
    fn enter_catch_clause(&mut self, path: &WalkPath, _node: &mut CatchClause) -> EnterResult {
        self.binding_kind_stack.push(BindingKind::Bound);

        // We need to differentiate between
        // `var ex; try { ... } catch(ex) { ... }` (both instances of `ex` are distinct)
        // and
        // `try { ... } catch(ex) { var ex; ... }` (both instances of `ex` are the same)
        // so we introduce a var scope in `catch(ex)`, as if `catch(ex) { ... }` was
        // a function.

        self.push_incomplete_var_scope(path);
        self.push_bound_names_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_catch_clause(
        &mut self,
        path: &WalkPath,
        node: &mut CatchClause,
    ) -> ExitResult<CatchClause> {
        assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::Bound));
        node.binding_scope = self.pop_bound_names_scope(path);
        let var_scope = self.pop_incomplete_var_scope(path)?;

        assert_eq!(var_scope.non_const_lexical_names.len(), 0, "The implicit scope of a catch should not contain lexically declared names. This requires an actual block.");
        assert_eq!(var_scope.const_lexical_names.len(), 0, "The implicit scope of a catch should not contain lexically declared names. This requires an actual block.");

        // Propagate any var_declared_names.
        for name in var_scope.var_names.into_iter() {
            debug!(target: "annotating", "exit_catch_clause: reinserting {:?}", name);
            self.var_names_stack.last_mut().unwrap().insert(name);
        }

        Ok(None)
    }

    // Explicit variable declarations

    fn enter_for_in_of_binding(
        &mut self,
        _path: &WalkPath,
        node: &mut ForInOfBinding,
    ) -> EnterResult {
        let kind = match node.kind {
            VariableDeclarationKind::Let => BindingKind::NonConstLexical,
            VariableDeclarationKind::Const => BindingKind::ConstLexical,
            VariableDeclarationKind::Var => BindingKind::Var,
        };
        self.binding_kind_stack.push(kind);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_for_in_of_binding(
        &mut self,
        _path: &WalkPath,
        node: &mut ForInOfBinding,
    ) -> ExitResult<ForInOfBinding> {
        let kind = match node.kind {
            VariableDeclarationKind::Let => BindingKind::NonConstLexical,
            VariableDeclarationKind::Const => BindingKind::ConstLexical,
            VariableDeclarationKind::Var => BindingKind::Var,
        };
        assert_eq!(self.binding_kind_stack.pop().unwrap(), kind);
        Ok(None)
    }

    fn enter_variable_declaration(
        &mut self,
        _path: &WalkPath,
        node: &mut VariableDeclaration,
    ) -> EnterResult {
        let kind = match node.kind {
            VariableDeclarationKind::Let => BindingKind::NonConstLexical,
            VariableDeclarationKind::Const => BindingKind::ConstLexical,
            VariableDeclarationKind::Var => BindingKind::Var,
        };
        self.binding_kind_stack.push(kind);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_variable_declaration(
        &mut self,
        _path: &WalkPath,
        node: &mut VariableDeclaration,
    ) -> ExitResult<VariableDeclaration> {
        let kind = match node.kind {
            VariableDeclarationKind::Let => BindingKind::NonConstLexical,
            VariableDeclarationKind::Const => BindingKind::ConstLexical,
            VariableDeclarationKind::Var => BindingKind::Var,
        };
        assert_eq!(self.binding_kind_stack.pop().unwrap(), kind);
        Ok(None)
    }

    // Functions, methods, arguments.
    fn enter_setter_contents(
        &mut self,
        path: &WalkPath,
        _node: &mut SetterContents,
    ) -> EnterResult {
        // Just like FormalParameters, push BindingKind::RestParam to mark
        // it's inside parameter.
        // See enter_formal_parameters and is_positional_parameter for more
        // details.
        self.binding_kind_stack.push(BindingKind::RestParam);

        self.push_param_scope(path);
        self.push_var_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_setter_contents(
        &mut self,
        path: &WalkPath,
        node: &mut SetterContents,
    ) -> ExitResult<SetterContents> {
        assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::RestParam));
        // Commit parameter scope and var scope.
        node.parameter_scope = self.pop_param_scope(path, &node.parameter_scope)?;
        node.body_scope = self.pop_var_scope(path)?;

        Ok(None)
    }

    fn enter_getter_contents(
        &mut self,
        path: &WalkPath,
        _node: &mut GetterContents,
    ) -> EnterResult {
        self.push_var_scope(path);
        Ok(VisitMe::HoldThis(()))
    }

    fn exit_getter_contents(
        &mut self,
        path: &WalkPath,
        node: &mut GetterContents,
    ) -> ExitResult<GetterContents> {
        node.body_scope = self.pop_var_scope(path)?;

        Ok(None)
    }

    fn enter_function_or_method_contents(
        &mut self,
        path: &WalkPath,
        _node: &mut FunctionOrMethodContents,
    ) -> EnterResult {
        self.push_var_scope(path);
        self.push_param_scope(path);
        self.push_this_captured();
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_function_or_method_contents(
        &mut self,
        path: &WalkPath,
        node: &mut FunctionOrMethodContents,
    ) -> ExitResult<FunctionOrMethodContents> {
        debug!(target: "annotating", "exit_function_or_method_contents at {:?}", path);
        node.is_this_captured = self.pop_this_captured();

        // Commit parameter scope and var scope.
        node.parameter_scope = self.pop_param_scope(path, &node.parameter_scope)?;
        node.body_scope = self.pop_var_scope(path)?;

        Ok(None)
    }

    fn enter_arrow_expression_contents_with_function_body(
        &mut self,
        path: &WalkPath,
        _node: &mut ArrowExpressionContentsWithFunctionBody,
    ) -> EnterResult {
        self.push_var_scope(path);
        self.push_param_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_arrow_expression_contents_with_function_body(
        &mut self,
        path: &WalkPath,
        node: &mut ArrowExpressionContentsWithFunctionBody,
    ) -> ExitResult<ArrowExpressionContentsWithFunctionBody> {
        // Commit parameter scope and var scope.
        node.parameter_scope = self.pop_param_scope(path, &node.parameter_scope)?;
        node.body_scope = self.pop_var_scope(path)?;

        Ok(None)
    }
    fn enter_arrow_expression_contents_with_expression(
        &mut self,
        path: &WalkPath,
        _node: &mut ArrowExpressionContentsWithExpression,
    ) -> EnterResult {
        self.push_var_scope(path);
        self.push_param_scope(path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_arrow_expression_contents_with_expression(
        &mut self,
        path: &WalkPath,
        node: &mut ArrowExpressionContentsWithExpression,
    ) -> ExitResult<ArrowExpressionContentsWithExpression> {
        // Commit parameter scope and var scope.
        node.parameter_scope = self.pop_param_scope(path, &node.parameter_scope)?;
        node.body_scope = self.pop_var_scope(path)?;

        Ok(None)
    }

    fn enter_function_expression_contents(
        &mut self,
        path: &WalkPath,
        _node: &mut FunctionExpressionContents,
    ) -> EnterResult {
        // For parity, push function name in its proper place as the outermost binding.
        // See comment below in exit_function_expression_contents for more context.
        if let Some(_) = self.function_expression_name() {
            self.push_function_name_captured();
        }
        self.push_var_scope(path);
        self.push_param_scope(path);
        self.push_this_captured();
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_function_expression_contents(
        &mut self,
        path: &WalkPath,
        node: &mut FunctionExpressionContents,
    ) -> ExitResult<FunctionExpressionContents> {
        node.is_this_captured = self.pop_this_captured();

        node.parameter_scope = self.pop_param_scope(path, &node.parameter_scope)?;
        node.body_scope = self.pop_var_scope(path)?;

        // Wait to pop the function name until after we handle vars and params.
        // If there's a shadowing var, the var is captured, not the function name.
        // Since there's only one stack for used names, and it expects things in scope-order,
        // we risk getting confused and claiming the wrong binding was closed over.
        if let Some(ref name) = self.function_expression_name() {
            node.is_function_name_captured = self.pop_function_name_captured(name.clone());
        } else {
            node.is_function_name_captured = false;
        }
        Ok(None)
    }

    fn enter_eager_function_expression(
        &mut self,
        _path: &WalkPath,
        node: &mut EagerFunctionExpression,
    ) -> EnterResult {
        self.push_function_expression_name(node.name.clone());
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_function_expression(
        &mut self,
        _path: &WalkPath,
        _node: &mut EagerFunctionExpression,
    ) -> ExitResult<EagerFunctionExpression> {
        self.pop_function_expression_name();
        Ok(None)
    }

    fn enter_lazy_function_expression(
        &mut self,
        _path: &WalkPath,
        node: &mut LazyFunctionExpression,
    ) -> EnterResult {
        self.push_function_expression_name(node.name.clone());
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_lazy_function_expression(
        &mut self,
        _path: &WalkPath,
        _node: &mut LazyFunctionExpression,
    ) -> ExitResult<LazyFunctionExpression> {
        self.pop_function_expression_name();
        Ok(None)
    }

    fn exit_eager_function_declaration(
        &mut self,
        path: &WalkPath,
        node: &mut EagerFunctionDeclaration,
    ) -> ExitResult<EagerFunctionDeclaration> {
        debug!(target: "annotating", "exit_eager_function_declaration {:?} at {:?}", node.name.name, path);
        self.exit_lazy_or_eager_function_declaration(path, &node.name)
    }

    fn exit_lazy_function_declaration(
        &mut self,
        path: &WalkPath,
        node: &mut LazyFunctionDeclaration,
    ) -> ExitResult<LazyFunctionDeclaration> {
        self.exit_lazy_or_eager_function_declaration(path, &node.name)
    }

    fn enter_formal_parameters(
        &mut self,
        _path: &WalkPath,
        _node: &mut FormalParameters,
    ) -> EnterResult {
        // Handle rest parameter field here.  Other parameters are handled in
        // BindingIdentifier/ObjectBinding/ArrayBinding.
        self.binding_kind_stack.push(BindingKind::RestParam);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_formal_parameters(
        &mut self,
        _path: &WalkPath,
        _node: &mut FormalParameters,
    ) -> ExitResult<FormalParameters> {
        assert_matches!(self.binding_kind_stack.pop(), Some(BindingKind::RestParam));
        Ok(None)
    }

    fn enter_object_binding(&mut self, path: &WalkPath, _node: &mut ObjectBinding) -> EnterResult {
        maybe_enter_destructuring_parameter(self, path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_object_binding(
        &mut self,
        path: &WalkPath,
        _node: &mut ObjectBinding,
    ) -> ExitResult<ObjectBinding> {
        maybe_exit_destructuring_parameter(self, path);
        Ok(None)
    }

    fn enter_array_binding(&mut self, path: &WalkPath, _node: &mut ArrayBinding) -> EnterResult {
        maybe_enter_destructuring_parameter(self, path);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_array_binding(
        &mut self,
        path: &WalkPath,
        _node: &mut ArrayBinding,
    ) -> ExitResult<ArrayBinding> {
        maybe_exit_destructuring_parameter(self, path);
        Ok(None)
    }
}

/// Perform a second pass to cleanup incorrect instances of `eval`.
struct EvalCleanupAnnotator {
    /// `true` if name `eval` was bound at this level or higher in the tree.
    eval_bindings: Vec<bool>,
}

impl Visitor<ScopeError> for EvalCleanupAnnotator {
    // FIXME: Anything that has a scope (including CatchClause and its invisible scope) should push an `eval_bindings`.
    // on entering, pop it on exit.
    fn enter_eager_function_declaration(
        &mut self,
        _path: &WalkPath,
        _node: &mut EagerFunctionDeclaration,
    ) -> EnterResult {
        // By default, adopt parent's behavior.
        // If necessary, reading the scope information will amend it.
        let has_eval_binding = *self.eval_bindings.last().unwrap();
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_function_declaration(
        &mut self,
        _path: &WalkPath,
        _node: &mut EagerFunctionDeclaration,
    ) -> ExitResult<EagerFunctionDeclaration> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_eager_function_expression(
        &mut self,
        _path: &WalkPath,
        node: &mut EagerFunctionExpression,
    ) -> EnterResult {
        // By default, adopt parent's behavior.
        // Don't forget that the internal name of the function may mask `eval`.
        let mut has_eval_binding = *self.eval_bindings.last().unwrap();
        if let Some(ref name) = node.name {
            has_eval_binding = has_eval_binding || name.name == "eval";
        }
        self.eval_bindings.push(has_eval_binding);
        // If necessary, reading the scope information will amend it.
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_function_expression(
        &mut self,
        _path: &WalkPath,
        _node: &mut EagerFunctionExpression,
    ) -> ExitResult<EagerFunctionExpression> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_eager_arrow_expression_with_function_body(
        &mut self,
        _path: &WalkPath,
        _node: &mut EagerArrowExpressionWithFunctionBody,
    ) -> EnterResult {
        // By default, adopt parent's behavior.
        // If necessary, reading the scope information will amend it.
        let has_eval_binding = *self.eval_bindings.last().unwrap();
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_arrow_expression_with_function_body(
        &mut self,
        _path: &WalkPath,
        _node: &mut EagerArrowExpressionWithFunctionBody,
    ) -> ExitResult<EagerArrowExpressionWithFunctionBody> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_eager_arrow_expression_with_expression(
        &mut self,
        _path: &WalkPath,
        _node: &mut EagerArrowExpressionWithExpression,
    ) -> EnterResult {
        // By default, adopt parent's behavior.
        // If necessary, reading the scope information will amend it.
        let has_eval_binding = *self.eval_bindings.last().unwrap();
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_arrow_expression_with_expression(
        &mut self,
        _path: &WalkPath,
        _node: &mut EagerArrowExpressionWithExpression,
    ) -> ExitResult<EagerArrowExpressionWithExpression> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_eager_getter(&mut self, _path: &WalkPath, node: &mut EagerGetter) -> EnterResult {
        // Don't forget that the internal name of the getter may mask `eval`.
        let mut has_eval_binding = *self.eval_bindings.last().unwrap();
        if let PropertyName::LiteralPropertyName(ref name) = node.name {
            has_eval_binding = has_eval_binding || name.value == "eval";
        }
        // If necessary, reading the scope information will amend it.
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_getter(
        &mut self,
        _path: &WalkPath,
        _node: &mut EagerGetter,
    ) -> ExitResult<EagerGetter> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_eager_setter(&mut self, _path: &WalkPath, node: &mut EagerSetter) -> EnterResult {
        // Don't forget that the internal name of the setter may mask `eval`.
        let mut has_eval_binding = *self.eval_bindings.last().unwrap();
        if let PropertyName::LiteralPropertyName(ref name) = node.name {
            has_eval_binding = has_eval_binding || name.value == "eval";
        }
        // If necessary, reading the scope information will amend it.
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_setter(
        &mut self,
        _path: &WalkPath,
        _node: &mut EagerSetter,
    ) -> ExitResult<EagerSetter> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_eager_method(&mut self, _path: &WalkPath, node: &mut EagerMethod) -> EnterResult {
        // Don't forget that the internal name of the method may mask `eval`.
        let mut has_eval_binding = *self.eval_bindings.last().unwrap();
        if let PropertyName::LiteralPropertyName(ref name) = node.name {
            has_eval_binding = has_eval_binding || name.value == "eval";
        }
        // If necessary, reading the scope information will amend it.
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_eager_method(
        &mut self,
        _path: &WalkPath,
        _node: &mut EagerMethod,
    ) -> ExitResult<EagerMethod> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }
    fn enter_catch_clause(&mut self, _path: &WalkPath, node: &mut CatchClause) -> EnterResult {
        // Don't forget that the implicitly declared variable may mask `eval`.
        let mut has_eval_binding = *self.eval_bindings.last().unwrap();
        match node.binding {
            Binding::BindingIdentifier(ref binding) => {
                has_eval_binding = has_eval_binding || binding.name == "eval";
            }
            _ => unimplemented!(), // FIXME: Patterns may also mask `eval`.
        }
        self.eval_bindings.push(has_eval_binding);
        Ok(VisitMe::HoldThis(()))
    }
    fn exit_catch_clause(
        &mut self,
        _path: &WalkPath,
        _node: &mut CatchClause,
    ) -> ExitResult<CatchClause> {
        self.eval_bindings.pop().unwrap();
        Ok(None)
    }

    // Update scopes themselves.
    fn exit_asserted_block_scope(
        &mut self,
        _path: &WalkPath,
        node: &mut AssertedBlockScope,
    ) -> ExitResult<AssertedBlockScope> {
        if node
            .declared_names
            .iter()
            .find(|e| e.name == "eval")
            .is_some()
        {
            *self.eval_bindings.last_mut().unwrap() = true;
        }
        if *self.eval_bindings.last().unwrap() {
            node.has_direct_eval = false;
        }
        Ok(None)
    }
    fn exit_asserted_var_scope(
        &mut self,
        _path: &WalkPath,
        node: &mut AssertedVarScope,
    ) -> ExitResult<AssertedVarScope> {
        if node
            .declared_names
            .iter()
            .find(|e| e.name == "eval")
            .is_some()
        {
            *self.eval_bindings.last_mut().unwrap() = true;
        }
        if *self.eval_bindings.last().unwrap() {
            node.has_direct_eval = false;
        }
        Ok(None)
    }
    fn exit_asserted_parameter_scope(
        &mut self,
        _path: &WalkPath,
        node: &mut AssertedParameterScope,
    ) -> ExitResult<AssertedParameterScope> {
        if node
            .param_names
            .iter()
            .find(|param| match param {
                AssertedMaybePositionalParameterName::AssertedPositionalParameterName(ref p) => {
                    p.name == "eval"
                }
                AssertedMaybePositionalParameterName::AssertedParameterName(ref p) => {
                    p.name == "eval"
                }
                AssertedMaybePositionalParameterName::AssertedRestParameterName(ref p) => {
                    p.name == "eval"
                }
                AssertedMaybePositionalParameterName::BinASTStolen => panic!(),
            })
            .is_some()
        {
            *self.eval_bindings.last_mut().unwrap() = true;
        }
        if *self.eval_bindings.last().unwrap() {
            node.has_direct_eval = false;
        }
        Ok(None)
    }
}

impl AnnotationVisitor {
    pub fn annotate_script(&mut self, script: &mut Script) -> Result<(), ScopeError> {
        // Annotate.

        // At this stage, we may have false positives for `hasDirectEval`.
        script.walk(&mut WalkPath::new(), self)?;

        // Cleanup false positives for `hasDirectEval`.
        let mut cleanup = EvalCleanupAnnotator {
            eval_bindings: vec![false],
        };
        script.walk(&mut WalkPath::new(), &mut cleanup)?;
        Ok(())
    }
}
