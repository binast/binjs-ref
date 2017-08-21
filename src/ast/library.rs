//! A library of specifications for versions of JavaScript.

use ast::grammar::*;
use ast::annotation::*;
use util::JSONGetter;

use serde_json::Value as JSON;
use serde_json;

type Object = serde_json::Map<String, JSON>;

pub static NULL_NAME: &'static str = "BINJS:Null";
pub static SCOPE_NAME: &'static str = "BINJS:Scope";
pub static BINJS_VAR_NAME: &'static str = "BINJS:VarDeclaredNames";
pub static BINJS_LET_NAME: &'static str = "BINJS:LetDeclaredNames";
pub static BINJS_CONST_NAME: &'static str = "BINJS:ConstDeclaredNames";
pub static BINJS_CAPTURED_NAME: &'static str = "BINJS:CapturedNames";
pub static BINJS_DIRECT_EVAL: &'static str = "BINJS:HasDirectEval";

/// The set of features requested for a syntax.
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Level {
    /// Empty syntax, for testing purposes.
    Minimal,
    /// All the features for ES5.
    ES5,
    /// All the features of the latest version of JavaScript.
    Latest,
    // FIXME: More levels to be implemented.
}

fn uses_strict(object: &Object) -> bool {
    if let Some(directives) = object["directives"].as_array() {
        if directives
            .iter()
            .find(|v| {
                if let Some("use strict") = v["value"]["value"].as_str() {
                    true
                } else {
                    false
                }
            }
            ).is_some()
        {
            return true;
        }
    }
    false
}

/// Special nodes used by BINJS. Not visible at source level.
fn setup_binjs(syntax: &mut SyntaxBuilder) -> Box<Annotator> {
    let field_var_decl_names = syntax.field_name(BINJS_VAR_NAME);
    let field_let_declared_names = syntax.field_name(BINJS_LET_NAME);
    let field_const_declared_names = syntax.field_name(BINJS_CONST_NAME);
    let field_captured_names = syntax.field_name(BINJS_CAPTURED_NAME);
    let field_has_direct_eval = syntax.field_name(BINJS_DIRECT_EVAL);

    // Special value used to encode `null` AST nodes (*not* `null` literals).
    let binjs_null = syntax.node_name(NULL_NAME);
    syntax.add_kinded_interface(&binjs_null).unwrap();

    // A scope, used to attach annotations.
    let binjs_scope = syntax.node_name(SCOPE_NAME);
    syntax.add_kinded_interface(&binjs_scope).unwrap()
        .with_field(&field_var_decl_names, Type::string().array())
        .with_field(&field_let_declared_names, Type::string().array())
        .with_field(&field_const_declared_names, Type::string().array())
        .with_field(&field_captured_names, Type::string().array())
        .with_field(&field_has_direct_eval, Type::bool());

    struct BaseAnnotator;
    impl Annotator for BaseAnnotator {
        fn name(&self) -> String {
            "BaseAnnotator".to_string()
        }

        fn process_references(&self, me: &Annotator, ctx: &mut Context<RefContents>, object: &mut Object) -> Result<(), ASTError> {
            // Default case: no free variable here, just process.
            for (name, field) in object.iter_mut() {
                if let Ok(mut ctx) = ctx.enter_field(name) {
                    me.process_references_aux(me, &mut ctx, field)?;
                }
            }
            // Dropping `ctx` will propagate everything to the parent.
            Ok(())
        }
        fn process_declarations(&self, me: &Annotator, ctx: &mut Context<DeclContents>, object: &mut Object) -> Result<(), ASTError> {
            // Default case: no declaration here, just process.
            for (name, field) in object.iter_mut() {
                if let Ok(mut ctx) = ctx.enter_field(name) {
                    me.process_declarations_aux(me, &mut ctx, field)?;
                }
            }
            Ok(())
        }
    }

    Box::new(BaseAnnotator)
}

/// Hardcoding for the subset of https://github.com/babel/babylon/blob/master/ast/spec.md
/// dedicated to ES5.
fn setup_es5(syntax: &mut SyntaxBuilder, parent: Box<Annotator>) -> Box<Annotator> {
    // Interface names (by alphabetical order)
    let array_expression = syntax.node_name("ArrayExpression");
    let assignment_expression = syntax.node_name("AssignmentExpression");
    let assignment_operator = syntax.node_name("AssignmentOperator");
    let binary_expression = syntax.node_name("BinaryExpression");
    let binary_operator = syntax.node_name("BinaryOperator");
    let binjs_scope = syntax.node_name(SCOPE_NAME);
    let block_statement = syntax.node_name("BlockStatement");
    let boolean_literal = syntax.node_name("BooleanLiteral");
    let break_statement = syntax.node_name("BreakStatement");
    let call_expression = syntax.node_name("CallExpression");
    let catch_clause = syntax.node_name("CatchClause");
    let conditional_expression = syntax.node_name("ConditionalExpression");
    let continue_statement = syntax.node_name("ContinueStatement");
    let debugger_statement = syntax.node_name("DebuggerStatement");
    let declaration = syntax.node_name("Declaration");
    let directive = syntax.node_name("Directive");
    let directive_literal = syntax.node_name("DirectiveLiteral");
    let do_while_statement = syntax.node_name("DoWhileStatement");
    let empty_statement = syntax.node_name("EmptyStatement");
    let expression = syntax.node_name("Expression");
    let expression_statement = syntax.node_name("ExpressionStatement");
    let for_statement = syntax.node_name("ForStatement");
    let for_in_statement = syntax.node_name("ForInStatement");
    let function = syntax.node_name("Function");
    let function_expression = syntax.node_name("FunctionExpression");
    let function_declaration = syntax.node_name("FunctionDeclaration");
    let identifier = syntax.node_name("Identifier");
    let if_statement = syntax.node_name("IfStatement");
    let labeled_statement = syntax.node_name("LabeledStatement");
    let literal = syntax.node_name("Literal");
    let logical_expression = syntax.node_name("LogicalExpression");
    let logical_operator = syntax.node_name("LogicalOperator");
    let member_expression = syntax.node_name("MemberExpression");
    let node = syntax.node_name("Node");
    let new_expression = syntax.node_name("NewExpression");
    let null_literal = syntax.node_name("NullLiteral");
    let numeric_literal = syntax.node_name("NumericLiteral");
    let object_expression = syntax.node_name("ObjectExpression");
    let object_member = syntax.node_name("ObjectMember");
    let object_method = syntax.node_name("ObjectMethod");
    let object_property = syntax.node_name("ObjectProperty");
    let pattern = syntax.node_name("Pattern");
    let program = syntax.node_name("Program");
    let property_kind = syntax.node_name("PropertyKind");
    let regexp_literal = syntax.node_name("RegExpLiteral");
    let return_statement = syntax.node_name("ReturnStatement");
    let sequence_expression = syntax.node_name("SequenceExpression");
    let string_literal = syntax.node_name("StringLiteral");
    let statement = syntax.node_name("Statement");
    let switch_case = syntax.node_name("SwitchCase");
    let switch_statement = syntax.node_name("SwitchStatement");
    let this_expression = syntax.node_name("ThisExpression");
    let throw_statement = syntax.node_name("ThrowStatement");
    let try_statement = syntax.node_name("TryStatement");
    let unary_expression = syntax.node_name("UnaryExpression");
    let unary_operator = syntax.node_name("UnaryOperator");
    let update_expression = syntax.node_name("UpdateExpression");
    let update_operator = syntax.node_name("UpdateOperator");
    let variable_declaration = syntax.node_name("VariableDeclaration");
    let variable_declarator = syntax.node_name("VariableDeclarator");
    let variable_kind = syntax.node_name("VariableKind");
    let while_statement = syntax.node_name("WhileStatement");
    let with_statement = syntax.node_name("WithStatement");


    // Field names (by alphabetical order)
    let field_alternate = syntax.field_name("alternate");
    let field_argument = syntax.field_name("argument");
    let field_arguments = syntax.field_name("arguments");
    let field_binjs_scope = syntax.field_name(SCOPE_NAME);
    let field_block = syntax.field_name("block");
    let field_callee = syntax.field_name("callee");
    let field_cases = syntax.field_name("cases");
    let field_consequent = syntax.field_name("consequent");
    let field_computed = syntax.field_name("computed");
    let field_body = syntax.field_name("body");
    let field_declarations = syntax.field_name("declarations");
    let field_directives = syntax.field_name("directives");
    let field_discriminant = syntax.field_name("discriminant");
    let field_elements = syntax.field_name("elements");
    let field_expression = syntax.field_name("expression");
    let field_expressions = syntax.field_name("expressions");
    let field_finalizer = syntax.field_name("finalizer");
    let field_flags = syntax.field_name("flags");
    let field_handler = syntax.field_name("handler");
    let field_id = syntax.field_name("id");
    let field_init = syntax.field_name("init");
    let field_key = syntax.field_name("key");
    let field_kind = syntax.field_name("kind");
    let field_label = syntax.field_name("label");
    let field_left = syntax.field_name("left");
    let field_name = syntax.field_name("name");
    let field_object = syntax.field_name("object");
    let field_operator = syntax.field_name("operator");
    let field_param = syntax.field_name("param");
    let field_params = syntax.field_name("params");
    let field_pattern = syntax.field_name("pattern");
    let field_prefix = syntax.field_name("prefix");
    let field_properties = syntax.field_name("properties");
    let field_property = syntax.field_name("property");
    let field_right = syntax.field_name("right");
    let field_test = syntax.field_name("test");
    let field_update = syntax.field_name("update");
    let field_value = syntax.field_name("value");


    // Node objects
    syntax.add_virtual_interface(&node).unwrap();

    // Identifiers
    syntax.add_kinded_interface(&identifier).unwrap()
        .with_field(&field_name, Type::string())
        .with_parent(&expression)
        .with_parent(&pattern);

    // Literals
    syntax.add_kinded_interface(&literal).unwrap()
        .with_parent(&expression);
    syntax.add_kinded_interface(&string_literal).unwrap()
        .with_field(&field_value, Type::string())
        .with_parent(&literal);
    syntax.add_kinded_interface(&boolean_literal).unwrap()
        .with_field(&field_value, Type::bool())
        .with_parent(&literal);
    syntax.add_kinded_interface(&null_literal).unwrap()
        .with_parent(&literal);
    syntax.add_kinded_interface(&numeric_literal).unwrap()
        .with_field(&field_value, Type::number())
        .with_parent(&literal);
    syntax.add_kinded_interface(&regexp_literal).unwrap()
        .with_field(&field_pattern, Type::string())
        .with_field(&field_flags, Type::string())
        .with_parent(&literal);
    syntax.add_kinded_interface(&directive_literal).unwrap()
        .with_parent(&string_literal);

    // Programs
    syntax.add_kinded_interface(&program).unwrap()
        .with_field(&field_body, Type::interface(&statement).array())
        .with_field(&field_binjs_scope, Type::interface(&binjs_scope).or_null().unwrap())
        .with_field(&field_directives, Type::interface(&directive).array())
        .with_parent(&node);

    // Functions (shared between function declaration, function statement, function expression)
    // Note that the scope information is stored as part of `field_body`.
    syntax.add_virtual_interface(&function).unwrap()
        .with_field(&field_id, Type::interface(&identifier).or_null().unwrap())
        .with_field(&field_params, Type::interface(&pattern).array())
        .with_field(&field_body, Type::interface(&block_statement))
        .with_field(&field_binjs_scope, Type::interface(&binjs_scope).or_null().unwrap())
        .with_parent(&node);

    // Statements
    syntax.add_virtual_interface(&statement).unwrap()
        .with_parent(&node);

    syntax.add_kinded_interface(&empty_statement).unwrap()
        .with_parent(&statement);

    syntax.add_kinded_interface(&block_statement).unwrap()
        .with_field(&field_body, Type::interface(&statement).array())
        .with_field(&field_binjs_scope, Type::interface(&binjs_scope).or_null().unwrap())
        .with_field(&field_directives, Type::interface(&directive).array()) // FIXME: This seems like a waste of space. Shouldn't we allow this only for functions?
        .with_parent(&statement);

    syntax.add_kinded_interface(&expression_statement).unwrap()
        .with_field(&field_expression, Type::interface(&expression))
        .with_parent(&statement);

    syntax.add_kinded_interface(&debugger_statement).unwrap()
        .with_parent(&statement);

    syntax.add_kinded_interface(&with_statement).unwrap()
        .with_field(&field_object, Type::interface(&expression))
        .with_field(&field_body, Type::interface(&statement))
        .with_parent(&statement);

    // Control flow
    syntax.add_kinded_interface(&return_statement).unwrap()
        .with_field(&field_argument, Type::interface(&expression).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&labeled_statement).unwrap()
        .with_field(&field_label, Type::interface(&identifier))
        .with_field(&field_body, Type::interface(&statement))
        .with_parent(&statement);

    syntax.add_kinded_interface(&break_statement).unwrap()
        .with_field(&field_label, Type::interface(&identifier).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&continue_statement).unwrap()
        .with_field(&field_label, Type::interface(&identifier).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&if_statement).unwrap()
        .with_field(&field_test, Type::interface(&expression))
        .with_field(&field_consequent, Type::interface(&statement))
        .with_field(&field_alternate, Type::interface(&statement).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&switch_statement).unwrap()
        .with_field(&field_discriminant, Type::interface(&expression))
        .with_field(&field_cases, Type::interface(&switch_case).array())
        .with_parent(&statement);

    syntax.add_kinded_interface(&switch_case).unwrap()
        .with_field(&field_test, Type::interface(&expression).or_null().unwrap())
        .with_field(&field_consequent, Type::interface(&statement).array())
        .with_parent(&statement);

    syntax.add_kinded_interface(&throw_statement).unwrap()
        .with_field(&field_argument, Type::interface(&expression))
        .with_parent(&statement);

    syntax.add_kinded_interface(&try_statement).unwrap()
        .with_field(&field_block, Type::interface(&block_statement))
        .with_field(&field_handler, Type::interface(&catch_clause).or_null().unwrap())
        .with_field(&field_finalizer, Type::interface(&block_statement).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&catch_clause).unwrap()
        .with_field(&field_param, Type::interface(&pattern))
        .with_field(&field_body, Type::interface(&block_statement))
        .with_parent(&node);

    syntax.add_kinded_interface(&while_statement).unwrap()
        .with_field(&field_test, Type::interface(&expression))
        .with_field(&field_body, Type::interface(&statement))
        .with_parent(&statement);

    syntax.add_kinded_interface(&do_while_statement).unwrap()
        .with_field(&field_body, Type::interface(&statement))
        .with_field(&field_test, Type::interface(&expression))
        .with_parent(&statement);

    syntax.add_kinded_interface(&for_statement).unwrap()
        .with_field(&field_init, Type::interfaces(&[
            &variable_declaration,
            &expression
        ]).or_null().unwrap())
        .with_field(&field_test, Type::interface(&expression).or_null().unwrap())
        .with_field(&field_update, Type::interface(&expression).or_null().unwrap())
        .with_field(&field_body, Type::interface(&statement))
        .with_field(&field_binjs_scope, Type::interface(&binjs_scope).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&for_in_statement).unwrap()
        .with_field(&field_left, Type::interfaces(&[
            &variable_declaration,
            &pattern
        ]))
        .with_field(&field_right, Type::interface(&expression))
        .with_field(&field_body, Type::interface(&statement))
        .with_field(&field_binjs_scope, Type::interface(&binjs_scope).or_null().unwrap())
        .with_parent(&statement);

    // Declarations
    syntax.add_virtual_interface(&declaration).unwrap()
        .with_parent(&statement);

    syntax.add_kinded_interface(&function_declaration).unwrap()
        .with_field(&field_id, Type::interface(&identifier))
        .with_parent(&function)
        .with_parent(&declaration);

    syntax.add_kinded_interface(&variable_declaration).unwrap()
        .with_field(&field_declarations, Type::interface(&variable_declarator).array())
        .with_field(&field_kind, Type::enumeration(&variable_kind))
        .with_parent(&declaration);

    syntax.add_enum(&variable_kind).unwrap()
        .with_strings(&[
            "let",
            "var",
            "const"
        ]);

    syntax.add_kinded_interface(&variable_declarator).unwrap()
        .with_field(&field_id, Type::interface(&pattern))
        .with_field(&field_init, Type::interface(&expression).or_null().unwrap())
        .with_parent(&node);

    // Expressions
    syntax.add_virtual_interface(&expression).unwrap()
        .with_parent(&node);

    syntax.add_kinded_interface(&this_expression).unwrap()
        .with_parent(&expression);

    syntax.add_kinded_interface(&array_expression).unwrap()
        .with_field(&field_elements, Type::interface(&expression).or_null().unwrap().array())
        .with_parent(&expression);

    syntax.add_kinded_interface(&object_expression).unwrap()
        .with_field(&field_properties, Type::interfaces(&[
            &object_property,
            &object_method
        ]).array())
        .with_parent(&expression);

    syntax.add_virtual_interface(&object_member).unwrap()
        .with_field(&field_key, Type::interface(&expression))
        .with_field(&field_computed, Type::bool()) // FIXME: Do we need this?
        .with_parent(&node);

    syntax.add_kinded_interface(&object_property).unwrap()
        .with_field(&field_value, Type::interface(&expression))
        .with_parent(&object_member);

    syntax.add_kinded_interface(&object_method).unwrap()
        .with_field(&field_kind, Type::enumeration(&property_kind))
        .with_parent(&object_member)
        .with_parent(&function);

    syntax.add_enum(&property_kind).unwrap()
        .with_strings(&[
            "get",
            "set",
            "method"
        ]);

    syntax.add_kinded_interface(&function_expression).unwrap()
        .with_parent(&expression)
        .with_parent(&function);

    syntax.add_kinded_interface(&unary_expression).unwrap()
        .with_field(&field_operator, Type::enumeration(&unary_operator))
        .with_field(&field_prefix, Type::bool())
        .with_field(&field_argument, Type::interface(&expression))
        .with_parent(&expression);

    syntax.add_enum(&unary_operator).unwrap()
        .with_strings(&[
            "-",
            "+",
            "!",
            "~",
            "typeof",
            "void",
            "delete"
        ]);

    syntax.add_kinded_interface(&update_expression).unwrap()
        .with_field(&field_operator, Type::enumeration(&update_operator))
        .with_field(&field_argument, Type::interface(&expression))
        .with_field(&field_prefix, Type::bool())
        .with_parent(&expression);

    syntax.add_enum(&update_operator).unwrap()
        .with_strings(&[
            "++",
            "--"
        ]);

    syntax.add_kinded_interface(&binary_expression).unwrap()
        .with_field(&field_operator, Type::enumeration(&binary_operator))
        .with_field(&field_left, Type::interface(&expression))
        .with_field(&field_right, Type::interface(&expression))
        .with_parent(&expression);

    syntax.add_enum(&binary_operator).unwrap()
        .with_strings(&[
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
            "instanceof"
        ]);

    syntax.add_kinded_interface(&assignment_expression).unwrap()
        .with_field(&field_operator, Type::enumeration(&assignment_operator))
        .with_field(&field_left, Type::interfaces(&[
            &pattern,
            &expression
        ]))
        .with_field(&field_right, Type::interface(&expression))
        .with_parent(&expression);

    syntax.add_enum(&assignment_operator).unwrap()
        .with_strings(&[
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
            "&="
        ]);

    syntax.add_kinded_interface(&logical_expression).unwrap()
        .with_field(&field_operator, Type::enumeration(&logical_operator))
        .with_field(&field_left, Type::interface(&expression))
        .with_field(&field_right, Type::interface(&expression))
        .with_parent(&expression);

    syntax.add_enum(&logical_operator).unwrap()
        .with_strings(&[
            "||",
            "&&"
        ]);

    syntax.add_kinded_interface(&member_expression).unwrap()
        .with_field(&field_object, Type::interface(&expression))
        .with_field(&field_property, Type::interface(&expression))
        .with_field(&field_computed, Type::bool())
        .with_parent(&expression);

    syntax.add_kinded_interface(&conditional_expression).unwrap()
        .with_field(&field_test, Type::interface(&expression))
        .with_field(&field_alternate, Type::interface(&expression))
        .with_field(&field_consequent, Type::interface(&expression))
        .with_parent(&expression);

    syntax.add_kinded_interface(&call_expression).unwrap()
        .with_field(&field_callee, Type::interface(&expression))
        .with_field(&field_arguments, Type::interface(&expression).array())
        .with_parent(&expression);

    syntax.add_kinded_interface(&new_expression).unwrap()
        .with_field(&field_callee, Type::interface(&expression))
        .with_field(&field_arguments, Type::interface(&expression).array())
        .with_parent(&expression);

    syntax.add_kinded_interface(&sequence_expression).unwrap()
        .with_field(&field_expressions, Type::interface(&expression).array())
        .with_parent(&expression);

    syntax.add_virtual_interface(&pattern).unwrap()
        .with_parent(&node);

    syntax.add_kinded_interface(&directive).unwrap()
        .with_field(&field_value, Type::interface(&directive_literal))
        .with_parent(&node);

    struct ES5Annotator {
        parent: Box<Annotator>,
    }
    impl Annotator for ES5Annotator {
        fn name(&self) -> String {
            "ES5Annotator".to_string()
        }

        fn process_declarations(&self, me: &Annotator, ctx: &mut Context<DeclContents>, object: &mut Object) -> Result<(), ASTError> {
            use ast::annotation::*;
            match ctx.kind_str() {
                "Identifier" => {
                    // Collect the name of the identifier.
                    let name = object.get_string("name", "Field `name` of `Identifier`")?;
                    let parent = match ctx.contents().parent() {
                        Some(parent) => parent,
                        None => return Ok(()) // If we are at toplevel, we don't really care about all this.
                    };
                    let mut parent = parent.borrow_mut();
                    match parent.kind_str() {
                        // FIXME: This would probably be much nicer if we had an iterator of ancestors.
                        "FunctionDeclaration" | "ObjectMethod" | "FunctionExpression" => {
                            if let Some("id") = parent.field_str() {
                                // 1. If the declaration is at the toplevel, this is a `var`.
                                // 2. If the declaration is in a function's toplevel block, this is a `var`.
                                // 3. Otherwise, this is a `let`.
                                if let Some(grand) = parent.parent() {
                                    let mut grand = grand.borrow_mut();
                                    if let "BlockStatement" = grand.kind_str() {
                                        if let Some(grandgrand) = grand.parent() {
                                            let grandgrand = grandgrand.borrow();
                                            match grandgrand.kind_str() {
                                                "FunctionDeclaration" | "ObjectMethod" | "FunctionExpression" => {
                                                    // Case 2.
                                                    grand.add_var_name(name)
                                                }
                                                _ => grand.add_let_name(name)
                                            }
                                        } else {
                                            grand.add_let_name(name)
                                        }
                                    } else {
                                        grand.add_var_name(name)
                                    }
                                } else {
                                    // Case 1.
                                    ctx.add_var_name(name);
                                }
                            }
                            // Otherwise, skip. We cannot declare variables in params. Well, not
                            // variables that deserve being stored.
                        },
                        "ForStatement" => {
                            // Handle `for (c = 0; ...; ...)` where `c` is declared implicitly.
                            if let Some("init") = parent.field_str() {
                                if !parent.is_lex_bound(&name) {
                                    parent.add_var_name(name);
                                }
                            }
                        }
                        "ForInStatement" => {
                            // Handle `for (c in ...)` where `c` is declared implicitly.
                            if let Some("left") = parent.field_str() {
                                if !parent.is_lex_bound(&name) {
                                    parent.add_var_name(name);
                                }
                            }
                        }
                        "VariableDeclarator" => {
                            if let Some("id") = parent.field_str() {
                                match parent.scope_kind() {
                                    ScopeKind::VarDecl => parent.add_var_name(name),
                                    ScopeKind::LetDecl => parent.add_let_name(name),
                                    ScopeKind::ConstDecl => parent.add_const_name(name),
                                    _ => return Err(ASTError::InvalidScope)
                                }
                            }
                            // Otherwise, skip. We cannot declare variables in `init`.
                        }
                        _ => {
                            // Ignore identifier.
                        }
                    }
                }
                "CatchClause" => {
                    ctx.set_scope_kind(ScopeKind::LetDecl);
                    self.parent.process_declarations(me, ctx, object)?;
                }
                "VariableDeclaration" => {
                    // Configure the scope kind.
                    let scope_kind = {
                        let variable_kind = object.get_string("kind", "Field `kind` of `VariableDeclaration`")?;
                        match variable_kind {
                            "let" => ScopeKind::LetDecl,
                            "const" => ScopeKind::ConstDecl,
                            "var" => ScopeKind::VarDecl,
                            _ => return Err(ASTError::invalid_value(
                                object.get("kind").unwrap(), // Checked just above.
                                "let | var | const"
                            ))
                        }
                    };
                    ctx.set_scope_kind(scope_kind);

                    // Then proceed as usual.
                    self.parent.process_declarations(me, ctx, object)?;
                }
                "BlockStatement" | "ForStatement" | "ForInStatement" => {
                    // Adopt usual behavior.
                    self.parent.process_declarations(me, ctx, object)?;

                    // Store available information.
                    ctx.store(object);

                    // Drop LexDecl, keep VarDecl
                    ctx.clear_lex_names();
                }
                "Program" => {
                    // Check directives
                    if uses_strict(object) {
                        ctx.set_uses_strict(true);
                    }

                    // Adopt usual behavior.
                    self.parent.process_declarations(me, ctx, object)?;

                    // Store available information.
                    ctx.store(object);
                }
                "FunctionDeclaration" | "ObjectMethod" | "FunctionExpression"  => {
                    // Check directives.
                    let uses_strict = ctx.uses_strict() ||
                        if let Some(body) = object["body"].as_object() {
                            uses_strict(body)
                        } else {
                            false
                        };
                    if uses_strict {
                        ctx.set_uses_strict(true);
                        // Babel allows any string to be used as a directive.
                        // This doesn't seem canonical.
                        object["body"].as_object_mut().unwrap().insert("directives".to_string(), json!([{
                            "type": "Directive",
                            "value": {
                                "type": "DirectiveLiteral",
                                "value": "use strict"
                            }
                        }]));
                    } else {
                        object["body"].as_object_mut().unwrap().insert("directives".to_string(), json!([]));
                    }

                    // Adopt usual behavior.
                    self.parent.process_declarations(me, ctx, object)?;

                    // Store available information.
                    ctx.store(object);

                    // Drop LexDecl and VarDecl
                    ctx.clear_lex_names();
                    ctx.clear_var_names();
                }
                _ => {
                    // Default to parent implementation.
                    // Note that we still pass `me` to ensure dynamic dispatch.
                    self.parent.process_declarations(me, ctx, object)?
                }
            }
            Ok(())
        }

        fn process_references(&self, me: &Annotator, ctx: &mut Context<RefContents>, object: &mut Object) -> Result<(), ASTError> {
            match ctx.kind_str() {
                "Identifier" => {
                    // There are three sorts of identifiers:
                    // 1. declaring a variable;
                    // 2. referencing a variable;
                    // 3. misc stuff that are not variables (e.g. fields keys, labels).
                    //
                    // With the exception of parameters and recursive functions, we have
                    // handled 1. in the previous pass, so we're just going to skip all
                    // declarations other than these two.
                    //
                    // We just skip any use of identifiers that is not a variable.

                    let name = object.get_string("name", "Field `name` of `Identifier`")?;
                    let parent = match ctx.contents().parent() {
                        Some(parent) => parent,
                        None => return Ok(()) // If we are at toplevel, we don't really care about all this.
                    };
                    let mut parent = parent.borrow_mut();
                    match parent.kind_str() {
                        "FunctionDeclaration" | "ObjectMethod" | "FunctionExpression" => {
                            match parent.field_str() {
                                Some("id") | Some("params") => {
                                    let fun_scope = parent.fun_scope()
                                        .expect("Expected a fun scope");
                                    fun_scope.borrow_mut().add_binding(name);
                                }
                                _ => return Err(ASTError::InvalidField("<FIXME: specify field>".to_string())),
                            }
                        }
                        "CatchClause" => {
                            if let Some("param") = parent.field_str() {
                                parent.add_binding(name)
                            } else {
                                return Err(ASTError::InvalidField("<FIXME: specify field>".to_string()))
                            }
                        }
                        "CallExpression" if name == "eval" => {
                            if let Some("callee") = parent.field_str() {
                                if !parent.is_bound("eval") {
                                    parent.add_direct_eval()
                                }
                            } else {
                                parent.add_free_name(name)
                            }
                        }
                        "LabeledStatement" | "BreakStatement" | "ContinueStatement" => {
                            // Ignore identifier, not a variable.
                        }
                        "VariableDeclarator" => {
                            if let Some("id") = parent.field_str() {
                                // Variable declaration, already handled.
                                assert!(parent.is_bound(&name), "Variable {} is declared explicitly, should have been marked as bound in the previous pass. Bound variables: {:?}", name, parent.bindings());
                            } else {
                                ctx.add_free_name(name)
                            }
                        }
                        "ForStatement" => {
                            if let Some("init") = parent.field_str() {
                                // Variable declaration, already handled.
                                assert!(parent.is_bound(&name), "Variable {} is declared by `for(;;)`, should have been marked as bound in the previous pass", name);
                            } else {
                                ctx.add_free_name(name)
                            }
                        }
                        "ForInStatement" => {
                            if let Some("left") = parent.field_str() {
                                // Variable declaration, already handled.
                                assert!(parent.is_bound(&name), "Variable {} is declared by `for(in)`, should have been marked as bound in the previous pass", name);
                            } else {
                                ctx.add_free_name(name)
                            }
                        }
                        "ExpressionStatement" | "WithStatement" | "ReturnStatement"
                            | "IfStatement" | "SwitchStatement" | "SwitchCase"
                            | "ThrowStatement" | "WhileStatement" | "DoWhileStatement"
                            | "ArrayExpression"
                            | "ObjectProperty" | "ObjectExpression" | "UnaryExpression" | "UpdateExpression"
                            | "BinaryExpression" | "AssignmentExpression" | "LogicalExpression"
                            | "MemberExpression" | "ConditionalExpression" | "CallExpression"
                            | "NewExpression" | "SequenceExpression" => {
                            ctx.add_free_name(name);
                        }
                        _ => {
                            panic!("I didn't expect to see a Identifier (namely \"{}\") in {} {:?}", name, parent.kind_str(), parent.field_str())
                        }
                    };
                }
                "ForStatement" | "ForInStatement" | "BlockStatement" | "Program" =>
                {
                    // Simply load the stored bindings, then handle fields.
                    ctx.load(object);
                    self.parent.process_references(me, ctx, object)?;
                    ctx.store(object)
                },
                "FunctionExpression" | "FunctionDeclaration" | "ObjectMethod" => {
                    ctx.use_as_fun_scope();

                    // Make sure that we handle the function id and the params before the body,
                    // as they generally introduce bindings.
                    for name in &["id", "params", "body"] {
                        if let Some(mut field) = object.get_mut(*name) {
                            if let Ok(mut ctx) = ctx.enter_field(name) {
                                me.process_references_aux(me, &mut ctx, field)?;
                            }
                        }
                    }

                    // Store information on the function itself.
                    ctx.store(object);
                    // Dropping `ctx` will convert free variables into
                    // free-in-nested-function variables.
                }
                "ObjectProperty" => {
                    // Simply ignore field `key`.
                    // It is specified as an `Expression`, which doesn't really make sense, as it
                    // must be either a literal or an identifier that is not a variable.
                    me.process_references_field(me, ctx, object, "value")?
                }
                "MemberExpression" => {
                    me.process_references_field(me, ctx, object, "object")?;
                    let computed = object.get_bool("computed", "Field `computed` of `MemberExpression`")?;
                    if computed {
                        // Just an expression, which could .
                        me.process_references_field(me, ctx, object, "property")?;
                    } else {
                        // Normally, `property` is an identifier, skip it.
                        // Sanity check.
                        let property = object.get_object_mut("property", "Field `property` of `MemberExpression`")?;
                        let kind = property.get_string("type", "Field `type` of `MemberExpression[\"property\"]`")?;
                        if kind != "Identifier" {
                            return Err(ASTError::InvalidValue {
                                got: kind.to_owned(),
                                expected: "Identifier".to_string()
                            });
                        }
                    }
                }
                _ => {
                    // Default to parent implementation.
                    // Note that we still pass `me` to ensure dynamic dispatch.
                    self.parent.process_references(me, ctx, object)?
                }
            }
            Ok(())
        }
    }

    Box::new(ES5Annotator {
        parent
    })
}


/// Construct a syntax for a specific version of JavaScript.
pub fn syntax(level: Level) -> Syntax {
    let mut builder = SyntaxBuilder::new();

    let program    = builder.node_name("Program");
    let binjs_null = builder.kind_name(NULL_NAME);

    let base_annotator = setup_binjs(&mut builder);

    let annotator = match level {
        Level::Minimal => {
            builder.add_virtual_interface(&program).unwrap();
            base_annotator
        }
        Level::ES5
        | Level::Latest => {
            setup_es5(&mut builder, base_annotator)
        }
    };
    builder.into_syntax(SyntaxOptions {
        root: &program,
        null: &binjs_null,
        annotator
    })
}



#[test]
fn test_syntax() {
    syntax(Level::Minimal);
    syntax(Level::ES5);
    syntax(Level::Latest);
}
