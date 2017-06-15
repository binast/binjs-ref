//! A library of specifications for versions of JavaScript.

use ast::grammar::*;
use ast::annotation;
use ast::annotation::*;
use util::JSONGetter;

use serde_json::Value as JSON;
use serde_json;

type Object = serde_json::Map<String, JSON>;

pub static NULL_NAME: &'static str = "BINJS:Null";
pub static SCOPE_NAME: &'static str = "BINJS:Scope";
pub static BINJS_VAR_NAME: &'static str = "BINJS:VarDeclaredNames";
pub static BINJS_LEX_NAME: &'static str = "BinJS:LexicallyDeclaredNames";
pub static BINJS_CAPTURED_NAME: &'static str = "BinJS:CapturedNames";
pub static BINJS_DIRECT_EVAL: &'static str = "BinJS::HasDirectEval";

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

/// Special nodes used by BINJS. Not visible at source level.
fn setup_binjs(syntax: &mut SyntaxBuilder) -> Box<Annotator> {
    let field_var_decl_names = syntax.field_name(BINJS_VAR_NAME);
    let field_lexically_declared_names = syntax.field_name(BINJS_LEX_NAME);
    let field_captured_names = syntax.field_name(BINJS_CAPTURED_NAME);
    let field_has_direct_eval = syntax.field_name(BINJS_DIRECT_EVAL);

    // Special value used to encode `null` AST nodes (*not* `null` literals).
    let binjs_null = syntax.node_name(NULL_NAME);
    syntax.add_kinded_interface(&binjs_null).unwrap();

    // A scope, used to attach annotations.
    let binjs_scope = syntax.node_name(SCOPE_NAME);
    syntax.add_virtual_interface(&binjs_scope).unwrap()
        .with_field(&field_var_decl_names, Type::string().array())
        .with_field(&field_lexically_declared_names, Type::string().array())
        .with_field(&field_captured_names, Type::string().array())
        .with_field(&field_has_direct_eval, Type::bool());

    struct BaseAnnotator;
    impl Annotator for BaseAnnotator {
        fn name(&self) -> String {
            "BaseAnnotator".to_string()
        }

        fn process_references_obj(&self, me: &Annotator, ctx: &mut RefContext, object: &mut Object, kind: &str) -> Result<(), ASTError> {
            println!("BaseAnnotator::process_references_obj {} {}", kind, me.name());
            // Default case: no free variable here, just process.
            let mut ctx = ctx.enter(annotation::RefPosition::other(kind));
            for (_, field) in object.iter_mut() {
                me.process_references(me, &mut ctx, field)?;
            }
            // Dropping `ctx` will propagate everything to the parent.
            Ok(())
        }
        fn process_declarations_obj(&self, me: &Annotator, ctx: &mut annotation::DeclContext, object: &mut Object, kind: &str) -> Result<(), ASTError> {
            println!("BaseAnnotator::process_declarations_obj {} {}", kind, me.name());
            // Default case: no declaration here, just process.
            let mut ctx = ctx.enter(annotation::DeclPosition::other(kind));
            for (_, field) in object.iter_mut() {
                me.process_declarations(me, &mut ctx, field)?;
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
    let field_block = syntax.field_name("block");
    let field_callee = syntax.field_name("callee");
    let field_cases = syntax.field_name("cases");
    let field_consequent = syntax.field_name("consequent");
    let field_computed = syntax.field_name("computed");
    let field_body = syntax.field_name("body");
    let field_declarations = syntax.field_name("declarations");
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

    // Programs
    syntax.add_kinded_interface(&program).unwrap()
        .with_field(&field_body, Type::Array(Box::new(Type::interface(&statement))))
        .with_parent(&node);

    // Functions (shared between function declaration, function statement, function expression)
    // Note that the scope information is stored as part of `field_body`.
    syntax.add_virtual_interface(&function).unwrap()
        .with_field(&field_id, Type::interfaces(&[&identifier]).or_null().unwrap())
        .with_field(&field_params, Type::interface(&pattern).array())
        .with_field(&field_body, Type::interface(&block_statement))
        .with_parent(&node);

    // Statements
    syntax.add_virtual_interface(&statement).unwrap()
        .with_parent(&node);

    syntax.add_kinded_interface(&empty_statement).unwrap()
        .with_parent(&statement);

    syntax.add_kinded_interface(&block_statement).unwrap()
        .with_field(&field_body, Type::interface(&statement).array())
        .with_parent(&statement)
        .with_parent(&binjs_scope);

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
        .with_parent(&statement)
        .with_parent(&binjs_scope);

    syntax.add_kinded_interface(&for_in_statement).unwrap()
        .with_field(&field_left, Type::interfaces(&[
            &variable_declaration,
            &pattern
        ]))
        .with_field(&field_right, Type::interface(&expression))
        .with_field(&field_body, Type::interface(&statement))
        .with_parent(&statement)
        .with_parent(&binjs_scope);

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


    struct ES5Annotator {
        parent: Box<Annotator>,
    }
    impl Annotator for ES5Annotator {
        fn name(&self) -> String {
            "ES5Annotator".to_string()
        }

        fn process_declarations_obj(&self, me: &Annotator, ctx: &mut annotation::DeclContext, object: &mut Object, kind: &str) -> Result<(), ASTError> {
            use ast::annotation::*;
            println!("ES5Annotator::process_declarations_obj {}", kind);
            match kind {
                "Identifier" => {
                    // Collect the name of the identifier.
                    let name = object.get_string("name", "Field `name` of `Identifier`")?;
                    ctx.add_identifier(name)?
                }
                "BlockStatement" => {
                    let mut ctx = ctx.enter(DeclPosition::Block);
                    me.process_declarations_field(me, &mut ctx, object, "body")?;

                    // Store available information.
                    ctx.store(object);

                    // Dropping `ctx` ensures that LexDecl names are not propagated.
                    // However, VarDecl names are propagated.
                }
                "FunctionDeclaration" | "ObjectMethod" | "FunctionExpression" => {
                    {
                        {
                            let mut ctx = ctx.enter(DeclPosition::Function);
                            {
                                // At this stage, we are not interested in `params` – except to detect whether
                                // they may reference `eval`.
                                //
                                // (we could check whether `eval` is also one of the arguments, to weed out
                                // some other false positives, but this would complicate things).
                                let mut ctx = ctx.enter(DeclPosition::FunctionArguments);
                                me.process_declarations_field(me, &mut ctx, object, "params")?;
                            }
                            {
                                // Now the body may contain anything.
                                // Do NOT enter a new context for the body, we need to know that we are at
                                // the toplevel of the function.
                                me.process_declarations_field(me, &mut ctx, object, "body")?;
                            }
                            ctx.store(object);
                        }

                        // Dropping `ctx` ensures that LexicallyDeclaredNames and VarDeclaredNames are
                        // not propagated.

                        // Now export the `id`, if available.
                        if kind == "FunctionDeclaration" {
                            // ES specifications turn function declarations into a VarDecl if it happens
                            // at the function's toplevel, a LexDecl otherwise.
                            let position = if let DeclPosition::Function = ctx.position() {
                                DeclPosition::VarDecl
                            } else {
                                DeclPosition::LexDecl
                            };
                            let mut ctx = ctx.enter(position);
                            me.process_declarations_field(me, &mut ctx, object, "id")?;
                        }

                        // We do not store the scope information manually, as it is stored as part
                        // of `field_body`.
                    }
                }
                "VariableDeclarator" => {
                    // Do NOT enter a ctx for `id`.
                    {
                        me.process_declarations_field(me, ctx, object, "id")?;
                    }
                    // DO enter a ctx for `init`, if specified.
                    if let Some(init) = object.get_mut("init") {
                        let mut ctx = ctx.enter(DeclPosition::Expression);
                        me.process_declarations(me, &mut ctx, init)?;
                    }
                }
                "VariableDeclaration" => {
                    let scope = object.get_string("kind", "Field `kind` of `VariableDeclaration`")
                        .map(str::to_string)?; // Avoid maintaining references.
                    let position = match scope.as_ref() {
                        "let" | "const" => Some(annotation::DeclPosition::LexDecl),
                        "var" => Some(annotation::DeclPosition::VarDecl),
                        _ => None
                    }
                        .ok_or_else(|| ASTError::InvalidValue {
                            got: scope.clone(),
                            expected: "var | let | const".to_string()
                        })?;

                    // Process declarations, both to add unclassified names
                    // and to look at `init`.
                    // Since position is VarDecl/LexDecl, `ctx` will classify the name
                    // of `declaration` as such.
                    let mut ctx = ctx.enter(position);
                    me.process_declarations_field(me, &mut ctx, object, "declarations")?;
                }
                "ForStatement" => {
                    // Special case: if `ForStatement` declares variables, it behaves as a scope.
                    let decl_ctx =
                        if let Ok(init) = object.get_object_mut("init", "" /*Field is optional, so errors are ignored*/) {
                            let kind = init.get_string("type", "Field `type` of `VariableDeclaration`, treating `ForStatement`")
                                .map(str::to_string)?;
                            if let "VariableDeclaration" = kind.as_ref() {
                                let mut ctx = ctx.enter(DeclPosition::other("init"));
                                me.process_declarations_obj(me, &mut ctx, init, "VariableDeclaration")?;
                                Some(ctx)
                            } else {
                                me.process_declarations_obj(me, ctx, init, &kind)?;
                                None
                            }
                        } else {
                            None
                        }
                        .unwrap_or_else(DeclContext::new);
                    decl_ctx.store(object);

                    // Now fall through default behavior.
                    // Any other var/lex declaration is stored as part of `field_body` (assuming
                    // that `field_body` is a `BlockStatement` – otherwise, it cannot declare anything).
                    let mut ctx = ctx.enter(DeclPosition::other("ForStatement"));
                    self.parent.process_declarations_field(me, &mut ctx, object, "test")?;
                    self.parent.process_declarations_field(me, &mut ctx, object, "update")?;
                    self.parent.process_declarations_field(me, &mut ctx, object, "body")?;
                }
                "ForInStatement" => {
                     let decl_ctx = {
                        let init = object.get_object_mut("init", "Field `init` of `ForInStatement`")?;
                        if let "VariableDeclaration" = init.get_string("type", "Field `type` of `ForInStatement[\"init\"]`")? {
                            let mut ctx = ctx.enter(DeclPosition::other("init"));
                            me.process_declarations_obj(me, &mut ctx, init, "VariableDeclaration")?;
                            Some(ctx)
                        } else {
                            me.process_declarations_obj(me, ctx, init, "VariableDeclaration")?;
                            None
                        }
                    }
                         .unwrap_or_else(DeclContext::new);
                    decl_ctx.store(object);

                    // Now fall through default behavior.
                    // Any other var/lex declaration is stored as part of `field_body` (assuming
                    // that `field_body` is a `BlockStatement` – otherwise, it cannot declare anything).
                    let mut ctx = ctx.enter(DeclPosition::other("ForInStatement"));
                    self.parent.process_declarations_field(me, &mut ctx, object, "right")?;
                    self.parent.process_declarations_field(me, &mut ctx, object, "body")?;
                }
                _ => {
                    // Default to parent implementation.
                    // Note that we still pass `me` to ensure dynamic dispatch.
                    self.parent.process_declarations_obj(me, ctx, object, kind)?
                }
            }
            Ok(())
        }

        fn process_references_obj(&self, me: &Annotator, ctx: &mut RefContext, object: &mut Object, kind: &str) -> Result<(), ASTError> {
            println!("ES5Annotator::process_references_obj {}", kind);
            match kind {
                "Identifier" => {
                    // There are three sorts of identifiers:
                    // 1. declaring a variable;
                    // 2. referencing a variable;
                    // 3. misc stuff that are not variables (e.g. fields keys, labels).
                    //
                    // With the exception of parameters and recursive functions, we have
                    // handled 1. in the previous pass, so we're just going to skil all
                    // declarations other than these two.
                    //
                    // We just skip any use of identifiers that is not a variable.

                    // Do not enter a subcontext.
                    let name = object.get_string("name", "Field `name` of `Identifier`")?;
                    ctx.add_identifier(name)
                    // If the context is `DeclPosition::Callee`, RefContext will check whether `name` is `eval` and act accordingly.
                    // If the context is `DeclPosition::FunctionArguments`, RefContext will add the name to the list of bound names,
                    // otherwise, add to the list of used names.
                }
                // Variable bindings and scopes
                "VariableDeclarator" => {
                    // Simply ignore field `id`, it has already been processed at scope-level
                    // by the previous pass.
                    me.process_references_field(me, ctx, object, "init")?
                }
                "ForStatement" | "ForInStatement" | "BlockStatement" => {
                    ctx.load(object);
                    self.parent.process_references_obj(me, ctx, object, kind)?;
                    ctx.store(object)
                },
                "FunctionDeclaration" | "ObjectMethod" | "FunctionExpression" => {
                    // This one is a little trickier, as we need to collect bindings from params as we go.
                    let mut ctx = ctx.enter(RefPosition::other("function"));
                    {
                        let mut ctx = ctx.enter(RefPosition::FunctionArguments);
                        {
                            // `ctx` recognizes that `FunctionArguments` identifiers are bindings.
                            me.process_references_field(me, &mut ctx, object, "params")?;
                        }

                        // `id` also behaves as a binding inside the function
                        if let Ok(id) = object.get_string("id", "" /*Optional field*/) {
                            ctx.add_identifier(id)
                        }
                        // When `ctx` is dropped, the arguments should remain as bindings.
                    }
                    ctx.load(object);
                    {
                        me.process_references_field(me, &mut ctx, object, "body")?;
                    }
                    ctx.store(object)
                }
                // Identifiers as something other than variables.
                "LabeledStatement" => {
                    // Simply ignore field `label`.
                    me.process_references_field(me, ctx, object, "body")?
                }
                "BreakStatement" | "ContinueStatement" => {
                    // Nothing to do.
                }
                "ObjectProperty" => {
                    // Simply ignore field `key`.
                    me.process_references_field(me, ctx, object, "value")?
                }
                "MemberExpression" => {
                    me.process_references_field(me, ctx, object, "object")?;
                    let computed = object.get_bool("computed", "Field `computed` of `MemberExpression`")?;
                    if computed {
                        // Just an expression, nothing to see.
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
                "CallExpression" => {
                    {
                        {
                            // Label `callee` as `DeclPosition::Callee`, to let the context handle `eval`.
                            let mut ctx = ctx.enter(RefPosition::Callee);
                            self.parent.process_references_field(me, &mut ctx, object, "callee")?;
                        }
                        {
                            let mut ctx = ctx.enter(RefPosition::other("arguments"));
                            self.parent.process_references_field(me, &mut ctx, object, "arguments")?;
                        }
                    }
                }
                _ => {
                    // Default to parent implementation.
                    // Note that we still pass `me` to ensure dynamic dispatch.
                    self.parent.process_references_obj(me, ctx, object, kind)?
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