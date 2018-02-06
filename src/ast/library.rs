//! A library of specifications for versions of JavaScript.
//!
//! For a human-readable documentation of the latest version of the library:
//! - build the examples
//!    `cargo build --examples`
//! - run example ast-doc
//!    `./target/debug/examples/ast-doc`.
//!

use ast::grammar::*;
use ast::annotation::*;
use ast::annotation::ScopeNodeName as ScopeKind;
use util::JSONGetter;

use std;

use json::JsonValue as JSON;
use json::object::Object as Object;

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

fn uses_strict(object: &Object) -> bool {
    for member in object["directives"].members() {
        if let Some("use strict") = member["value"]["value"].as_str() {
            return true;
        }
    }
    false
}

/// Special nodes used by BINJS. Not visible at source level.
fn setup_binjs(_: &mut SyntaxBuilder) -> Box<Annotator> {
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

fn setup_es6(syntax: &mut SyntaxBuilder, parent: Box<Annotator>) -> Box<Annotator> {
    // String enum names (by lexicographical order)
    let binary_operator = syntax.node_name("BinaryOperator");
    let compound_assignment_operator = syntax.node_name("CompoundAssignmentOperator");
    let unary_operator = syntax.node_name("UnaryOperator");
    let update_operator = syntax.node_name("UpdateOperator");
    let variable_declaration_kind = syntax.node_name("VariableDeclarationKind");


    // Typedef names (by lexicographical order)
    let arguments = syntax.node_name("Arguments");
    let assignment_target = syntax.node_name("AssignmentTarget");
    let assignment_target_pattern = syntax.node_name("AssignmentTargetPattern");
    let assignment_target_property = syntax.node_name("AssignmentTargetProperty");
    let binding = syntax.node_name("Binding");
    let binding_pattern = syntax.node_name("BindingPattern");
    let binding_property = syntax.node_name("BindingProperty");
    let export_declaration = syntax.node_name("ExportDeclaration");
    let expression = syntax.node_name("Expression");
    let identifier = syntax.node_name("Identifier");
    let identifier_name = syntax.node_name("IdentifierName");
    let import_declaration = syntax.node_name("ImportDeclaration");
    let iteration_statement = syntax.node_name("IterationStatement");
    let label = syntax.node_name("Label");
    let literal = syntax.node_name("Literal");
    let method_definition = syntax.node_name("MethodDefinition");
    let object_property = syntax.node_name("ObjectProperty");
    let parameter = syntax.node_name("Parameter");
    let program = syntax.node_name("Program");
    let property_name = syntax.node_name("PropertyName");
    let simple_assignment_target = syntax.node_name("SimpleAssignmentTarget");
    let statement = syntax.node_name("Statement");
    let string = syntax.node_name("string");


    // Interface names (by lexicographical order)
    let array_assignment_target = syntax.node_name("ArrayAssignmentTarget");
    let array_binding = syntax.node_name("ArrayBinding");
    let array_expression = syntax.node_name("ArrayExpression");
    let arrow_expression = syntax.node_name("ArrowExpression");
    let asserted_block_scope = syntax.node_name("AssertedBlockScope");
    let asserted_top_level_scope = syntax.node_name("AssertedTopLevelScope");
    let assignment_expression = syntax.node_name("AssignmentExpression");
    let assignment_target_identifier = syntax.node_name("AssignmentTargetIdentifier");
    let assignment_target_property_identifier = syntax.node_name("AssignmentTargetPropertyIdentifier");
    let assignment_target_property_property = syntax.node_name("AssignmentTargetPropertyProperty");
    let assignment_target_with_initializer = syntax.node_name("AssignmentTargetWithInitializer");
    let await_expression = syntax.node_name("AwaitExpression");
    let binary_expression = syntax.node_name("BinaryExpression");
    let binding_identifier = syntax.node_name("BindingIdentifier");
    let binding_property_identifier = syntax.node_name("BindingPropertyIdentifier");
    let binding_property_property = syntax.node_name("BindingPropertyProperty");
    let binding_with_initializer = syntax.node_name("BindingWithInitializer");
    let block = syntax.node_name("Block");
    let break_statement = syntax.node_name("BreakStatement");
    let call_expression = syntax.node_name("CallExpression");
    let catch_clause = syntax.node_name("CatchClause");
    let class_declaration = syntax.node_name("ClassDeclaration");
    let class_element = syntax.node_name("ClassElement");
    let class_expression = syntax.node_name("ClassExpression");
    let compound_assignment_expression = syntax.node_name("CompoundAssignmentExpression");
    let computed_member_assignment_target = syntax.node_name("ComputedMemberAssignmentTarget");
    let computed_member_expression = syntax.node_name("ComputedMemberExpression");
    let computed_property_name = syntax.node_name("ComputedPropertyName");
    let conditional_expression = syntax.node_name("ConditionalExpression");
    let continue_statement = syntax.node_name("ContinueStatement");
    let data_property = syntax.node_name("DataProperty");
    let debugger_statement = syntax.node_name("DebuggerStatement");
    let directive = syntax.node_name("Directive");
    let do_while_statement = syntax.node_name("DoWhileStatement");
    let empty_statement = syntax.node_name("EmptyStatement");
    let export = syntax.node_name("Export");
    let export_all_from = syntax.node_name("ExportAllFrom");
    let export_default = syntax.node_name("ExportDefault");
    let export_from = syntax.node_name("ExportFrom");
    let export_from_specifier = syntax.node_name("ExportFromSpecifier");
    let export_local_specifier = syntax.node_name("ExportLocalSpecifier");
    let export_locals = syntax.node_name("ExportLocals");
    let expression_statement = syntax.node_name("ExpressionStatement");
    let for_in_of_binding = syntax.node_name("ForInOfBinding");
    let for_in_statement = syntax.node_name("ForInStatement");
    let for_of_statement = syntax.node_name("ForOfStatement");
    let for_statement = syntax.node_name("ForStatement");
    let formal_parameters = syntax.node_name("FormalParameters");
    let function_body = syntax.node_name("FunctionBody");
    let function_declaration = syntax.node_name("FunctionDeclaration");
    let function_expression = syntax.node_name("FunctionExpression");
    let getter = syntax.node_name("Getter");
    let identifier_expression = syntax.node_name("IdentifierExpression");
    let if_statement = syntax.node_name("IfStatement");
    let import = syntax.node_name("Import");
    let import_namespace = syntax.node_name("ImportNamespace");
    let import_specifier = syntax.node_name("ImportSpecifier");
    let labelled_statement = syntax.node_name("LabelledStatement");
    let literal_boolean_expression = syntax.node_name("LiteralBooleanExpression");
    let literal_infinity_expression = syntax.node_name("LiteralInfinityExpression");
    let literal_null_expression = syntax.node_name("LiteralNullExpression");
    let literal_numeric_expression = syntax.node_name("LiteralNumericExpression");
    let literal_property_name = syntax.node_name("LiteralPropertyName");
    let literal_reg_exp_expression = syntax.node_name("LiteralRegExpExpression");
    let literal_string_expression = syntax.node_name("LiteralStringExpression");
    let method = syntax.node_name("Method");
    let module = syntax.node_name("Module");
    let new_expression = syntax.node_name("NewExpression");
    let new_target_expression = syntax.node_name("NewTargetExpression");
    let object_assignment_target = syntax.node_name("ObjectAssignmentTarget");
    let object_binding = syntax.node_name("ObjectBinding");
    let object_expression = syntax.node_name("ObjectExpression");
    let return_statement = syntax.node_name("ReturnStatement");
    let script = syntax.node_name("Script");
    let setter = syntax.node_name("Setter");
    let shorthand_property = syntax.node_name("ShorthandProperty");
    let spread_element = syntax.node_name("SpreadElement");
    let static_member_assignment_target = syntax.node_name("StaticMemberAssignmentTarget");
    let static_member_expression = syntax.node_name("StaticMemberExpression");
    let super_ = syntax.node_name("Super");
    let switch_case = syntax.node_name("SwitchCase");
    let switch_default = syntax.node_name("SwitchDefault");
    let switch_statement = syntax.node_name("SwitchStatement");
    let switch_statement_with_default = syntax.node_name("SwitchStatementWithDefault");
    let template_element = syntax.node_name("TemplateElement");
    let template_expression = syntax.node_name("TemplateExpression");
    let this_expression = syntax.node_name("ThisExpression");
    let throw_statement = syntax.node_name("ThrowStatement");
    let try_catch_statement = syntax.node_name("TryCatchStatement");
    let try_finally_statement = syntax.node_name("TryFinallyStatement");
    let unary_expression = syntax.node_name("UnaryExpression");
    let update_expression = syntax.node_name("UpdateExpression");
    let variable_declaration = syntax.node_name("VariableDeclaration");
    let variable_declarator = syntax.node_name("VariableDeclarator");
    let while_statement = syntax.node_name("WhileStatement");
    let with_statement = syntax.node_name("WithStatement");
    let yield_expression = syntax.node_name("YieldExpression");
    let yield_star_expression = syntax.node_name("YieldStarExpression");
    let null = syntax.node_name("_Null");



    // Field names (by lexicographical order)
    let field_alternate = syntax.field_name("alternate");
    let field_arguments = syntax.field_name("arguments");
    let field_binding = syntax.field_name("binding");
    let field_body = syntax.field_name("body");
    let field_callee = syntax.field_name("callee");
    let field_captured_names = syntax.field_name("capturedNames");
    let field_cases = syntax.field_name("cases");
    let field_catch_clause = syntax.field_name("catchClause");
    let field_consequent = syntax.field_name("consequent");
    let field_declaration = syntax.field_name("declaration");
    let field_declarators = syntax.field_name("declarators");
    let field_default_binding = syntax.field_name("defaultBinding");
    let field_default_case = syntax.field_name("defaultCase");
    let field_directives = syntax.field_name("directives");
    let field_discriminant = syntax.field_name("discriminant");
    let field_elements = syntax.field_name("elements");
    let field_exported_name = syntax.field_name("exportedName");
    let field_expression = syntax.field_name("expression");
    let field_finalizer = syntax.field_name("finalizer");
    let field_flags = syntax.field_name("flags");
    let field_has_direct_eval = syntax.field_name("hasDirectEval");
    let field_init = syntax.field_name("init");
    let field_is_async = syntax.field_name("isAsync");
    let field_is_generator = syntax.field_name("isGenerator");
    let field_is_prefix = syntax.field_name("isPrefix");
    let field_is_static = syntax.field_name("isStatic");
    let field_items = syntax.field_name("items");
    let field_kind = syntax.field_name("kind");
    let field_label = syntax.field_name("label");
    let field_left = syntax.field_name("left");
    let field_lexically_declared_names = syntax.field_name("lexicallyDeclaredNames");
    let field_method = syntax.field_name("method");
    let field_module_specifier = syntax.field_name("moduleSpecifier");
    let field_name = syntax.field_name("name");
    let field_named_exports = syntax.field_name("namedExports");
    let field_named_imports = syntax.field_name("namedImports");
    let field_namespace_binding = syntax.field_name("namespaceBinding");
    let field_object = syntax.field_name("object");
    let field_operand = syntax.field_name("operand");
    let field_operator = syntax.field_name("operator");
    let field_param = syntax.field_name("param");
    let field_params = syntax.field_name("params");
    let field_pattern = syntax.field_name("pattern");
    let field_post_default_cases = syntax.field_name("postDefaultCases");
    let field_pre_default_cases = syntax.field_name("preDefaultCases");
    let field_properties = syntax.field_name("properties");
    let field_property = syntax.field_name("property");
    let field_raw_value = syntax.field_name("rawValue");
    let field_rest = syntax.field_name("rest");
    let field_right = syntax.field_name("right");
    let field_scope = syntax.field_name("scope");
    let field_statements = syntax.field_name("statements");
    let field_super_ = syntax.field_name("super");
    let field_tag = syntax.field_name("tag");
    let field_test = syntax.field_name("test");
    let field_update = syntax.field_name("update");
    let field_value = syntax.field_name("value");
    let field_var_declared_names = syntax.field_name("varDeclaredNames");



    // Enumerations
    syntax.add_string_enum(&compound_assignment_operator).unwrap()
        .with_strings(&[
            "+=",
            "-=",
            "*=",
            "/=",
            "%=",
            "**=",
            "<<=",
            ">>=",
            ">>>=",
            "|=",
            "^=",
            "&="
        ]);

    syntax.add_string_enum(&binary_operator).unwrap()
        .with_strings(&[
            ",",
            "||",
            "&&",
            "|",
            "^",
            "&",
            "==",
            "!=",
            "===",
            "!==",
            "<",
            "<=",
            ">",
            ">=",
            "in",
            "instanceof",
            "<<",
            ">>",
            ">>>",
            "+",
            "-",
            "*",
            "/",
            "%",
            "**"
        ]);

    syntax.add_string_enum(&update_operator).unwrap()
        .with_strings(&[
            "++",
            "--"
        ]);

    syntax.add_string_enum(&variable_declaration_kind).unwrap()
        .with_strings(&[
            "var",
            "let",
            "const"
        ]);

    syntax.add_string_enum(&unary_operator).unwrap()
        .with_strings(&[
            "+",
            "-",
            "!",
            "~",
            "typeof",
            "void",
            "delete"
        ]);

    syntax.add_typedef(&identifier).unwrap()
        .with_type(
            Type::named(&string).required());

    syntax.add_typedef(&expression).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&literal),
                Type::named(&literal_reg_exp_expression),
                Type::named(&array_expression),
                Type::named(&arrow_expression),
                Type::named(&assignment_expression),
                Type::named(&binary_expression),
                Type::named(&call_expression),
                Type::named(&compound_assignment_expression),
                Type::named(&computed_member_expression),
                Type::named(&conditional_expression),
                Type::named(&class_expression),
                Type::named(&function_expression),
                Type::named(&identifier_expression),
                Type::named(&new_expression),
                Type::named(&new_target_expression),
                Type::named(&object_expression),
                Type::named(&unary_expression),
                Type::named(&static_member_expression),
                Type::named(&template_expression),
                Type::named(&this_expression),
                Type::named(&update_expression),
                Type::named(&yield_expression),
                Type::named(&yield_star_expression),
                Type::named(&await_expression)
            ]).required());

    syntax.add_typedef(&assignment_target).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&assignment_target_pattern),
                Type::named(&simple_assignment_target)
            ]).required());

    syntax.add_typedef(&arguments).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&spread_element),
                Type::named(&expression)
            ]).required().array().required());

    syntax.add_typedef(&export_declaration).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&export_all_from),
                Type::named(&export_from),
                Type::named(&export_locals),
                Type::named(&export_default),
                Type::named(&export)
            ]).required());

    syntax.add_typedef(&string).unwrap()
        .with_type(
            Type::string().required());

    syntax.add_typedef(&parameter).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&binding),
                Type::named(&binding_with_initializer)
            ]).required());

    syntax.add_typedef(&identifier_name).unwrap()
        .with_type(
            Type::named(&string).required());

    syntax.add_typedef(&object_property).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&method_definition),
                Type::named(&data_property),
                Type::named(&shorthand_property)
            ]).required());

    syntax.add_typedef(&import_declaration).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&import_namespace),
                Type::named(&import)
            ]).required());

    syntax.add_typedef(&binding_property).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&binding_property_identifier),
                Type::named(&binding_property_property)
            ]).required());

    syntax.add_typedef(&property_name).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&computed_property_name),
                Type::named(&literal_property_name)
            ]).required());

    syntax.add_typedef(&statement).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&block),
                Type::named(&break_statement),
                Type::named(&continue_statement),
                Type::named(&class_declaration),
                Type::named(&debugger_statement),
                Type::named(&empty_statement),
                Type::named(&expression_statement),
                Type::named(&function_declaration),
                Type::named(&if_statement),
                Type::named(&iteration_statement),
                Type::named(&labelled_statement),
                Type::named(&return_statement),
                Type::named(&switch_statement),
                Type::named(&switch_statement_with_default),
                Type::named(&throw_statement),
                Type::named(&try_catch_statement),
                Type::named(&try_finally_statement),
                Type::named(&variable_declaration),
                Type::named(&with_statement)
            ]).required());

    syntax.add_typedef(&program).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&script),
                Type::named(&module)
            ]).required());

    syntax.add_typedef(&binding_pattern).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&object_binding),
                Type::named(&array_binding)
            ]).required());

    syntax.add_typedef(&assignment_target_property).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&assignment_target_property_identifier),
                Type::named(&assignment_target_property_property)
            ]).required());

    syntax.add_typedef(&method_definition).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&method),
                Type::named(&getter),
                Type::named(&setter)
            ]).required());

    syntax.add_typedef(&iteration_statement).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&do_while_statement),
                Type::named(&for_in_statement),
                Type::named(&for_of_statement),
                Type::named(&for_statement),
                Type::named(&while_statement)
            ]).required());

    syntax.add_typedef(&literal).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&literal_boolean_expression),
                Type::named(&literal_infinity_expression),
                Type::named(&literal_null_expression),
                Type::named(&literal_numeric_expression),
                Type::named(&literal_string_expression)
            ]).required());

    syntax.add_typedef(&binding).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&binding_pattern),
                Type::named(&binding_identifier)
            ]).required());

    syntax.add_typedef(&simple_assignment_target).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&assignment_target_identifier),
                Type::named(&computed_member_assignment_target),
                Type::named(&static_member_assignment_target)
            ]).required());

    syntax.add_typedef(&assignment_target_pattern).unwrap()
        .with_type(
            Type::sum(&[
                Type::named(&object_assignment_target),
                Type::named(&array_assignment_target)
            ]).required());

    syntax.add_typedef(&label).unwrap()
        .with_type(
            Type::named(&string).required());

    syntax.add_interface(&debugger_statement).unwrap()
    ;

    syntax.add_interface(&switch_statement).unwrap()
        .with_field(
            &field_discriminant,
            Type::named(&expression).required()
        )
        .with_field(
            &field_cases,
            Type::named(&switch_case).required().array().required()
        );

    syntax.add_interface(&setter).unwrap()
        .with_field(
            &field_name,
            Type::named(&property_name).required()
        )
        .with_field(
            &field_param,
            Type::named(&parameter).required()
        )
        .with_field(
            &field_body,
            Type::named(&function_body).required()
        );

    syntax.add_interface(&spread_element).unwrap()
        .with_field(
            &field_expression,
            Type::named(&expression).required()
        );

    syntax.add_interface(&class_declaration).unwrap()
        .with_field(
            &field_name,
            Type::named(&binding_identifier).required()
        )
        .with_field(
            &field_super_,
            Type::named(&expression).optional()
        )
        .with_field(
            &field_elements,
            Type::named(&class_element).required().array().required()
        );

    syntax.add_interface(&static_member_expression).unwrap()
        .with_field(
            &field_object,
            Type::sum(&[
                Type::named(&expression),
                Type::named(&super_)
            ]).required()
        )
        .with_field(
            &field_property,
            Type::named(&identifier_name).required()
        );

    syntax.add_interface(&binding_with_initializer).unwrap()
        .with_field(
            &field_binding,
            Type::named(&binding).required()
        )
        .with_field(
            &field_init,
            Type::named(&expression).required()
        );

    syntax.add_interface(&binding_identifier).unwrap()
        .with_field(
            &field_name,
            Type::named(&identifier).required()
        );

    syntax.add_interface(&literal_string_expression).unwrap()
        .with_field(
            &field_value,
            Type::named(&string).required()
        );

    syntax.add_interface(&binding_property_property).unwrap()
        .with_field(
            &field_name,
            Type::named(&property_name).required()
        )
        .with_field(
            &field_binding,
            Type::sum(&[
                Type::named(&binding),
                Type::named(&binding_with_initializer)
            ]).required()
        );

    syntax.add_interface(&catch_clause).unwrap()
        .with_field(
            &field_binding,
            Type::named(&binding).required()
        )
        .with_field(
            &field_body,
            Type::named(&block).required()
        );

    syntax.add_interface(&function_declaration).unwrap()
        .with_field(
            &field_is_async,
            Type::bool().required()
        )
        .with_field(
            &field_is_generator,
            Type::bool().required()
        )
        .with_field(
            &field_scope,
            Type::named(&asserted_top_level_scope).optional()
        )
        .with_field(
            &field_name,
            Type::named(&binding_identifier).required()
        )
        .with_field(
            &field_params,
            Type::named(&formal_parameters).required()
        )
        .with_field(
            &field_body,
            Type::named(&function_body).required()
        );

    syntax.add_interface(&export_from).unwrap()
        .with_field(
            &field_named_exports,
            Type::named(&export_from_specifier).required().array().required()
        )
        .with_field(
            &field_module_specifier,
            Type::named(&string).required()
        );

    syntax.add_interface(&this_expression).unwrap()
    ;

    syntax.add_interface(&object_binding).unwrap()
        .with_field(
            &field_properties,
            Type::named(&binding_property).required().array().required()
        );

    syntax.add_interface(&await_expression).unwrap()
        .with_field(
            &field_expression,
            Type::named(&expression).required()
        );

    syntax.add_interface(&export_default).unwrap()
        .with_field(
            &field_body,
            Type::sum(&[
                Type::named(&function_declaration),
                Type::named(&class_declaration),
                Type::named(&expression)
            ]).required()
        );

    syntax.add_interface(&labelled_statement).unwrap()
        .with_field(
            &field_label,
            Type::named(&label).required()
        )
        .with_field(
            &field_body,
            Type::named(&statement).required()
        );

    syntax.add_interface(&data_property).unwrap()
        .with_field(
            &field_name,
            Type::named(&property_name).required()
        )
        .with_field(
            &field_expression,
            Type::named(&expression).required()
        );

    syntax.add_interface(&block).unwrap()
        .with_field(
            &field_scope,
            Type::named(&asserted_block_scope).optional()
        )
        .with_field(
            &field_statements,
            Type::named(&statement).required().array().required()
        );

    syntax.add_interface(&variable_declaration).unwrap()
        .with_field(
            &field_kind,
            Type::named(&variable_declaration_kind).required()
        )
        .with_field(
            &field_declarators,
            Type::named(&variable_declarator).required().array().required()
        );

    syntax.add_interface(&asserted_block_scope).unwrap()
        .with_field(
            &field_lexically_declared_names,
            Type::named(&identifier_name).required().array().required()
        )
        .with_field(
            &field_captured_names,
            Type::named(&identifier_name).required().array().required()
        )
        .with_field(
            &field_has_direct_eval,
            Type::bool().required()
        );

    syntax.add_interface(&assignment_target_property_identifier).unwrap()
        .with_field(
            &field_binding,
            Type::named(&assignment_target_identifier).required()
        )
        .with_field(
            &field_init,
            Type::named(&expression).optional()
        );

    syntax.add_interface(&class_expression).unwrap()
        .with_field(
            &field_name,
            Type::named(&binding_identifier).optional()
        )
        .with_field(
            &field_super_,
            Type::named(&expression).optional()
        )
        .with_field(
            &field_elements,
            Type::named(&class_element).required().array().required()
        );

    syntax.add_interface(&import_specifier).unwrap()
        .with_field(
            &field_name,
            Type::named(&identifier_name).optional()
        )
        .with_field(
            &field_binding,
            Type::named(&binding_identifier).required()
        );

    syntax.add_interface(&literal_numeric_expression).unwrap()
        .with_field(
            &field_value,
            Type::number().required()
        );

    syntax.add_interface(&assignment_expression).unwrap()
        .with_field(
            &field_binding,
            Type::named(&assignment_target).required()
        )
        .with_field(
            &field_expression,
            Type::named(&expression).required()
        );

    syntax.add_interface(&super_).unwrap()
    ;

    syntax.add_interface(&while_statement).unwrap()
        .with_field(
            &field_test,
            Type::named(&expression).required()
        )
        .with_field(
            &field_body,
            Type::named(&statement).required()
        );

    syntax.add_interface(&object_expression).unwrap()
        .with_field(
            &field_properties,
            Type::named(&object_property).required().array().required()
        );

    syntax.add_interface(&function_body).unwrap()
        .with_field(
            &field_directives,
            Type::named(&directive).required().array().required()
        )
        .with_field(
            &field_statements,
            Type::named(&statement).required().array().required()
        );

    syntax.add_interface(&computed_member_assignment_target).unwrap()
        .with_field(
            &field_object,
            Type::sum(&[
                Type::named(&expression),
                Type::named(&super_)
            ]).required()
        )
        .with_field(
            &field_expression,
            Type::named(&expression).required()
        );

    syntax.add_interface(&asserted_top_level_scope).unwrap()
        .with_field(
            &field_lexically_declared_names,
            Type::named(&identifier_name).required().array().required()
        )
        .with_field(
            &field_var_declared_names,
            Type::named(&identifier_name).required().array().required()
        )
        .with_field(
            &field_captured_names,
            Type::named(&identifier_name).required().array().required()
        )
        .with_field(
            &field_has_direct_eval,
            Type::bool().required()
        );

    syntax.add_interface(&new_expression).unwrap()
        .with_field(
            &field_callee,
            Type::named(&expression).required()
        )
        .with_field(
            &field_arguments,
            Type::named(&arguments).required()
        );

    syntax.add_interface(&assignment_target_with_initializer).unwrap()
        .with_field(
            &field_binding,
            Type::named(&assignment_target).required()
        )
        .with_field(
            &field_init,
            Type::named(&expression).required()
        );

    syntax.add_interface(&template_expression).unwrap()
        .with_field(
            &field_tag,
            Type::named(&expression).optional()
        )
        .with_field(
            &field_elements,
            Type::sum(&[
                Type::named(&expression),
                Type::named(&template_element)
            ]).required().array().required()
        );

    syntax.add_interface(&export_local_specifier).unwrap()
        .with_field(
            &field_name,
            Type::named(&identifier_expression).required()
        )
        .with_field(
            &field_exported_name,
            Type::named(&identifier_name).optional()
        );

    syntax.add_interface(&switch_case).unwrap()
        .with_field(
            &field_test,
            Type::named(&expression).required()
        )
        .with_field(
            &field_consequent,
            Type::named(&statement).required().array().required()
        );

    syntax.add_interface(&try_finally_statement).unwrap()
        .with_field(
            &field_body,
            Type::named(&block).required()
        )
        .with_field(
            &field_catch_clause,
            Type::named(&catch_clause).optional()
        )
        .with_field(
            &field_finalizer,
            Type::named(&block).required()
        );

    syntax.add_interface(&break_statement).unwrap()
        .with_field(
            &field_label,
            Type::named(&label).optional()
        );

    syntax.add_interface(&class_element).unwrap()
        .with_field(
            &field_is_static,
            Type::bool().required()
        )
        .with_field(
            &field_method,
            Type::named(&method_definition).required()
        );

    syntax.add_interface(&switch_default).unwrap()
        .with_field(
            &field_consequent,
            Type::named(&statement).required().array().required()
        );

    syntax.add_interface(&getter).unwrap()
        .with_field(
            &field_name,
            Type::named(&property_name).required()
        )
        .with_field(
            &field_body,
            Type::named(&function_body).required()
        );

    syntax.add_interface(&import).unwrap()
        .with_field(
            &field_module_specifier,
            Type::named(&string).required()
        )
        .with_field(
            &field_default_binding,
            Type::named(&binding_identifier).optional()
        )
        .with_field(
            &field_named_imports,
            Type::named(&import_specifier).required().array().required()
        );

    syntax.add_interface(&binding_property_identifier).unwrap()
        .with_field(
            &field_binding,
            Type::named(&binding_identifier).required()
        )
        .with_field(
            &field_init,
            Type::named(&expression).optional()
        );

    syntax.add_interface(&literal_reg_exp_expression).unwrap()
        .with_field(
            &field_pattern,
            Type::named(&string).required()
        )
        .with_field(
            &field_flags,
            Type::named(&string).required()
        );

    syntax.add_interface(&arrow_expression).unwrap()
        .with_field(
            &field_is_async,
            Type::bool().required()
        )
        .with_field(
            &field_params,
            Type::named(&formal_parameters).required()
        )
        .with_field(
            &field_body,
            Type::sum(&[
                Type::named(&function_body),
                Type::named(&expression)
            ]).required()
        );

    syntax.add_interface(&call_expression).unwrap()
        .with_field(
            &field_callee,
            Type::sum(&[
                Type::named(&expression),
                Type::named(&super_)
            ]).required()
        )
        .with_field(
            &field_arguments,
            Type::named(&arguments).required()
        );

    syntax.add_interface(&assignment_target_identifier).unwrap()
        .with_field(
            &field_name,
            Type::named(&identifier).required()
        );

    syntax.add_interface(&computed_member_expression).unwrap()
        .with_field(
            &field_object,
            Type::sum(&[
                Type::named(&expression),
                Type::named(&super_)
            ]).required()
        )
        .with_field(
            &field_expression,
            Type::named(&expression).required()
        );

    syntax.add_interface(&if_statement).unwrap()
        .with_field(
            &field_test,
            Type::named(&expression).required()
        )
        .with_field(
            &field_consequent,
            Type::named(&statement).required()
        )
        .with_field(
            &field_alternate,
            Type::named(&statement).optional()
        );

    syntax.add_interface(&for_statement).unwrap()
        .with_field(
            &field_init,
            Type::sum(&[
                Type::named(&variable_declaration),
                Type::named(&expression)
            ]).optional()
        )
        .with_field(
            &field_test,
            Type::named(&expression).optional()
        )
        .with_field(
            &field_update,
            Type::named(&expression).optional()
        )
        .with_field(
            &field_body,
            Type::named(&statement).required()
        );

    syntax.add_interface(&try_catch_statement).unwrap()
        .with_field(
            &field_body,
            Type::named(&block).required()
        )
        .with_field(
            &field_catch_clause,
            Type::named(&catch_clause).required()
        );

    syntax.add_interface(&template_element).unwrap()
        .with_field(
            &field_raw_value,
            Type::named(&string).required()
        );

    syntax.add_interface(&variable_declarator).unwrap()
        .with_field(
            &field_binding,
            Type::named(&binding).required()
        )
        .with_field(
            &field_init,
            Type::named(&expression).optional()
        );

    syntax.add_interface(&static_member_assignment_target).unwrap()
        .with_field(
            &field_object,
            Type::sum(&[
                Type::named(&expression),
                Type::named(&super_)
            ]).required()
        )
        .with_field(
            &field_property,
            Type::named(&identifier_name).required()
        );

    syntax.add_interface(&array_expression).unwrap()
        .with_field(
            &field_elements,
            Type::sum(&[
                Type::named(&spread_element),
                Type::named(&expression)
            ]).optional().array().required()
        );

    syntax.add_interface(&throw_statement).unwrap()
        .with_field(
            &field_expression,
            Type::named(&expression).required()
        );

    syntax.add_interface(&identifier_expression).unwrap()
        .with_field(
            &field_name,
            Type::named(&identifier).required()
        );

    syntax.add_interface(&null).unwrap()
    ;

    syntax.add_interface(&switch_statement_with_default).unwrap()
        .with_field(
            &field_discriminant,
            Type::named(&expression).required()
        )
        .with_field(
            &field_pre_default_cases,
            Type::named(&switch_case).required().array().required()
        )
        .with_field(
            &field_default_case,
            Type::named(&switch_default).required()
        )
        .with_field(
            &field_post_default_cases,
            Type::named(&switch_case).required().array().required()
        );

    syntax.add_interface(&directive).unwrap()
        .with_field(
            &field_raw_value,
            Type::named(&string).required()
        );

    syntax.add_interface(&script).unwrap()
        .with_field(
            &field_scope,
            Type::named(&asserted_top_level_scope).optional()
        )
        .with_field(
            &field_directives,
            Type::named(&directive).required().array().required()
        )
        .with_field(
            &field_statements,
            Type::named(&statement).required().array().required()
        );

    syntax.add_interface(&array_assignment_target).unwrap()
        .with_field(
            &field_elements,
            Type::sum(&[
                Type::named(&assignment_target),
                Type::named(&assignment_target_with_initializer)
            ]).required().array().required()
        )
        .with_field(
            &field_rest,
            Type::named(&assignment_target).optional()
        );

    syntax.add_interface(&export_from_specifier).unwrap()
        .with_field(
            &field_name,
            Type::named(&identifier_name).required()
        )
        .with_field(
            &field_exported_name,
            Type::named(&identifier_name).optional()
        );

    syntax.add_interface(&computed_property_name).unwrap()
        .with_field(
            &field_expression,
            Type::named(&expression).required()
        );

    syntax.add_interface(&update_expression).unwrap()
        .with_field(
            &field_is_prefix,
            Type::bool().required()
        )
        .with_field(
            &field_operator,
            Type::named(&update_operator).required()
        )
        .with_field(
            &field_operand,
            Type::named(&simple_assignment_target).required()
        );

    syntax.add_interface(&return_statement).unwrap()
        .with_field(
            &field_expression,
            Type::named(&expression).optional()
        );

    syntax.add_interface(&export_locals).unwrap()
        .with_field(
            &field_named_exports,
            Type::named(&export_local_specifier).required().array().required()
        );

    syntax.add_interface(&literal_property_name).unwrap()
        .with_field(
            &field_value,
            Type::named(&string).required()
        );

    syntax.add_interface(&literal_null_expression).unwrap()
    ;

    syntax.add_interface(&array_binding).unwrap()
        .with_field(
            &field_elements,
            Type::sum(&[
                Type::named(&binding),
                Type::named(&binding_with_initializer)
            ]).optional().array().required()
        )
        .with_field(
            &field_rest,
            Type::named(&binding).optional()
        );

    syntax.add_interface(&shorthand_property).unwrap()
        .with_field(
            &field_name,
            Type::named(&identifier_expression).required()
        );

    syntax.add_interface(&conditional_expression).unwrap()
        .with_field(
            &field_test,
            Type::named(&expression).required()
        )
        .with_field(
            &field_consequent,
            Type::named(&expression).required()
        )
        .with_field(
            &field_alternate,
            Type::named(&expression).required()
        );

    syntax.add_interface(&for_of_statement).unwrap()
        .with_field(
            &field_left,
            Type::sum(&[
                Type::named(&for_in_of_binding),
                Type::named(&assignment_target)
            ]).required()
        )
        .with_field(
            &field_right,
            Type::named(&expression).required()
        )
        .with_field(
            &field_body,
            Type::named(&statement).required()
        );

    syntax.add_interface(&continue_statement).unwrap()
        .with_field(
            &field_label,
            Type::named(&label).optional()
        );

    syntax.add_interface(&module).unwrap()
        .with_field(
            &field_scope,
            Type::named(&asserted_top_level_scope).optional()
        )
        .with_field(
            &field_directives,
            Type::named(&directive).required().array().required()
        )
        .with_field(
            &field_items,
            Type::sum(&[
                Type::named(&import_declaration),
                Type::named(&export_declaration),
                Type::named(&statement)
            ]).required().array().required()
        );

    syntax.add_interface(&export_all_from).unwrap()
        .with_field(
            &field_module_specifier,
            Type::named(&string).required()
        );

    syntax.add_interface(&method).unwrap()
        .with_field(
            &field_is_async,
            Type::bool().required()
        )
        .with_field(
            &field_is_generator,
            Type::bool().required()
        )
        .with_field(
            &field_name,
            Type::named(&property_name).required()
        )
        .with_field(
            &field_params,
            Type::named(&formal_parameters).required()
        )
        .with_field(
            &field_body,
            Type::named(&function_body).required()
        );

    syntax.add_interface(&yield_star_expression).unwrap()
        .with_field(
            &field_expression,
            Type::named(&expression).required()
        );

    syntax.add_interface(&with_statement).unwrap()
        .with_field(
            &field_object,
            Type::named(&expression).required()
        )
        .with_field(
            &field_body,
            Type::named(&statement).required()
        );

    syntax.add_interface(&yield_expression).unwrap()
        .with_field(
            &field_expression,
            Type::named(&expression).optional()
        );

    syntax.add_interface(&export).unwrap()
        .with_field(
            &field_declaration,
            Type::sum(&[
                Type::named(&function_declaration),
                Type::named(&class_declaration),
                Type::named(&variable_declaration)
            ]).required()
        );

    syntax.add_interface(&unary_expression).unwrap()
        .with_field(
            &field_operator,
            Type::named(&unary_operator).required()
        )
        .with_field(
            &field_operand,
            Type::named(&expression).required()
        );

    syntax.add_interface(&literal_boolean_expression).unwrap()
        .with_field(
            &field_value,
            Type::bool().required()
        );

    syntax.add_interface(&assignment_target_property_property).unwrap()
        .with_field(
            &field_name,
            Type::named(&property_name).required()
        )
        .with_field(
            &field_binding,
            Type::sum(&[
                Type::named(&assignment_target),
                Type::named(&assignment_target_with_initializer)
            ]).required()
        );

    syntax.add_interface(&compound_assignment_expression).unwrap()
        .with_field(
            &field_operator,
            Type::named(&compound_assignment_operator).required()
        )
        .with_field(
            &field_binding,
            Type::named(&simple_assignment_target).required()
        )
        .with_field(
            &field_expression,
            Type::named(&expression).required()
        );

    syntax.add_interface(&new_target_expression).unwrap()
    ;

    syntax.add_interface(&literal_infinity_expression).unwrap()
    ;

    syntax.add_interface(&empty_statement).unwrap()
    ;

    syntax.add_interface(&for_in_statement).unwrap()
        .with_field(
            &field_left,
            Type::sum(&[
                Type::named(&for_in_of_binding),
                Type::named(&assignment_target)
            ]).required()
        )
        .with_field(
            &field_right,
            Type::named(&expression).required()
        )
        .with_field(
            &field_body,
            Type::named(&statement).required()
        );

    syntax.add_interface(&do_while_statement).unwrap()
        .with_field(
            &field_test,
            Type::named(&expression).required()
        )
        .with_field(
            &field_body,
            Type::named(&statement).required()
        );

    syntax.add_interface(&function_expression).unwrap()
        .with_field(
            &field_is_async,
            Type::bool().required()
        )
        .with_field(
            &field_is_generator,
            Type::bool().required()
        )
        .with_field(
            &field_scope,
            Type::named(&asserted_top_level_scope).optional()
        )
        .with_field(
            &field_name,
            Type::named(&binding_identifier).optional()
        )
        .with_field(
            &field_params,
            Type::named(&formal_parameters).required()
        )
        .with_field(
            &field_body,
            Type::named(&function_body).required()
        );

    syntax.add_interface(&expression_statement).unwrap()
        .with_field(
            &field_expression,
            Type::named(&expression).required()
        );

    syntax.add_interface(&for_in_of_binding).unwrap()
        .with_field(
            &field_kind,
            Type::named(&variable_declaration_kind).required()
        )
        .with_field(
            &field_binding,
            Type::named(&binding).required()
        );

    syntax.add_interface(&binary_expression).unwrap()
        .with_field(
            &field_operator,
            Type::named(&binary_operator).required()
        )
        .with_field(
            &field_left,
            Type::named(&expression).required()
        )
        .with_field(
            &field_right,
            Type::named(&expression).required()
        );

    syntax.add_interface(&object_assignment_target).unwrap()
        .with_field(
            &field_properties,
            Type::named(&assignment_target_property).required().array().required()
        );

    syntax.add_interface(&import_namespace).unwrap()
        .with_field(
            &field_module_specifier,
            Type::named(&string).required()
        )
        .with_field(
            &field_default_binding,
            Type::named(&binding_identifier).optional()
        )
        .with_field(
            &field_namespace_binding,
            Type::named(&binding_identifier).required()
        );

    syntax.add_interface(&formal_parameters).unwrap()
        .with_field(
            &field_items,
            Type::named(&parameter).required().array().required()
        )
        .with_field(
            &field_rest,
            Type::named(&binding).optional()
        );




    struct ES6Annotator {
        parent: Box<Annotator>,
    }
    impl Annotator for ES6Annotator {
        fn name(&self) -> String {
            "ES6Annotator".to_string()
        }

        fn process_declarations(&self, me: &Annotator, ctx: &mut Context<DeclContents>, object: &mut Object) -> Result<(), ASTError> {
            debug!(target: "library", "Visiting {}", ctx.kind_str());
            match ctx.kind_str() {
                "BindingIdentifier" => {
                    // Collect the name of the identifier.
                    let name = object.get_string("name", "Field `name` of `BindingIdentifier`")?;
                    debug!(target: "library", "While visiting {kind}, found name {name} with parent {parent:?}",
                        parent = ctx.contents().parent().map(|x| x.borrow().kind_str().to_string()),
                        name = name,
                        kind = ctx.kind_str()
                    );
                    let parent = match ctx.contents().parent() {
                        Some(parent) => parent,
                        None => return Ok(()) // If we are at toplevel, we don't really care about all this.
                    };
                    let mut parent = parent.borrow_mut();
                    match parent.kind_str() {
                        // FIXME: This would probably be much nicer if we had an iterator of ancestors.
                        "FunctionDeclaration" | "Method" | "Getter" | "Setter" | "FunctionExpression" => {
                            if let Some("name") = parent.field_str() {
                                // 1. If the declaration is at the toplevel, this is a `var`.
                                // 2. If the declaration is in a function's toplevel block, this is a `var`.
                                // 3. Otherwise, this is a `let`.
                                if let Some(grand) = parent.parent() {
                                    let mut grand = grand.borrow_mut();
                                    if let "FunctionBody" = grand.kind_str() {
                                        // Case 2.
                                        grand.add_var_name(name)
                                    } else {
                                        // Case 3.
                                        grand.add_let_name(name)
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
                        "CatchClause" => {
                            // Handle `catch (e) { ...} ` where `e` is declared implicitly.
                            if let Some("param") = parent.field_str() {
                                if !parent.is_lex_bound(&name) {
                                    parent.add_let_name(name);
                                }
                            }
                        }
                        "VariableDeclarator" => {
                            if let Some("binding") = parent.field_str() {
                                debug!(target: "library", "Inserting binding");
                                match parent.scope_kind() {
                                    ScopeKind::VarDecl => parent.add_var_name(name),
                                    ScopeKind::LetDecl => parent.add_let_name(name),
                                    ScopeKind::ConstDecl => parent.add_const_name(name),
                                    _ => return Err(ASTError::InvalidScope)
                                }
                            } else {
                                debug!(target: "library", "Skipping binding {:?}", parent.field_str());
                            }
                            // Otherwise, skip. We cannot declare variables in `init`.
                        }
                        _ => {
                            // We don't know what this identifier is yet. Could be declared
                            // in some enclosing scope or never, in which case it is implicitly
                            // a global variable.
                            parent.add_unknown_name(name)
                        }
                    }
                }
                "CatchClause" => {
                    ctx.set_scope_kind(ScopeKind::LetDecl);
                    self.parent.process_declarations(me, ctx, object)?;

                    // Store available information.
                    ctx.store(object, false);

                    // Drop LexDecl, keep VarDecl
                    ctx.clear_lex_names();
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
                "Block" | "FunctionBody" | "ForStatement" | "ForInStatement" => {
                    // Adopt usual behavior.
                    self.parent.process_declarations(me, ctx, object)?;

                    // Store available information.
                    ctx.store(object, ctx.kind_str() == "FunctionBody");

                    // Drop LexDecl, keep VarDecl
                    ctx.clear_lex_names();
                }
                "Script" | "Module" => {
                    // Check directives
                    if uses_strict(object) {
                        ctx.set_uses_strict(true);
                    }

                    // Adopt usual behavior.
                    self.parent.process_declarations(me, ctx, object)?;

                    // Store available information.
                    ctx.clear_special_unknown_names(&["eval"]);
                    ctx.promote_unknown_names_to_var();
                    ctx.store(object, true);
                }
                "FunctionDeclaration" | "Getter"  | "Setter" | "Method" | "FunctionExpression"  => {
                    // Check directives.
                    let uses_strict = ctx.uses_strict() ||
                        if let JSON::Object(ref body) = object["body"] {
                            uses_strict(body)
                        } else {
                            false
                        };
                    if uses_strict {
                        ctx.set_uses_strict(true);
                    }

                    // Adopt usual behavior.
                    self.parent.process_declarations(me, ctx, object)?;

                    // Store available information.
                    ctx.store(object, false);

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
                        "FunctionDeclaration" | "Method" | "Getter" | "Setter" | "FunctionExpression" => {
                            match parent.field_str() {
                                Some("name") | Some("params") => {
                                    let fun_scope = parent.fun_scope() .expect("Expected a fun scope");
                                    fun_scope.borrow_mut().add_binding(name);
                                }
                                _ => return Err(ASTError::InvalidField("<FIXME: specify field>".to_string())),
                            }
                        }
                        "CatchClause" => {
                            if let Some("binding") = parent.field_str() {
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
                        "LabelledStatement" | "BreakStatement" | "ContinueStatement" => {
                            // Ignore identifier, not a variable.
                        }
                        "VariableDeclarator" => {
                            if let Some("binding") = parent.field_str() {
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
                                assert!(parent.is_bound(&name), "Variable {} is used by `for(in)`, should have been marked as bound in the previous pass", name);
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
                            | "BracketExpression" | "DotExpression"
                            | "ConditionalExpression" | "CallExpression"
                            | "NewExpression" | "SequenceExpression" => {
                            ctx.add_free_name(name);
                        }
                        _ => {
                            panic!("I didn't expect to see a Identifier (namely \"{}\") in {} {:?}", name, parent.kind_str(), parent.field_str())
                        }
                    };
                }
                "ForStatement" | "ForInStatement" | "Block" | "Program" | "FunctionBody" | "CatchClause" =>
                {
                    // Simply load the stored bindings, then handle fields.
                    ctx.load(object);
                    self.parent.process_references(me, ctx, object)?;
                    ctx.store(object, ctx.kind_str() == "FunctionBody")
                },
                "FunctionExpression" | "FunctionDeclaration" | "Method" | "Getter" | "Setter" => {
                    ctx.use_as_fun_scope();

                    // Make sure that we handle the function id and the params before the body,
                    // as they generally introduce bindings.
                    for name in &["name", "params", "body"] {
                        if let Some(field) = object.get_mut(*name) {
                            if let Ok(mut ctx) = ctx.enter_field(name) {
                                me.process_references_aux(me, &mut ctx, field)?;
                            }
                        }
                    }

                    // Store information on the function itself.
                    ctx.store(object, false);
                    // Dropping `ctx` will convert free variables into
                    // free-in-nested-function variables.
                }
                "ObjectProperty" => {
                    // Simply ignore field `key`.
                    // It is specified as an `Expression`, which doesn't really make sense, as it
                    // must be either a literal or an identifier that is not a variable.
                    me.process_references_field(me, ctx, object, "value")?
                }
                "DotExpression" => {
                    me.process_references_field(me, ctx, object, "object")?;
                    // Field `property` contains an identifier, skip it.
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

    Box::new(ES6Annotator {
        parent
    })
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
            setup_es6(&mut builder, base_annotator)
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
