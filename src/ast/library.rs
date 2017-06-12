//! A library of specifications for versions of JavaScript.

use ast::grammar::*;


/// The set of features requested for a syntax.
pub enum Level {
    /// Empty syntax, for testing purposes.
    Empty,
    /// All the features for ES5.
    ES5,
    /// All the features of the latest version of JavaScript.
    Latest,
    // FIXME: More levels to be implemented.
}

/// Hardcoding for https://github.com/estree/estree/blob/master/es5.md#exceptions
fn setup_es5(syntax: &mut SyntaxBuilder) {
    // Interface names (by alphabetical order)

    let array_expression = syntax.node_name("ArrayExpression");
    let assignment_expression = syntax.node_name("AssignmentExpression");
    let assignment_operator = syntax.node_name("AssignmentOperator");
    let binary_expression = syntax.node_name("BinaryExpression");
    let binary_operator = syntax.node_name("BinaryOperator");
    let block_statement = syntax.node_name("BlockStatement");
    let boolean_literal = syntax.node_name("BooleanLiteral");
    let break_statement = syntax.node_name("BreakStatement");
    let call_expression = syntax.node_name("CallExpression");
    let catch_clause = syntax.node_name("CatchClause");
    let conditional_expression = syntax.node_name("ConditionalExpression");
    let contine_statement = syntax.node_name("ContinueStatement");
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
    let labelled_statement = syntax.node_name("LabelledStatement");
    let literal = syntax.node_name("Literal");
    let logical_expression = syntax.node_name("LogicalExpression");
    let logical_operator = syntax.node_name("LogicalOperator");
    let member_expression = syntax.node_name("MemberExpression");
    let node = syntax.node_name("Node");
    let new_expression = syntax.node_name("NewExpression");
    let null = syntax.node_name("Null");
    let null_literal = syntax.node_name("NullLiteral");
    let numeric_literal = syntax.node_name("NumericLiteral");
    let object_expression = syntax.node_name("ObjectExpression");
    let object_member = syntax.node_name("ObjectMember");
    let object_method = syntax.node_name("ObjectMethod");
    let object_property = syntax.node_name("ObjectProperty");
    let pattern = syntax.node_name("Pattern");
    let program = syntax.node_name("Program");
    let property = syntax.node_name("Property");
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
        .with_field(&field_name, Type::String)
        .with_parent(&expression)
        .with_parent(&pattern);

    // Literals
    syntax.add_kinded_interface(&literal).unwrap()
        .with_parent(&expression);
    syntax.add_kinded_interface(&string_literal).unwrap()
        .with_field(&field_value, Type::String)
        .with_parent(&literal);
    syntax.add_kinded_interface(&boolean_literal).unwrap()
        .with_field(&field_value, Type::Boolean)
        .with_parent(&literal);
    syntax.add_kinded_interface(&null_literal).unwrap()
        .with_parent(&literal);
    syntax.add_kinded_interface(&numeric_literal).unwrap()
        .with_field(&field_value, Type::Number)
        .with_parent(&literal);
    syntax.add_kinded_interface(&regexp_literal).unwrap()
        .with_field(&field_pattern, Type::String)
        .with_field(&field_flags, Type::String)
        .with_parent(&literal);



    // Programs
    syntax.add_kinded_interface(&program).unwrap()
        .with_field(&field_body, Type::Array(Box::new(Type::interface(&statement))))
        .with_parent(&node);

    // Functions (shared between function declaration, function statement, function expression)
    syntax.add_virtual_interface(&function).unwrap()
        .with_field(&field_id, Type::Interfaces {
            names: vec![identifier.clone()],
            or_null: false
        })
        .with_field(&field_params, Type::Array(Box::new(Type::interface(&pattern))))
        .with_field(&field_body, Type::interface(&block_statement))
        .with_parent(&node);

    // Statements
    syntax.add_virtual_interface(&statement).unwrap()
        .with_parent(&node);

    syntax.add_kinded_interface(&empty_statement).unwrap()
        .with_parent(&statement);

    syntax.add_kinded_interface(&block_statement).unwrap()
        .with_field(&field_body, Type::interface(&statement).array())
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

    syntax.add_kinded_interface(&labelled_statement).unwrap()
        .with_field(&field_label, Type::interface(&identifier))
        .with_field(&field_body, Type::interface(&statement))
        .with_parent(&statement);

    syntax.add_kinded_interface(&break_statement).unwrap()
        .with_field(&field_label, Type::interface(&identifier).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&contine_statement).unwrap()
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
            &expression,
            &null
        ]))
        .with_field(&field_test, Type::interface(&expression).or_null().unwrap())
        .with_field(&field_update, Type::interface(&expression).or_null().unwrap())
        .with_field(&field_body, Type::interface(&statement))
        .with_parent(&statement);

    syntax.add_kinded_interface(&for_in_statement).unwrap()
        .with_field(&field_left, Type::interfaces(&[
            &variable_declaration,
            &pattern
        ]))
        .with_field(&field_right, Type::interface(&expression))
        .with_field(&field_body, Type::interface(&statement))
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

    syntax.add_kinded_interface(&object_member).unwrap()
        .with_field(&field_key, Type::interface(&expression))
        .with_field(&field_computed, Type::Boolean)
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
        .with_field(&field_prefix, Type::Boolean)
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
        .with_field(&field_prefix, Type::Boolean)
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
        .with_field(&field_computed, Type::Boolean)
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
}

fn setup_binjs(syntax: &mut SyntaxBuilder) {
    // Special value `Null`.
    let null = syntax.node_name("Null");
    syntax.add_kinded_interface(&null).unwrap();
}

/// Construct a syntax for a specific version of JavaScript.
pub fn syntax(level: Level) -> Syntax {
    let mut builder = SyntaxBuilder::new();
    match level { 
        Level::Empty => {
            return builder.into_syntax()
        }
        Level::ES5
        | Level::Latest => {
            setup_es5(&mut builder)
        }
    }
    setup_binjs(&mut builder);
    builder.into_syntax()
}

#[test]
fn test_syntax() {
    syntax(Level::ES5);
}