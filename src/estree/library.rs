use estree::grammar::*;

/// The set of features requested for a syntax.
pub enum Level {
    /// All the features for ES5.
    ES5,
}

/// Hardcoding for https://github.com/estree/estree/blob/master/es5.md#exceptions
fn setup_es5(syntax: &mut SyntaxBuilder) {
    let node = syntax.interface_name("Node");
    let string = syntax.interface_name("String");
    let boolean = syntax.interface_name("Boolean");
    let null = syntax.interface_name("Null");
    let number = syntax.interface_name("Number");
    let regexp = syntax.interface_name("RegExp");
    let statement = syntax.interface_name("Statement");
    let identifier = syntax.interface_name("Identifier");
    let pattern = syntax.interface_name("Pattern");
    let block_statement = syntax.interface_name("BlockStatement");
    let expression = syntax.interface_name("Expression");
    let switch_case = syntax.interface_name("SwitchCase");
    let catch_clause = syntax.interface_name("CatchClause");
    let variable_declaration = syntax.interface_name("VariableDeclaration");
    let variable_declarator = syntax.interface_name("VariableDeclarator");
    let property = syntax.interface_name("Property");
    let literal = syntax.interface_name("Literal");
    let unary_operator = syntax.interface_name("UnaryOperator");
    let binary_operator = syntax.interface_name("BinaryOperator");
    let assignment_operator = syntax.interface_name("AssignmentOperator");
    let function = syntax.interface_name("Function");
    let declaration = syntax.interface_name("Declaration");
    let regexp_literal = syntax.interface_name("RegexpLiteral");
    let program = syntax.interface_name("Program");
    let empty_statement = syntax.interface_name("EmptyStatement");
    let expression_statement = syntax.interface_name("ExpressionStatement");
    let debugger_statement = syntax.interface_name("DebuggerStatement");
    let with_statement = syntax.interface_name("WithStatement");
    let return_statement = syntax.interface_name("ReturnStatement");
    let labelled_statement = syntax.interface_name("LabelledStatement");
    let break_statement = syntax.interface_name("BreakStatement");
    let contine_statement = syntax.interface_name("ContinueStatement");
    let if_statement = syntax.interface_name("IfStatement");
    let switch_statement = syntax.interface_name("SwitchStatement");
    let throw_statement = syntax.interface_name("ThrowStatement");
    let try_statement = syntax.interface_name("TryStatement");
    let while_statement = syntax.interface_name("WhileStatement");
    let do_while_statement = syntax.interface_name("DoWhileStatement");
    let for_statement = syntax.interface_name("ForStatement");
    let for_in_statement = syntax.interface_name("ForInStatement");
    let function_declaration = syntax.interface_name("FunctionDeclaration");
    let this_expression = syntax.interface_name("ThisExpression");
    let array_expression = syntax.interface_name("ArrayExpression");
    let object_expression = syntax.interface_name("ObjectExpression");
    let function_expression = syntax.interface_name("FunctionExpression");
    let unary_expression = syntax.interface_name("UnaryExpression");
    let update_expression = syntax.interface_name("UpdateExpression");
    let update_operator = syntax.interface_name("UpdateOperator");
    let binary_expression = syntax.interface_name("BinaryExpression");
    let assignment_expression = syntax.interface_name("AssignmentExpression");
    let logical_expression = syntax.interface_name("LogicalExpression");
    let logical_operator = syntax.interface_name("LogicalOperator");
    let member_expression = syntax.interface_name("MemberExpression");
    let conditional_expression = syntax.interface_name("ConditionalExpression");
    let call_expression = syntax.interface_name("CallExpression");
    let new_expression = syntax.interface_name("NewExpression");
    let sequence_expression = syntax.interface_name("SequenceExpression");

    // Node objects
    syntax.add_virtual_interface(&node).unwrap();

    // Identifiers
    syntax.add_kinded_interface(&identifier).unwrap()
        .with_field("name", Type::String)
        .with_parent(&expression)
        .with_parent(&pattern);

    // Literals
    syntax.add_kinded_interface(&literal).unwrap()
        .with_field("value", Type::Interfaces(vec![
            string.clone(),
            boolean.clone(),
            null.clone(),
            number.clone(),
            regexp.clone()
        ]))
        .with_parent(&expression);

    // FIXME: As `RegexpLiteral` is virtual, this should be compiled as adding an optional field to `Literal`.

    syntax.add_virtual_interface(&regexp_literal).unwrap()
        .with_field("regex", Type::Obj(Obj::new()
            .with_field("pattern", Type::String)
            .with_field("flags", Type::String)))
        .with_parent(&literal);



    // Programs
    syntax.add_kinded_interface(&program).unwrap()
        .with_field("body", Type::Array(Box::new(Type::interface(&statement))))
        .with_parent(&node);

    // Functions (shared between function declaration, function statement, function expression)
    syntax.add_virtual_interface(&function).unwrap()
        .with_field("id", Type::Interfaces(vec![
            identifier.clone(),
            null.clone()
        ]))
        .with_field("params", Type::Array(Box::new(Type::interface(&pattern))))
        .with_field("body", Type::interface(&block_statement))
        .with_parent(&node);

    // Statements
    syntax.add_virtual_interface(&statement).unwrap()
        .with_parent(&node);

    syntax.add_kinded_interface(&empty_statement).unwrap()
        .with_parent(&statement);


    syntax.add_kinded_interface(&expression_statement).unwrap()
        .with_field("body", Type::interface(&statement).array())
        .with_parent(&statement);

    syntax.add_kinded_interface(&debugger_statement).unwrap()
        .with_parent(&statement);

    syntax.add_kinded_interface(&with_statement).unwrap()
        .with_field("object", Type::interface(&expression))
        .with_field("body", Type::interface(&statement))
        .with_parent(&statement);

    // Control flow
    syntax.add_kinded_interface(&return_statement).unwrap()
        .with_field("argument", Type::interface(&expression).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&labelled_statement).unwrap()
        .with_field("label", Type::interface(&identifier))
        .with_field("body", Type::interface(&statement))
        .with_parent(&statement);

    syntax.add_kinded_interface(&break_statement).unwrap()
        .with_field("label", Type::interface(&identifier).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&contine_statement).unwrap()
        .with_field("label", Type::interface(&identifier).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&if_statement).unwrap()
        .with_field("test", Type::interface(&expression))
        .with_field("consequent", Type::interface(&statement))
        .with_field("alternate", Type::interface(&statement).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&switch_statement).unwrap()
        .with_field("discriminant", Type::interface(&expression))
        .with_field("cases", Type::interface(&switch_case).array())
        .with_parent(&statement);

    syntax.add_kinded_interface(&switch_case).unwrap()
        .with_field("test", Type::interface(&expression).or_null().unwrap())
        .with_field("consequent", Type::interface(&statement).array())
        .with_parent(&statement);

    syntax.add_kinded_interface(&throw_statement).unwrap()
        .with_field("argument", Type::interface(&expression))
        .with_parent(&statement);

    syntax.add_kinded_interface(&try_statement).unwrap()
        .with_field("block", Type::interface(&block_statement))
        .with_field("handler", Type::interface(&catch_clause).or_null().unwrap())
        .with_field("finalizer", Type::interface(&block_statement).or_null().unwrap())
        .with_parent(&statement);

    syntax.add_kinded_interface(&catch_clause).unwrap()
        .with_field("param", Type::interface(&pattern))
        .with_field("body", Type::interface(&block_statement))
        .with_parent(&node);

    syntax.add_kinded_interface(&while_statement).unwrap()
        .with_field("test", Type::interface(&expression))
        .with_field("body", Type::interface(&statement))
        .with_parent(&statement);

    syntax.add_kinded_interface(&do_while_statement).unwrap()
        .with_field("body", Type::interface(&statement))
        .with_field("test", Type::interface(&expression))
        .with_parent(&statement);

    syntax.add_kinded_interface(&for_statement).unwrap()
        .with_field("init", Type::interfaces(&[
            &variable_declaration,
            &expression,
            &null
        ]))
        .with_field("test", Type::interface(&expression).or_null().unwrap())
        .with_field("update", Type::interface(&expression).or_null().unwrap())
        .with_field("body", Type::interface(&statement))
        .with_parent(&statement);

    syntax.add_kinded_interface(&for_in_statement).unwrap()
        .with_field("left", Type::interfaces(&[
            &variable_declaration,
            &pattern
        ]))
        .with_field("right", Type::interface(&expression))
        .with_field("body", Type::interface(&statement))
        .with_parent(&statement);

    // Declarations
    syntax.add_virtual_interface(&declaration).unwrap()
        .with_parent(&statement);

    syntax.add_kinded_interface(&function_declaration).unwrap()
        .with_field("id", Type::interface(&identifier))
        .with_parent(&function)
        .with_parent(&declaration);

    syntax.add_kinded_interface(&variable_declaration).unwrap()
        .with_field("declarations", Type::interface(&variable_declarator).array())
        .with_field("kind", Type::one_of_strings(&["var"]))
        .with_parent(&declaration);

    syntax.add_kinded_interface(&variable_declarator).unwrap()
        .with_field("id", Type::interface(&pattern))
        .with_field("init", Type::interface(&expression).or_null().unwrap())
        .with_parent(&node);

    // Expressions
    syntax.add_virtual_interface(&expression).unwrap()
        .with_parent(&node);

    syntax.add_kinded_interface(&this_expression).unwrap()
        .with_parent(&expression);

    syntax.add_kinded_interface(&array_expression).unwrap()
        .with_field("elements", Type::interface(&expression).or_null().unwrap().array())
        .with_parent(&expression);

    syntax.add_kinded_interface(&object_expression).unwrap()
        .with_field("properties", Type::interface(&property).array())
        .with_parent(&expression);

    syntax.add_kinded_interface(&property).unwrap()
        .with_field("key", Type::interfaces(&[
            &literal,
            &identifier
        ]))
        .with_field("value", Type::interface(&expression))
        .with_field("kind", Type::one_of_strings(&[
            "get",
            "set",
            "init"
        ]))
        .with_parent(&node);

    syntax.add_kinded_interface(&function_expression).unwrap()
        .with_parent(&expression)
        .with_parent(&function);

    syntax.add_kinded_interface(&unary_expression).unwrap()
        .with_field("operator", Type::interface(&unary_operator))
        .with_field("prefix", Type::Boolean)
        .with_field("argument", Type::interface(&expression))
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
        .with_field("operator", Type::interface(&update_operator))
        .with_field("argument", Type::interface(&expression))
        .with_field("prefix", Type::Boolean)
        .with_parent(&expression);

    syntax.add_enum(&update_operator).unwrap()
        .with_strings(&[
            "++",
            "--"
        ]);

    syntax.add_kinded_interface(&binary_expression).unwrap()
        .with_field("operator", Type::interface(&binary_operator))
        .with_field("left", Type::interface(&expression))
        .with_field("right", Type::interface(&expression))
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
        .with_field("operator", Type::interface(&assignment_operator))
        .with_field("left", Type::interfaces(&[
            &pattern,
            &expression
        ]))
        .with_field("right", Type::interface(&expression))
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
        .with_field("operator", Type::interface(&logical_operator))
        .with_field("left", Type::interface(&expression))
        .with_field("right", Type::interface(&expression))
        .with_parent(&expression);

    syntax.add_enum(&logical_operator).unwrap()
        .with_strings(&[
            "||",
            "&&"
        ]);

    syntax.add_kinded_interface(&member_expression).unwrap()
        .with_field("object", Type::interface(&expression))
        .with_field("property", Type::interface(&expression))
        .with_field("computed", Type::Boolean)
        .with_parent(&expression);

    syntax.add_kinded_interface(&conditional_expression).unwrap()
        .with_field("test", Type::interface(&expression))
        .with_field("alternate", Type::interface(&expression))
        .with_field("consequent", Type::interface(&expression))
        .with_parent(&expression);

    syntax.add_kinded_interface(&call_expression).unwrap()
        .with_field("callee", Type::interface(&expression))
        .with_field("arguments", Type::interface(&expression).array())
        .with_parent(&expression);

    syntax.add_kinded_interface(&new_expression).unwrap()
        .with_field("callee", Type::interface(&expression))
        .with_field("arguments", Type::interface(&expression).array())
        .with_parent(&expression);

    syntax.add_kinded_interface(&sequence_expression).unwrap()
        .with_field("expressions", Type::interface(&expression).array())
        .with_parent(&expression);

    syntax.add_virtual_interface(&pattern).unwrap()
        .with_parent(&node);
}

fn setup_binjs(syntax: &mut SyntaxBuilder) {
    let string = syntax.interface_name("String");
    let boolean = syntax.interface_name("Boolean");
    let null = syntax.interface_name("Null");
    let number = syntax.interface_name("Number");
    let regexp = syntax.interface_name("Regexp");

    syntax.add_kinded_interface(&string).unwrap()
        .with_field("value", Type::String);

    syntax.add_kinded_interface(&boolean).unwrap()
        .with_field("value", Type::Boolean);

    syntax.add_kinded_interface(&null).unwrap();

    syntax.add_kinded_interface(&number).unwrap()
        .with_field("value", Type::Number);

    syntax.add_kinded_interface(&regexp).unwrap()
        .with_field("value", Type::Obj(Obj::new()
            .with_field("pattern", Type::String)
            .with_field("flags", Type::String)));
}

pub fn syntax(level: Level) -> Syntax {
    let mut builder = SyntaxBuilder::new();
    match level {
        Level::ES5 => {
            setup_es5(&mut builder)
        }
    }
    setup_binjs(&mut builder);
    builder.as_syntax()
}