use estree::grammar::*;

pub enum Level {
    ES5,
}

/// Hardcoding for https://github.com/estree/estree/blob/master/es5.md#exceptions
fn setup_es5(syntax: &mut Syntax) {
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
    let update_operator = syntax.interface_name("UpdateOperator");
    let assignment_operator = syntax.interface_name("AssignmentOperator");
    let logical_operator = syntax.interface_name("LogicalOperator");

    // Node objects
    syntax.add_virtual_interface("Node");

    // Identifiers
    syntax.add_tagged_interface("Identifier")
        .with_field("name", Type::String)
        .refines("Expression")
        .refines("Pattern");

    // Literals
    syntax.add_tagged_interface("Literal")
        .with_field("value", Type::Interfaces(vec![
            string.clone(),
            boolean.clone(),
            null.clone(),
            number.clone(),
            regexp.clone()
        ]))
        .refines("Expression");

    // FIXME: As `RegexpLiteral` is virtual, this should be compiled as adding an optional field to `Literal`.
    syntax.add_virtual_interface("RegExpLiteral")
        .with_field("regex", Type::Structure(Structure::new()
            .with_field("pattern", Type::String)
            .with_field("flags", Type::String)))
        .refines("Literal");



    // Programs
    syntax.add_tagged_interface("Program")
        .with_field("body", Type::Array(Box::new(Type::interface(&statement))))
        .refines("Node");

    // Functions (shared between function declaration, function statement, function expression)
    syntax.add_virtual_interface("Function")
        .with_field("id", Type::Interfaces(vec![
            identifier.clone(),
            null.clone()
        ]))
        .with_field("params", Type::Array(Box::new(Type::interface(&pattern))))
        .with_field("body", Type::interface(&block_statement))
        .refines("Node");

    // Statements
    syntax.add_virtual_interface("Statement")
        .refines("Node");

    syntax.add_tagged_interface("EmptyStatement")
        .refines("Statement");

    syntax.add_tagged_interface("ExpressionStatement")
        .with_field("body", Type::interface(&statement).array())
        .refines("Statement");

    syntax.add_tagged_interface("DebuggerStatement")
        .refines("Statement");

    syntax.add_tagged_interface("WithStatement")
        .with_field("object", Type::interface(&expression))
        .with_field("body", Type::interface(&statement))
        .refines("Statement");

    // Control flow
    syntax.add_tagged_interface("ReturnStatement")
        .with_field("argument", Type::interface(&expression).or_null())
        .refines("Statement");

    syntax.add_tagged_interface("LabelledStatement")
        .with_field("label", Type::interface(&identifier))
        .with_field("body", Type::interface(&statement))
        .refines("Statement");

    syntax.add_tagged_interface("BreakStatement")
        .with_field("label", Type::interface(&identifier).or_null())
        .refines("Statement");

    syntax.add_tagged_interface("ContinueStatement")
        .with_field("label", Type::interface(&identifier).or_null())
        .refines("Statement");

    syntax.add_tagged_interface("IfStatement")
        .with_field("test", Type::interface(&expression))
        .with_field("consequent", Type::interface(&statement))
        .with_field("alternate", Type::interface(&statement).or_null())
        .refines("Statement");

    syntax.add_tagged_interface("SwitchStatement")
        .with_field("discriminant", Type::interface(&expression))
        .with_field("cases", Type::interface(&switch_case).array())
        .refines("Statement");

    syntax.add_tagged_interface("SwitchCase")
        .with_field("test", Type::interface(&expression).or_null())
        .with_field("consequent", Type::interface(&statement).array())
        .refines("Statement");

    syntax.add_tagged_interface("ThrowStatement")
        .with_field("argument", Type::interface(&expression))
        .refines("Statement");

    syntax.add_tagged_interface("TryStatement")
        .with_field("block", Type::interface(&block_statement))
        .with_field("handler", Type::interface(&catch_clause).or_null())
        .with_field("finalizer", Type::interface(&block_statement).or_null())
        .refines("TryStatement");

    syntax.add_tagged_interface("CatchClause")
        .with_field("param", Type::interface(&pattern))
        .with_field("body", Type::interface(&block_statement))
        .refines("Node");

    syntax.add_tagged_interface("WhileStatement")
        .with_field("test", Type::interface(&expression))
        .with_field("body", Type::interface(&statement))
        .refines("Statement");

    syntax.add_tagged_interface("DoWhileStatement")
        .with_field("body", Type::interface(&statement))
        .with_field("test", Type::interface(&expression))
        .refines("Statement");

    syntax.add_tagged_interface("ForStatement")
        .with_field("init", Type::interfaces(&[
            &variable_declaration,
            &expression,
            &null
        ]))
        .with_field("test", Type::interface(&expression).or_null())
        .with_field("update", Type::interface(&expression).or_null())
        .with_field("body", Type::interface(&statement))
        .refines("Statement");

    syntax.add_tagged_interface("ForInStatement")
        .with_field("left", Type::interfaces(&[
            &variable_declaration,
            &pattern
        ]))
        .with_field("right", Type::interface(&expression))
        .with_field("body", Type::interface(&statement))
        .refines("Statement");

    // Declarations
    syntax.add_virtual_interface("Declaration")
        .refines("Statement");

    syntax.add_tagged_interface("FunctionDeclaration")
        .with_field("id", Type::interface(&identifier))
        .refines("Function")
        .refines("Declaration");

    syntax.add_tagged_interface("variable_declaration")
        .with_field("declarations", Type::interface(&variable_declarator).array())
        .with_field("kind", Type::one_of_strings(&["var"]))
        .refines("Declaration");

    syntax.add_tagged_interface("variable_declarator")
        .with_field("id", Type::interface(&pattern))
        .with_field("init", Type::interface(&expression).or_null())
        .refines("Node");

    // Expressions
    syntax.add_virtual_interface("Expression")
        .refines("Node");

    syntax.add_tagged_interface("ThisExpression")
        .refines("Expression");

    syntax.add_tagged_interface("ArrayExpression")
        .with_field("elements", Type::interface(&expression).or_null().array())
        .refines("Expression");

    syntax.add_tagged_interface("ObjectExpression")
        .with_field("properties", Type::interface(&property).array())
        .refines("Expression");

    syntax.add_tagged_interface("Property")
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
        .refines("Node");

    syntax.add_tagged_interface("FunctionExpression")
        .refines("Expression")
        .refines("Function");

    syntax.add_tagged_interface("UnaryExpression")
        .with_field("operator", Type::interface(&unary_operator))
        .with_field("prefix", Type::Boolean)
        .with_field("argument", Type::interface(&expression))
        .refines("Expression");

    syntax.add_enum("UnaryOperator")
        .with_strings(&[
            "-",
            "+",
            "!",
            "~",
            "typeof",
            "void",
            "delete"
        ]);

    syntax.add_tagged_interface("UpdateExpression")
        .with_field("operator", Type::interface(&update_operator))
        .with_field("argument", Type::interface(&expression))
        .with_field("prefix", Type::Boolean)
        .refines("Expression");

    syntax.add_enum("UpdateOperator")
        .with_strings(&[
            "++",
            "--"
        ]);

    syntax.add_tagged_interface("BinaryExpression")
        .with_field("operator", Type::interface(&binary_operator))
        .with_field("left", Type::interface(&expression))
        .with_field("right", Type::interface(&expression))
        .refines("Expression");

    syntax.add_enum("BinaryOperator")
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

    syntax.add_tagged_interface("AssignmentExpression")
        .with_field("operator", Type::interface(&assignment_operator))
        .with_field("left", Type::interfaces(&[
            &pattern,
            &expression
        ]))
        .with_field("right", Type::interface(&expression))
        .refines("Expression");

    syntax.add_enum("AssignmentOperator")
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

    syntax.add_tagged_interface("LogicalExpression")
        .with_field("operator", Type::interface(&logical_operator))
        .with_field("left", Type::interface(&expression))
        .with_field("right", Type::interface(&expression))
        .refines("Expression");

    syntax.add_enum("LogicalOperator")
        .with_strings(&[
            "||",
            "&&"
        ]);

    syntax.add_tagged_interface("MemberExpression")
        .with_field("object", Type::interface(&expression))
        .with_field("property", Type::interface(&expression))
        .with_field("computed", Type::Boolean)
        .refines("Expression");

    syntax.add_tagged_interface("ConditionalExpression")
        .with_field("test", Type::interface(&expression))
        .with_field("alternate", Type::interface(&expression))
        .with_field("consequent", Type::interface(&expression))
        .refines("Expression");

    syntax.add_tagged_interface("CallExpression")
        .with_field("callee", Type::interface(&expression))
        .with_field("arguments", Type::interface(&expression).array())
        .refines("Expression");

    syntax.add_tagged_interface("NewExpression")
        .with_field("callee", Type::interface(&expression))
        .with_field("arguments", Type::interface(&expression).array())
        .refines("Expression");

    syntax.add_tagged_interface("SequenceExpression")
        .with_field("expressions", Type::interface(&expression).array())
        .refines("Expression");

    syntax.add_virtual_interface("Pattern")
        .refines("Node");
}

fn setup_binjs(syntax: &mut Syntax) {
    syntax.add_tagged_interface("String")
        .with_field("value", Type::String);

    syntax.add_tagged_interface("Boolean")
        .with_field("value", Type::Boolean);

    syntax.add_tagged_interface("Null");

    syntax.add_tagged_interface("Number")
        .with_field("value", Type::Number);

    syntax.add_tagged_interface("Regexp")
        .with_field("value", Type::Structure(Structure::new()
            .with_field("pattern", Type::String)
            .with_field("flags", Type::String)));
}

pub fn syntax(level: Level) -> Syntax {
    let mut syntax = Syntax::new();
    match level {
        Level::ES5 => {
            setup_es5(&mut syntax)
        }
    }
    setup_binjs(&mut syntax);
    syntax
}