extern crate binjs;
extern crate itertools;

use binjs::generic::{ FromJSON, IdentifierName, InterfaceName, PropertyKey, SharedString };
use binjs::source::{ Shift, SourceParser };
use binjs::io::{ TokenSerializer };
use binjs::io::entropy::model::{ Dictionary, DictionaryBuilder, KindedStringMap };
use binjs::specialized::es6::io::IOPath;

use std::collections::HashMap;

use itertools::Itertools;

#[macro_use]
extern crate test_logger;

test!(test_model_dictionary_builder, {
    let parser = Shift::new();

    let mut dictionary = Dictionary::default();
    let mut files_containing_string = KindedStringMap::default();
    let sources = [
        "var x = y",
        "let x = y",
        "'use strict'",
        "function foo(x, y) { var i; for (i = 0; i < 100; ++i) { console.log('Some text', x, y + i, x + y + i, x + y + i + 1); } }"
    ];
    for source in sources.into_iter() {
        let builder = DictionaryBuilder::new(3, &mut dictionary, &mut files_containing_string);

        println!("Parsing");
        let ast  = parser.parse_str(source)
            .expect("Could not parse source");
        let mut ast = binjs::specialized::es6::ast::Script::import(&ast)
            .expect("Could not import AST");

        println!("Annotating");
        binjs::specialized::es6::scopes::AnnotationVisitor::new()
            .annotate_script(&mut ast);

        println!("Extracting dictionary");
        let mut serializer = binjs::specialized::es6::io::Serializer::new(builder);
        let mut path = IOPath::new();
        serializer.serialize(&ast, &mut path)
            .expect("Could not walk");
        let _ = serializer.done()
            .expect("Could not walk");
    }

    // We may now access data.
    eprintln!("Built a dictionary with length {}", dictionary.len());

    eprintln!("Checking identifiers per file");
    check_strings(
        &files_containing_string.identifier_name_instances,
        vec![("console", 1), ("foo", 1), ("i", 1), ("x", 3), ("y", 3)],
        |name| Some(IdentifierName::from_string(name.to_string()))
    );

    eprintln!("Checking property keys per file");
    check_strings(
        &files_containing_string.property_key_instances,
        vec![("log", 1)],
        |name| Some(PropertyKey::from_string(name.to_string()))
    );

    eprintln!("Checking interface names per file");
    check_strings(
        &files_containing_string.interface_name_instances,
        vec![
            ("", 1), // FIXME: Where is this `null`?
            ("AssertedBlockScope", 1),
            ("AssertedDeclaredName", 3),
            ("AssertedParameterScope", 1),
            ("AssertedPositionalParameterName", 1), // FIXME: Where is this?
            ("AssertedScriptGlobalScope", 4),
            ("AssertedVarScope", 1),
            ("AssignmentExpression", 1),
            ("AssignmentTargetIdentifier", 1),
            ("BinaryExpression", 1),
            ("BindingIdentifier", 3),
            ("Block", 1),
            ("CallExpression", 1),
            ("Directive", 1),
            ("EagerFunctionDeclaration", 1),
            ("ExpressionStatement", 1),
            ("ForStatement", 1),
            ("FormalParameters", 1),
            ("FunctionOrMethodContents", 1),
            ("IdentifierExpression", 3),
            ("LiteralNumericExpression", 1),
            ("LiteralStringExpression", 1),
            ("Script", 4),
            ("StaticMemberExpression", 1),
            ("UpdateExpression", 1),
            ("VariableDeclaration", 3),
            ("VariableDeclarator", 3),
        ],
        |name| InterfaceName::from_string(name.to_string())
    );

    eprintln!("String literals per file");
    check_strings(
        &files_containing_string.string_literal_instances,
        vec![
            ("Some text", 1),
            ("use strict", 1),
        ],
        |value| Some(SharedString::from_string(value.to_string()))
    );

    eprintln!("String enum instances");
    check_strings(
        &files_containing_string.string_enum_instances,
        vec![
            ("+", 1),
            ("var", 2),
            ("<", 1),
            ("let", 1),
            ("++", 1),
            ("non-const lexical", 1),
        ],
        |value| SharedString::from_string(value.to_string())
    );
});

fn check_strings<T, F>(found: &HashMap<T, usize>, expected: Vec<(&str, usize)>, f: F)
    where
        F: Fn(&str) -> T,
        T: Eq + std::hash::Hash + Ord + Clone + std::fmt::Debug,
{
    let found = found
        .iter()
        .map(|(name, instances)| ((*name).clone(), *instances))
        .sorted();
    let expected = expected
        .into_iter()
        .map(|(name, instances)| {
            (f(name), instances)
        })
        .sorted();
    assert_eq!(found, expected);

}