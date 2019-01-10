extern crate binjs;
extern crate itertools;

use binjs::generic::{FromJSON, IdentifierName, InterfaceName, Offset, PropertyKey, SharedString};
use binjs::io::entropy;
use binjs::io::entropy::dictionary::{
    Dictionary, DictionaryBuilder, FilesContaining, KindedStringMap,
};
use binjs::io::entropy::probabilities::InstancesToProbabilities;
use binjs::io::{Deserialization, TokenSerializer};
use binjs::source::{Shift, SourceParser};
use binjs::specialized::es6::ast::{Script, Visitor, WalkPath, Walker};
use binjs::specialized::es6::io::IOPath;

use std::collections::HashMap;

use itertools::Itertools;

#[macro_use]
extern crate test_logger;

/// A visitor designed to reset offsets to 0.
struct OffsetCleanerVisitor;
impl Visitor<()> for OffsetCleanerVisitor {
    fn visit_offset(&mut self, _path: &WalkPath, node: &mut Offset) -> Result<(), ()> {
        *node = binjs::generic::Offset(0);
        Ok(())
    }
}

test!(test_entropy_roundtrip, {
    let parser = Shift::new();

    let mut dictionary = Dictionary::new(3, 32);
    let mut files_containing_string = KindedStringMap::default();
    let sources = [
        "var x = y",
        "let x = y",
        "'use strict'",
        "function foo(x, y) { var i; for (i = 0; i < 100; ++i) { console.log('Some text', x, y + i, x + y + i, x + y + i + 1); } }"
    ];
    for source in &sources {
        let builder = DictionaryBuilder::new(&mut dictionary, &mut files_containing_string);

        println!("Parsing");
        let ast = parser.parse_str(source).expect("Could not parse source");
        let mut ast =
            binjs::specialized::es6::ast::Script::import(&ast).expect("Could not import AST");

        println!("Annotating");
        binjs::specialized::es6::scopes::AnnotationVisitor::new().annotate_script(&mut ast);

        println!("Extracting dictionary");
        let mut serializer = binjs::specialized::es6::io::Serializer::new(builder);
        let mut path = IOPath::new();
        serializer
            .serialize(&ast, &mut path)
            .expect("Could not walk");
        let _ = serializer.done().expect("Could not walk");
    }

    // We may now access data.
    println!(
        "Built a dictionary with {} states, {} strings",
        dictionary.len(),
        files_containing_string.len()
    );

    println!("Checking identifiers per file");
    check_strings(
        &files_containing_string.identifier_name_instances,
        vec![("console", 1), ("foo", 1), ("i", 1), ("x", 3), ("y", 3)],
        |name| Some(IdentifierName::from_string(name.to_string())),
    );

    println!("Checking property keys per file");
    check_strings(
        &files_containing_string.property_key_instances,
        vec![("log", 1)],
        |name| Some(PropertyKey::from_string(name.to_string())),
    );

    println!("Checking interface names per file");
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
        |name| InterfaceName::from_string(name.to_string()),
    );

    println!("String literals per file");
    check_strings(
        &files_containing_string.string_literal_instances,
        vec![("Some text", 1), ("use strict", 1)],
        |value| Some(SharedString::from_string(value.to_string())),
    );

    println!("String enum instances");
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
        |value| SharedString::from_string(value.to_string()),
    );

    let options = entropy::Options::new(dictionary.instances_to_probabilities("dictionary"));

    println!("Starting roundtrip with dictionary");
    for source in &sources {
        println!("Reparsing");
        let ast = parser.parse_str(source).expect("Could not parse source");
        let mut ast =
            binjs::specialized::es6::ast::Script::import(&ast).expect("Could not import AST");

        println!("Reannotating");
        binjs::specialized::es6::scopes::AnnotationVisitor::new().annotate_script(&mut ast);

        let mut path = IOPath::new();

        println!("Serializing with entropy");
        let encoder = entropy::write::Encoder::new(options.clone());
        let mut serializer = binjs::specialized::es6::io::Serializer::new(encoder);
        serializer
            .serialize(&ast, &mut path)
            .expect("Could not walk");
        let data = serializer.done().expect("Could not walk");
        assert_eq!(path.len(), 0);

        println!("Deserializing with entropy");
        let decoder = entropy::read::Decoder::new(options.clone(), std::io::Cursor::new(data))
            .expect("Could not create decoder");
        let mut deserializer = binjs::specialized::es6::io::Deserializer::new(decoder);
        let mut script: Script = deserializer
            .deserialize(&mut path)
            .expect("Could not deserialize");

        println!("Checking equality between ASTs");
        // At this stage, we have a problem: offsets are 0 in `ast`, but not necessarily
        // in `decoded`.
        script
            .walk(&mut WalkPath::new(), &mut OffsetCleanerVisitor)
            .expect("Could not cleanup offsets");
        assert_eq!(ast, script);
    }
});

fn check_strings<T, F>(found: &HashMap<T, FilesContaining>, expected: Vec<(&str, usize)>, f: F)
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
        .map(|(name, instances)| (f(name), FilesContaining(instances)))
        .sorted();
    assert_eq!(found, expected);
}
