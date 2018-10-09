//! Test that the paths we collect when parsing/serializing a source
//! make sense.
//!
//! For this purpose, we create a dummy serializer whose sole role
//! is to store a trace of every node we have visited, along with
//! the Path we took to visit it.

extern crate binjs;

use binjs::generic::{ FieldName, FromJSON, IdentifierName, InterfaceName, PropertyKey, SharedString };
use binjs::source::{ Shift, SourceParser };
use binjs::io::{ TokenWriter, TokenWriterError };
use binjs::specialized::es6::io::IOPath;

use std::rc::Rc;
use std::cell::RefCell;

#[macro_use]
extern crate test_logger;

/// Individual events encountered while walking the AST using a serializer.
#[derive(Debug)]
enum Event {
    IdentifierNameAt {
        value: Option<IdentifierName>,
        path: IOPath
    },
    PropertyKeyAt {
        value: Option<PropertyKey>,
        path: IOPath
    },
    StringAt {
        value: Option<SharedString>,
        path: IOPath,
    },
    StringEnumAt {
        value: SharedString,
        path: IOPath,
    },
    BoolAt {
        value: Option<bool>,
        path: IOPath,
    },
    TaggedTupleAt {
        interface: InterfaceName,
        path: IOPath,
    },
    ListAt {
        len: usize,
        path: IOPath,
    },
    UnsignedLongAt {
        value: u32,
        path: IOPath,
    },
    FloatAt {
        value: Option<f64>,
        path: IOPath,
    },
}

struct PathTraceWriter {
    trace: Rc<RefCell<Vec<Event>>>,
}

impl TokenWriter for PathTraceWriter {
    type Tree = ();
    type Statistics = usize;
    type Data = [u8;0];
    fn identifier_name_at(&mut self, value: Option<&IdentifierName>, path: &IOPath) -> Result<(), TokenWriterError> {
        self.trace.borrow_mut().push(Event::IdentifierNameAt {
            value: value.cloned(),
            path: path.clone()
        });
        Ok(())
    }
    fn string_at(&mut self, value: Option<&SharedString>, path: &IOPath) -> Result<(), TokenWriterError> {
        self.trace.borrow_mut().push(Event::StringAt {
            value: value.cloned(),
            path: path.clone()
        });
        Ok(())
    }
    fn string_enum_at(&mut self, value: &SharedString, path: &IOPath) -> Result<(), TokenWriterError> {
        self.trace.borrow_mut().push(Event::StringEnumAt {
            value: value.clone(),
            path: path.clone()
        });
        Ok(())
    }
    fn bool_at(&mut self, value: Option<bool>, path: &IOPath) -> Result<(), TokenWriterError> {
        self.trace.borrow_mut().push(Event::BoolAt {
            value: value.clone(),
            path: path.clone()
        });
        Ok(())
    }
    fn unsigned_long_at(&mut self, value: u32, path: &IOPath) -> Result<(), TokenWriterError> {
        self.trace.borrow_mut().push(Event::UnsignedLongAt {
            value: value.clone(),
            path: path.clone()
        });
        Ok(())
    }
    fn tagged_tuple_at(&mut self, tag: &InterfaceName, _children: &[(FieldName, Self::Tree)], path: &IOPath) -> Result<Self::Tree, TokenWriterError> {
        self.trace.borrow_mut().push(Event::TaggedTupleAt {
            interface: tag.clone(),
            path: path.clone()
        });
        Ok(())
    }
    fn list_at(&mut self, items: Vec<()>, path: &IOPath) -> Result<Self::Tree, TokenWriterError> {
        self.trace.borrow_mut().push(Event::ListAt {
            len: items.len(),
            path: path.clone()
        });
        Ok(())
    }
    fn float_at(&mut self, value: Option<f64>, path: &IOPath) -> Result<Self::Tree, TokenWriterError> {
        self.trace.borrow_mut().push(Event::FloatAt {
            value: value.clone(),
            path: path.clone()
        });
        Ok(())
    }
    fn property_key_at(&mut self, value: Option<&PropertyKey>, path: &IOPath) -> Result<Self::Tree, TokenWriterError> {
        self.trace.borrow_mut().push(Event::PropertyKeyAt {
            value: value.cloned(),
            path: path.clone()
        });
        Ok(())
    }
    fn done(self) ->  Result<(Self::Data, Self::Statistics), TokenWriterError> {
        unimplemented!()
    }
}

test!(test_es6_paths, {
    println!("Preparing test.");

    let trace = Rc::new(RefCell::new(vec![]));
    let parser = Shift::new();
    let writer = PathTraceWriter { trace: trace.clone() };

    let source = "function foo(x, y) { var i; for (i = 0; i < 100; ++i) { console.log(x, y + i, x + y + i, x + y + i + 1); } }";

    println!("Parsing");
    let ast  = parser.parse_str(source)
        .expect("Could not parse source");
    let mut ast = binjs::specialized::es6::ast::Script::import(&ast)
        .expect("Could not import AST");

    println!("Annotating");
    binjs::specialized::es6::scopes::AnnotationVisitor::new()
        .annotate_script(&mut ast);

    println!("Walking paths");
    let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
    let mut path = IOPath::new();
    serializer.serialize(&ast, &mut path)
        .expect("Could not walk");

    // Extract properties, as a sample.
    let mut properties : Vec<_> = trace.borrow()
        .iter()
        .filter_map(|step| {
            if let Event::PropertyKeyAt { ref value, ref path } = *step {
                Some((value.clone(), path.clone()))
            } else {
                None
            }
        })
        .collect();

    // The first (and only) item of `properties` should be the `log` of `console.log`.
    assert_eq!(properties.len(), 1);
    let log = properties.pop().unwrap();

    println!("{:?}", log);

    println!("Checking property value");
    let name = log.0
        .expect("PropertyKey `log` should be Some(...)");
    assert_eq!(name.as_str(), "log");

    println!("Checking path");
    let path = log.1;
    assert_eq!(path.len(), 8); // May need to be adapted if/when we change webidl.

    let mut iter = path.iter();

    // Script
    let path_item = iter.next().unwrap();
    assert_eq!(path_item.interface().as_str(), "Script");
    assert_eq!(path_item.field().1.as_str(), "statements");
    assert_eq!(path_item.field().0, 2);

    // Script.statements
    let path_item = iter.next().unwrap();
    assert_eq!(path_item.interface().as_str(), "EagerFunctionDeclaration");
    assert_eq!(path_item.field().1.as_str(), "contents");
    assert_eq!(path_item.field().0, 5);

    // Script.statements > EagerFunctionDeclaration.contents
    let path_item = iter.next().unwrap();
    assert_eq!(path_item.interface().as_str(), "FunctionOrMethodContents");
    assert_eq!(path_item.field().1.as_str(), "body");
    assert_eq!(path_item.field().0, 4);

    // Script.statements > EagerFunctionDeclaration.contents > FunctionOrMethodContents.body
    let path_item = iter.next().unwrap();
    assert_eq!(path_item.interface().as_str(), "ForStatement");
    assert_eq!(path_item.field().1.as_str(), "body");
    assert_eq!(path_item.field().0, 3);

    // Script.statements > EagerFunctionDeclaration.contents > FunctionOrMethodContents.body > ForStatement.body
    let path_item = iter.next().unwrap();
    assert_eq!(path_item.interface().as_str(), "Block");
    assert_eq!(path_item.field().1.as_str(), "statements");
    assert_eq!(path_item.field().0, 1);

    // Script.statements > EagerFunctionDeclaration.contents > FunctionOrMethodContents.body > ForStatement.body > Block.statements
    let path_item = iter.next().unwrap();
    assert_eq!(path_item.interface().as_str(), "ExpressionStatement");
    assert_eq!(path_item.field().1.as_str(), "expression");
    assert_eq!(path_item.field().0, 0);

    // Script.statements > EagerFunctionDeclaration.contents > FunctionOrMethodContents.body > ForStatement.body > Block.statements
    //  > ExpressionStatement.expression
    let path_item = iter.next().unwrap();
    assert_eq!(path_item.interface().as_str(), "CallExpression");
    assert_eq!(path_item.field().1.as_str(), "callee");
    assert_eq!(path_item.field().0, 0);


    // Script.statements > EagerFunctionDeclaration.contents > FunctionOrMethodContents.body > ForStatement.body > Block.statements
    //  > ExpressionStatement.expression > CalExpression.callee
    let path_item = iter.next().unwrap();
    assert_eq!(path_item.interface().as_str(), "StaticMemberExpression");
    assert_eq!(path_item.field().1.as_str(), "property");
    assert_eq!(path_item.field().0, 1);
});