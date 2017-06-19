//! Grammars for specifying an AST that this tool can manipulate.
//!
//! This abstracts away the concepts of [ESTree](https://github.com/estree/estree).

use ast;
use util::f64_of;

use serde_json;
use serde_json::Value as JSON;

use std;
use std::cell::*;
use std::collections::{ HashMap, HashSet };
use std::fmt::Debug;
use std::hash::*;
use std::ops::Deref;
use std::rc::*;


/// The name of an interface or enum.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct NodeName(Rc<String>);
impl NodeName {
    pub fn to_string(&self) -> &String {
        self.0.as_ref()
    }
    pub fn to_str(&self) -> &str {
        &self.0
    }
}
impl Debug for NodeName {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        self.to_str().fmt(formatter)
    }
}

/// The kind attached to an actual AST node, representing the interface
/// it inhabits (aka "dynamic type").
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Kind(Rc<String>);
impl Kind {
    pub fn to_string(&self) -> &String {
        self.0.as_ref()
    }
    pub fn to_str(&self) -> &str {
        &self.0
    }
}

/// The name of a field.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct FieldName(Rc<String>);
impl FieldName {
    pub fn to_str(&self) -> &str {
        self.0.as_ref()
    }
    pub fn to_string(&self) -> &String {
        self.0.as_ref()
    }
}

/// Representation of a field in an object.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Field {
    name: FieldName,
    type_: Type,
}
impl Hash for Field {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.name.hash(state)
    }
}
impl Field {
    pub fn new(name: FieldName, type_: Type) -> Self {
        Field {
            name,
            type_
        }
    }
    pub fn name(&self) -> &FieldName {
        &self.name
    }
    pub fn type_(&self) -> &Type {
        &self.type_
    }
}

/// A type, typically that of a field.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /// An array of values of the same type.
    Array(Box<Type>),

    /// A choice between several literals, e.g. `"get" | "set"`.
    Enum(NodeName),

    /// A value that may belong to one or more interfaces.
    Interfaces {
        /// The names of interfaces. Never empty.
        names: Vec<NodeName>,

        /// If `true`, `null` is an acceptable value.
        or_null: bool
    },

    /// A boolean.
    Boolean { or_null: bool },

    /// A string.
    String { or_null: bool },

    /// A number.
    Number { or_null: bool },
}

impl Type {
    /// Shorthand constructor.
    pub fn interface(name: &NodeName) -> Self {
        Type::Interfaces {
            names: vec![name.clone()],
            or_null: false
        }
    }
    pub fn enumeration(name: &NodeName) -> Self {
        Type::Enum(name.clone())
    }
    pub fn interfaces(names: &[&NodeName]) -> Self {
        Type::Interfaces {
            names: names.iter().cloned().cloned().collect(),
            or_null: false
        }
    }
    pub fn string() -> Self {
        Type::String { or_null: false }
    }
    pub fn number() -> Self {
        Type::Number { or_null: false }
    }
    pub fn bool() -> Self {
        Type::Boolean { or_null: false }
    }
    pub fn array(self) -> Self {
        Type::Array(Box::new(self))
    }
    pub fn or_null(self) -> Option<Self> {
        match self {
            Type::Interfaces { names, .. } =>
                Some(Type::Interfaces {
                    names,
                    or_null: true
                }),
            Type::String { .. } =>
                Some(Type::String { or_null: true }),
            Type::Boolean { .. } =>
                Some(Type::Boolean { or_null: true }),
            Type::Number { .. } =>
                Some(Type::Number { or_null: true }),
            _ => None
        }
    }
}

/// Representation of an object, i.e. a set of fields.
///
/// Field order is *not* specified, but is expected to remain stable during encoding
/// operations and during decoding operations. Note in particular that the order may
/// change between encoding and decoding.
#[derive(Clone, Debug)]
pub struct Obj {
    fields: Vec<Field>,
}
impl PartialEq for Obj {
    fn eq(&self, other: &Self) -> bool {
        // Normalize order before comparing.
        let me : HashSet<_> = self.fields.iter().collect();
        let other : HashSet<_> = other.fields.iter().collect();
        me == other
    }
}
impl Eq for Obj {}

impl Obj {
    /// Create a new empty structure
    pub fn new() -> Self {
        Obj {
            fields: Vec::new()
        }
    }
    /// A list of the fields in the structure.
    pub fn fields<'a>(&'a self) -> &'a [Field] {
        &self.fields
    }
    /// Fetch a specific field in the structure
    pub fn field<'a>(&'a self, name: &FieldName) -> Option<&'a Field> {
        self.fields.iter().find(|field| &field.name == name)
    }

    pub fn with_own_field(self, field: Field) -> Self {
        if self.field(field.name()).is_some() {
            warn!("Field: attempting to overwrite {:?}", field.name());
            return self
        }
        let mut fields = self.fields;
        fields.push(field);
        Obj {
            fields
        }
    }

    /// Extend a structure with a field.
    pub fn with_field(self, name: &FieldName, type_: Type) -> Self {
        if self.field(name).is_some() {
            warn!("Field: attempting to overwrite {:?}", name);
            return self
        }
        let mut fields = self.fields;
        fields.push(Field {
            name: name.clone(),
            type_
        });
        Obj {
            fields
        }
    }
}

/// Structure of an enum of strings. `null` is never an acceptable value.
#[derive(Clone, Debug)]
pub struct Enum {
    /// Unordered list of strings, without duplicates.
    strings: Vec<String>,
}
impl Default for Enum {
    fn default() -> Self {
        Enum {
            strings: Vec::new(),
        }
    }
}
impl Enum {
    pub fn strings(&self) -> &[String] {
        &self.strings
    }

    /// Add a string to the enum. Idempotent.
    pub fn with_string(&mut self, string: &str) -> &mut Self {
        let string = string.to_string();
        if self.strings.iter().find(|x| **x == string).is_none() {
            self.strings.push(string.to_string())
        }
        self
    }
    /// Add several enums to the list. Idempotent.
    pub fn with_strings(&mut self, strings: &[&str]) -> &mut Self {
        for string in strings {
            self.with_string(string);
        }
        self
    }
}

#[derive(Clone, Debug)]
pub struct InterfaceDeclaration {
    /// The name of the interface, e.g. `Node`.
    name: NodeName,

    /// The kind used to differentiate node that inhabit this
    /// interface from nodes inhabiting other interfaces.
    ///
    /// May be `None` for interfaces such as `Node` or `Expression`
    /// that serve only as a common ancestor for a sum of refined sub-interfaces
    /// and have no inhabitants of their own.
    kind: Option<Kind>,

    /// The parents of this interface.
    parent_interfaces: Vec<NodeName>,

    /// The contents of this interface, excluding the contents of parent interfaces.
    own_contents: Obj,
}

impl InterfaceDeclaration {
    pub fn with_field(&mut self, name: &FieldName, type_: Type) -> &mut Self {
        // FIXME: There must be a better way to do this.
        let mut contents = Obj::new();
        std::mem::swap(&mut self.own_contents, &mut contents);
        self.own_contents = contents.with_field(name, type_);
        self
    }
    pub fn with_own_field(&mut self, field: Field) -> &mut Self {
        // FIXME: There must be a better way to do this.
        let mut contents = Obj::new();
        std::mem::swap(&mut self.own_contents, &mut contents);
        self.own_contents = contents.with_own_field(field);
        self
    }
    /// Add a parent to the interface.
    /// An interface may have several parents. Each parent MUST be
    /// an interface, however, parents may be added at any time before
    /// the call to `into_syntax`.
    pub fn with_parent(&mut self, parent: &NodeName) -> &mut Self {
        if self.parent_interfaces.iter().find(|x| *x == parent).is_none() {
            self.parent_interfaces.push(parent.clone())
        }
        self
    }
}

/// A data structure used to progressively construct the `Syntax`.
pub struct SyntaxBuilder {
    /// All the interfaces entered so far.
    interfaces: HashMap<NodeName, RefCell<InterfaceDeclaration>>,

    /// All the enums entered so far.
    enums: HashMap<NodeName, RefCell<Enum>>,

    names: HashMap<String, Rc<String>>,
}

impl SyntaxBuilder {
    pub fn new() -> Self {
        SyntaxBuilder {
            interfaces: HashMap::new(),
            enums: HashMap::new(),
            names: HashMap::new()
        }
    }

    /// Return an `NodeName` for a name. Equality comparison
    /// on `NodeName` can be performed by checking physical
    /// equality.
    pub fn node_name(&mut self, name: &str) -> NodeName {
        if let Some(result) = self.names.get(name) {
            return NodeName(result.clone())
        }
        let shared = Rc::new(name.to_string());
        let result = NodeName(shared.clone());
        self.names.insert(name.to_string(), shared);
        result
    }

    pub fn field_name(&mut self, name: &str) -> FieldName {
        if let Some(result) = self.names.get(name) {
            return FieldName(result.clone());
        }
        let shared = Rc::new(name.to_string());
        let result = FieldName(shared.clone());
        self.names.insert(name.to_string(), shared);
        result
    }

    pub fn kind_name(&mut self, name: &str) -> Kind {
        if let Some(result) = self.names.get(name) {
            return Kind(result.clone());
        }
        let shared = Rc::new(name.to_string());
        let result = Kind(shared.clone());
        self.names.insert(name.to_string(), shared);
        result
    }

    /// Add an interface with a `kind` identical to its name.
    pub fn add_kinded_interface(&mut self, name: &NodeName) -> Option<RefMut<InterfaceDeclaration>> {
        let kind = Kind(name.0.clone());
        let result = self.add_virtual_interface(name)
            .map(|mut result| {result.kind = Some(kind); result});
        result
    }

    /// Add a virtual interface, i.e. one that doesn't have a `kind`,
    /// i.e. one that does not have immediate inhabitants. Super-interfaces
    /// or sub-interfaces with a `kind` may have inhabitants.
    pub fn add_virtual_interface(&mut self, name: &NodeName) -> Option<RefMut<InterfaceDeclaration>> {
        if self.interfaces.get(name).is_some() {
            return None;
        }
        let interface = InterfaceDeclaration {
            name: name.clone(),
            kind: None,
            own_contents: Obj::new(),
            parent_interfaces: Vec::new(),
        };
        self.interfaces.insert(name.clone(), RefCell::new(interface));
        self.interfaces.get(name).map(RefCell::borrow_mut)
    }

    /// Add a named enumeration.
    pub fn add_enum(&mut self, name: &NodeName) -> Option<RefMut<Enum>> {
        if self.enums.get(name).is_some() {
            return None;
        }
        let e = RefCell::new(Enum::default());
        self.enums.insert(name.clone(), e);
        self.enums.get(name).map(RefCell::borrow_mut)
    }

    /// Generate the graph.
    pub fn into_syntax<'a>(self, options: SyntaxOptions<'a>) -> Syntax {
        let mut interfaces_by_name = HashMap::new();
        let mut interfaces_by_kind = HashMap::new();
        let mut node_names = HashMap::new();
        let mut kinds = HashMap::new();
        let mut field_names : HashMap<String, FieldName> = HashMap::new();

        assert!(self.interfaces.get(options.root).is_some(),
            "Cannot find root interface {:?}", options.root);

        for (name, interface) in &self.interfaces {
            debug!("Registering name and interface.");
            {
                let string = name.to_str().to_string();
                assert!(node_names.insert(string.clone(), name.clone()).is_none());
            }

            if let Some(ref kind) = interface.borrow().kind {
                assert!(kinds.insert(kind.to_string().clone(), kind.clone()).is_none());
            }

            debug!("Checking that all enums/interfaces are defined.");
            {
                for field in interface.borrow().own_contents.fields() {
                    match *field.type_() {
                        Type::Enum(ref field_name) =>
                            assert!(self.enums.get(field_name).is_some(),
                                "While compiling {:?}, could not find an enum named {:?}",
                                name, field_name),
                        Type::Interfaces { ref names, .. } =>
                            for interface_name in names {
                                assert!(self.interfaces.get(interface_name).is_some(),
                                    "While compiling {:?}, could not find an interface named {:?}",
                                    name, interface_name)
                            },
                        _ => {}
                    }
                }
            }


            // Compute the fields and ancestors of `interface`.
            let mut ancestors_met = HashSet::new();
            let mut my_fields = HashMap::new();

            // To do so, walk the ancestors of `interface`. Algorithmically,
            // this could explode, but in practice, I haven't seen a depth higher than 4.
            let mut roots = vec![name.clone()];
            let mut all_my_ancestors = HashSet::new();
            while let Some(root) = roots.pop() {
                if ancestors_met.contains(&root) {
                    // With mutual inheritance, let's not copy stuff more than
                    // once. Should also prevent (but not detect) infinite loops.
                    continue;
                }

                all_my_ancestors.insert(root.clone());
                ancestors_met.insert(root.clone());
                let node = self.interfaces.get(&root).unwrap();
                debug_assert_eq!(node.borrow().name, root);

                for parent_names in &node.borrow().parent_interfaces {
                    roots.push(parent_names.clone());
                }
                for field in &node.borrow().own_contents.fields {
                    let name = field_names.entry(field.name.to_string().clone())
                        .or_insert_with(|| field.name().clone())
                        .clone();
                    if let Some(prev) = my_fields.get(&name) {
                        if prev != field.type_() {
                            warn!("Conflict: attempting to insert {:?}", name);
                            warn!("Previous: {:?}", prev);
                            warn!("Overwrite: {:?}", field.type_());
                            warn!("While treating {:?}", root);
                        }
                        debug!("Skipping");
                        // FIXME: We should make more efforts to ensure that
                        // we always end up with the bottom-most version
                        continue;
                    }
                    my_fields.insert(name, field.type_.clone());
                    // FIXME: We should handle the case in which a field is updated,
                    // e.g. `VariableDeclaration.kind` is extended from `"var"` to
                    // `"var" | "let" | "const"`.
                    // I believe that we need to make sure that we never overwrite
                    // a child with a parent.
                }
            }

            let fields = my_fields.drain()
                .map(|(name, type_)| Field { name, type_ })
                .collect();
            let declaration = interface.borrow().clone();
            let node = Rc::new(Interface {
                ancestors: all_my_ancestors.drain().collect(),
                declaration,
                full_contents: Obj { fields }
            });

            if let Some(ref kind) = node.declaration.kind {
                assert!(interfaces_by_kind.insert(kind.clone(), node.clone()).is_none());
            }

            assert!(interfaces_by_name.insert(name.clone(), node).is_none());
        }
        // FIXME: What about RegexpLiteral? & co

        // Now handle `enums`.
        for key in self.enums.keys() {
            let string = key.to_str().to_string();
            assert!(node_names.insert(string.clone(), key.clone()).is_none());
        }
        let enums_by_name = self.enums;

        assert!(interfaces_by_kind.get(options.null).is_some(),
            "Cannot find null interface {:?}", options.null);

        Syntax {
            interfaces_by_name,
            interfaces_by_kind,
            enums_by_name,
            node_names,
            kinds,
            fields: field_names,
            root: options.root.clone(),
            null: options.null.clone(),
            annotator: options.annotator
        }
    }
}

/// An interface, once compiled through
/// `SyntaxBuilder::as_syntax`.
pub struct Interface {
    declaration: InterfaceDeclaration,

    /// All the ancestors of this interface.
    ancestors: Vec<NodeName>,

    /// The full contents of this interface, including parents interfaces.
    full_contents: Obj,
}

impl Interface {
    /// Returns the full list of fields for this structure.
    /// This method is in charge of:
    /// - ensuring that the fields of parent structures are properly accounted for;
    /// - disregarding ignored fields (i.e. `position`, `type`);
    /// - disregarding fields with a single possible value.
    pub fn contents(&self) -> &Obj {
        &self.full_contents
    }

    pub fn name(&self) -> &NodeName {
        &self.declaration.name
    }

    pub fn kind(&self) -> Option<Kind> {
        match self.declaration.kind {
            None => None,
            Some(ref x) => Some(x.clone())
        }
    }

    pub fn type_(&self) -> Type {
        Type::interfaces(&[self.name()])
    }
}

/// Immutable representation of the syntax.
pub struct Syntax {
    interfaces_by_name: HashMap<NodeName, Rc<Interface>>,
    interfaces_by_kind: HashMap<Kind, Rc<Interface>>,
    enums_by_name: HashMap<NodeName, RefCell<Enum>>,
    node_names: HashMap<String, NodeName>,
    kinds: HashMap<String, Kind>,
    fields: HashMap<String, FieldName>,
    root: NodeName,
    null: Kind,
    annotator: Box<ast::annotation::Annotator>,
}

impl Syntax {
    /// Return all the ancestors of an interface, including itself.
    pub fn get_ancestors_by_name_including_self(&self, name: &NodeName) -> Option<&[NodeName]> {
        self.interfaces_by_name
            .get(name)
            .map(|node| node.ancestors.as_slice())
    }
    pub fn get_interface_by_kind(&self, kind: &Kind) -> Option<&Interface> {
        self.interfaces_by_kind
            .get(kind)
            .map(Rc::deref)
    }
    pub fn get_interface_by_name(&self, name: &NodeName) -> Option<&Interface> {
        self.interfaces_by_name
            .get(name)
            .map(Rc::deref)
    }
    pub fn get_enum_by_name(&self, name: &NodeName) -> Option<Ref<Enum>> {
        self.enums_by_name
            .get(name)
            .map(RefCell::borrow)
    }
    pub fn get_kind(&self, name: &str) -> Option<&Kind> {
        self.kinds
            .get(name)
    }
    pub fn get_field_name(&self, name: &str) -> Option<&FieldName> {
        self.fields
            .get(name)
    }
    pub fn get_node_name(&self, name: &str) -> Option<&NodeName> {
        self.node_names
            .get(name)
    }

    /// The starting point for parsing.
    pub fn get_root(&self) -> &Interface {
        self.get_interface_by_name(&self.root)
            .unwrap()
    }

    pub fn get_null(&self) -> &Kind {
        &self.null
    }

    /// Ensure that a value is an inhabitant of the grammar.
    pub fn validate(&self, a: &JSON) -> Result<(), ASTError> {
        self.validate_from(a, &self.get_root().type_())
    }

    /// Ensure that a value is an inhabitant of the grammar.
    pub fn validate_from(&self, a: &JSON, type_: &Type) -> Result<(), ASTError> {
        use serde_json::Value::*;
        match (type_, a) {
            (&Type::Boolean {..}, &Bool(_)) => Ok(()),
            (&Type::Boolean {or_null: true}, &Null) => Ok(()),
            (&Type::String {..},  &String(_)) => Ok(()),
            (&Type::String {or_null: true}, &Null) => Ok(()),
            (&Type::Number {..},  &Number(_)) => Ok(()),
            (&Type::Number {or_null: true}, &Null) => Ok(()),
            (&Type::Array(ref type_), &Array(ref values)) => {
                for value in values {
                    self.validate_from(value, type_)?;
                }
                Ok(())
            }
            (&Type::Enum(ref name), &String(ref a)) => {
                let enum_ = self.get_enum_by_name(name)
                    .expect("Could not find enum in grammar"); // At this stage, this shouldn't be possible.
                enum_.strings.iter().find(|x| *x == a).
                    ok_or_else(|| ASTError::InvalidValue {
                            got: a.clone(),
                            expected: format!("{:?}", enum_.strings)
                    })?;
                Ok(())
            }
            (&Type::Interfaces { or_null: true, .. }, &Null) => Ok(()),
            (&Type::Interfaces { ref names, .. }, &Object(ref a))
                if a.get("type").is_some() =>
            {
                let check_valid = |obj: &serde_json::Map<_, _>| {
                    let type_ = obj.get("type").unwrap(); // Just checked above.
                    let name = type_.as_str()
                        .ok_or_else(|| ASTError::InvalidValue {
                            expected: "String".to_string(),
                            got: serde_json::to_string(type_).unwrap(),
                        })?;
                    let kind = self.get_kind(&name)
                        .ok_or_else(|| ASTError::InvalidType(name.to_string()))?;
                    let interface = self.get_interface_by_kind(&kind)
                        .expect("Could not find interface by kind");
                    if self.has_ancestor_in(interface, &names) {
                        Ok(interface)
                    } else {
                        Err(ASTError::InvalidDescendent {
                            got: kind.to_string().clone(),
                            valid: names.iter().map(NodeName::to_string).cloned().collect()
                        })
                    }
                };

                let interface = check_valid(a)?;

                for field in interface.full_contents.fields() {
                    let a = a.get(field.name().to_str())
                        .ok_or_else(|| ASTError::MissingField(field.name().to_string().clone()))?;
                    self.validate_from(a, field.type_())?;
                }
                Ok(())
            },
            _ => Err(ASTError::InvalidValue {
                expected: format!("{:?}", type_),
                got: format!("{:?}", a)
            })
        }
    }

    pub fn annotate(&self, tree: &mut JSON) -> Result<(), ASTError> {
        use ast::annotation::*;
        self.annotator.process_declarations_aux(self.annotator.as_ref(), &mut Context::new(self), tree)?;
        self.annotator.process_references_aux(self.annotator.as_ref(),   &mut Context::new(self), tree)?;
        Ok(())
    }

    /// Compare two ASTs, restricting comparison to the
    /// items that appear in the grammar.
    ///
    /// This method assumes that both items are full ASTs.
    pub fn compare(&self, a: &JSON, b: &JSON) -> Result<bool, ASTError> {
        self.compare_from(a, b, &self.get_root().type_())
    }

    /// Compare two ASTs, restricting comparison to the
    /// items that appear in the grammar.
    pub fn compare_from(&self, a: &JSON, b: &JSON, type_: &Type) -> Result<bool, ASTError> {
        use serde_json::Value::*;
        match (type_, a, b) {
            (&Type::Boolean { .. }, &Bool(ref a), &Bool(ref b)) =>
                Ok(a == b),
            (&Type::Boolean { or_null: true }, &Null, &Null) =>
                Ok(true),
            (&Type::String { .. }, &String(ref a), &String(ref b)) =>
                Ok(a == b),
            (&Type::String { or_null: true }, &Null, &Null) =>
                Ok(true),
            (&Type::Number { .. }, &Number(ref a), &Number(ref b)) => {
                Ok(f64_of(a) == f64_of(b))
            }
            (&Type::Number { or_null: true }, &Null, &Null) => {
                Ok(true)
            }
            (&Type::Array(ref type_), &Array(ref vec_a), &Array(ref vec_b)) => {
                if vec_a.len() != vec_b.len() {
                    Ok(false)
                } else {
                    for (a, b) in vec_a.iter().zip(vec_b.iter()) {
                        if !self.compare_from(a, b, type_)? {
                            return Ok(false)
                        }
                    }
                    Ok(true)
                }
            }
            (&Type::Enum(ref name), &String(ref a), &String(ref b)) => {
                let enum_ = self.get_enum_by_name(name)
                    .expect("Could not find enum in grammar"); // At this stage, this shouldn't be possible.
                if enum_.strings.iter().find(|x| *x == a).is_some() {
                    if enum_.strings.iter().find(|x| *x == b).is_some() {
                        Ok(a == b)
                    } else {
                        Err(ASTError::InvalidValue {
                            got: b.clone(),
                            expected: format!("{:?}", enum_.strings)
                        })
                    }
                } else {
                    Err(ASTError::InvalidValue {
                        got: a.clone(),
                        expected: format!("{:?}", enum_.strings)
                    })
                }
            }
            (&Type::Interfaces { or_null: true, .. }, &Null, &Null) => Ok(true),
            (&Type::Interfaces { ref names, .. }, &Object(ref a), &Object(ref b))
                if a.get("type").is_some() && b.get("type").is_some() =>
            {
                let check_valid = |obj: &serde_json::Map<_, _>| {
                    let type_ = obj.get("type").unwrap(); // Just checked above.
                    let name = type_.as_str()
                        .ok_or_else(|| ASTError::InvalidValue {
                            expected: "String".to_string(),
                            got: serde_json::to_string(type_).unwrap(),
                        })?;
                    let kind = self.get_kind(&name)
                        .ok_or_else(|| ASTError::InvalidType(name.to_string()))?;
                    let interface = self.get_interface_by_kind(&kind)
                        .expect("Could not find interface by kind");
                    if self.has_ancestor_in(interface, &names) {
                        Ok(interface)
                    } else {
                        Err(ASTError::InvalidDescendent {
                            got: kind.to_string().clone(),
                            valid: names.iter().map(NodeName::to_string).cloned().collect()
                        })
                    }
                };

                let interface_a = check_valid(a)?;
                let interface_b = check_valid(b)?; // Could generally be avoided.

                if interface_a.name() != interface_b.name() {
                    return Ok(false)
                }
                if interface_a.full_contents != interface_b.full_contents {
                    return Ok(false)
                }
                for field in interface_a.full_contents.fields() {
                    let a = a.get(field.name().to_str())
                        .ok_or_else(|| ASTError::MissingField(field.name().to_string().clone()))?;
                    let b = b.get(field.name().to_str())
                        .ok_or_else(|| ASTError::MissingField(field.name().to_string().clone()))?;
                    if !self.compare_from(a, b, field.type_())? {
                        return Ok(false)
                    }
                }
                Ok(true)
            },
            _ => Err(ASTError::InvalidValue {
                expected: format!("{:?}", type_),
                got: format!("{:?}", (a, b))
            })
        }
    }

    pub fn has_ancestor_in(&self, interface: &Interface, one_of: &[NodeName]) -> bool {
        self.get_ancestors_by_name_including_self(interface.name())
            .unwrap()
            .iter()
            .find(|ancestor|
                one_of.iter()
                    .find(|candidate| {
                        debug!("Looking for {:?} =?= {:?}", candidate, ancestor);
                        candidate == ancestor
                    })
                    .is_some()
            ).is_some()
    }
}

#[derive(Debug)]
pub enum ASTError {
    InvalidKind(String),
    InvalidField(String),
    Mismatch(Type),
    InvalidValue {
        got: String,
        expected: String,
    },
    InvalidType(String),
    InvalidDescendent {
        got: String,
        valid: Vec<String>,
    },
    MissingParent(String),
    MissingField(String),
    InvalidScope
}
impl ASTError {
    pub fn invalid_kind(kind: &str) -> Self {
        ASTError::InvalidKind(kind.to_string())
    }
    pub fn invalid_field(name: &str) -> Self {
        ASTError::InvalidField(name.to_string())
    }
    pub fn invalid_value(value: &JSON, expected: &str) -> Self {
        ASTError::InvalidValue {
            got: serde_json::to_string(value).expect("Could not serialize value"),
            expected: expected.to_string()
        }
    }
    pub fn missing_field(name: &str) -> Self {
        ASTError::MissingField(name.to_string())
    }
}

/// Informations passed during the creation of a `Syntax` object.
pub struct SyntaxOptions<'a> {
    /// The kind of the special node used to encode null AST nodes (NOT null literals).
    pub null: &'a Kind,

    /// The name of the node used to start encoding.
    pub root: &'a NodeName,

    pub annotator: Box<ast::annotation::Annotator>,
}

