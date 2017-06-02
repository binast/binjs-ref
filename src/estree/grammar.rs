//! A grammar for specifying an AST that this tool can manipulate.
//!
//! This abstracts away [ESTree](https://github.com/estree/estree).

#![allow(dead_code, unused)]

use std;
use std::cell::*;
use std::collections::{ HashMap, HashSet };
use std::ops::Deref;
use std::rc::*;

#[derive(Hash, PartialEq, Eq, Clone)]
enum InterfaceNameImpl {
    /// Special hardcoded name, because it makes our life easier.
    Null,
    Named(Rc<String>)
}
impl InterfaceNameImpl {
    pub fn to_str(&self) -> &str {
        match *self {
            InterfaceNameImpl::Null => "null",
            InterfaceNameImpl::Named(ref name) => name.as_ref()
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct InterfaceName(InterfaceNameImpl);
impl InterfaceName {
    pub fn to_str(&self) -> &str {
        self.0.to_str()
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Kind(Rc<String>);
impl Kind {
    pub fn to_string(&self) -> &String {
        self.0.as_ref()
    }
}

#[derive(Clone)]
pub struct Field {
    name: String,
    type_: Type,
}
impl Field {
    pub fn name(&self) -> &String {
        &self.name
    }
    pub fn type_(&self) -> &Type {
        &self.type_
    }
}

/// A type, typically that of a field.
#[derive(Clone)]
pub enum Type {
    Array(Box<Type>),
    Obj(Obj),

    /// A choice between several literals, e.g. `"get" | "set"`.
    Enum(Enum),

    /// A value that may belong to one or more interfaces.
    ///
    /// Note that sum types between primitive types MUST be upgraded
    /// to sum types between their corresponding interfaces, e.g.
    /// `boolean | string | null` will be represented as
    /// `Boolean | String | Null`
    Interfaces(Vec<InterfaceName>),

    // Primitive types
    Boolean,
    String,
    Number,
}

impl Type {
    /// Shorthand constructor.
    pub fn interface(name: &InterfaceName) -> Self {
        Type::Interfaces(vec![name.clone()])
    }
    pub fn interfaces(names: &[&InterfaceName]) -> Self {
        Type::Interfaces(names.iter().cloned().cloned().collect())
    }
    pub fn one_of_strings(strings: &[&str]) -> Self {
        Type::Enum(Enum {
            strings: strings.iter().cloned().map(str::to_string).collect(),
            or_null: false
        })
    }
    pub fn array(self) -> Self {
        Type::Array(Box::new(self))
    }
    pub fn or_null(self) -> Option<Self> {
        match self {
            Type::Enum(e) =>
                Some(Type::Enum(Enum {
                    or_null: true,
                    .. e
                })),
            Type::Interfaces(mut interfaces) => {
                if interfaces.iter().find(|x| x.to_str() == "null").is_some() {
                    Some(Type::Interfaces(interfaces))
                } else {
                    interfaces.push(InterfaceName(InterfaceNameImpl::Null));
                    Some(Type::Interfaces(interfaces))
                }
            },
            _ => None
        }
    }
}

/// Obj of an object-like value.
#[derive(Clone)]
pub struct Obj {
    fields: Vec<Field>,
}
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
    pub fn field<'a>(&'a self, name: &str) -> Option<&'a Field> {
        self.fields.iter().find(|field| &field.name == name)
    }
    /// Extend a structure with a field.
    pub fn with_field(self, name: &str, type_: Type) -> Self {
        if self.field(name).is_some() {
            return self
        }
        let mut fields = self.fields;
        fields.push(Field {
            name: name.to_string(),
            type_
        });
        Obj {
            fields
        }
    }
}

/// Structure of an enum of strings.
#[derive(Clone)]
pub struct Enum {
    /// Unordered list of strings, without duplicates.
    strings: Vec<String>,

    /// If `true`, `null` is an acceptable value.
    or_null: bool
}
impl Default for Enum {
    fn default() -> Self {
        Enum {
            strings: Vec::new(),
            or_null: false
        }
    }
}
impl Enum {
    pub fn strings(&self) -> &[String] {
        &self.strings
    }
    pub fn or_null(&self) -> bool {
        self.or_null
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

#[derive(Clone)]
pub struct Interface {
    /// The name of the interface, e.g. `Node`.
    name: InterfaceName,

    /// The kind used to differentiate node that inhabit this
    /// interface from nodes inhabiting other interfaces.
    ///
    /// May be `None` for interfaces such as `Node` or `Expression`
    /// that serve only as a common ancestor for a sum of refined sub-interfaces
    /// and have no inhabitants of their own.
    kind: Option<Kind>,

    /// The parents of this interface.
    parent_interfaces: Vec<InterfaceName>,

    /// The contents of this interface, excluding the contents of parent interfaces.
    own_contents: Obj,
}

impl Interface {
    pub fn with_field(&mut self, name: &str, type_: Type) -> &mut Self {
        // FIXME: There must be a better way to do this.
        let mut contents = Obj::new();
        std::mem::swap(&mut self.own_contents, &mut contents);
        self.own_contents = contents.with_field(name, type_);
        self
    }
    pub fn with_parent(&mut self, parent: &InterfaceName) -> &mut Self {
        if self.parent_interfaces.iter().find(|x| *x == parent).is_none() {
            self.parent_interfaces.push(parent.clone())
        }
        self
    }
}

/// A data structure used to progressively the `Syntax`.
pub struct SyntaxBuilder {
    /// All the interfaces entered so far.
    interfaces: HashMap<InterfaceName, RefCell<Interface>>,

    /// All the enums entered so far.
    enums: HashMap<InterfaceName, RefCell<Enum>>,

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

    /// Return an `InterfaceName` for a name. Equality comparison
    /// on `InterfaceName` can be performed by checking physical
    /// equality.
    pub fn interface_name(&mut self, name: &str) -> InterfaceName {
        if name == "null" {
            return InterfaceName(InterfaceNameImpl::Null);
        }
        if let Some(result) = self.names.get(name) {
            return InterfaceName(InterfaceNameImpl::Named(result.clone()))
        }
        let shared = Rc::new(name.to_string());
        let result = InterfaceName(InterfaceNameImpl::Named(shared.clone()));
        self.names.insert(name.to_string(), shared);
        result
    }

    /// Add an interface with a `kind` identical to its name.
    pub fn add_kinded_interface(&mut self, name: &InterfaceName) -> Option<RefMut<Interface>> {
        let kind = match name.0 {
            InterfaceNameImpl::Null => return None,
            InterfaceNameImpl::Named(ref rc) => Kind(rc.clone())
        };
        self.add_virtual_interface(name)
            .map(|mut result| {result.kind = Some(kind); result})
    }

    /// Add a virtual interface, i.e. one that doesn't have a `kind`,
    /// i.e. one that does not have immediate inhabitants. Super-interfaces
    /// or sub-interfaces with a `kind` may have inhabitants.
    pub fn add_virtual_interface(&mut self, name: &InterfaceName) -> Option<RefMut<Interface>> {
        if self.interfaces.get(name).is_some() {
            return None;
        }
        let interface = Interface {
            name: name.clone(),
            kind: None,
            own_contents: Obj::new(),
            parent_interfaces: Vec::new(),
        };
        self.interfaces.insert(name.clone(), RefCell::new(interface));
        self.interfaces.get(name).map(RefCell::borrow_mut)
    }

    /// Add a named enumeration.
    pub fn add_enum(&mut self, name: &InterfaceName) -> Option<RefMut<Enum>> {
        if self.enums.get(name).is_some() {
            return None;
        }
        let e = RefCell::new(Enum::default());
        self.enums.insert(name.clone(), e);
        self.enums.get(name).map(RefCell::borrow_mut)
    }

    /// Generate the graph.
    pub fn as_syntax(self) -> Syntax {
        let mut interfaces_by_name = HashMap::new();
        let mut interfaces_by_kind = HashMap::new();
        let mut names = HashMap::new();
        let mut kinds = HashMap::new();


        for (name, interface) in &self.interfaces {
            {
                let string = name.to_str().to_string();
                assert!(names.insert(string.clone(), Rc::new(string)).is_none());
            }

            // Compute the fields and ancestors of `interface`.
            let mut already_met = HashSet::new();
            let mut fields = HashMap::new();

            // To do so, walk the ancestors of `interface`. Algorithmically,
            // this could explode, but in practice, I haven't seen a depth higher than 4.
            let mut roots = vec![name.clone()];
            let mut ancestors = HashSet::new();
            while let Some(root) = roots.pop() {
                if already_met.contains(&root) {
                    // With mutual inheritance, let's not copy stuff more than
                    // once. Should also prevent (but not detect) infinite loops.
                    continue;
                }
                ancestors.insert(root.clone());
                already_met.insert(root.clone());
                let node = self.interfaces.get(&name).unwrap();
                if let Some(ref kind) = node.borrow().kind {
                    assert!(kinds.insert(kind.to_string().clone(), kind.clone()).is_none());
                }
                for parent_names in &node.borrow().parent_interfaces {
                    roots.push(parent_names.clone());
                }
                for field in &node.borrow().own_contents.fields {
                    if fields.insert(field.name.clone(), field.type_.clone()).is_some() {
                        unimplemented!()
                    }
                    // FIXME: We should handle the case in which a field is updated,
                    // e.g. `VariableDeclaration.kind` is extended from `"var"` to
                    // `"var" | "let" | "const"`.
                    // I believe that we need to make sure that we never overwrite
                    // a child with a parent.
                }
            }

            let fields = fields.drain()
                .map(|(name, type_)| Field { name, type_ })
                .collect();
            let node = Rc::new(InterfaceNode {
                ancestors: ancestors.drain().collect(),
                interface: interface.borrow().clone(),
                full_contents: Obj { fields }
            });

            if let Some(ref kind) = node.interface.kind {
                assert!(interfaces_by_kind.insert(kind.clone(), node.clone()).is_none());
            }

            assert!(interfaces_by_name.insert(name.clone(), node).is_none());
        }
        // FIXME: What about RegexpLiteral? & co

        for key in self.enums.keys() {
            let string = key.to_str().to_string();
            assert!(names.insert(string.clone(), Rc::new(string)).is_none());
        }
        let enums_by_name = self.enums;
        Syntax {
            interfaces_by_name,
            interfaces_by_kind,
            enums_by_name,
            names,
            kinds
        }
    }
}

/// An interface, with additional data computed during the call to
/// `SyntaxBuilder::as_syntax`.
pub struct InterfaceNode {
    interface: Interface,

    /// All the ancestors of this interface.
    ancestors: Vec<InterfaceName>,

    full_contents: Obj,
}

impl InterfaceNode {
    /// Returns the full list of fields for this structure.
    /// This method is in charge of:
    /// - ensuring that the fields of parent structures are properly accounted for;
    /// - disregarding ignored fields (i.e. `position`, `type`);
    /// - disregarding fields with a single possible value.
    pub fn contents(&self, syntax: &Syntax) -> &Obj {
        &self.full_contents
    }

    pub fn name(&self) -> &InterfaceName {
        &self.interface.name
    }
}

/// Immutable representation of the syntax.
pub struct Syntax {
    interfaces_by_name: HashMap<InterfaceName, Rc<InterfaceNode>>,
    interfaces_by_kind: HashMap<Kind, Rc<InterfaceNode>>,
    enums_by_name: HashMap<InterfaceName, RefCell<Enum>>,
    names: HashMap<String, Rc<String>>,
    kinds: HashMap<String, Kind>,
}

impl Syntax {
    /// Return all the ancestors of an interface, including itself.
    pub fn get_ancestors_by_name_including_self(&self, name: &InterfaceName) -> Option<&[InterfaceName]> {
        self.interfaces_by_name
            .get(name)
            .map(|node| node.ancestors.as_slice())
    }
    pub fn get_interface_by_kind(&self, kind: &Kind) -> Option<&InterfaceNode> {
        self.interfaces_by_kind
            .get(kind)
            .map(Rc::deref)
    }
    pub fn get_interface_by_name(&self, name: &InterfaceName) -> Option<&InterfaceNode> {
        self.interfaces_by_name
            .get(name)
            .map(Rc::deref)
    }
    pub fn get_kind(&self, name: &str) -> Option<Kind> {
        self.kinds
            .get(name)
            .cloned()
    }
}