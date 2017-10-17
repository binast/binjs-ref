//! Grammars for specifying an AST that this tool can manipulate.
//!
//! One of the most important specificities of BinJS is that it is a format designed for a living
//! language. Critically, a BinJS file that can be parsed today must still be parsed after any
//! number of evolutions of the language, without having to write a distinct parser per version
//! of the language.
//!
//! To ensure this, we define not only the AST of EcmaScript as the language is today, but
//! the mechanism to define and evolve this AST, without incurring breaking changes in BinJS.
//!
//! This abstracts away the concepts of [ESTree](https://github.com/estree/estree).
//!
//! # Specifications
//!
//! A Grammar is composed of Interfaces and Enumerations, which may be *declared*
//! and *amended*.
//!
//! ## Declarations
//!
//! ### Interface Declaration
//!
//! FIXME: Order of fields inside a declaration is specified.
//! FIXME: Order of inherited fields is unspecified.
//!
//! Interfaces represent nodes in the AST. Interfaces are related to each other through
//! inheritance (e.g. an `Identifier` is also a `Pattern`, so `Identifier` inherits from
//! `Pattern`) or properties (e.g. a `BinaryExpression` has several properties, including `left`
//! and `right`, both of which are `Expression`s).
//!
//! Each Interface Declaration consists in *all* of:
//!
//! - *name*: `InterfaceName` (e.g. `Expression`, `ExpressionStatement`, ...);
//! - *virtual*: boolean;
//! - *parents*: `[InterfaceName]` a (possibly empty) list of `InterfaceName`.
//! - *own_structure*: `Structure`
//!
//! Property *name* is used to represent relationships between Interfaces.
//!
//! Some interfaces are purely virtual, insofar as there exist no nodes with their name
//! (e.g. `Expression`). Other interfaces are concrete, insofar as there may be nodes with
//! their name (e.g. `BinaryOperation`).
//!
//! Property *own_structure* determines the list of properties defined by this interface.
//! This does not include properties defined by parent interfaces.
//!
//!
//! ### Structure Declarations
//!
//! Each `Structure` is:
//! - properties: `[Property]`, a (possibly empty) list of properties;
//!
//! Each `Property` consists in *all* of:
//!
//! - *name*: `PropertyName` (e.g. `body`);
//! - *type*: `Type` (e.g. `Number`).
//!
//!
//! ### Enumeration Declarations
//!
//! Each Enumeration Declaration consists in *all* of:
//!
//! - *name*: `EnumerationName`;
//! - *cases*: `[String]` (a non empty list);
//!
//! Enumerations describe one of several possible strings, e.g.: `"+" | "-" | "/" | "-"`. Value`
//! `null` is never an acceptable string.
//!
//!
//! ### Type Declarations
//!
//! Each `Type` is *one* of:
//!
//! - `Enumeration`: `EnumerationName`; or
//! - `Interfaces`: `([InterfaceName], Nullability)`;
//! - `Boolean`: Nullability; or
//! - `String`: Nullability; or
//! - `Number`: Nullability; or
//! - `Array`: `Type`
//!
//! `Nullability` is *one* of:
//! - `CanBeNull`: empty; or
//! - `CannotBeNull`: empty.
//!
//!
//! ## Amendments
//!
//! Amendments are used to modify existing Interfaces and Enumerations, to let them handle more
//! sophisticated cases.
//!
//! ## Interface Amendment
//!
//! An Interface Amendment modifies an existing Interface. The most common use of an Interface
//! Amendment is to extend an Interface with new properties or to make an existing property
//! accept values that it previously didn't accept.
//!
//! Interface Amendments CANNOT:
//! - remove properties;
//! - remove parent interfaces;
//! - make a property reject any AST that would previously have been accepted (FIXME: Specify this).
//!
//! An Interface Amendment consists in *all* of:
//!
//! - `name`: an `InterfaceName`;
//! - `structure`: a `StructureAmendment`.
//!
//! ## Structure Amendment
//!
//! Each `StructureAmendment` is:
//! - properties: `[Property | PropertyAmendment]`, a (possibly empty) list of properties;
//!
//! If a property is an instance of `Property`, it MUST be a property that appears neither in
//! - `own_structure` of the interface; nor in
//! - `own_structure` of any of its ancestors.
//!
//! Conversely, if a property is an instance of `PropertyAmendment`, it MUST be a property that
//! appears either in
//! - `own_structure` of the interface; or in
//! - `own_structure` of any of its ancestors.
//!
//! FIXME: Specify this as an algorithm.
//!
//! ## Property Amendment
//!
//! Each `PropertyAmendment` consists in *all* of:
//!
//! - *name*: `PropertyName` (e.g. `body`);
//! - *type*: `TypeAmendment` (e.g. `Number`).
//!
//! ## Type Amendment
//!
//! Each `TypeAmendment` is *one* of:
//!
//! - `Interfaces`: `([InterfaceName], Nullability)` (a list of new interfaces supported by the property);
//! - `Boolean`: CanBeNull; or
//! - `String`: CanBeNull; or
//! - `Number`: CanBeNull; or
//! - `Array`: `TypeAmendment`
//!
//! A Type Amendment MUST NOT make an existing value be rejected.
//!
//! FIXME: Specify this as an algorithm.
//!
//! ## Enumeration Amendment
//!
//! An Enumeration Amendment adds new possible values to an enumeration.
//!
//! An `EnumerationAmendment` is:
//! - *name*: `EnumerationName`;
//! - *cases*: `[String]` (a non empty list of new accepted values).
//!
//!
//! ## Grammar start
//!
//! The Grammar start specifies the root node of an AST.
//!
//! - `root`: `InterfaceName`.
//!
//! # Inhabiting a grammar
//!
//! **Warning** The AST described here may be more expressive in places than the JavaScript source
//! grammar. Therefore, it is possible that not all the inhabitants of the grammar are correct
//! (or even syntactically-correct) JavaScript ASTs.
//!
//! For instance, it is unlikely that any variant of the grammar can detect that the following
//! snippets are syntactically incorrect:
//!
//! ```javascript
//! let x;
//! let x;
//! ```
//!
//! ## Algorithm
//!
//! ```javascript
//!
//! Grammar.prototype.inhabits = function(ast) {
//!   return inhabitsType(ast, new Type.Interfaces([this.root()]), grammar);
//! };
//!
//! Grammar.prototype.inhabitsType = function(ast, type) {
//!   if (type.isString()) {
//!     return typeof ast == "string" || (type.canBeNull() && ast == null);
//!   } else if (type.isBoolean()) {
//!     return typeof ast == "boolean" || (type.canBeNull() && ast == null);
//!   } else if (type.isNumber()) {
//!     return typeof ast == "number" || (type.canBeNull() && ast == null);
//!   } else if (type.isArray()) {
//!     if (!Array.isArray(ast)) {
//!       return false;
//!     }
//!     let wrapped = type.wrapped();
//!     for (let subtree of ast) {
//!       if (!this.inhabitsType(subtree, wrapped)) {
//!         return false;
//!       }
//!     }
//!     return true;
//!   } else if (type.isEnumeration()) {
//!     let name = type.enumerationName;
//!     let enumeration = self.grammar.resolveEnumeration(name); // FIXME: Specify (affected by amendments)
//!     if (typeof ast != "string") {
//!       return false;
//!     }
//!     for (let candidate of enumeration) {
//!       if (candidate == ast) {
//!         return true;
//!       }
//!     }
//!     return false;
//!   } else if (type.isInterfaceList()) {
//!     if (type.canBeNull() && ast == null) {
//!        return true;
//!     }
//!     if (typeof ast != "object") {
//!       return false;
//!     }
//!     let interface  = self.grammar.resolveInterface(ast.name());   // FIXME: Specify
//!     let list = type.interfaceList;
//!     for (let candidateName of list) {
//!       let parentInterface = self.grammar.resolveInterface(candidateName); // FIXME: Specify
//!       if (interface.isInstanceOf(parentInterface)) {             // FIXME: Specify
//!         // At this stage, `ast` has the right name.
//!         // Now check that the contents of `ast` match the structure.
//!         for (let property of interface.properties()) {           // FIXME: Specify (affected by inheritance and amendments)
//!           // Determine whether `ast` has a property with the right type.
//!           let value = ast.getProperty(property.name());
//!           if (typeof value == "undefined") {
//!             return false;
//!           }
//!           if (!this.inhabitsType(value, property.type())) {        // FIXME: Specify (affected by amendments).
//!             return false;
//!           }
//!         }
//!         return true;
//!       }
//!     }
//!     return false;
//!   }
//! };
//! ```
//!
use ast;

use json;
use json::JsonValue as JSON;
use rand;
use topological_sort;

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
#[derive(Clone, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
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
#[derive(Clone, Debug, PartialEq)]
pub enum TypeSpec {
    /// An array of values of the same type.
    Array {
        contents: Box<Type>,
        supports_empty: bool,
    },

    /// A choice between several literals, e.g. `"get" | "set"`.
    Enum(NodeName),

    /// A value that may belong to one or more interfaces.
    Interfaces(Vec<NodeName>),

    /// A boolean.
    Boolean,

    /// A string.
    String,

    /// A number.
    Number,
}

impl TypeSpec {
    pub fn array(self) -> Type {
        TypeSpec::Array {
            contents: Box::new(Type {
                spec: self,
                defaults_to: None
            }),
            supports_empty: true,
        }.close()
    }

    pub fn non_empty_array(self) -> Type {
        TypeSpec::Array {
            contents: Box::new(Type {
                spec: self,
                defaults_to: None
            }),
            supports_empty: false,
        }.close()
    }

    pub fn defaults_to(self, value: JSON) -> Type {
        Type {
            spec: self,
            defaults_to: Some(value)
        }
    }
    pub fn close(self) -> Type {
        match self {
            TypeSpec::Array { supports_empty: true, .. } => Type {
                spec: self,
                defaults_to: Some(JSON::Array(vec![]))
            },
            TypeSpec::Array { supports_empty: false, .. } => Type {
                spec: self,
                defaults_to: None,
            },
            TypeSpec::Boolean => Type {
                spec: self,
                defaults_to: Some(JSON::Boolean(false))
            },
            TypeSpec::Number => Type {
                spec: self,
                defaults_to: Some(JSON::from(0))
            },
            _ => Type {
                spec: self,
                defaults_to: None,
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    spec: TypeSpec,

    /// If the value is not specified, it defaults to...
    /// (`None` if the value MUST be specified)
    defaults_to: Option<JSON>,
}
impl Eq for Type {}

impl Type {
    pub fn spec(&self) -> &TypeSpec {
        &self.spec
    }
    pub fn default(&self) -> Option<&JSON> {
        self.defaults_to.as_ref()
    }

    /// Shorthand constructor.
    pub fn interface(name: &NodeName) -> TypeSpec {
        TypeSpec::Interfaces(vec![name.clone()])
    }
    pub fn enumeration(name: &NodeName) -> TypeSpec {
        TypeSpec::Enum(name.clone())
    }
    pub fn interfaces(names: &[&NodeName]) -> TypeSpec {
        TypeSpec::Interfaces(names.iter().cloned().cloned().collect())
    }
    pub fn string() -> TypeSpec {
        TypeSpec::String
    }
    pub fn number() -> TypeSpec {
        TypeSpec::Number
    }
    pub fn bool() -> TypeSpec {
        TypeSpec::Boolean
    }

    pub fn random<T: rand::Rng>(&self, syntax: &Syntax, rng: &mut T, depth_limit: isize) -> JSON {
        if let Some(ref value) = self.defaults_to {
            // 50% chance of returning the default value
            if depth_limit <= 0 || rng.gen() {
                return value.clone()
            }
        }
        const MAX_ARRAY_LEN: usize = 16;
        match self.spec {
            TypeSpec::Array { supports_empty, contents: ref type_ } => {
                if supports_empty && depth_limit <= 0 {
                    return array![]
                }
                let min = if supports_empty { 0 } else { 1 };
                let len = rng.gen_range(min, MAX_ARRAY_LEN);
                let mut buf = Vec::with_capacity(len);
                for _ in 0..len {
                    buf.push(type_.random(syntax, rng, depth_limit - 1));
                }
                JSON::Array(buf)
            }
            TypeSpec::Enum(ref name) => {
                let enum_ = syntax.get_enum_by_name(name)
                    .expect("Could not find enum");
                let choice = rng.choose(&enum_.strings)
                    .expect("Empty enum");
                json::from(choice as &str)
            }
            TypeSpec::Interfaces(ref names) => {
                // Pick one of the interfaces.
                let max = names.len();
                let pick = rng.gen_range(0, max);
                if pick == names.len() {
                    return JSON::Null
                }
                let interface = syntax.get_interface_by_name(&names[pick])
                    .unwrap_or_else(|| panic!("Interface doesn't exist {:?}", names[pick]));
                interface.random(syntax, rng, depth_limit - 1)
            }
            TypeSpec::Boolean => {
                JSON::Boolean(rng.gen())
            }
            TypeSpec::String => {
                const MAX_STRING_LEN : usize = 10;
                let len = rng.gen_range(0, MAX_STRING_LEN);
                let string : String = rng.gen_ascii_chars().take(len).collect();
                json::from(string)
            }
            TypeSpec::Number => {
                json::from(rng.next_f64())
            }
        }
    }

    pub fn pretty(&self, prefix: &str, indent: &str) -> String {
        let pretty_type = match self.spec {
            TypeSpec::Array { ref contents, supports_empty: false } =>
                format!("[{}] /* Non-empty */", contents.pretty(prefix, indent)),
            TypeSpec::Array { ref contents, supports_empty: true } =>
                format!("[{}]", contents.pretty(prefix, indent)),
            TypeSpec::Boolean =>
                "bool".to_string(),
            TypeSpec::String =>
                "string".to_string(),
            TypeSpec::Number =>
                "number".to_string(),
            TypeSpec::Enum(ref name) =>
                name.to_str().to_string(),
            TypeSpec::Interfaces(ref names) => {
                let mut result = String::new();
                let mut first = true;
                for name in names {
                    if first {
                        first = false;
                    } else {
                        result.push_str(" | ");
                    }
                    result.push_str(name.to_str());
                }
                result
            }
        };
        let pretty_default = match self.defaults_to {
            None => String::new(),
            Some(ref default) =>
                format!(" = {}", default.dump())
        };
        format!("{}{}", pretty_type, pretty_default)
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
    name: NodeName,

    /// Unordered list of strings, without duplicates.
    strings: Vec<String>,
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

    pub fn pretty(&self, prefix: &str, indent: &str) -> String {
        let mut result = format!("{prefix}enum {name} {{\n",
            prefix = prefix,
            name = self.name.to_str());
        {
            let prefix = format!("{prefix}{indent}", prefix=prefix, indent=indent);
            for string in &self.strings {
                result.push_str(&format!("{prefix}\"{string}\",\n", prefix=prefix, string=string));
            }
        }
        result.push_str(prefix);
        result.push_str("}\n");
        result
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

    /// The descendants of this interface.
    sub_interfaces: Vec<NodeName>,

    /// All the ancestor interfaces (not including self).
    ancestor_interfaces: Vec<NodeName>,

    /// The contents of this interface, excluding the contents of parent interfaces.
    own_contents: Obj,

    /// Requirements on the order between fields.
    order_requirements: Vec<(FieldName, FieldName)>,
}

impl InterfaceDeclaration {
    pub fn with_order(&mut self, before: &FieldName, after: &FieldName) -> &mut Self {
        assert!(self.own_contents.fields.iter().find(|x| before == x.name()).is_some());
        assert!(self.own_contents.fields.iter().find(|x| after == x.name()).is_some());
        self.order_requirements.push((before.clone(), after.clone()));
        self
    }

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
            sub_interfaces: Vec::new(),
            ancestor_interfaces: Vec::new(),
            order_requirements: Vec::new(),
        };
        self.interfaces.insert(name.clone(), RefCell::new(interface));
        self.interfaces.get(name).map(RefCell::borrow_mut)
    }

    /// Add a named enumeration.
    pub fn add_enum(&mut self, name: &NodeName) -> Option<RefMut<Enum>> {
        if self.enums.get(name).is_some() {
            return None;
        }
        let e = RefCell::new(Enum {
            name: name.clone(),
            strings: vec![]
        });
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

        // First walk: generate lists of ancestors/descendants.
        for (name, interface) in &self.interfaces {
            let mut ancestors_met = HashSet::new();

            // To do so, walk the ancestors of `interface`. Algorithmically,
            // this could explode, but in practice, I haven't seen a depth higher than 4.
            let mut ancestors = vec![name.clone()];

            while let Some(ancestor) = ancestors.pop() {
                if ancestors_met.contains(&ancestor) {
                    // With multiple inheritance, let's not copy stuff more than
                    // once. Should also prevent (but not detect) infinite loops.
                    continue;
                }
                ancestors_met.insert(ancestor.clone());

                // Handle node.
                let node = self.interfaces.get(&ancestor).unwrap();
                debug_assert_eq!(node.borrow().name, ancestor);
                for parent_names in &node.borrow().parent_interfaces {
                    ancestors.push(parent_names.clone());
                }

                if name != &ancestor {
                    debug!("Adding descendant {:?} to ancestor {:?}", name, ancestor);
                    node.borrow_mut().sub_interfaces.push(name.clone());
                    interface.borrow_mut().ancestor_interfaces.push(ancestor.clone());
                }
            }
        }

        // Second walk: generate `Interface`.
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
                    match field.type_().spec {
                        TypeSpec::Enum(ref field_name) =>
                            assert!(self.enums.get(field_name).is_some(),
                                "While compiling {:?}, could not find an enum named {:?}",
                                name, field_name),
                        TypeSpec::Interfaces(ref names) =>
                            for interface_name in names {
                                assert!(self.interfaces.get(interface_name).is_some(),
                                    "While compiling {:?}, could not find an interface named {:?}",
                                    name, interface_name)
                            },
                        _ => {}
                    }
                }
            }


            // Compute the fields of `interface`.
            let mut my_fields = HashMap::new(); // name => type
            let mut sorter = topological_sort::TopologicalSort::<FieldName>::new();
            let borrow = interface.borrow();

            // Visiting from ancestor to most descendant.
            for ancestor_name in borrow.ancestor_interfaces.iter().chain(&[name.clone()]) {
                debug!("Visiting ancestor {:?} of {:?}", ancestor_name, name);

                let ancestor = self.interfaces.get(ancestor_name)
                    .expect("Could not find ancestor");

                let ancestor = ancestor.borrow();
                // Collect dependencies between fields.
                for field in &ancestor.own_contents.fields {
                    let name = field_names.entry(field.name.to_string().clone())
                        .or_insert_with(|| field.name().clone())
                        .clone();

                    my_fields.insert(name.clone(), field.type_().clone());
                    // FIXME: We should check that we always overwrite something with something at least as general.

                    // Make sure that all names appear in the topological sort.
                    sorter.insert(name.clone());
                }

                for &(ref before, ref after) in &ancestor.order_requirements {
                    sorter.add_dependency(before.clone(), after.clone());
                }
            }

            // Now copy the fields in an appropriate order to the interface.
            let mut fields = Vec::with_capacity(my_fields.len());
            while let Some(name) = sorter.pop() {
                let type_ = my_fields.remove(&name)
                    .expect("my_fields should contain all the names output by the topological sort");
                fields.push( Field { name, type_ });
            }
            assert_eq!(sorter.len(), 0, "FIXME: Fail gracefully in a grammar with cyclic dependencies: {:?}", sorter);
            assert_eq!(my_fields.len(), 0, "We didn't collect all names: {:?}", my_fields);

            let declaration = borrow.clone();
            let node = Rc::new(Interface {
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

        Syntax {
            interfaces_by_name,
            interfaces_by_kind,
            enums_by_name,
            node_names,
            kinds,
            fields: field_names,
            root: options.root.clone(),
            annotator: options.annotator
        }
    }
}

/// An interface, once compiled through
/// `SyntaxBuilder::as_syntax`.
pub struct Interface {
    declaration: InterfaceDeclaration,

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

    pub fn spec(&self) -> TypeSpec {
        Type::interfaces(&[self.name()])
    }

    pub fn type_(&self) -> Type {
        Type::interfaces(&[self.name()])
            .close()
    }

    pub fn get_field_by_name(&self, name: &FieldName) -> Option<&Field> {
        for field in self.contents().fields() {
            if name == field.name() {
                return Some(field)
            }
        }
        None
    }

    /// Export a description of this interface.
    pub fn pretty(&self, prefix: &str, indent: &str) -> String {
        let inherits = if self.declaration.parent_interfaces.len() == 0 {
            String::new()
        } else {
            let mut result = ":< ".to_string();
            let mut first = true;
            for parent in &self.declaration.parent_interfaces {
                if !first {
                    result.push_str(", ");
                } else {
                    first = false;
                }
                result.push_str(parent.to_str());
            }
            result
        };
        let mut result = format!("{prefix} interface {name} {inherits} {{\n", prefix=prefix, name=self.name().to_str(), inherits=inherits);
        {
            let prefix = format!("{prefix}{indent}",
                prefix=prefix,
                indent=indent);
            for field in self.declaration.own_contents.fields() {
                let mut requirements = String::new();
                for &(ref before, ref after) in &self.declaration.order_requirements {
                    if before == field.name() {
                        if requirements.len() == 0 {
                            requirements = format!("// MUST appear before `{}`", after.to_str());
                        } else {
                            requirements.push_str(&format!(", `{}`", after.to_str()));
                        }
                    }
                }
                result.push_str(&format!("{prefix}{name}: {description}; {requirements}\n",
                    prefix = prefix,
                    name = field.name().to_str(),
                    description = field.type_().pretty(&prefix, indent),
                    requirements = requirements));
            }
        }
        result.push_str(&format!("{prefix} }}\n", prefix=prefix));
        result
    }

    /// Generate a random instance of this interface matching the syntax.
    fn random<T: rand::Rng>(&self, syntax: &Syntax, rng: &mut T, depth_limit: isize) -> JSON {
        // Pick one of the descendants of `start`. Exclude `start` if it is virtual
        let max = if self.declaration.kind.is_some() {
            self.declaration.sub_interfaces.len() + 1
        } else {
            self.declaration.sub_interfaces.len()
        };
        assert!(max > 0, "This interface is purely virtual but doesn't have any concrete descendent {:?}", self.name());

        let index = rng.gen_range(0, max);
        let start =
            if index < self.declaration.sub_interfaces.len() {
                syntax.interfaces_by_name.get(&self.declaration.sub_interfaces[index])
                    .unwrap_or_else(|| panic!("Interface doesn't exist {:?}", self.declaration.sub_interfaces[index]))
            } else {
                self
            };

        if start.declaration.kind.is_none() {
            // Ah, we picked a virtual node. Let's do it again.
            return start.random(syntax, rng, depth_limit - 1)
        }

        // At this stage, we know that `start` is a non-virtual interface.
        // Let's build the contents.
        let mut result = json::object::Object::with_capacity(start.full_contents.fields().len() + 1);

        let kind = start.declaration.kind
            .as_ref()
            .unwrap()
            .to_string()
            .clone();

        result.insert("type", json::from(kind));
        for field in start.full_contents.fields() {
            result.insert(field.name.to_str(), field.type_().random(syntax, rng, depth_limit - 1));
        }

        JSON::Object(result)
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
    annotator: Box<ast::annotation::Annotator>,
}

impl Syntax {
    /// Return all the ancestors of an interface, including itself.
    pub fn get_ancestors_by_name(&self, name: &NodeName) -> Option<&[NodeName]> {
        self.interfaces_by_name
            .get(name)
            .map(|node| node.declaration.ancestor_interfaces.as_slice())
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

    /// Ensure that a value is an inhabitant of the grammar.
    pub fn validate(&self, a: &JSON) -> Result<(), ASTError> {
        self.validate_from(a, &self.get_root().type_())
    }

    /// Ensure that a value is an inhabitant of the grammar.
    pub fn validate_from(&self, a: &JSON, type_: &Type) -> Result<(), ASTError> {
        use json::JsonValue::*;
        if let Some(ref default) = type_.defaults_to {
            if a == default {
                return Ok(())
            }
        }
        match (type_.spec(), a) {
            (&TypeSpec::Boolean, &Boolean(_)) => Ok(()),
            (&TypeSpec::String,  &String(_)) => Ok(()),
            (&TypeSpec::String,  &Short(_))  => Ok(()),
            (&TypeSpec::Number,  &Number(_)) => Ok(()),
            (&TypeSpec::Array { supports_empty, contents: ref type_ }, &Array(ref values)) => {
                if !supports_empty && values.len() == 0 {
                    return Err(ASTError::InvalidValue {
                        got: a.dump(),
                        expected: format!("{:?}", type_.spec())
                    });
                }
                for value in values {
                    self.validate_from(value, type_)?;
                }
                Ok(())
            }
            (&TypeSpec::Enum(ref name), &String(ref a)) => {
                let enum_ = self.get_enum_by_name(name)
                    .expect("Could not find enum in grammar"); // At this stage, this shouldn't be possible.
                enum_.strings.iter().find(|x| *x == a).
                    ok_or_else(|| ASTError::InvalidValue {
                            got: a.clone(),
                            expected: format!("{:?}", enum_.strings)
                    })?;
                Ok(())
            }
            (&TypeSpec::Interfaces(ref names), &Object(ref a))
                if a.get("type").is_some() =>
            {
                let check_valid = |obj: &json::object::Object| {
                    let type_ = obj.get("type").unwrap(); // Just checked above.
                    let name = type_.as_str()
                        .ok_or_else(|| ASTError::InvalidValue {
                            expected: "String".to_string(),
                            got: type_.dump(),
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
    pub fn compare_from(&self, left: &JSON, right: &JSON, inhabit: &Type) -> Result<bool, ASTError> {
        use json::JsonValue::*;
        match (&inhabit.spec, &inhabit.defaults_to, left, right) {
            (_, &Some(Null), &Null, &Null) => // This is the only case in which we accept `null` as a value.
                Ok(true),
            (&TypeSpec::Boolean, _, &Boolean(ref a), &Boolean(ref b)) =>
                Ok(a == b),
            (&TypeSpec::String, _, _, _) if left.as_str().is_some() && right.as_str().is_some() => // Strings are complicated as they have two different representations in JSON.
                Ok(left.as_str() == right.as_str()),
            (&TypeSpec::Number, _, &Number(ref a), &Number(ref b)) =>
                Ok(a == b),
            (&TypeSpec::Array { contents: ref type_, .. }, _, &Array(ref vec_a), &Array(ref vec_b)) => {
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
            (&TypeSpec::Enum(ref name), _, _, _) if left.as_str().is_some() && right.as_str().is_some() => { // Strings are complicated as they have two different representations in JSON.
                let a = left.as_str().unwrap();  // Checked above.
                let b = right.as_str().unwrap(); // Checked above.
                let enum_ = self.get_enum_by_name(name)
                    .expect("Could not find enum in grammar"); // At this stage, this shouldn't be possible.
                if enum_.strings.iter().find(|x| *x == a).is_some() {
                    if enum_.strings.iter().find(|x| *x == b).is_some() {
                        Ok(a == b)
                    } else {
                        Err(ASTError::InvalidValue {
                            got: b.to_string(),
                            expected: format!("{:?}", enum_.strings)
                        })
                    }
                } else {
                    Err(ASTError::InvalidValue {
                        got: a.to_string(),
                        expected: format!("{:?}", enum_.strings)
                    })
                }
            }
            (&TypeSpec::Interfaces(ref names), _, &Object(ref a), &Object(ref b))
                if a.get("type").is_some() && b.get("type").is_some() =>
            {
                let check_valid = |obj: &json::object::Object| {
                    let type_ = obj.get("type").unwrap(); // Just checked above.
                    let name = type_.as_str()
                        .ok_or_else(|| ASTError::InvalidValue {
                            expected: "String".to_string(),
                            got: type_.dump(),
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
                    if !self.compare_from(a, b, field.type_()) ? {
                        return Ok(false)
                    }
                }
                Ok(true)
            },
            _ => {
                Err(ASTError::InvalidValue {
                    expected: format!("{:?}", inhabit),
                    got: format!("{:?} =?= {:?}", left, right)
                })
            }
        }
    }

    pub fn has_ancestor_in(&self, interface: &Interface, one_of: &[NodeName]) -> bool {
        for name in one_of {
            if interface.name() == name {
                return true;
            }
        }
        self.get_ancestors_by_name(interface.name())
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

    /// Generate a random AST matching the grammar.
    ///
    /// `depth_limit` is used as *hint* to control the depth of the tree
    pub fn random<T: rand::Rng>(&self, rng: &mut T, depth_limit: isize) -> JSON {
        let root = self.interfaces_by_name.get(&self.root)
            .expect("Root interface doesn't exist");
        root.random(self, rng, depth_limit)
    }

    pub fn pretty(&self, indent: &str) -> String {
        let mut result = String::new();

        result.push_str(" // # Interfaces.\n");
        result.push_str(" //\n");
        result.push_str(" // The order of fields matters: if an interface `Foo` defines field `a` before field `b`,\n");
        result.push_str(" // any implementation of the format MUST encode the contents of `a` before the contents of `b`\n");
        result.push_str(" // The order of fields between subinterfaces and superinterfaces is not specified, as long\n");
        result.push_str(" // as the above is respected at each level\n");
        let mut interfaces : Vec<_> = self.interfaces_by_name.iter().collect();
        interfaces.sort_unstable_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        for (name, interface) in interfaces {
            if name == &self.root {
                result.push_str(" // Root of the AST.\n")
            }
            result.push_str(&interface.pretty("", indent));
            result.push_str("\n");
        }

        result.push_str("\n\n // # Enums.\n");
        result.push_str(" //\n");
        result.push_str(" // The order of enum values does NOT matter.\n");
        let mut enums : Vec<_> = self.enums_by_name.iter().collect();
        enums.sort_unstable_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        for (_, enum_) in enums {
            result.push_str(&enum_.borrow().pretty("", indent));
            result.push_str("\n");
        };

        result
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
            got: value.dump(),
            expected: expected.to_string()
        }
    }
    pub fn missing_field(name: &str) -> Self {
        ASTError::MissingField(name.to_string())
    }
}

/// Informations passed during the creation of a `Syntax` object.
pub struct SyntaxOptions<'a> {
    /// The name of the node used to start encoding.
    pub root: &'a NodeName,

    pub annotator: Box<ast::annotation::Annotator>,
}

