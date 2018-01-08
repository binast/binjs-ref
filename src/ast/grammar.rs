//! Grammars for specifying an AST that this tool can manipulate.
//!
//! One of the most important specificities of BinJS is that it is a format designed for a living
//! language. Critically, a BinJS file that can be parsed today must still be parsed after any
//! number of evolutions of the language, without having to write a distinct parser per version
//! of the language.
//!
//! To ensure this, we define not only the AST of EcmaScript as the language is today, but
//! the mechanism to define and evolve this AST, without incurring breaking changes in BinJS.

use ast;
use util::{ pick, to_snake_case, type_of };

use json;
use json::JsonValue as JSON;
use rand;

use std;
use std::cell::*;
use std::collections::{ HashMap, HashSet };
use std::fmt::Debug;
use std::hash::*;
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


/// The name of a field in an interface.
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FieldName(Rc<String>);
impl FieldName {
    pub fn to_string(&self) -> &String {
        self.0.as_ref()
    }
    pub fn to_str(&self) -> &str {
        self.0.as_ref()
    }
}
impl Debug for FieldName {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        self.to_str().fmt(formatter)
    }
}

/// An enumeration of strings.
///
/// A valid value is any of these strings.
#[derive(Debug)]
pub struct StringEnum {
    name: NodeName,
    // Invariant: values are distinct // FIXME: Not checked yet.
    values: Vec<String>,
}

/// An enumeration of interfaces.
#[derive(Debug)]
pub struct TypeSum {
    name: NodeName,
    values: Vec<NodeName>,
}
impl TypeSum {
    pub fn with_types(&mut self, names: &[&NodeName]) -> &mut Self {
        for name in names {
            self.values.push((*name).clone())
        }
        self
    }
}

/// Representation of a field in an interface.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Field {
    name: FieldName,
    type_: Type,
    documentation: Option<String>,
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
            type_,
            documentation: None,
        }
    }
    pub fn name(&self) -> &FieldName {
        &self.name
    }
    pub fn type_(&self) -> &Type {
        &self.type_
    }
    pub fn doc(&self) -> Option<&str> {
        match self.documentation {
            None => None,
            Some(ref s) => Some(&*s)
        }
    }
}

/// A type, typically that of a field.
#[derive(Clone, Debug, PartialEq)]
pub enum TypeSpec {
    /// An array of values of the same type.
    Array {
        /// The type of values in the array.
        contents: Box<Type>,

        /// If `true`, the array may be empty.
        supports_empty: bool,
    },

    NamedType(NodeName),

    TypeSum(Vec<TypeSpec>),

    /// A boolean.
    Boolean,

    /// A string.
    String,

    /// A number.
    Number,

    Void,
}

#[derive(Clone, Debug)]
pub enum NamedType {
    Interface(Rc<Interface>),
    Typedef(Rc<Type>), // FIXME: Check that there are no cycles.
    StringEnum(Rc<StringEnum>),
}

impl NamedType {
    pub fn into_interface(self) -> Option<Rc<Interface>> {
        if let NamedType::Interface(result) = self {
            Some(result)
        } else {
            None
        }
    }
    pub fn as_interface(&self, syntax: &Syntax) -> Option<Rc<Interface>> {
        match *self {
            NamedType::Interface(ref result) => Some(result.clone()),
            NamedType::Typedef(ref type_) => {
                if let TypeSpec::NamedType(ref named) = *type_.spec() {
                    let named = syntax.get_type_by_name(named)
                        .expect("Type not found");
                    named.as_interface(syntax)
                } else {
                    None
                }
            }
            NamedType::StringEnum(_) => None,
        }
    }

    fn compare(&self, syntax: &Syntax, left: &JSON, right: &JSON) -> Result<bool, ASTError> {
        match *self {
            NamedType::Interface(ref interface) =>
                return interface.compare(syntax, left, right),
            NamedType::Typedef(ref type_) =>
                return type_.compare(syntax, left, right),
            NamedType::StringEnum(_) if left.as_str().is_some() && right.as_str().is_some() => {
                if left.as_str() == right.as_str() {
                    return Ok(true)
                }
                return Ok(false)
            }
            _ => {}
        }
        return Err(ASTError::InvalidValue {
            got: format!("{} =?= {}", left.dump(), right.dump()),
            expected: format!("{:?}", self)
        })
    }
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
                defaults_to: None,
            },
            TypeSpec::Array { supports_empty: false, .. } => Type {
                spec: self,
                defaults_to: None,
            },
            TypeSpec::Boolean => Type {
                spec: self,
                defaults_to: None,
            },
            TypeSpec::Number => Type {
                spec: self,
                defaults_to: None,
            },
            _ => Type {
                spec: self,
                defaults_to: None,
            },
        }
    }

    pub fn to_rust_source(&self) -> String {
        match *self {
            TypeSpec::Array { ref contents, supports_empty: false } => {
                format!("{}.non_empty_array()", contents.to_rust_source())
            }
            TypeSpec::Array { ref contents, supports_empty: true } => {
                format!("{}.array()", contents.to_rust_source())
            }
            TypeSpec::Boolean => "Type::bool()".to_string(),
            TypeSpec::String => "Type::string()".to_string(),
            TypeSpec::Number => "Type::number()".to_string(),
            TypeSpec::NamedType(ref name) => format!("Type::named(&{})", to_snake_case(name.to_str())),
            TypeSpec::TypeSum(ref types) => {
                let mut source = String::new();
                let mut first = true;
                for type_ in types {
                    if first {
                        first = false;
                    } else {
                        source.push_str(",");
                    }
                    source.push_str("\n\t");
                    source.push_str(&type_.to_rust_source())
                }
                format!("Type::sum(&[{}\n])", source)
            }
            TypeSpec::Void => "void".to_string()
        }
    }

    pub fn pretty(&self, prefix: &str, indent: &str) -> String {
        match *self {
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
            TypeSpec::NamedType(ref name) =>
                name.to_str().to_string(),
            TypeSpec::TypeSum(ref types) => {
                let mut result = String::new();
                result.push('(');
                let mut first = true;
                for typ in types {
                    if first {
                        first = false;
                    } else {
                        result.push_str(" or ");
                    }
                    result.push_str(&typ.pretty("", indent));
                }
                result.push(')');
                result
            }
            TypeSpec::Void => "void".to_string()
        }
    }

    pub fn random<T: rand::Rng>(&self, syntax: &Syntax, rng: &mut T, depth_limit: isize) -> JSON {
        const MAX_ARRAY_LEN: usize = 16;
        match *self {
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
            TypeSpec::NamedType(ref name) => {
                use self::NamedType::*;
                match syntax.get_type_by_name(name) {
                    Some(Interface(interface)) =>
                        interface.random(syntax, rng, depth_limit),
                    Some(Typedef(typedef)) =>
                        typedef.random(syntax, rng, depth_limit),
                    Some(StringEnum(string_enum)) => {
                        let string = pick(rng, &string_enum.values);
                        JSON::String(string.clone()) // FIXME: Use json::from
                    }
                    None => {
                        panic!("Could not find named type {:?}", name)
                    }
                }
            }
            TypeSpec::TypeSum(ref types) => {
                let type_ = pick(rng, &*types);
                type_.random(syntax, rng, depth_limit)
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
            TypeSpec::Void =>
                JSON::Null
        }
    }

    fn compare(&self, syntax: &Syntax, left: &JSON, right: &JSON) -> Result<bool, ASTError> {
        use json::JsonValue::*;
        match (self, left, right) {
            (&TypeSpec::Boolean, &Boolean(ref a), &Boolean(ref b)) =>
                Ok(a == b),
            (&TypeSpec::String, _, _) if left.as_str().is_some() && right.as_str().is_some() => // Strings are complicated as they have two different representations in JSON.
                Ok(left.as_str() == right.as_str()),
            (&TypeSpec::Number, &Number(ref a), &Number(ref b)) =>
                Ok(a == b),
            (&TypeSpec::Array { contents: ref type_, .. }, &Array(ref vec_a), &Array(ref vec_b)) => {
                if vec_a.len() != vec_b.len() {
                    Ok(false)
                } else {
                    for (a, b) in vec_a.iter().zip(vec_b.iter()) {
                        if !type_.compare(syntax, a, b)? {
                            return Ok(false)
                        }
                    }
                    Ok(true)
                }
            }
            (&TypeSpec::NamedType(ref name), _, _) => {
                match syntax.get_type_by_name(name) {
                    Some(NamedType::StringEnum(_)) if left.as_str().is_some() && right.as_str().is_some() =>
                        return Ok(left.as_str().unwrap() == right.as_str().unwrap()),
                    Some(NamedType::Interface(interface)) =>
                        return interface.compare(syntax, left, right),
                    Some(NamedType::Typedef(typedef)) =>
                        return typedef.compare(syntax, left, right),
                    None =>
                        panic!("Could not find a type named {:?}", name),
                    _ => {}
                }
                return Err(ASTError::InvalidValue {
                    expected: format!("{:?}", self),
                    got: format!("{:?} =?= {:?}", left, right)
                })
            },
            (&TypeSpec::TypeSum(ref types), _, _) => {
                for type_ in types {
                    if let Ok(result) = type_.compare(syntax, left, right) {
                        return Ok(result)
                    }
                }
                Err(ASTError::InvalidValue {
                    expected: format!("{:?}", self),
                    got: format!("{:?} =?= {:?}", left, right)
                })
            }
            _ => {
                Err(ASTError::InvalidValue {
                    expected: format!("{:?}", self),
                    got: format!("{:?} =?= {:?}", left, right)
                })
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub spec: TypeSpec,

    /// If the value is not specified, it defaults to...
    /// (`None` if the value MUST be specified)
    pub defaults_to: Option<JSON>,
}
impl Eq for Type {}

impl Type {
    pub fn with_default(&mut self, default: JSON) -> &mut Self {
        self.defaults_to = Some(default);
        self
    }

    pub fn with_named_sum(&mut self, names: &[&NodeName]) -> &mut Self {
        let sum = names.iter()
            .cloned()
            .cloned()
            .map(|name| TypeSpec::NamedType(name))
            .collect();
        self.spec = TypeSpec::TypeSum(sum);
        self
    }

    pub fn with_spec(&mut self, spec: TypeSpec) -> &mut Self {
        self.spec = spec;
        self
    }

    pub fn with_type(&mut self, type_: Type) -> &mut Self {
        self.spec = type_.spec;
        self.defaults_to = type_.defaults_to;
        self
    }

    pub fn spec(&self) -> &TypeSpec {
        &self.spec
    }
    pub fn default(&self) -> Option<&JSON> {
        self.defaults_to.as_ref()
    }

    /// Shorthand constructors.
    pub fn named(name: &NodeName) -> TypeSpec {
        TypeSpec::NamedType(name.clone())
    }
    pub fn sum(types: &[TypeSpec]) -> TypeSpec {
        TypeSpec::TypeSum(types.iter().cloned().collect())
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

    pub fn array(self) -> TypeSpec {
        TypeSpec::Array {
            contents: Box::new(self),
            supports_empty: true,
        }
    }

    pub fn non_empty_array(self) -> TypeSpec {
        TypeSpec::Array {
            contents: Box::new(self),
            supports_empty: false,
        }
    }

    pub fn random<T: rand::Rng>(&self, syntax: &Syntax, rng: &mut T, depth_limit: isize) -> JSON {
        if let Some(ref value) = self.defaults_to {
            // 50% chance of returning the default value
            if depth_limit <= 0 || rng.gen() {
                return value.clone()
            }
        }
        self.spec.random(syntax, rng, depth_limit)
    }

    pub fn to_rust_source(&self) -> String {
        let pretty_type = self.spec.to_rust_source();
        match self.defaults_to {
            None => format!("{}.close()", pretty_type),
            Some(JSON::Null) => {
                format!("{}.defaults_to(JSON::Null)",
                    pretty_type)
            }
            _ => unimplemented!()
        }
    }

    pub fn pretty(&self, prefix: &str, indent: &str) -> String {
        let pretty_type = self.spec.pretty(prefix, indent);
        let pretty_default = match self.defaults_to {
            None => String::new(),
            Some(ref default) =>
                format!(" = {}", default.dump())
        };
        format!("{}{}", pretty_type, pretty_default)
    }

    /// Compare two ASTs, restricting comparison to the
    /// items that appear in the grammar.
    pub fn compare(&self, syntax: &Syntax, left: &JSON, right: &JSON) -> Result<bool, ASTError> {
        use json::JsonValue::*;
        if let (&Some(Null), &Null, &Null) = (&self.defaults_to, left, right) {
            // This is the only case in which we accept `null` as a value.
            return Ok(true)
        }
        self.spec.compare(syntax, left, right)
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

    pub fn with_full_field(&mut self, field: Field) -> &mut Self {
        if self.field(field.name()).is_some() {
            warn!("Field: attempting to overwrite {:?}", field.name());
            return self
        }
        self.fields.push(field);
        self
    }

    fn with_field_aux(self, name: &FieldName, type_: Type, doc: Option<&str>) -> Self {
        if self.field(name).is_some() {
            warn!("Field: attempting to overwrite {:?}", name);
            return self
        }
        let mut fields = self.fields;
        fields.push(Field {
            name: name.clone(),
            type_,
            documentation: doc.map(str::to_string),
        });
        Obj {
            fields
        }

    }

    /// Extend a structure with a field.
    pub fn with_field(self, name: &FieldName, type_: Type) -> Self {
        self.with_field_aux(name, type_, None)
    }

    pub fn with_field_doc(self, name: &FieldName, type_: Type, doc: &str) -> Self {
        self.with_field_aux(name, type_, Some(doc))
    }
}

impl StringEnum {
    pub fn strings(&self) -> &[String] {
        &self.values
    }

    /// Add a string to the enum. Idempotent.
    pub fn with_string(&mut self, string: &str) -> &mut Self {
        let string = string.to_string();
        if self.values.iter().find(|x| **x == string).is_none() {
            self.values.push(string.to_string())
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
            for string in &self.values {
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

    /// The contents of this interface, excluding the contents of parent interfaces.
    contents: Obj,
}

impl InterfaceDeclaration {
    pub fn with_full_field(&mut self, contents: Field) -> &mut Self {
        let _ = self.contents.with_full_field(contents);
        self
    }
    pub fn with_field(&mut self, name: &FieldName, type_: Type) -> &mut Self {
        self.with_field_aux(name, type_, None)
    }
    pub fn with_field_doc(&mut self, name: &FieldName, type_: Type, doc: &str) -> &mut Self {
        self.with_field_aux(name, type_, Some(doc))
    }
    fn with_field_aux(&mut self, name: &FieldName, type_: Type, doc: Option<&str>) -> &mut Self {
        let mut contents = Obj::new();
        std::mem::swap(&mut self.contents, &mut contents);
        self.contents = contents.with_field_aux(name, type_, doc);
        self
    }
}

/// A data structure used to progressively construct the `Syntax`.
pub struct SyntaxBuilder {
    /// All the interfaces entered so far.
    interfaces_by_name: HashMap<NodeName, RefCell<InterfaceDeclaration>>,

    /// All the enums entered so far.
    string_enums_by_name: HashMap<NodeName, RefCell<StringEnum>>,

    typedefs_by_name: HashMap<NodeName, RefCell<Type>>,

    names: HashMap<String, Rc<String>>,
}

impl SyntaxBuilder {
    pub fn new() -> Self {
        SyntaxBuilder {
            interfaces_by_name: HashMap::new(),
            string_enums_by_name: HashMap::new(),
            typedefs_by_name: HashMap::new(),
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

    pub fn add_interface(&mut self, name: &NodeName) -> Option<RefMut<InterfaceDeclaration>> {
        if self.interfaces_by_name.get(name).is_some() {
            return None;
        }
        let result = RefCell::new(InterfaceDeclaration {
            name: name.clone(),
            contents: Obj::new(),
        });
        self.interfaces_by_name.insert(name.clone(), result);
        self.interfaces_by_name.get(name)
            .map(RefCell::borrow_mut)
    }
    pub fn get_interface(&mut self, name: &NodeName) -> Option<RefMut<InterfaceDeclaration>> {
        self.interfaces_by_name.get(name)
            .map(RefCell::borrow_mut)
    }

    /// Add a named enumeration.
    pub fn add_string_enum(&mut self, name: &NodeName) -> Option<RefMut<StringEnum>> {
        if self.string_enums_by_name.get(name).is_some() {
            return None;
        }
        let e = RefCell::new(StringEnum {
            name: name.clone(),
            values: vec![]
        });
        self.string_enums_by_name.insert(name.clone(), e);
        self.string_enums_by_name.get(name).map(RefCell::borrow_mut)
    }

    pub fn add_typedef(&mut self, name: &NodeName) -> Option<RefMut<Type>> {
        if self.typedefs_by_name.get(name).is_some() {
            return None;
        }
        let e = RefCell::new(TypeSpec::Void.close());
        self.typedefs_by_name.insert(name.clone(), e);
        self.typedefs_by_name.get(name).map(RefCell::borrow_mut)
    }

    /// Generate the graph.
    pub fn into_syntax<'a>(self, options: SyntaxOptions<'a>) -> Syntax {
        // FIXME: Check that all node names are defined.
        // FIXME: Check that all field names are defined.
        // FIXME: Check that each name is only defined once.
        // FIXME: Check that all typesums resolve to sums of interfaces.
        let mut interfaces_by_name = self.interfaces_by_name;
        let interfaces_by_name : HashMap<_, _> = interfaces_by_name.drain()
            .map(|(k, v)| (k, Rc::new(Interface {
                declaration: RefCell::into_inner(v)
            })))
            .collect();
        let mut string_enums_by_name = self.string_enums_by_name;
        let string_enums_by_name : HashMap<_, _> = string_enums_by_name.drain()
            .map(|(k, v)| (k, Rc::new(RefCell::into_inner(v))))
            .collect();
        let mut typedefs_by_name = self.typedefs_by_name;
        let typedefs_by_name : HashMap<_, _> = typedefs_by_name.drain()
            .map(|(k, v)| (k, Rc::new(RefCell::into_inner(v))))
            .collect();

        let mut node_names = HashMap::new();
        for name in interfaces_by_name.keys().chain(string_enums_by_name.keys()).chain(typedefs_by_name.keys()) {
            node_names.insert(name.to_string().clone(), name.clone());
        }
        let mut fields = HashMap::new();
        for interface in interfaces_by_name.values() {
            for field in &interface.declaration.contents.fields {
                fields.insert(field.name.to_string().clone(), field.name.clone());
            }
        }

        Syntax {
            interfaces_by_name: interfaces_by_name,
            string_enums_by_name: string_enums_by_name,
            typedefs_by_name: typedefs_by_name,
            node_names: node_names,
            fields: fields,
            root: options.root.clone(),
            annotator: options.annotator,
        }
    }

    pub fn into_rust_source(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str("// Node names (by lexicographical order)\n");
        let mut names : Vec<_> = self.string_enums_by_name.keys().chain(self.interfaces_by_name.keys()).chain(self.typedefs_by_name.keys())
            .map(|x| x.to_string())
            .collect();
        names.sort();
        for name in names {
            let source = format!("let {snake} = syntax.node_name(\"{original}\");\n",
                snake = to_snake_case(name),
                original = name);
            buffer.push_str(&source);
        }

        buffer.push_str("\n\n\n// Field names (by lexicographical order)\n");
        let mut fields = HashSet::new();
        for interface in self.interfaces_by_name.values() {
            for field in &interface.borrow().contents.fields {
                fields.insert(field.name.to_string().clone());
            }
        }
        let mut fields : Vec<_> = fields.drain().collect();
        fields.sort();
        for name in fields {
            let source = format!("let field_{snake} = syntax.field_name(\"{original}\");\n",
                snake = to_snake_case(&name),
                original = name);
            buffer.push_str(&source);
        }

        buffer.push_str("\n\n\n// Enumerations\n");
        for (name, def) in &self.string_enums_by_name {
            let mut strings = String::new();
            let mut first = true;
            for string in &def.borrow().values {
                if first {
                    first = false;
                } else {
                    strings.push_str(",\n\t");
                }
                strings.push_str("\"");
                strings.push_str(&*string);
                strings.push_str("\"");
            }
            let source = format!("syntax.add_string_enum(&{name}).unwrap()
                .with_strings(&[
                    {strings}
                ]);\n\n",
                name = to_snake_case(name.to_str()),
                strings = strings);
            buffer.push_str(&source);
        }
        for (name, def) in &self.typedefs_by_name {
            let source = format!("syntax.add_typedef(&{name}).unwrap()
                .with_type({spec});\n\n",
                name = to_snake_case(name.to_str()),
                spec = def.borrow().to_rust_source());
            buffer.push_str(&source);
        }
        for (name, def) in &self.interfaces_by_name {
            let mut fields = String::new();
            for field in &def.borrow().contents.fields {
                let source = format!("\n  .with_field(&field_{name}, {spec})",
                    name = to_snake_case(field.name.to_str()),
                    spec = field.type_.to_rust_source());
                fields.push_str(&source);
            }
            let source = format!("syntax.add_interface(&{name}).unwrap(){fields};\n\n",
                name = to_snake_case(name.to_str()),
                fields = fields);
            buffer.push_str(&source);
        }
        buffer
    }
}

/// An interface, once compiled through
/// `SyntaxBuilder::as_syntax`.
#[derive(Debug)]
pub struct Interface {
    declaration: InterfaceDeclaration,
}

impl Interface {
    /// Returns the full list of fields for this structure.
    /// This method is in charge of:
    /// - ensuring that the fields of parent structures are properly accounted for;
    /// - disregarding ignored fields (i.e. `position`, `type`);
    /// - disregarding fields with a single possible value.
    pub fn contents(&self) -> &Obj {
        &self.declaration.contents
    }

    pub fn name(&self) -> &NodeName {
        &self.declaration.name
    }

    pub fn spec(&self) -> TypeSpec {
        TypeSpec::NamedType(self.name().clone())
    }

    pub fn type_(&self) -> Type {
        self.spec().close()
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
        let mut result = format!("{prefix} interface {name} : Node {{\n", prefix=prefix, name=self.name().to_str());
        {
            let prefix = format!("{prefix}{indent}",
                prefix=prefix,
                indent=indent);
            for field in self.declaration.contents.fields() {
                if let Some(ref doc) = field.doc() {
                    result.push_str(&format!("{prefix}// {doc}\n", prefix = prefix, doc = doc));
                }
                result.push_str(&format!("{prefix}{name}: {description};\n",
                    prefix = prefix,
                    name = field.name().to_str(),
                    description = field.type_().pretty(&prefix, indent)
                ));
                if field.doc().is_some() {
                    result.push_str("\n");
                }
            }
        }
        result.push_str(&format!("{prefix} }}\n", prefix=prefix));
        result
    }

    /// Generate a random instance of this interface matching the syntax.
    fn random<T: rand::Rng>(&self, syntax: &Syntax, rng: &mut T, depth_limit: isize) -> JSON {
        let mut obj = json::object::Object::with_capacity(self.declaration.contents.fields.len());
        for field in &self.declaration.contents.fields {
            let value = field.type_.random(syntax, rng, depth_limit - 1);
            obj.insert(field.name.to_str(), value);
        }
        json::JsonValue::Object(obj)
    }

    fn compare(&self, syntax: &Syntax, left: &JSON, right: &JSON) -> Result<bool, ASTError> {
        // Compare types
        if left["type"].as_str() != right["type"].as_str() {
            return Ok(false)
        }
        // Compare fields
        for field in &self.declaration.contents.fields {
            let index = field.name().to_str();
            let result = field.type_().compare(syntax, &left[index], &right[index])?;
            if !result {
                return Ok(false)
            }
        }
        // Everything is fine.
        Ok(true)
    }
}

/// Immutable representation of the syntax.
pub struct Syntax {
    interfaces_by_name: HashMap<NodeName, Rc<Interface>>,
    string_enums_by_name: HashMap<NodeName, Rc<StringEnum>>,
    typedefs_by_name: HashMap<NodeName, Rc<Type>>,
    node_names: HashMap<String, NodeName>,
    fields: HashMap<String, FieldName>,
    root: NodeName,
    annotator: Box<ast::annotation::Annotator>,
}

impl Syntax {
    pub fn get_interface_by_name(&self, name: &NodeName) -> Option<&Interface> {
        self.interfaces_by_name.get(name)
            .map(std::borrow::Borrow::borrow)
    }
    pub fn get_type_by_name(&self, name: &NodeName) -> Option<NamedType> {
        if let Some(interface) = self.interfaces_by_name
            .get(name) {
            return Some(NamedType::Interface(interface.clone()))
        }
        if let Some(strings_enum) = self.string_enums_by_name
            .get(name) {
            return Some(NamedType::StringEnum(strings_enum.clone()))
        }
        if let Some(type_) = self.typedefs_by_name
            .get(name) {
            return Some(NamedType::Typedef(type_.clone()))
        }
        None
    }
    pub fn get_field_name(&self, name: &str) -> Option<&FieldName> {
        self.fields
            .get(name)
    }
    pub fn get_node_name(&self, name: &str) -> Option<&NodeName> {
        self.node_names
            .get(name)
    }

    pub fn get_root_name(&self) -> &NodeName {
        &self.root
    }

    /// The starting point for parsing.
    pub fn get_root(&self) -> NamedType {
        self.get_type_by_name(&self.root)
            .unwrap()
    }

    /// Ensure that a value is an inhabitant of the grammar.
    pub fn validate(&self, a: &JSON) -> Result<(), ASTError> {
        struct ValidationVisitor;
        impl ASTVisitor for ValidationVisitor { /* Do nothing */  }
        let mut walker = ASTWalker::new(self, ValidationVisitor);
        walker.walk(a)
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
        self.get_root().compare(self, a, b)
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
        result.push_str(" // Unless specified otherwise in comments, the order of fields does NOT matter.\n\n\n");
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
        let mut enums : Vec<_> = self.string_enums_by_name.iter().collect();
        enums.sort_unstable_by(|a, b| str::cmp(a.0.to_str(), b.0.to_str()));
        for (_, enum_) in enums {
            result.push_str(&enum_.pretty("", indent));
            result.push_str("\n");
        };

        result
    }
}

#[derive(Debug)]
pub enum ASTError {
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
    pub fn invalid_field(name: &str) -> Self {
        ASTError::InvalidField(name.to_string())
    }
    pub fn invalid_value<T>(value: T, expected: &str) -> Self where T: std::ops::Deref<Target = JSON> {
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

/// Define immutable and mutable visitors.
macro_rules! make_ast_visitor {
    ($visitor_name:ident, $walker_name:ident, $($mutability: ident)*) => {
        pub trait $visitor_name {
            fn enter_type(&mut self, _value: & $($mutability)* JSON, _type_: &Type, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn exit_type(&mut self, _value: & $($mutability)* JSON, _type_: &Type, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn enter_typespec(&mut self, _value: & $($mutability)* JSON, _typespec_: &TypeSpec, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn exit_typespec(&mut self, _value: & $($mutability)* JSON, _typespec_: &TypeSpec, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn enter_string_enum(&mut self, _value: & $($mutability)* JSON, _enum_: &StringEnum, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn exit_string_enum(&mut self, _value: & $($mutability)* JSON, _enum_: &StringEnum, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn enter_interface(&mut self, _value: & $($mutability)* JSON, _interface: &Interface, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
            fn exit_interface(&mut self, _value: & $($mutability)* JSON, _interface: &Interface, _name: &NodeName) -> Result<(), ASTError> {
                // Do nothing
                Ok(())
            }
        }

        pub struct $walker_name<'a, V> where V: $visitor_name {
            syntax: &'a Syntax,
            visitor: V,
        }

        // Indirection is either &JSON or &mut JSON
        impl<'a, V> $walker_name<'a, V> where V: $visitor_name {
            pub fn new(syntax: &'a Syntax, visitor: V) -> Self {
                $walker_name {
                    syntax,
                    visitor,
                }
            }
            pub fn walk(&mut self, value: & $($mutability)* JSON) -> Result<(), ASTError> {
                let root = self.syntax.get_root();
                let name = self.syntax.get_root_name();
                self.walk_named_type(value, &root, name)
            }
            pub fn walk_named_type(&mut self, value: & $($mutability)* JSON, named: &NamedType, name: &NodeName) -> Result<(), ASTError> {
                match *named {
                    NamedType::StringEnum(ref enum_) => self.walk_string_enum(value, enum_, name),
                    NamedType::Interface(ref interface_) => self.walk_interface(value, interface_, name),
                    NamedType::Typedef(ref typedef_) => self.walk_type(value, typedef_, name),
                }
            }
            pub fn walk_string_enum(&mut self, value: & $($mutability)* JSON, enum_: &StringEnum, name: &NodeName) -> Result<(), ASTError> {
                self.visitor.enter_string_enum(value, enum_, name)?;
                if let Some(ref s) = value.as_str() {
                    if enum_.values.iter()
                        .find(|x| x == s)
                        .is_none()
                    {
                        return Err(ASTError::InvalidValue {
                            expected: format!("One of {:?}", enum_.values),
                            got: s.to_string()
                        })
                    }
                } else {
                    return Err(ASTError::InvalidValue {
                        expected: "string".to_string(),
                        got: format!("{:?}", *value)
                    })
                }
                self.visitor.exit_string_enum(value, enum_, name)?;
                Ok(())
            }
            pub fn walk_type(&mut self, value: & $($mutability)* JSON, type_: &Type, name: &NodeName) -> Result<(), ASTError> {
                self.visitor.enter_type(value, type_, name)?;
                if let JSON::Null = *value {
                    if let Some(_) = type_.defaults_to {
                        self.visitor.exit_type(value, type_, name)?;
                        return Ok(())
                    }
                }
                self.walk_type_spec(value, type_.spec(), name)?;
                self.visitor.exit_type(value, type_, name)?;
                Ok(())
            }
            pub fn walk_type_spec(&mut self, value: & $($mutability)* JSON, spec: &TypeSpec, name: &NodeName) -> Result<(), ASTError> {
                self.visitor.enter_typespec(value, spec, name)?;
                self.walk_type_spec_aux(value, spec, name)?;
                self.visitor.exit_typespec(value, spec, name)?;
                Ok(())
            }

            pub fn walk_interface(&mut self, value: & $($mutability)* JSON, interface: &Interface, name: &NodeName) -> Result<(), ASTError> {
                // Let the visitor rewrite the object if necessary.
                self.visitor.enter_interface(value, interface, name)?;
                // Check type.
                let is_ok =
                    if let Some(name) = value["type"].as_str() {
                        name == interface.name().to_str()
                    } else {
                        false
                    };
                if !is_ok {
                    return Err(ASTError::invalid_value(value, &format!("Instance of {:?}", interface.name())))                    
                }
                if value.is_object() {
                    // Visit fields, ignoring excess fields and, if necessary, optional fields.
                    for field in &interface.declaration.contents.fields {
                        let ref $($mutability)* value = value[field.name().to_str()];
                        self.walk_type(value, field.type_(), interface.name())?;
                    }
                } else {
                    return Err(ASTError::InvalidValue {
                        expected: "object".to_string(),
                        got: type_of(&*value)
                    })
                }
                self.visitor.exit_interface(value, interface, name)?;
                Ok(())
            }
            fn walk_type_spec_aux(&mut self, value: & $($mutability)* JSON, spec: &TypeSpec, name: &NodeName) -> Result<(), ASTError> {
                match spec {
                    &TypeSpec::Boolean => {
                        if let JSON::Boolean(_) = *value {
                            return Ok(())
                        }
                    }
                    &TypeSpec::Number => {
                        if let JSON::Number(_) = *value {
                            return Ok(())
                        }
                    }
                    &TypeSpec::String => {
                        if value.as_str().is_some() {
                            return Ok(())
                        }
                    }
                    &TypeSpec::Void => {
                        if let JSON::Null = *value {
                            return Ok(())
                        }
                    }
                    &TypeSpec::NamedType(ref name) => {
                        let named = self.syntax.get_type_by_name(name)
                            .ok_or_else(|| ASTError::InvalidType(name.to_str().to_string()))?;
                        return self.walk_named_type(value, &named, name);
                    }
                    &TypeSpec::Array { ref contents, supports_empty } => {
                        if value.is_array() {
                            if value.len() == 0 {
                                if supports_empty {
                                    return Ok(())
                                } else {
                                    return Err(ASTError::InvalidValue {
                                        expected: "non-empty array".to_string(),
                                        got: "empty array".to_string()
                                    })
                                }
                            }
                            for i in 0..value.len() {
                                let ref $($mutability)* value = value[i];
                                self.walk_type(value, contents, name)?;
                            }
                            return Ok(())
                        }
                    }
                    &TypeSpec::TypeSum(ref sum) => {
                        // Find out to which element in the sum we belong.
                        let interface = {
                            let kind = value["type"].as_str()
                                .ok_or_else(|| ASTError::InvalidValue {
                                    expected: "object with a type".to_string(),
                                    got: value.dump()
                                })?;
                            let kind_name = self.syntax.get_node_name(kind)
                                .ok_or_else(|| ASTError::InvalidType(kind.to_string()))?;
                            let interface = sum.get_interface(self.syntax, kind_name)
                                .ok_or_else(|| ASTError::InvalidType(kind.to_string()))?;
                            interface.clone()
                        };
                        return self.walk_interface(value, &interface, name);
                    }
                }
                return Err(ASTError::InvalidValue {
                    expected: format!("{:?}", spec),
                    got: value.dump()
                })
            }
        }
    }
}

/// Define immutable AST Visitor/walker
make_ast_visitor!(ASTVisitor,ASTWalker,);

/// Define mutable AST Visitor/walker
make_ast_visitor!(MutASTVisitor, MutASTWalker, mut);

pub trait HasInterfaces {
    fn get_interface(&self, grammar: &Syntax, name: &NodeName) -> Option<Rc<Interface>>;
}

impl HasInterfaces for Vec<TypeSpec> {
    fn get_interface(&self, grammar: &Syntax, name: &NodeName) -> Option<Rc<Interface>> {
        debug!(target: "grammar", "get_interface, looking for {:?} in sum {:?}", name, self);
        for item in self {
            let result = item.get_interface(grammar, name);
            if result.is_some() {
                return result
            }
        }
        None
    }
}


impl HasInterfaces for NamedType {
    fn get_interface(&self, grammar: &Syntax, name: &NodeName) -> Option<Rc<Interface>> {
        debug!(target: "grammar", "get_interface, looking for {:?} in named type {:?}", name, self);
        match *self {
            NamedType::Interface(_) => None,
            NamedType::StringEnum(_) => None,
            NamedType::Typedef(ref type_) =>
                type_.spec().get_interface(grammar, name)
        }
    }
}

impl HasInterfaces for TypeSpec {
    fn get_interface(&self, grammar: &Syntax, name: &NodeName) -> Option<Rc<Interface>> {
        debug!(target: "grammar", "get_interface, looking for {:?} in spec {:?}", name, self);
        match *self {
            TypeSpec::NamedType(ref my_name) => {
                let follow = grammar.get_type_by_name(my_name);
                if let Some(follow) = follow {
                    if name == my_name {
                        follow.as_interface(grammar)
                    } else {
                        follow.get_interface(grammar, name)
                    }
                } else {
                    None
                }
            },
            TypeSpec::TypeSum(ref sum) =>
                sum.get_interface(grammar, name),
            _ => None
        }
    }
}
