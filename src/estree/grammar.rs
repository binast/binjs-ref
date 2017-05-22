//! A grammar for specifying an AST that this tool can manipulate.
//!
//! This abstracts away [ESTree](https://github.com/estree/estree).

#![allow(dead_code, unused)]

use std::collections::HashMap;
use std::rc::*;

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct InterfaceName(Rc<String>);
impl InterfaceName {
    pub fn to_string(&self) -> &String {
        self.0.as_ref()
    }
}

#[derive(Hash, PartialEq, Eq)]
pub struct Tag(Rc<String>);
impl Tag {
    pub fn to_string(&self) -> &String {
        self.0.as_ref()
    }
}

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
pub enum Type {
    Array(Box<Type>),
    Structure(Structure),

    /// A choice between several literals, e.g. `"get" | "set"`.
    OneOfStrings {strings: Vec<String>, or_null: bool},

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


/// Representation of an object-like value.
pub struct Structure {
    fields: Vec<Field>,
}
impl Structure {
    pub fn fields<'a>(&'a self) -> &'a [Field] {
        unimplemented!()
    }
    pub fn field<'a>(&'a self, name: &str) -> Option<&'a Field> {
        unimplemented!()
    }
}

pub struct Interface {
    /// The name of the interface, e.g. `Node`.
    name: InterfaceName,

    /// The tag used to differentiate node that inhabit this
    /// interface from nodes inhabiting other interfaces.
    ///
    /// May be `None` for interfaces such as `Node` or `Expression`
    /// that serve only as a common ancestor for a sum of refined sub-interfaces
    /// and have no inhabitants of their own.
    tag: Option<Tag>,

    /// Sub-interfaces, refining or amending this interface.
    refinements: Vec<InterfaceName>, // FIXME: Strings are probably not the best keys for this thing.

    /// If this is a subinterface, the parent interfaces.
    parent: Vec<Weak<Interface>>,

    /// The contents of this interface, excluding the contents of parent interfaces.
    own_contents: Structure,
}

impl Interface {
    fn by_type(&self, kind: &Tag) -> Option<&Interface> {
        unimplemented!()
    }
    pub fn name(&self) -> &InterfaceName {
        &self.name
    }
    /// Returns the full list of fields for this structure.
    /// This method is in charge of:
    /// - ensuring that the fields of parent structures are properly accounted for;
    /// - disregarding ignored fields (i.e. `position`, `type`);
    /// - disregarding fields with a single possible value.
    pub fn contents(&self) -> &Structure {
        unimplemented!()
    }
    pub fn get_refinement(&self, tag: &Tag) -> Option<&Interface> {
        unimplemented!()
    }
}

pub struct Syntax {
    interfaces: HashMap<InterfaceName, Interface>
}

impl Syntax {
    pub fn add_interface(&mut self, interface: Interface) {
        unimplemented!()
    }
    pub fn get_interface(&self, name: &InterfaceName) -> Option<Interface> {
        unimplemented!()
    }
    pub fn get_tag(&self, name: &str) -> Option<Tag> {
        unimplemented!()
    }
}

