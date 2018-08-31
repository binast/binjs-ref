use util::pick;

use binjs_meta::spec::*;

use json;
use json::JsonValue as JSON;
use rand;

pub trait Pick {
    fn random<T: rand::Rng>(&self, syntax: &Spec, rng: &mut T, depth_limit: isize) -> JSON;
}

pub struct Picker;
impl Pick for Picker {
    fn random<T: rand::Rng>(&self, syntax: &Spec, rng: &mut T, depth_limit: isize) -> JSON {
        syntax.get_root()
            .random(syntax, rng, depth_limit)
    }
}

impl Pick for NamedType {
    fn random<T: rand::Rng>(&self, syntax: &Spec, rng: &mut T, depth_limit: isize) -> JSON {
        match *self {
            NamedType::Interface(ref interface) =>
                interface.random(syntax, rng, depth_limit),
            NamedType::Typedef(ref typedef) =>
                typedef.random(syntax, rng, depth_limit),
            NamedType::StringEnum(ref string_enum) => {
                let string = pick(rng, &string_enum.strings());
                JSON::from(string.clone())
            }
        }
    }
}

impl Pick for TypeSpec {
    fn random<T: rand::Rng>(&self, syntax: &Spec, rng: &mut T, depth_limit: isize) -> JSON {
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
                if let Some(named) = syntax.get_type_by_name(name) {
                    named.random(syntax, rng, depth_limit)
                } else {
                    panic!("Could not find named type {:?}", name)
                }
            }
            TypeSpec::TypeSum(ref types) => {
                let type_ = pick(rng, types.types());
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
                JSON::Null,
            TypeSpec::Offset |
            TypeSpec::UnsignedLong => {
                json::from(rng.gen_range(0, u32::max_value()))
            }
        }
    }
}

impl Pick for Type {
    fn random<T: rand::Rng>(&self, syntax: &Spec, rng: &mut T, depth_limit: isize) -> JSON {
        if self.is_optional() {
            // 10% chance of returning the default value
            if depth_limit <= 0 || rng.gen_range(0, 10) > 0 {
                return JSON::Null
            }
        }
        self.spec.random(syntax, rng, depth_limit)
    }
}

impl Pick for Interface {
    /// Generate a random instance of this interface matching the syntax.
    fn random<T: rand::Rng>(&self, syntax: &Spec, rng: &mut T, depth_limit: isize) -> JSON {
        let mut obj = json::object::Object::with_capacity(self.contents().fields().len());
        for field in self.contents().fields() {
            let value = field.type_().random(syntax, rng, depth_limit - 1);
            obj.insert(field.name().to_str(), value);
        }
        json::JsonValue::Object(obj)
    }
}

impl Pick for Spec {
    /// Generate a random AST matching the grammar.
    ///
    /// `depth_limit` is used as *hint* to control the depth of the tree
    fn random<T: rand::Rng>(&self, _: &Spec, rng: &mut T, depth_limit: isize) -> JSON {
        let root = self.interfaces_by_name().get(&self.get_root_name())
            .expect("Root interface doesn't exist");
        root.random(self, rng, depth_limit)
    }
}
