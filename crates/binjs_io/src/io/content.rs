
/// A container for information associated with a type of data we write to the stream
/// as part of the content (i.e. not the header).
///
/// Typically used to collect/display the number of bytes written in each category.
#[derive(Debug, Default, Add, Clone, AddAssign)]
pub struct ContentInfo<T> {
    pub bools: T,
    pub floats: T,
    pub unsigned_longs: T,
    pub string_enums: T,
    pub property_keys: T,
    pub identifier_names: T,
    pub interface_names: T,
    pub string_literals: T,
    pub list_lengths: T,
}
impl<T> ContentInfo<T> {
    /// Initialize a new `ContentInfo`.
    pub fn with<F>(f: F) -> Self
        where F: Fn(&str) -> T
    {
        ContentInfo {
            bools: f("bools"),
            floats: f("floats"),
            unsigned_longs: f("unsigned_longs"),
            string_enums: f("string_enums"),
            property_keys: f("property_keys"),
            identifier_names: f("identifier_names"),
            interface_names: f("interface_names"),
            string_literals: f("string_literals"),
            list_lengths: f("list_lengths"),
        }
    }

    /// Convert a `ContentInfo` into another one.
    pub fn into_with<F, U>(self, f: F) -> ContentInfo<U>
        where F: Fn(&str, T) -> U
    {
        ContentInfo {
            bools: f("bools", self.bools),
            floats: f("floats", self.floats),
            unsigned_longs: f("unsigned_longs", self.unsigned_longs),
            string_enums: f("string_enums", self.string_enums),
            property_keys: f("property_keys", self.property_keys),
            identifier_names: f("identifier_names", self.identifier_names),
            interface_names: f("interface_names", self.interface_names),
            string_literals: f("string_literals", self.string_literals),
            list_lengths: f("list_lengths", self.list_lengths),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&'static str, &T)> {
        vec![
            ("bools", &self.bools),
            ("floats", &self.floats),
            ("unsigned_longs", &self.unsigned_longs),
            ("string_enums", &self.string_enums),
            ("property_keys", &self.property_keys),
            ("identifier_names", &self.identifier_names),
            ("interface_names", &self.interface_names),
            ("string_literals", &self.string_literals),
            ("list_lengths", &self.list_lengths),
        ].into_iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&'static str, &mut T)> {
        vec![
            ("bools", &mut self.bools),
            ("floats", &mut self.floats),
            ("unsigned_longs", &mut self.unsigned_longs),
            ("string_enums", &mut self.string_enums),
            ("property_keys", &mut self.property_keys),
            ("identifier_names", &mut self.identifier_names),
            ("interface_names", &mut self.interface_names),
            ("string_literals", &mut self.string_literals),
            ("list_lengths", &mut self.list_lengths),
        ].into_iter()
    }

    pub fn into_iter(self) -> impl Iterator<Item = (&'static str, T)> {
        vec![
            ("bools", self.bools),
            ("floats", self.floats),
            ("unsigned_longs", self.unsigned_longs),
            ("string_enums", self.string_enums),
            ("property_keys", self.property_keys),
            ("identifier_names", self.identifier_names),
            ("interface_names", self.interface_names),
            ("string_literals", self.string_literals),
            ("list_lengths", self.list_lengths),
        ].into_iter()
    }
}
