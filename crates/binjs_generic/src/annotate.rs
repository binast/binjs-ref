use json::JsonValue as JSON;

pub trait Annotator {
    fn annotate(&self, _ast: &mut JSON) {
        // By default, do nothing.
    }
}
