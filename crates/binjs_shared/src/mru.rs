use std::collections::LinkedList;
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Seen {
    /// The entry has already been seen, `N` calls to `access` ago. So if
    /// we call `access(foo)` twice in a row, the second call will return
    /// `Age(0)`.
    Age(usize),

    /// The entry has never been seen.
    Never(usize)
}

/// A structure used to access values with repeated patterns.
///
/// ```
/// use binjs_shared::mru::{ MRU, Seen };
///
/// let mut mru = MRU::new();
///
/// assert_eq!(mru.access(&'a'), Seen::Never(0), "Introducing a");
/// assert_eq!(mru.access(&'a'), Seen::Age(0),   "Just introduced a");
/// assert_eq!(mru.access(&'a'), Seen::Age(0),   "Just accessed a");
/// assert_eq!(mru.access(&'b'), Seen::Never(1), "Introducing b");
/// assert_eq!(mru.access(&'b'), Seen::Age(0),   "Just introduced b");
/// assert_eq!(mru.access(&'a'), Seen::Age(1),   "Accessing previous a");
/// assert_eq!(mru.access(&'a'), Seen::Age(0),   "Just accessed a again");
/// assert_eq!(mru.access(&'c'), Seen::Never(2), "Just introduced c");
/// assert_eq!(mru.access(&'a'), Seen::Age(1),   "Accessing previous a, again");
/// assert_eq!(mru.access(&'a'), Seen::Age(0),   "Accessing previous a, again");
/// assert_eq!(mru.access(&'b'), Seen::Age(2),   "Accessing previous b");
///```
pub struct MRU<T> where T: Hash + Eq + Clone {
    items: LinkedList<T>,
}
impl<T> MRU<T> where T: Hash + Eq + Clone {
    pub fn new() -> Self {
        Self {
            items: LinkedList::new(),
        }
    }
    pub fn access(&mut self, value: &T) -> Seen {
        let position = self.items.iter()
            .position(|x| x == value);
        match position {
            None => {
                let len = self.items.len();
                self.items.push_front(value.clone());
                Seen::Never(len)
            }
            Some(0) => {
                Seen::Age(0)
            }
            Some(position) => {
                // Remove item from position `position`.
                let mut suffix = self.items.split_off(position);
                let hd = suffix.pop_front()
                    .unwrap();
                assert!(&hd == value);
                self.items.append(&mut suffix);
                // Add it at position 0.
                self.items.push_front(hd);
                Seen::Age(position)
            }
        }
    }
}