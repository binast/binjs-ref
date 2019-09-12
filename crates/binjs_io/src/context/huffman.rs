#[allow(dead_code)] // Let's get rid of dead code warnings until they make sense.
use io::statistics::Instances;

use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;

/// A newtype for `u8` used to count the length of a key in bits.
#[derive(
    Debug,
    Default,
    Display,
    Serialize,
    Deserialize,
    From,
    Into,
    Add,
    AddAssign,
    Sub,
    SubAssign,
    Clone,
    Copy,
    PartialOrd,
    Ord,
    PartialEq,
    Eq,
)]
pub struct BitLen(u8);

/// Convenience implementation of operator `<<` in
/// `bits << bit_len`
impl std::ops::Shl<BitLen> for u32 {
    type Output = u32;
    fn shl(self, rhs: BitLen) -> u32 {
        self << Into::<u8>::into(rhs)
    }
}

/// The largerst acceptable length for a key.
///
/// Hardcoded in the format.
const MAX_CODE_BIT_LENGTH: u8 = 20;

/// A Huffman key
#[derive(Debug)]
struct Key {
    /// The bits in the key.
    ///
    /// Note that we only use the `bit_len` lowest-weight bits.
    /// Any other bit is ignored.
    bits: u32,

    /// The number of bits of `bits` to use.
    bit_len: BitLen,
}

/// A node in the Huffman tree.
struct Node<T> {
    /// The total number of instances of all `NodeContent::Leaf(T)` in this subtree.
    instances: Instances,

    /// The content of the node.
    content: NodeContent<T>,
}

/// Contents of a node in the Huffman tree.
enum NodeContent<T> {
    /// A value from the stream of values.
    Leaf(T),

    /// An internal node obtained by joining two subtrees.
    Internal {
        left: Box<NodeContent<T>>,
        right: Box<NodeContent<T>>,
    },
}

/// Custom ordering of `NodeContent`.
///
/// We compare *only* by number of instances.
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.instances.partial_cmp(&other.instances)
    }
}
impl<T> Ord for Node<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.instances.cmp(&other.instances)
    }
}
impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.instances.eq(&other.instances)
    }
}
impl<T> Eq for Node<T> {}

/// Keys associated to a sequence of values.
#[derive(Debug)]
pub struct Keys<T>
where
    T: Ord + Clone,
{
    /// The sequence of keys.
    ///
    /// Order is meaningful.
    keys: Vec<(T, Key)>,
}

impl<T> Keys<T>
where
    T: Ord + Clone,
{
    /// Compute a `Keys` from a sequence of values.
    ///
    /// # Performance
    ///
    /// Values (type `T`) will be cloned regularly, so you should make
    /// sure that their cloning is reasonably cheap.
    pub fn from_sequence<S>(source: S) -> Self
    where
        S: IntoIterator<Item = T>,
        T: PartialEq + Hash,
    {
        // Count the values.
        let mut map = HashMap::new();
        for item in source {
            let counter = map.entry(item).or_insert(0.into());
            *counter += 1.into();
        }
        // Then compute the `Keys`.
        Self::from_instances(map)
    }

    /// Compute a `Keys` from a sequence of values
    /// with a number of instances already attached.
    ///
    /// # Requirement
    ///
    /// Values of `T` in the source MUST be distinct.
    pub fn from_instances<S>(source: S) -> Self
    where
        S: IntoIterator<Item = (T, Instances)>,
    {
        let mut bit_lengths = Self::compute_bit_lengths(source);

        // Canonicalize order: (BitLen, T)
        // As values of `T` are
        bit_lengths.sort_unstable_by_key(|&(ref value, ref bit_len)| (*bit_len, value.clone()));

        // The bits associated to the next value.
        let mut bits = 0;
        let mut keys = Vec::with_capacity(bit_lengths.len());

        for i in 0..bit_lengths.len() - 1 {
            let (bit_len, symbol, next_bit_len) = (
                bit_lengths[i].1,
                bit_lengths[i].0.clone(),
                bit_lengths[i + 1].1,
            );
            keys.push((symbol.clone(), Key { bits, bit_len }));
            bits = (bits + 1) << (next_bit_len - bit_len);
        }
        // Handle the last element.
        let (ref symbol, bit_len) = bit_lengths[bit_lengths.len() - 1];
        keys.push((symbol.clone(), Key { bits, bit_len }));

        return Self { keys };
    }

    /// Convert a sequence of values labelled by their number of instances
    /// into a sequence of values labelled by the length for their path
    /// in the Huffman tree, aka the bitlength of their Huffman key.
    ///
    /// Values that have 0 instances are skipped.
    pub fn compute_bit_lengths<S>(source: S) -> Vec<(T, BitLen)>
    where
        S: IntoIterator<Item = (T, Instances)>,
    {
        // Build a min-heap sorted by number of instances.
        use std::cmp::Reverse;
        let mut heap = BinaryHeap::new();

        // Skip values that have 0 instances.
        for (value, instances) in source {
            if !instances.is_zero() {
                heap.push(Reverse(Node {
                    instances,
                    content: NodeContent::Leaf(value),
                }));
            }
        }

        let len = heap.len();
        if len == 0 {
            // Special case: no tree to build.
            return vec![];
        }

        // Take the two rarest nodes, merge them behind a prefix,
        // turn them into a single node with combined number of
        // instances. Repeat.
        while heap.len() > 1 {
            let left = heap.pop().unwrap();
            let right = heap.pop().unwrap();
            heap.push(Reverse(Node {
                instances: left.0.instances + right.0.instances,
                content: NodeContent::Internal {
                    left: Box::new(left.0.content),
                    right: Box::new(right.0.content),
                },
            }));
        }

        // Convert tree into bit lengths
        let root = heap.pop().unwrap(); // We have checked above that there is at least one value.
        let mut bit_lengths = Vec::with_capacity(len);
        fn aux<T>(bit_lengths: &mut Vec<(T, BitLen)>, depth: u8, node: &NodeContent<T>)
        where
            T: Clone,
        {
            match *node {
                NodeContent::Leaf(ref value) => bit_lengths.push((value.clone(), BitLen(depth))),
                NodeContent::Internal {
                    ref left,
                    ref right,
                } => {
                    aux(bit_lengths, depth + 1, left);
                    aux(bit_lengths, depth + 1, right)
                }
            }
        }
        aux(&mut bit_lengths, 0, &root.0.content);

        bit_lengths
    }
}

#[test]
fn test_coded_from_sequence() {
    let sample = "appl";
    let coded = Keys::from_sequence(sample.chars());

    // Symbol 'p' appears twice, we should see 3 codes.
    assert_eq!(coded.keys.len(), 3);

    // Check order of symbols.
    assert_eq!(coded.keys[0].0, 'p');
    assert_eq!(coded.keys[1].0, 'a');
    assert_eq!(coded.keys[2].0, 'l');

    // Check bit length of symbols.
    assert_eq!(coded.keys[0].1.bit_len, 1.into());
    assert_eq!(coded.keys[1].1.bit_len, 2.into());
    assert_eq!(coded.keys[2].1.bit_len, 2.into());

    // Check code of symbols.
    assert_eq!(coded.keys[0].1.bits, 0b00);
    assert_eq!(coded.keys[1].1.bits, 0b10);
    assert_eq!(coded.keys[2].1.bits, 0b11);
}
