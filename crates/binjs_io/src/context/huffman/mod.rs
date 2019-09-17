use io::statistics::Instances;

use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;

/// Reading from bitstreams and decoding their contents using Huffman tables.
pub mod read;

/// A newtype for `u8` used to count the length of a key in bits.
#[derive(
    Constructor,
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
impl BitLen {
    pub fn as_u8(&self) -> u8 {
        self.0
    }
}

/// Convenience implementation of operator `<<` in
/// `bits << bit_len`
impl std::ops::Shl<BitLen> for u32 {
    type Output = u32;
    fn shl(self, rhs: BitLen) -> u32 {
        self << Into::<u8>::into(rhs)
    }
}
impl std::ops::Shl<BitLen> for usize {
    type Output = usize;
    fn shl(self, rhs: BitLen) -> usize {
        self << Into::<u8>::into(rhs)
    }
}

/// Convenience implementation of operator `>>` in
/// `bits >> bit_len`
impl std::ops::Shr<BitLen> for u32 {
    type Output = u32;
    fn shr(self, rhs: BitLen) -> u32 {
        self >> Into::<u8>::into(rhs)
    }
}
impl std::ops::Shr<BitLen> for usize {
    type Output = usize;
    fn shr(self, rhs: BitLen) -> usize {
        self >> Into::<u8>::into(rhs)
    }
}

/// The largerst acceptable length for a key.
///
/// Hardcoded in the format.
const MAX_CODE_BIT_LENGTH: u8 = 20;

/// A sequence of bits, read from a bit stream.
///
/// Typically used for lookup of entries in Huffman tables.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BitSequence {
    bits: u32,
    bit_len: BitLen,
}
impl BitSequence {
    pub fn new(bits: u32, bit_len: BitLen) -> Self {
        Self { bits, bit_len }
    }

    pub fn bits(&self) -> u32 {
        self.bits
    }

    /// The number of bits of `bits` to use.
    pub fn bit_len(&self) -> BitLen {
        self.bit_len
    }

    /// Split the bits into a prefix of `bit_len` bits and a suffix containing the
    /// remaining bits.
    ///
    /// If `bit_len` is larger than the number of bits, the prefix is padded with
    /// lower-weight bits into `bit_len` bits.
    pub fn split_bits(&self, bit_len: BitLen) -> (u32, u32) {
        debug_assert!(bit_len.as_u8() <= 32);
        if self.bit_len <= bit_len {
            let padding = bit_len - self.bit_len;
            (self.bits << padding, 0)
        } else {
            let shift = self.bit_len - bit_len;
            match shift.into() {
                32u8 => (0, self.bits), // Special case: cannot >> 32
                shift => (
                    self.bits >> shift,
                    self.bits & (std::u32::MAX >> 32 - shift),
                ),
            }
        }
    }

    /// Split the bits into a prefix of `bit_len` bits and a suffix of `self.bit_len - bit_len`
    /// bits.
    ///
    /// # Failure
    ///
    /// This function panics if `bit_len > self.bit_len`.
    pub fn split(&self, bit_len: BitLen) -> (BitSequence, BitSequence) {
        let (prefix, suffix) = self.split_bits(bit_len);
        (
            BitSequence::new(prefix, bit_len),
            BitSequence::new(
                suffix,
                if self.bit_len >= bit_len {
                    self.bit_len - bit_len
                } else {
                    BitLen::new(0)
                },
            ),
        )
    }

    /// Add lowest-weight to this bit sequence bits until it reaches
    /// a sufficient bit length.
    ///
    /// Does nothing if the bit sequence already has a sufficient bitlength.
    pub fn pad_lowest_to(&self, total_bit_len: BitLen) -> Cow<BitSequence> {
        assert!(total_bit_len.0 <= 32u8);
        if total_bit_len <= self.bit_len {
            return Cow::Borrowed(self);
        }
        let shift = total_bit_len - self.bit_len;
        if shift.0 == 32u8 {
            return Cow::Owned(BitSequence::new(0, BitLen(32)));
        }
        Cow::Owned(BitSequence::new(self.bits << shift, total_bit_len))
    }

    /// Prepend a sequence of bits to a sequencce.s
    pub fn with_prefix(&self, prefix: &BitSequence) -> Self {
        assert!((prefix.bit_len() + self.bit_len()).as_u8() <= 32);
        let bits = self.bits | (prefix.bits() << self.bit_len);
        let bit_len = self.bit_len + prefix.bit_len;
        BitSequence::new(bits, bit_len)
    }

    /// Return a range representing all possible suffixes of this `BitSequence`
    /// containing exactly `bit_len` bits.
    ///
    /// If this `BitSequence` is already at least `bit_len` bits long, we
    /// truncate the `BitSequence` to `bit_len` bits by removing the
    /// lower-weight bits and there is only one such suffix.
    ///
    /// ```
    /// use binjs_io::context::huffman::{ BitLen, BitSequence };
    ///
    /// let zero = BitSequence::new(0, BitLen::new(0));
    ///
    /// let range = zero.suffixes(BitLen::new(0));
    /// assert_eq!(range, 0..1);
    ///
    /// let range = zero.suffixes(BitLen::new(2));
    /// assert_eq!(range, 0..4);
    ///
    /// let range = zero.suffixes(BitLen::new(3));
    /// assert_eq!(range, 0..8);
    ///
    /// let range = zero.suffixes(BitLen::new(4));
    /// assert_eq!(range, 0..16);
    ///
    /// let sequence = BitSequence::new(0b00000100, BitLen::new(3));
    ///
    /// let range = sequence.suffixes(BitLen::new(0));
    /// assert_eq!(range, 0..1);
    ///
    /// let range = sequence.suffixes(BitLen::new(2));
    /// assert_eq!(range, 2..3);
    ///
    /// let range = sequence.suffixes(BitLen::new(3));
    /// assert_eq!(range, 4..5);
    ///
    /// let range = sequence.suffixes(BitLen::new(4));
    /// assert_eq!(range, 8..10); // 0b000001000 to 0b00001001 included
    /// ```
    pub fn suffixes(&self, bit_len: BitLen) -> std::ops::Range<u32> {
        debug_assert!(bit_len.as_u8() as usize <= 8 * std::mem::size_of_val(&self.bits()));
        debug_assert!(
            std::mem::size_of_val(&self.bits()) == std::mem::size_of::<u32>(),
            "The arithmetics relies upon the fact that we're only using `u32` for Huffman keys"
        );
        let (first, last) = if bit_len <= self.bit_len() {
            // We have too many bits, we need to truncate the bits,
            // then return a single element.
            let shearing: u8 = (self.bit_len() - bit_len).as_u8();
            let first = if shearing == 32 {
                0
            } else {
                self.bits() >> shearing
            };
            (first, first)
        } else {
            // We need to pad with lower-weight 0s.
            let padding: u8 = (bit_len - self.bit_len()).as_u8();
            let first = self.bits() << padding;
            let len = std::u32::MAX >> (8 * std::mem::size_of::<u32>() as u8 - padding);
            (first, first + len)
        };
        first..(last + 1)
    }
}

#[test]
fn test_bit_sequence_split() {
    let bits = 0b11111111_11111111_00000000_00000000;
    let key = BitSequence::new(bits, BitLen(32));
    assert_eq!(key.split_bits(BitLen(0)), (0, bits));
    assert_eq!(key.split_bits(BitLen(32)), (bits, 0));
    assert_eq!(key.split_bits(BitLen(16)), (0b11111111_11111111, 0));

    let bits = 0b00000000_00000000_00000000_11111111;
    let key = BitSequence::new(bits, BitLen(16));
    assert_eq!(key.split_bits(BitLen(0)), (0, bits));
    assert_eq!(key.split_bits(BitLen(16)), (bits, 0));
    assert_eq!(key.split_bits(BitLen(8)), (0, 0b11111111));
}

/// A Huffman key
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Key(BitSequence);

impl Key {
    /// Create a new Key.
    ///
    /// Note that we only use the `bit_len` lowest-weight bits.
    /// Any other bit MUST BE 0.
    pub fn new(bits: u32, bit_len: BitLen) -> Self {
        debug_assert!({
            let bit_len: u8 = bit_len.into();
            bit_len <= 32
        });
        debug_assert!({
            let bit_len: u8 = bit_len.into();
            if bit_len < 32 {
                bits >> bit_len == 0
            } else {
                true
            }
        });
        Key(BitSequence { bits, bit_len })
    }

    pub fn from_bit_sequence(sequence: BitSequence) -> Self {
        Self::new(sequence.bits, sequence.bit_len)
    }

    /// The bits in this Key.
    ///
    /// # Invariant
    ///
    /// Only the `self.bit_len()` lowest-weight bits may be non-0.
    pub fn bits(&self) -> u32 {
        self.0.bits
    }

    /// The number of bits of `bits` to use.
    pub fn bit_len(&self) -> BitLen {
        self.0.bit_len
    }

    pub fn as_bit_sequence(&self) -> &BitSequence {
        &self.0
    }

    pub fn with_prefix(&self, prefix: &BitSequence) -> Self {
        let sequence = self.0.with_prefix(prefix);
        Key::from_bit_sequence(sequence)
    }
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

/// Codebook associated to a sequence of values.
#[derive(Clone, Debug)]
pub struct Codebook<T> {
    /// The longest bit length that actually appears in `mappings`.
    highest_bit_len: BitLen,

    /// The sequence of keys.
    ///
    /// Order is meaningful.
    mappings: Vec<(T, Key)>,
}

impl<T> Codebook<T> {
    /// The number of elements in this Codebook.
    pub fn len(&self) -> usize {
        self.mappings.len()
    }

    /// The longest bit length that acctually appears in this Codebook.
    pub fn highest_bit_len(&self) -> BitLen {
        self.highest_bit_len
    }
}

impl<T> IntoIterator for Codebook<T> {
    type Item = (T, Key);
    type IntoIter = std::vec::IntoIter<(T, Key)>;
    fn into_iter(self) -> Self::IntoIter {
        self.mappings.into_iter()
    }
}

impl<T> Codebook<T>
where
    T: Ord + Clone,
{
    /// Compute a `Codebook` from a sequence of values.
    ///
    /// Optionally, `max_bit_len` may specify a largest acceptable bit length.
    /// If the `Codebook` may not be computed without exceeding this bit length,
    /// fail with `Err(problemantic_bit_len)`.
    ///
    /// The current implementation only attempts to produce the best compression
    /// level. This may cause us to exceed `max_bit_len` even though an
    /// alternative table, with a lower compression level, would let us
    /// proceed without exceeding `max_bit_len`.
    ///
    /// # Performance
    ///
    /// Values (type `T`) will be cloned regularly, so you should make
    /// sure that their cloning is reasonably cheap.
    pub fn from_sequence<S>(source: S, max_bit_len: u8) -> Result<Self, u8>
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
        // Then compute the `Codebook`.
        Self::from_instances(map, max_bit_len)
    }

    /// Compute a `Codebook` from a sequence of values
    /// with a number of instances already attached.
    ///
    /// The current implementation only attempts to produce the best compression
    /// level. This may cause us to exceed `max_bit_len` even though an
    /// alternative table, with a lower compression level, would let us
    /// proceed without exceeding `max_bit_len`.
    ///
    /// # Requirement
    ///
    /// Values of `T` in the source MUST be distinct.
    pub fn from_instances<S>(source: S, max_bit_len: u8) -> Result<Self, u8>
    where
        S: IntoIterator<Item = (T, Instances)>,
    {
        let mut bit_lengths = Self::compute_bit_lengths(source, max_bit_len)?;
        let mut highest_bit_len = BitLen(0);

        // Canonicalize order: (BitLen, T)
        bit_lengths.sort_unstable_by_key(|&(ref value, ref bit_len)| (*bit_len, value.clone()));

        // The bits associated to the next value.
        let mut bits = 0;
        let mut mappings = Vec::with_capacity(bit_lengths.len());

        for i in 0..bit_lengths.len() - 1 {
            let (bit_len, symbol, next_bit_len) = (
                bit_lengths[i].1,
                bit_lengths[i].0.clone(),
                bit_lengths[i + 1].1,
            );
            mappings.push((symbol.clone(), Key::new(bits, bit_len)));
            bits = (bits + 1) << (next_bit_len - bit_len);
            if bit_len > highest_bit_len {
                highest_bit_len = bit_len;
            }
        }
        // Handle the last element.
        let (ref symbol, bit_len) = bit_lengths[bit_lengths.len() - 1];
        mappings.push((symbol.clone(), Key::new(bits, bit_len)));

        return Ok(Self {
            highest_bit_len,
            mappings,
        });
    }

    /// Convert a sequence of values labelled by their number of instances
    /// into a sequence of values labelled by the length for their path
    /// in the Huffman tree, aka the bitlength of their Huffman key.
    ///
    /// Values that have 0 instances are skipped.
    pub fn compute_bit_lengths<S>(source: S, max_bit_len: u8) -> Result<Vec<(T, BitLen)>, u8>
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
            return Ok(vec![]);
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
        fn aux<T>(
            bit_lengths: &mut Vec<(T, BitLen)>,
            max_bit_len: u8,
            depth: u8,
            node: &NodeContent<T>,
        ) -> Result<(), u8>
        where
            T: Clone,
        {
            match *node {
                NodeContent::Leaf(ref value) => {
                    if depth > max_bit_len {
                        return Err(depth);
                    }
                    bit_lengths.push((value.clone(), BitLen(depth)));
                    Ok(())
                }
                NodeContent::Internal {
                    ref left,
                    ref right,
                } => {
                    aux(bit_lengths, max_bit_len, depth + 1, left)?;
                    aux(bit_lengths, max_bit_len, depth + 1, right)?;
                    Ok(())
                }
            }
        }
        aux(&mut bit_lengths, max_bit_len, 0, &root.0.content)?;

        Ok(bit_lengths)
    }
}

#[test]
fn test_coded_from_sequence() {
    let sample = "appl";
    let coded = Codebook::from_sequence(sample.chars(), std::u8::MAX).unwrap();

    // Symbol 'p' appears twice, we should see 3 codes.
    assert_eq!(coded.mappings.len(), 3);

    // Check order of symbols.
    assert_eq!(coded.mappings[0].0, 'p');
    assert_eq!(coded.mappings[1].0, 'a');
    assert_eq!(coded.mappings[2].0, 'l');

    // Check bit length of symbols.
    assert_eq!(coded.mappings[0].1.bit_len(), 1.into());
    assert_eq!(coded.mappings[1].1.bit_len(), 2.into());
    assert_eq!(coded.mappings[2].1.bit_len(), 2.into());

    // Check code of symbols.
    assert_eq!(coded.mappings[0].1.bits(), 0b00);
    assert_eq!(coded.mappings[1].1.bits(), 0b10);
    assert_eq!(coded.mappings[2].1.bits(), 0b11);

    // Let's try again with a limit to 1 bit paths.
    assert_eq!(Codebook::from_sequence(sample.chars(), 1).unwrap_err(), 2);
}

impl<T> Codebook<T> {
    /// Create an empty Codebook
    pub fn new() -> Self {
        Self {
            highest_bit_len: BitLen::new(0),
            mappings: vec![],
        }
    }

    /// Create an empty Codebook
    pub fn with_capacity(len: usize) -> Self {
        Self {
            highest_bit_len: BitLen::new(0),
            mappings: Vec::with_capacity(len),
        }
    }

    /// Add a mapping to a Codebook.
    ///
    /// This method does **not** check that the resulting Codebook is correct.
    pub unsafe fn add_mapping(&mut self, value: T, key: Key) {
        if key.bit_len() > self.highest_bit_len {
            self.highest_bit_len = key.bit_len();
        }
        self.mappings.push((value, key));
    }

    /// Return the mappings of a Codebook.
    pub fn mappings(self) -> Vec<(T, Key)> {
        self.mappings
    }

    pub fn map<F, U>(self, mut f: F) -> Codebook<U>
    where
        F: FnMut(T) -> U,
    {
        Codebook {
            highest_bit_len: self.highest_bit_len,
            mappings: self
                .mappings
                .into_iter()
                .map(|(value, key)| (f(value), key))
                .collect(),
        }
    }
}
