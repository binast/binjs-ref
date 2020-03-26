use io::statistics::Instances;

use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;
use std::io;

/// Huffman trees.
mod codebook;

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

/// The maximal number of bits permitted in a Huffman key
/// in this format.
pub const MAX_CODE_BIT_LEN: BitLen = BitLen(20);

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
        if rhs.as_u8() == 32 {
            return 0;
        }
        self >> rhs.as_u8()
    }
}
impl std::ops::Shr<BitLen> for usize {
    type Output = usize;
    fn shr(self, rhs: BitLen) -> usize {
        if rhs.as_u8() == 32 {
            return 0;
        }
        self >> rhs.as_u8()
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
        assert!(bit_len.as_u8() <= 32);
        assert_eq!(bits >> bit_len, 0);
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
    pub fn split_raw_bits(&self, bit_len: BitLen) -> (u32, u32) {
        debug_assert!(bit_len.as_u8() <= 32);
        if self.bit_len <= bit_len {
            let padding = bit_len - self.bit_len;
            (self.bits << padding, 0)
        } else {
            let shift: BitLen = self.bit_len - bit_len;
            let co_shift: BitLen = BitLen::new(32) - shift;
            (self.bits >> shift, self.bits & (std::u32::MAX >> co_shift))
        }
    }

    /// Split the bits into a prefix of `bit_len` bits and a suffix of `self.bit_len - bit_len`
    /// bits.
    ///
    /// # Failure
    ///
    /// This function panics if `bit_len > self.bit_len`.
    pub fn split(&self, bit_len: BitLen) -> (BitSequence, BitSequence) {
        let (prefix, suffix) = self.split_raw_bits(bit_len);
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
        assert!(total_bit_len.as_u8() <= 32);
        if total_bit_len <= self.bit_len {
            return Cow::Borrowed(self);
        }
        let shift: BitLen = total_bit_len - self.bit_len;
        Cow::Owned(BitSequence::new(self.bits << shift, total_bit_len))
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
            let shearing: BitLen = self.bit_len() - bit_len;
            let first = self.bits() >> shearing;
            (first, first)
        } else {
            // We need to pad with lower-weight 0s.
            let padding: BitLen = bit_len - self.bit_len();
            let co_padding = BitLen::new(32) - padding;
            let first = self.bits() << padding;
            let len = std::u32::MAX >> co_padding;
            (first, first + len)
        };
        first..(last + 1)
    }
}

#[test]
fn test_bit_sequence_split() {
    let bits = 0b11111111_11111111_00000000_00000000;
    let key = BitSequence::new(bits, BitLen(32));
    assert_eq!(key.split_raw_bits(BitLen(0)), (0, bits));
    assert_eq!(key.split_raw_bits(BitLen(32)), (bits, 0));
    assert_eq!(key.split_raw_bits(BitLen(16)), (0b11111111_11111111, 0));

    let bits = 0b00000000_00000000_00000000_11111111;
    let key = BitSequence::new(bits, BitLen(16));
    assert_eq!(key.split_raw_bits(BitLen(0)), (0, bits));
    assert_eq!(key.split_raw_bits(BitLen(16)), (bits, 0));
    assert_eq!(key.split_raw_bits(BitLen(8)), (0, 0b11111111));
}

/// A Huffman key
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Key(BitSequence);

impl Key {
    /// Create a new Key.
    ///
    /// Note that we only use the `bit_len` lowest-weight bits.
    ///
    /// # Failure
    ///
    /// - Panic if any bit other than the `bit_len` lowest-weight bits is 0.
    /// - Panic if the bit length is greater than 20.
    pub fn new(bits: u32, bit_len: BitLen) -> Self {
        assert!(bit_len <= BitLen::new(20));
        Self::try_new(bits, bit_len).expect("Invalid Key")
    }

    /// Create a new Key.
    ///
    /// Note that we only use the `bit_len` lowest-weight bits.
    /// Any other bit MUST BE 0.
    pub fn try_new(bits: u32, bit_len: BitLen) -> Result<Self, io::Error> {
        // May the value fit in a `Key`?
        if bit_len.as_u8() > 32 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "bitlength exceeds Key capacity",
            ));
        }
        // Are the heavy-weight bits 0s, as expected?
        if bit_len.as_u8() < 32 {
            if bits >> bit_len != 0 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Invalid Key content",
                ));
            }
        }
        Ok(Key(BitSequence { bits, bit_len }))
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

/// An alphabet of symbols.
pub trait Alphabet {
    type Symbol: Ord + Clone;

    /// Read a symbol from an input stream.
    fn read_literal<R>(input: R) -> Result<Self::Symbol, io::Error>
    where
        R: io::Read;

    fn write_literal<W>(symbol: &Self::Symbol, output: W) -> Result<(), io::Error>
    where
        W: io::Write;
}

/// An alphabet of symbols known statically from the grammar.
/// Also known as `Implicit Symbols` in the grammar.
///
/// For instance, in most languages, there is a finite set of
/// arithmetic operators specified by the grammar.
pub trait StaticAlphabet: Alphabet {
    /// The number of symbols in this static alphabet.
    fn len() -> u32;

    /// Return the nth value of the alphabet of `None` if there is no such value.
    fn symbol(u32) -> Option<Self::Symbol>;
}

/// An alphabet of symbols known dynamically from the file.
/// Also known as `Explicit Symbols` in the grammar.
///
/// For instance, in most languages, the set of literal strings
/// actually used in a file is determined by the user, not by
/// the grammar.
pub trait DynamicAlphabet: Alphabet {}
