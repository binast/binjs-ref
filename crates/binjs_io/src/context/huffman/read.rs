//! Huffman tables for reading.
//!
//! These tables are designed to aid decoding from sequences of bits
//! into values.

use context::huffman::*;

use std::convert::{TryFrom, TryInto};

/// A Huffman table.
///
/// We have several implementations of HuffmanTable designed for
/// distinct space/speed tradeoffs.
pub trait HuffmanTable<T> {
    /// Return the number of elements in the table.
    fn len(&self) -> usize;

    /// Return bit length of the table with most elements.
    fn highest_bit_len(&self) -> BitLen;

    /// Lookup a value from a sequence of bits.
    ///
    /// The sequence of bits MUST be at least as long as `highest_bit_len`.
    /// Use the `Key` result to determine how many bits need to actually be
    /// consumed from the bit stream.
    fn lookup(&self, key: &BitSequence) -> Option<&(T, Key)>;
}

/// A type that has a maximal value.
pub trait ValueIndex: TryFrom<usize> + TryInto<usize> + Clone {
    fn max_value() -> Self;
}
impl ValueIndex for u8 {
    fn max_value() -> u8 {
        std::u8::MAX
    }
}
impl ValueIndex for u32 {
    fn max_value() -> u32 {
        std::u32::MAX
    }
}
impl ValueIndex for usize {
    fn max_value() -> usize {
        std::usize::MAX
    }
}

/// An implementation of Huffman Tables as a vector designed to allow
/// constant-time lookups at the expense of high space complexity.
///
/// Type parameter `V` is the internal type of indices. Instantiating
/// with `V = u8` will provide the maximal speed and space-efficiency
/// but will only work if the table contains at most 2^8 values.
/// Alternatively, you may instantiate with `u32` or `usize` for
/// larger tables.
///
/// # Time complexity
///
/// Lookups take constant time, which essentially consists in two
/// simple vector lookups.
///
/// # Space complexity
///
/// After initialization, a `SingleLookupHuffmanTable`
/// requires O(2 ^ max bit length in the table) space:
///
/// - A vector `values` containing one entry per symbol.
/// - A vector `saturated` containing exactly 2 ^ (max bit length in the
///   table) entries, which we use to map any combination of `maxBitLength`
///   bits onto the only `HuffmanEntry` that may be reached by a prefix
///   of these `maxBitLength` bits. See below for more details.
///
/// # Algorithm
///
/// Consider the following Huffman table
///
/// Symbol | Binary Code  | Int value of Code | Bit Length
/// ------ | ------------ | ----------------- | ----------
/// A      | 11000        | 24                | 5
/// B      | 11001        | 25                | 5
/// C      | 1101         | 13                | 4
/// D      | 100          | 4                 | 3
/// E      | 101          | 5                 | 3
/// F      | 111          | 7                 | 3
/// G      | 00           | 0                 | 2
/// H      | 01           | 1                 | 2
///
/// By definition of a Huffman Table, the Binary Codes represent
/// paths in a Huffman Tree. Consequently, padding these codes
/// to the end would not change the result.
///
/// Symbol | Binary Code  | Int value of Code | Bit Length
/// ------ | ------------ | ----------------- | ----------
/// A      | 11000        | 24                | 5
/// B      | 11001        | 25                | 5
/// C      | 1101?        | [26...27]         | 4
/// D      | 100??        | [16...19]         | 3
/// E      | 101??        | [20..23]          | 3
/// F      | 111??        | [28..31]          | 3
/// G      | 00???        | [0...7]           | 2
/// H      | 01???        | [8...15]          | 2
///
/// Row "Int value of Code" now contains all possible values
/// that may be expressed in 5 bits. By using these values
/// as array indices, we may therefore represent the
/// Huffman table as an array:
///
/// Index     |   Symbol   |   Bit Length
/// --------- | ---------- | -------------
/// [0...7]   |  G         | 2
/// [8...15]  |  H         | 2
/// [16...19] |  D         | 3
/// [20...23] |  E         | 3
/// 24        |  A         | 5
/// 25        |  B         | 5
/// [26...27] |  C         | 4
/// [28...31] |  F         | 3
///
/// By using the next 5 bits in the bit buffer, we may, in
/// a single lookup, determine the symbol and the bit length.
///
/// In the current implementation, to save some space, we have
/// two distinct arrays, one (`values`) with a single instance of each
/// symbols bit length, and one (`saturated`) with indices into that
/// array.
#[derive(Debug)]
pub struct SingleLookupHuffmanTable<T, V> {
    highest_bit_len: BitLen,
    saturated: Vec<V>,
    values: Vec<(T, Key)>,
}
impl<T, V> SingleLookupHuffmanTable<T, V>
where
    V: ValueIndex,
{
    pub fn from_keys(keys: Keys<T>) -> Self {
        assert!(
            keys.len()
                <= V::max_value()
                    .try_into()
                    .unwrap_or_else(|_| panic!("Too many keys for ValueIndex"))
        );
        let highest_bit_len = keys.highest_bit_len();

        let mut values = Vec::with_capacity(keys.len());

        // Fill `saturated` with a default value of `V::max_value()`.
        // This is the value most likely to trigger errors in case
        // we have a bug in the implementation of `SingleLookupHuffmanTable`
        // or if the data provided is inconsistent.
        let mut saturated = Vec::with_capacity(1usize << highest_bit_len);
        saturated.resize(1usize << highest_bit_len, V::max_value());

        for (value_index, (value, key)) in keys.into_iter().enumerate() {
            let value_index: V = value_index
                .try_into()
                .unwrap_or_else(|_| panic!("Too many keys for ValueIndex"));

            // When we perform lookup, we will extract `highest_bit_len` bits from the key
            // into a value `0bB...B`. We have a match for `value` if and only if
            // `0bB...B` may be decomposed into `0bC...CX...X` such that
            //    - `0bC...C` is `bit_len` bits long;
            //    - `0bC...C == bits`.
            //
            // To perform a fast lookup, we precompute all possible values of `0bB...B`
            // for which this condition is true. That's all the values of segment
            // `[0bC...C0...0, 0bC...C1...1]`.
            let padding = highest_bit_len - key.bit_len();
            assert!(padding.as_u8() < 32);

            // `seg_begin` holds `0bC...C0...0` above
            let seg_begin = (key.bits() << padding) as usize;

            // `seg_len` holds `0bC...C1...1` - `0bC...C0...0`
            let seg_len: usize = if padding.as_u8() == 0 {
                0
            } else {
                let shift: u8 =
                    u8::checked_sub(8 * std::mem::size_of::<usize>() as u8, padding.into())
                        .unwrap();
                std::usize::MAX >> shift
            } + 1;
            for entry in &mut saturated[seg_begin..seg_begin + seg_len] {
                *entry = value_index.clone();
            }

            values.push((value, key));
        }

        Self {
            highest_bit_len,
            saturated,
            values,
        }
    }
}

impl<T, V> HuffmanTable<T> for SingleLookupHuffmanTable<T, V>
where
    V: ValueIndex,
{
    fn len(&self) -> usize {
        self.values.len()
    }

    fn highest_bit_len(&self) -> BitLen {
        self.highest_bit_len
    }

    fn lookup(&self, key: &BitSequence) -> Option<&(T, Key)> {
        assert!(key.bit_len() >= self.highest_bit_len());
        let (prefix, _) = key.split(self.highest_bit_len());
        let value_index = self.saturated[prefix as usize].clone();
        let value_index: usize = value_index
            .try_into()
            .unwrap_or_else(|_| panic!("Value index does not fit into a usize"));
        self.values.get(value_index)
    }
}

#[test]
fn test_single_lookup_huffman_table() {
    // Check against a hardcoded constant, to ensure consistency
    // with fbssdc implementation.

    fn run_test<V>()
    where
        V: ValueIndex,
    {
        let sample = "appl";
        let coded = Keys::from_sequence(sample.chars(), std::u8::MAX).unwrap();
        let table: SingleLookupHuffmanTable<char, V> = SingleLookupHuffmanTable::from_keys(coded);

        assert_eq!(table.len(), 3);

        // Test with all possible 2 bit sequences.
        let candidate = BitSequence::new(0b10, BitLen(2));
        let result = table.lookup(&candidate).unwrap();
        assert_eq!(result, &('a', Key::new(0b10, BitLen(2))));

        let candidate = BitSequence::new(0b11, BitLen(2));
        let result = table.lookup(&candidate).unwrap();
        assert_eq!(result, &('l', Key::new(0b11, BitLen(2))));

        // With a bit length of 2, there are two keys that
        // should return 'p'
        for prefix in &[0b00, 0b01] {
            let candidate = BitSequence::new(*prefix, BitLen(2));
            let result = table.lookup(&candidate).unwrap();
            assert_eq!(result, &('p', Key::new(0, BitLen(1))));
        }

        // Test values with all possible 3 bit sequences.
        for prefix in &[0b100, 0b101] {
            let candidate = BitSequence::new(*prefix, BitLen(3));
            let result = table.lookup(&candidate).unwrap();
            assert_eq!(result, &('a', Key::new(0b10, BitLen(2))));
        }

        for prefix in &[0b110, 0b111] {
            let candidate = BitSequence::new(*prefix, BitLen(3));
            let result = table.lookup(&candidate).unwrap();
            assert_eq!(result, &('l', Key::new(0b11, BitLen(2))));
        }

        for prefix in &[0b000, 0b001, 0b010, 0b011] {
            let candidate = BitSequence::new(*prefix, BitLen(3));
            let result = table.lookup(&candidate).unwrap();
            assert_eq!(result, &('p', Key::new(0, BitLen(1))));
        }
    }

    run_test::<u8>();
    run_test::<u32>();
}

#[test]
fn test_single_lookup_huffman_table_2() {
    // Check internal consistency.

    fn run_test<V>()
    where
        V: ValueIndex,
    {
        let sample = "Lorem ipsum dolor sit amet consectetur adipiscing elit convallis nostra, integer diam odio mus eros ut sodales sociis cursus, montes imperdiet morbi rhoncus felis venenatis curabitur magna. Volutpat tincidunt sociosqu pharetra id feugiat enim eget, integer quisque magna in senectus mollis, himenaeos malesuada convallis faucibus ornare egestas. Netus platea himenaeos suscipit nostra montes mattis, lobortis ut arcu facilisi hac ornare, integer ante sociosqu placerat morbi.

Viverra arcu dapibus nam magna a imperdiet inceptos cubilia libero lobortis praesent habitasse, tortor id leo consequat sollicitudin elementum fames fringilla himenaeos donec. Phasellus posuere congue ultricies scelerisque senectus vivamus facilisi, vestibulum consequat aptent lectus ad sociis porta, purus libero eros leo at nec. Netus viverra urna nisl sapien conubia porta sed luctus penatibus cras, pulvinar iaculis sagittis fusce fringilla et rutrum sollicitudin ligula, dui vestibulum interdum pretium montes diam nibh inceptos ante.
";
        let coded = Keys::from_sequence(sample.chars(), std::u8::MAX).unwrap();
        let table: SingleLookupHuffmanTable<char, V> =
            SingleLookupHuffmanTable::from_keys(coded.clone());
        for (value, key) in coded {
            // Test that candidate keys obtained by extending `key` with additional bits
            // return the expected `(value, key)`.
            for bit_len in table.highest_bit_len().as_u8()
                ..=std::cmp::min(table.highest_bit_len().as_u8() + 5, 32)
            {
                let candidate = key.as_bit_sequence().pad_lowest_to(BitLen(bit_len));
                let lookup = table.lookup(&candidate).expect("Lookup value not found");
                assert_eq!(lookup.0, value);
                assert_eq!(lookup.1, key);
            }
        }
    }
    run_test::<u8>();
    run_test::<u32>();
}
