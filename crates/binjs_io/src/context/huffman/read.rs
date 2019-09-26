//! Huffman tables for reading.
//!
//! These tables are designed to aid decoding from sequences of bits
//! into values.

use context::huffman::codebook::*;
use context::huffman::*;

use std::convert::{TryFrom, TryInto};

/// A Huffman table.
///
/// We have several implementations of HuffmanTable designed for
/// distinct space/speed tradeoffs.
pub trait HuffmanTable<T>
where
    T: Clone,
{
    /// Return the number of elements in the table.
    fn len(&self) -> usize;

    /// Return bit length of the table with most elements.
    fn highest_bit_len(&self) -> BitLen;

    /// Lookup a value from a sequence of bits.
    ///
    /// The sequence of bits MUST be at least as long as `highest_bit_len`.
    /// Use the `Key` result to determine how many bits need to actually be
    /// consumed from the bit stream.
    fn lookup(&self, key: &BitSequence) -> Option<Cow<(T, Key)>>;
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
    /// Construct a Huffman table from a Codebook.
    ///
    /// Time complexity: `O(2^codebook.max_bit_len())`.
    pub fn from_codebook(codebook: Codebook<T>) -> Self {
        assert!(
            codebook.len()
                <= V::max_value()
                    .try_into()
                    .unwrap_or_else(|_| panic!("Too many keys for ValueIndex"))
        );
        let highest_bit_len = codebook.highest_bit_len();

        let mut values = Vec::with_capacity(codebook.len());

        // Fill `saturated` with a default value of `V::max_value()`.
        // This is the value most likely to trigger errors in case
        // we have a bug in the implementation of `SingleLookupHuffmanTable`
        // or if the data provided is inconsistent.
        let mut saturated = Vec::with_capacity(1usize << highest_bit_len);
        saturated.resize(1usize << highest_bit_len, V::max_value());

        for (value_index, (value, key)) in codebook.into_iter().enumerate() {
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
    T: Clone,
{
    /// Constant time length access.
    fn len(&self) -> usize {
        self.values.len()
    }

    /// Constant time highest bit access.
    fn highest_bit_len(&self) -> BitLen {
        self.highest_bit_len
    }

    /// Constant-time lookup.
    fn lookup(&self, key: &BitSequence) -> Option<Cow<(T, Key)>> {
        assert!(key.bit_len() >= self.highest_bit_len());
        let (prefix, _) = key.split_bits(self.highest_bit_len());
        let value_index = self.saturated[prefix as usize].clone();
        let value_index: usize = value_index
            .try_into()
            .unwrap_or_else(|_| panic!("Value index does not fit into a usize"));
        let entry = self.values.get(value_index)?;
        Some(Cow::Borrowed(entry))
    }
}

/// An alias for `SingleLookupHuffmanTable::from_codebook`, meant mainly to be used in
/// `MultiLookupHuffmanTable::from_codebook`.
impl<T, V> From<Codebook<T>> for SingleLookupHuffmanTable<T, V>
where
    V: ValueIndex,
{
    fn from(codebook: Codebook<T>) -> Self {
        Self::from_codebook(codebook)
    }
}

/// A table designed to support fast lookup in large sets of data.
/// In most cases, lookup will be slower than a `SingleLookupHuffmanTable`
/// but, particularly in heavily unbalanced trees, the table will
/// take ~2^prefix_len fewer internal entries than a `SingleLookupHuffmanTable`.
///
/// Typically, use this table whenever codes range between 10 and 20 bits.
///
/// # Time complexity
///
/// Assuming that lookups in `Subtable` take constant time, a lookup in `MultiLookupHuffmanTable`
/// will also take constant time:
///
/// - a constant-time lookup to determine into which Subtable to perform the lookup;
/// - a constant-time lookup into Subtable;
/// - a final constant-time lookup to extract the result. // FIXME: We could get rid of this final lookup.
///
///
/// # Space complexity
///
/// TBD. Highly dependent on the shape of the Huffman Tree.
///
///
/// # Algorithm
///
/// Consider the following Huffman table
///
/// Symbol | Binary Code  | Bit Length
/// ------ | ------------ | ----------
/// A      | 11000        | 5
/// B      | 11001        | 5
/// C      | 1101         | 4
/// D      | 100          | 3
/// E      | 101          | 3
/// F      | 111          | 3
/// G      | 00           | 2
/// H      | 01           | 2
///
/// With a prefix length of 3, we will precompute all possible 3-bit prefixes
/// and split the table across such prefixes.
///
/// Prefix | Int Value of Prefix | Symbols   | Max bit length
/// ------ | ------------------- | --------- | --------------
/// 000    | 0                   | G         | 0
/// 001    | 1                   | G         | 0
/// 010    | 2                   | H         | 0
/// 011    | 3                   | H         | 0
/// 100    | 4                   | D         | 0
/// 101    | 5                   | E         | 0
/// 110    | 6                   | A, B, C   | 2
/// 111    | 7                   | F         | 0
///
/// For each prefix, we build the table containing the Symbols,
/// stripping prefix from the Binary Code.
///
/// - Prefix 000
///
/// Symbol | Binary Code | Bit Length
/// ------ | ----------- | ----------
/// G      | (none)      | 0
///
/// - Prefix 001
///
/// Symbol | Binary Code | Bit Length
/// ------ | ----------- | ----------
/// G      | (none)      | 0
///
/// - Prefix 010
///
/// Symbol | Binary Code | Bit Length
/// ------ | ----------- | ----------
/// H      | (none)      | 0
///
/// - Prefix 11
///
/// Symbol | Binary Code | Bit Length
/// ------ | ----------- | ----------
/// H      | (none)      | 0
///
/// - Prefix 100
///
/// Symbol | Binary Code | Bit Length
/// ------ | ----------- | ----------
/// D      | (none)      | 0
///
/// - Prefix 101
///
/// Symbol | Binary Code | Bit Length
/// ------ | ----------- | ----------
/// E      | (none)      | 0
///
/// - Prefix 110
///
/// Symbol | Binary Code | Bit Length
/// ------ | ----------- | ----------
/// A      | 00          | 2
/// B      | 01          | 2
/// C      | 1           | 1
///
/// - Prefix 111
///
/// Symbol | Binary Code | Bit Length
/// ------ | ----------- | ----------
/// F      | (none)      | 0
///
/// With this transformation, we have represented one table
/// with an initial max bit length of 5 as:
///
/// - 1 table with a max bit length of 2;
/// - 7 tables with a max bit length of 0.
///
/// Consequently, instead of storing 2^5 = 32 internal references,
/// as we would have done with a SingleLookupHuffmanTable, we only
/// need to store (assuming that `SubTable` is a `SingleLookupHuffmanTable`):
///
/// - 7 subtables with 1 reference each;
/// - 1 subtable with 2^2 = 4 references.
pub struct MultiLookupHuffmanTable<T, SubTable> {
    /// The highest bit length.
    highest_bit_len: BitLen,

    /// Invariant: `prefix_len < highest_bit_len`.
    prefix_len: BitLen,

    /// A mapping from 0..2^prefix_len such that index `i`
    /// maps to a subtable that holds all values associated
    /// with a key that starts with `Key::new(i, prefix_len)`.
    ///
    /// Note that, to allow the use of smaller tables, keys
    /// inside the subtables have been stripped
    /// from the prefix `Key::new(i, prefix_len)`.
    by_prefix: Vec<SubTable>,

    /// The number of entries in this table.
    len: usize,

    values: Vec<(T, Key)>,
}

impl<T, SubTable> MultiLookupHuffmanTable<T, SubTable>
where
    SubTable: HuffmanTable<usize> + From<Codebook<usize>>,
    T: Clone,
{
    pub fn from_codebook(prefix_len: BitLen, codebook: Codebook<T>) -> Self {
        let len = codebook.len();
        let mut values = Vec::with_capacity(codebook.len());
        let highest_bit_len = codebook.highest_bit_len();

        // At this stage, we cannot immediately create subtables, as
        // we first need to determine the `highest_bit_len`. So we
        // first need to split our Codebook into a forest of Codebooks
        // sharing the same prefix.
        let mut buckets = Vec::with_capacity(1usize << prefix_len);
        buckets.resize_with(1usize << prefix_len, || Codebook::new());

        // Dispatch each (value, key) to its buckets.
        for (value, key) in codebook.into_iter() {
            let (prefix, suffix) = key.as_bit_sequence().split(prefix_len);
            for index in prefix.suffixes(prefix_len) {
                let ref mut bucket = buckets[index as usize];
                // Store the new mapping:
                // - in the smaller Codebook, we only need the remaining bits (`suffix`);
                // - in the smaller Codebook, we don't use the `value` itself but rather
                //   a reference to value stored in `values`.
                unsafe {
                    bucket.add_mapping(values.len(), Key::from_bit_sequence(suffix.clone()));
                }
            }
            values.push((value, key));
        }

        // Now convert buckets into Huffman tables
        let mut by_prefix = Vec::with_capacity(1usize << prefix_len);
        for bucket in buckets {
            by_prefix.push(SubTable::from(bucket));
        }

        Self {
            highest_bit_len,
            prefix_len,
            by_prefix,
            len,
            values,
        }
    }
}

impl<T, SubTable> HuffmanTable<T> for MultiLookupHuffmanTable<T, SubTable>
where
    SubTable: HuffmanTable<usize>,
    T: Clone,
{
    /// Constant-time length.
    fn len(&self) -> usize {
        self.len
    }

    /// Constant time highest bit length.
    fn highest_bit_len(&self) -> BitLen {
        self.highest_bit_len
    }

    /// Constant-time lookup.
    fn lookup(&self, key: &BitSequence) -> Option<Cow<(T, Key)>>
    where
        T: Clone,
    {
        assert!(key.bit_len() >= self.highest_bit_len());

        // Find in which `SingleLookupHuffmanTable` to look for the entry.
        let (prefix, suffix) = key.split_bits(self.prefix_len);
        let ref table = self.by_prefix.get(prefix as usize)?;

        // Now lookup in second table.
        let suffix = BitSequence::new(suffix, key.bit_len() - self.prefix_len);
        let suffix = suffix.pad_lowest_to(table.highest_bit_len());
        let lookup = table.lookup(&suffix)?;

        // Finally, build the result.
        Some(Cow::Borrowed(&self.values[lookup.0]))
    }
}

#[test]
fn test_huffman_lookup() {
    // Check against a hardcoded constant, to ensure consistency
    // with fbssdc implementation.

    fn run_test<H, F>(from_codebook: F)
    where
        F: Fn(Codebook<char>) -> H,
        H: HuffmanTable<char>,
    {
        let sample = "appl";
        let codebook = Codebook::from_sequence(sample.chars(), BitLen::new(std::u8::MAX)).unwrap();
        let table = from_codebook(codebook);

        assert_eq!(table.len(), 3);

        // Test with all possible 2 bit sequences.
        let candidate = BitSequence::new(0b10, BitLen(2));
        let result = table.lookup(&candidate).unwrap();
        assert_eq!(result.as_ref(), &('a', Key::new(0b10, BitLen(2))));

        let candidate = BitSequence::new(0b11, BitLen(2));
        let result = table.lookup(&candidate).unwrap();
        assert_eq!(result.as_ref(), &('l', Key::new(0b11, BitLen(2))));

        // With a bit length of 2, there are two keys that
        // should return 'p'
        for prefix in &[0b00, 0b01] {
            let candidate = BitSequence::new(*prefix, BitLen(2));
            let result = table.lookup(&candidate).unwrap();
            assert_eq!(result.as_ref(), &('p', Key::new(0, BitLen(1))));
        }

        // Test values with all possible 3 bit sequences.
        for prefix in &[0b100, 0b101] {
            let candidate = BitSequence::new(*prefix, BitLen(3));
            let result = table.lookup(&candidate).unwrap();
            assert_eq!(result.as_ref(), &('a', Key::new(0b10, BitLen(2))));
        }

        for prefix in &[0b110, 0b111] {
            let candidate = BitSequence::new(*prefix, BitLen(3));
            let result = table.lookup(&candidate).unwrap();
            assert_eq!(result.as_ref(), &('l', Key::new(0b11, BitLen(2))));
        }

        for prefix in &[0b000, 0b001, 0b010, 0b011] {
            let candidate = BitSequence::new(*prefix, BitLen(3));
            let result = table.lookup(&candidate).unwrap();
            assert_eq!(result.as_ref(), &('p', Key::new(0, BitLen(1))));
        }
    }

    run_test::<SingleLookupHuffmanTable<char, u8>, _>(SingleLookupHuffmanTable::from_codebook);
    run_test::<SingleLookupHuffmanTable<char, u32>, _>(SingleLookupHuffmanTable::from_codebook);
    run_test::<SingleLookupHuffmanTable<char, usize>, _>(SingleLookupHuffmanTable::from_codebook);
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, u8>>, _>(|codebook| {
        MultiLookupHuffmanTable::from_codebook(BitLen(1), codebook)
    });
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, u32>>, _>(
        |codebook| MultiLookupHuffmanTable::from_codebook(BitLen(1), codebook),
    );
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, usize>>, _>(
        |codebook| MultiLookupHuffmanTable::from_codebook(BitLen(1), codebook),
    );
}

#[test]
fn test_huffman_lookup_2() {
    // Check internal consistency.

    fn run_test<H, F>(from_codebook: F)
    where
        F: Fn(Codebook<char>) -> H,
        H: HuffmanTable<char>,
    {
        let sample = "Lorem ipsum dolor sit amet consectetur adipiscing elit convallis nostra, integer diam odio mus eros ut sodales sociis cursus, montes imperdiet morbi rhoncus felis venenatis curabitur magna. Volutpat tincidunt sociosqu pharetra id feugiat enim eget, integer quisque magna in senectus mollis, himenaeos malesuada convallis faucibus ornare egestas. Netus platea himenaeos suscipit nostra montes mattis, lobortis ut arcu facilisi hac ornare, integer ante sociosqu placerat morbi.

Viverra arcu dapibus nam magna a imperdiet inceptos cubilia libero lobortis praesent habitasse, tortor id leo consequat sollicitudin elementum fames fringilla himenaeos donec. Phasellus posuere congue ultricies scelerisque senectus vivamus facilisi, vestibulum consequat aptent lectus ad sociis porta, purus libero eros leo at nec. Netus viverra urna nisl sapien conubia porta sed luctus penatibus cras, pulvinar iaculis sagittis fusce fringilla et rutrum sollicitudin ligula, dui vestibulum interdum pretium montes diam nibh inceptos ante.
";
        let codebook = Codebook::from_sequence(sample.chars(), BitLen::new(std::u8::MAX)).unwrap();
        let table = from_codebook(codebook.clone());
        for (value, key) in codebook {
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
    // Test with a single lookup.
    run_test::<SingleLookupHuffmanTable<char, u8>, _>(SingleLookupHuffmanTable::from_codebook);
    run_test::<SingleLookupHuffmanTable<char, u32>, _>(SingleLookupHuffmanTable::from_codebook);
    run_test::<SingleLookupHuffmanTable<char, usize>, _>(SingleLookupHuffmanTable::from_codebook);

    // Test with two lookups, with a very short prefix length.
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, u8>>, _>(|codebook| {
        MultiLookupHuffmanTable::from_codebook(BitLen(1), codebook)
    });
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, u32>>, _>(
        |codebook| MultiLookupHuffmanTable::from_codebook(BitLen(1), codebook),
    );
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, usize>>, _>(
        |codebook| MultiLookupHuffmanTable::from_codebook(BitLen(1), codebook),
    );

    // Test with two lookups, still with a very short prefix length.
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, u8>>, _>(|codebook| {
        MultiLookupHuffmanTable::from_codebook(BitLen(2), codebook)
    });
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, u32>>, _>(
        |codebook| MultiLookupHuffmanTable::from_codebook(BitLen(2), codebook),
    );
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, usize>>, _>(
        |codebook| MultiLookupHuffmanTable::from_codebook(BitLen(2), codebook),
    );

    // Test with two lookups, with an unreasonably large prefix length.
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, u8>>, _>(|codebook| {
        MultiLookupHuffmanTable::from_codebook(BitLen(10), codebook)
    });
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, u32>>, _>(
        |codebook| MultiLookupHuffmanTable::from_codebook(BitLen(10), codebook),
    );
    run_test::<MultiLookupHuffmanTable<char, SingleLookupHuffmanTable<usize, usize>>, _>(
        |codebook| MultiLookupHuffmanTable::from_codebook(BitLen(10), codebook),
    );
}
