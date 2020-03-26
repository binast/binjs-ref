use context::huffman::*;
use context::varnum::{ReadVaru32, WriteVaru32};

use std::io::{self, Read, Write};

const TABLE_HEADER_UNIT: u8 = 0;
const TABLE_HEADER_MULTI: u8 = 1;
const TABLE_HEADER_EMPTY: u8 = 2;

const VEC_MAX_PRE_ALLOC: usize = 1024;

/// Codebook associated to a sequence of values.
#[derive(Clone, Debug, PartialEq, Eq)]
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
    pub fn from_sequence<S>(source: S, max_bit_len: BitLen) -> Result<Self, io::Error>
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
    pub fn from_instances<S>(source: S, max_bit_len: BitLen) -> Result<Self, io::Error>
    where
        S: IntoIterator<Item = (T, Instances)>,
    {
        let bit_lengths = Self::compute_bit_lengths(source, max_bit_len)?;
        Self::from_bit_lens(bit_lengths, max_bit_len)
    }

    /// Compute a `Codebook` from a sequence of values
    /// with a bit length already attached.
    ///
    /// The current implementation only attempts to produce the best compression
    /// level. This may cause us to exceed `max_bit_len` even though an
    /// alternative table, with a lower compression level, would let us
    /// proceed without exceeding `max_bit_len`.
    ///
    /// # Requirement
    ///
    /// Values of `T` in the source MUST be distinct.
    pub fn from_bit_lens(
        mut bit_lens: Vec<(T, BitLen)>,
        max_bit_len: BitLen,
    ) -> Result<Self, io::Error> {
        let mut highest_bit_len = BitLen(0);

        // Canonicalize order: (BitLen, T)
        bit_lens.sort_unstable_by_key(|&(ref value, ref bit_len)| (*bit_len, value.clone()));

        // The bits associated to the next value.
        let mut bits = 0;
        let mut mappings = Vec::with_capacity(bit_lens.len());

        for i in 0..bit_lens.len() - 1 {
            let (bit_len, symbol, next_bit_len) =
                (bit_lens[i].1, bit_lens[i].0.clone(), bit_lens[i + 1].1);
            mappings.push((symbol.clone(), Key::try_new(bits, bit_len)?));
            bits = (bits + 1) << (next_bit_len - bit_len);
            if bit_len > highest_bit_len {
                highest_bit_len = bit_len;
            }
        }
        // Handle the last element.
        let (ref symbol, bit_len) = bit_lens[bit_lens.len() - 1];
        if bit_len > highest_bit_len {
            highest_bit_len = bit_len;
        }
        mappings.push((symbol.clone(), Key::new(bits, bit_len)));

        if highest_bit_len > max_bit_len {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Could not create a codebook that fits into this bit length",
            ));
        }

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
    pub fn compute_bit_lengths<S>(
        source: S,
        max_bit_len: BitLen,
    ) -> Result<Vec<(T, BitLen)>, std::io::Error>
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
            max_bit_len: BitLen,
            depth: u8,
            node: &NodeContent<T>,
        ) -> Result<(), std::io::Error>
        where
            T: Clone,
        {
            match *node {
                NodeContent::Leaf(ref value) => {
                    if depth > max_bit_len.as_u8() {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidInput,
                            "Could not create a codebook that fits into this bit length",
                        ));
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
    let try_make_codebook = |bit_len| Codebook::from_sequence(sample.chars(), bit_len);
    let coded = try_make_codebook(BitLen::new(std::u8::MAX)).unwrap();

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
    assert!(try_make_codebook(BitLen::new(1)).is_err());
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

    /// Iterate through this Codebook.
    pub fn iter(&self) -> impl Iterator<Item = &(T, Key)> {
        self.mappings.iter()
    }
}

/// Writing
impl<T> Codebook<T>
where
    T: Ord + Clone + Hash,
{
    /// Write a Codebook for `StaticAlphabet`.
    fn write_static<A, W>(&self, mut out: W) -> Result<(), io::Error>
    where
        A: StaticAlphabet<Symbol = T>,
        W: Write,
    {
        match self.len() {
            0 => {
                /* spec: EmptyCodeTable */
                out.write_all(&[TABLE_HEADER_EMPTY])?;
                Ok(())
            }
            1 => {
                /* spec: UnitCodeTable */
                out.write_all(&[TABLE_HEADER_UNIT])?;
                A::write_literal(&self.mappings[0].0, out)?;
                Ok(())
            }
            _ => {
                /* spec: MultiCodeTableImplicit */
                out.write_all(&[TABLE_HEADER_MULTI])?;
                let map: HashMap<_, _> = self.mappings.iter().cloned().collect();
                for i in 0..A::len() {
                    let symbol = A::symbol(i).unwrap(); // We're in 0..A::len()
                    let bit_len = map
                        .get(&symbol)
                        .map(|key| key.bit_len().clone())
                        .unwrap_or(BitLen::new(0));
                    out.write_all(&[bit_len.as_u8()])?;
                }
                Ok(())
            }
        }
    }

    /// Write a Codebook for `DynamicAlphabet`.
    fn write_dynamic<A, W>(&self, mut out: W) -> Result<(), io::Error>
    where
        A: DynamicAlphabet<Symbol = T>,
        W: Write,
    {
        match self.len() {
            0 => {
                /* spec: EmptyCodeTable */
                out.write_all(&[TABLE_HEADER_EMPTY])?;
                Ok(())
            }
            1 => {
                /* spec: UnitCodeTable */
                out.write_all(&[TABLE_HEADER_UNIT])?;
                A::write_literal(&self.mappings[0].0, out)?;
                Ok(())
            }
            _ => {
                /* spec: MultiCodeTableExplicit */

                // First the header.
                out.write_all(&[TABLE_HEADER_MULTI])?;

                // Now, the length.
                out.write_varu32(self.len() as u32)?;

                // Then bit lengths.
                for &(_, ref key) in &self.mappings {
                    out.write_all(&[key.bit_len().as_u8()])?;
                }
                self.mappings.len();

                // Then symbols.
                for &(ref symbol, _) in &self.mappings {
                    A::write_literal(symbol, &mut out)?;
                }

                Ok(())
            }
        }
    }
}

/// Reading
impl<T> Codebook<T>
where
    T: Ord + Clone,
{
    /// Parse a Codebook containing a single symbol.
    fn read_single_symbol<A, R>(mut inp: R) -> Result<Self, io::Error>
    where
        A: Alphabet<Symbol = T>,
        R: Read,
    {
        let symbol = A::read_literal(&mut inp)?;
        Codebook::from_bit_lens(vec![(symbol, BitLen::new(0))], MAX_CODE_BIT_LEN).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "Could not derive a Codebook that does not exceed MAX_CODE_BIT_LEN",
            )
        })
    }

    /// Parse a Codebook for `StaticAlphabet`.
    pub fn read_static<A, R>(mut inp: R) -> Result<Self, io::Error>
    where
        A: StaticAlphabet<Symbol = T>,
        R: Read,
    {
        let mut byte = [0];
        inp.read_exact(&mut byte)?;
        match byte[0] {
            0 =>
            /* spec: UnitCodeTable */
            {
                Self::read_single_symbol::<A, R>(inp)
            }
            1 =>
            /* spec: MultiCodeTableImplicit */
            {
                let number_of_symbols = A::len();
                let mut bit_lens =
                    Vec::with_capacity(usize::min(number_of_symbols as usize, VEC_MAX_PRE_ALLOC));
                for i in 0..number_of_symbols {
                    // Read the bit length.
                    let mut byte = [0];
                    inp.read_exact(&mut byte)?;
                    let bit_len = BitLen::new(byte[0]);

                    if bit_len > BitLen::new(0) {
                        // Extract the symbol from the grammar.
                        let symbol = A::symbol(i).unwrap(); // We're within 0..A::len()

                        bit_lens.push((symbol, bit_len));
                    }
                }
                // Finally, build a codebook.
                Codebook::from_bit_lens(bit_lens, MAX_CODE_BIT_LEN).map_err(|_| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Could not derive a Codebook that does not exceed MAX_CODE_BIT_LEN",
                    )
                })
            }
            2 =>
            /* spec: EmptyCodeTable */
            {
                Ok(Codebook::new())
            }
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Incorrect CodeTable kind",
            )),
        }
    }

    /// Parse a Codebook for `DynamicAlphabet`.
    pub fn read_dynamic<A, R>(mut inp: R) -> Result<Self, io::Error>
    where
        A: DynamicAlphabet<Symbol = T>,
        R: Read,
        T: Default,
    {
        let mut byte = [0];
        inp.read_exact(&mut byte)?;
        match byte[0] {
            0 =>
            /* spec: UnitCodeTable */
            {
                Self::read_single_symbol::<A, R>(inp)
            }
            1 =>
            /* spec: MultiCodeTableExplicit */
            {
                let number_of_symbols = *inp.read_varu32_no_normalization()?.value();
                let mut bit_lens =
                    Vec::with_capacity(usize::min(number_of_symbols as usize, VEC_MAX_PRE_ALLOC));

                // Read bit lengths.
                for _ in 0..number_of_symbols {
                    let mut byte = [0];
                    inp.read_exact(&mut byte)?;
                    bit_lens.push((T::default(), BitLen::new(byte[0])));
                }

                // Amend with symbols
                for i in 0..number_of_symbols {
                    let symbol = A::read_literal(&mut inp)?;
                    bit_lens[i as usize].0 = symbol;
                }

                // Finally, build a codebook.
                Codebook::from_bit_lens(bit_lens, MAX_CODE_BIT_LEN).map_err(|_| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Could not derive a Codebook that does not exceed MAX_CODE_BIT_LEN",
                    )
                })
            }
            2 =>
            /* spec: EmptyCodeTable */
            {
                Ok(Codebook::new())
            }
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Incorrect CodeTable kind",
            )),
        }
    }
}

#[test]
fn read_and_write_codebook() {
    use std::convert::TryInto;
    use std::io;

    for sample in &[
        "appl",
        "Lorem ipsum dolor sit amet consectetur adipiscing elit nunc, ridiculus hac natoque ante quisque imperdiet primis et euismod, pellentesque per turpis purus vestibulum quam dui. Himenaeos inceptos hac laoreet purus eros donec imperdiet, aliquam habitant felis class fusce etiam nulla facilisi, pretium eu nisl ultrices augue dictum. Venenatis mauris semper ultricies platea interdum sapien iaculis, habitasse eget habitant nec nam tincidunt, nulla aptent arcu duis laoreet volutpat.

Torquent facilisi vestibulum erat eleifend diam convallis ac at, feugiat nullam vulputate euismod lacinia mollis quis venenatis, gravida porttitor cursus nascetur lacus per nostra. Platea ante curae netus torquent diam ultrices massa orci, vulputate sociis curabitur himenaeos litora sed aliquam nisi rutrum, cras porttitor per etiam iaculis eget arcu. Varius turpis libero metus luctus senectus condimentum cum mattis arcu, faucibus volutpat dapibus torquent ultrices fusce primis morbi, sed augue ridiculus magnis vitae placerat tempus curabitur.

Aliquam habitant eu curae est eget orci auctor, non vehicula augue montes litora ac, class quis cum volutpat condimentum ullamcorper. Quisque consequat est vehicula volutpat at proin gravida sociosqu, nec dis ac ultricies phasellus viverra donec nullam, eros potenti facilisis mauris ad curabitur quis. Magna nisl ligula tellus conubia accumsan fringilla iaculis inceptos leo litora, eget integer malesuada rhoncus varius a tempor augue. Posuere nullam parturient eleifend quisque ornare vulputate curae ultricies iaculis est, odio scelerisque hendrerit non primis ut leo ante libero, nisi eu quam euismod habitant velit per lectus cubilia.

Blandit quisque urna proin nostra praesent dui, magnis sollicitudin auctor ultrices platea sociis habitant, ut faucibus habitasse luctus elementum. Hendrerit elementum rutrum in erat nulla facilisi mauris torquent mus, diam consequat pulvinar tempor sociosqu conubia ornare ante, vehicula litora scelerisque magna placerat eleifend sapien risus. Pellentesque curabitur parturient per facilisi rhoncus porta posuere enim hendrerit, lacus litora aptent etiam vel id ante rutrum donec, platea gravida integer urna tristique est potenti class. Mus ante ut cursus in lacinia, sollicitudin posuere inceptos ullamcorper a, nam cras mi venenatis.

Arcu magna lacus habitant eleifend cursus vitae, fermentum diam scelerisque nisi habitasse, conubia felis quis suscipit facilisis. Sociosqu erat lectus etiam aliquam quis vulputate praesent pharetra cras nam fermentum ultricies, nunc parturient fames imperdiet sem posuere molestie mi felis suscipit. Tortor etiam ligula leo nunc senectus sem pharetra, viverra suscipit egestas cum eu ullamcorper netus accumsan, eleifend porttitor sed lectus varius integer. Sem nascetur ligula ultrices risus eros nisl quisque, pulvinar lacinia sagittis magna primis odio dictum, metus a curabitur ante taciti inceptos.

Mollis laoreet sollicitudin augue tortor facilisis cubilia molestie auctor erat sociis, condimentum parturient vestibulum lacinia urna potenti nascetur vehicula varius tempor mattis, velit maecenas tristique a habitant et porttitor tempus netus. Habitant interdum penatibus litora himenaeos dignissim torquent quam nulla, praesent elementum ad potenti accumsan class urna malesuada ut, aliquet aliquam egestas venenatis leo eu rhoncus pellentesque, augue ultricies posuere fames nullam aenean pretium. Tristique penatibus neque leo dignissim vulputate bibendum rhoncus pharetra, sem rutrum vehicula mauris lobortis proin platea, viverra metus natoque accumsan hendrerit posuere nunc. Aliquam nam porttitor leo tortor vel tempor nulla non, sollicitudin habitasse ornare magnis feugiat metus viverra quisque libero, risus eget enim orci torquent aptent molestie.

Dignissim laoreet quis ligula non auctor id pellentesque justo, varius platea eget convallis dictum dui faucibus nec porttitor, porta praesent eu ante in rhoncus congue. Massa etiam eget vel torquent dis potenti accumsan ultrices, pulvinar et cursus cubilia maecenas diam himenaeos nunc blandit, semper vulputate turpis at scelerisque porttitor primis. Nam odio venenatis maecenas at tortor viverra metus, turpis suscipit ad facilisis elementum primis felis luctus, tempor curabitur suspendisse lobortis nunc ligula.

Ante aliquet ultricies est lobortis a sollicitudin urna parturient eu, nec massa cursus mollis sagittis id risus accumsan condimentum, nisl platea habitant aenean eros leo fringilla blandit. Mi semper convallis posuere dictum integer torquent suspendisse, in rhoncus nulla himenaeos sociosqu cras praesent quam, nostra turpis scelerisque tempor facilisi velit. Mauris nec ut risus imperdiet varius venenatis quam ligula, luctus cursus velit scelerisque ullamcorper ultrices sociis viverra, vulputate lectus volutpat sodales nostra tincidunt suspendisse. Senectus fermentum bibendum a tristique sed sociosqu potenti, lectus ante egestas ac consequat donec eros, penatibus enim ridiculus luctus cursus malesuada.

Dui dictum dignissim dis ultricies justo donec nisi, cum quisque rhoncus aliquam interdum iaculis dapibus, fringilla phasellus accumsan eget odio inceptos. Placerat laoreet iaculis nullam enim praesent diam semper porta montes, nisi commodo tempus rutrum nostra in himenaeos cum primis mollis, auctor congue venenatis a sed sollicitudin pulvinar ad. Duis faucibus penatibus mauris turpis tempus suscipit, litora habitasse ultricies potenti auctor, semper in ac placerat sollicitudin.

Turpis at taciti lacus aenean cum, donec facilisi diam neque, pellentesque mattis sem auctor. Duis donec maecenas consequat nullam a fusce cubilia malesuada, hendrerit ad porttitor ac neque netus dictum, felis suscipit est nisl parturient porta elementum. Ullamcorper tempor porttitor quis integer nullam proin taciti facilisis eget dui habitasse, nisl ad erat placerat curae dictum litora lectus urna facilisi, varius tincidunt nam enim lacus tellus est suspendisse porta cum.

Conubia a rhoncus metus felis nullam dictumst tempus dignissim, egestas neque pulvinar tincidunt feugiat congue suscipit elementum, hac sociosqu fringilla nunc bibendum magna curae. Netus massa suspendisse tellus sapien a montes, metus varius aenean mauris tempus dis fames, tincidunt eu vulputate quis pulvinar. Lobortis curabitur molestie tortor aliquam posuere magnis consequat, tellus suspendisse purus pretium ultricies nibh fermentum, potenti odio egestas tempus varius id.

Placerat fames proin suspendisse porta posuere quam orci senectus integer sed, nostra diam elementum phasellus vulputate dictum litora accumsan platea, sociosqu morbi dictumst nascetur parturient lacinia cubilia blandit pretium. Felis nostra natoque facilisis taciti diam nam netus est malesuada, tellus accumsan montes arcu lacinia et dictum rhoncus commodo, cum purus dui maecenas egestas sollicitudin eu risus. Augue ullamcorper penatibus at curae urna hac habitant suspendisse fringilla platea, fames sed fermentum sociis etiam sapien ac dictum maecenas cras, volutpat nullam tempus ornare leo ultricies lobortis mus arcu. Velit consequat fermentum facilisis eleifend vestibulum ullamcorper platea mi faucibus potenti sagittis nisl, himenaeos volutpat pellentesque nascetur gravida tempus interdum enim tristique sed curabitur mollis, commodo magnis facilisi tempor ultrices vehicula vel nisi metus iaculis varius.

Rhoncus aliquet fermentum imperdiet senectus porttitor vulputate pharetra tortor, feugiat suscipit proin magnis cubilia primis magna urna, blandit facilisis cum aenean purus curabitur platea. Pharetra dis vivamus cursus proin hendrerit faucibus himenaeos praesent, mus facilisi sodales sed curabitur scelerisque aliquam, aenean velit platea mollis ultrices integer tincidunt. Montes vivamus phasellus tempus tellus a fermentum habitant hendrerit parturient ligula mollis et, varius dapibus sed cras nam libero blandit eu vestibulum laoreet nunc, porttitor ut pretium curae dictum id justo erat nisl nisi integer.

Ultrices iaculis per netus odio condimentum molestie penatibus nibh, ultricies faucibus cras sagittis neque ante pulvinar, justo ad ullamcorper at malesuada tellus nisl. Porttitor lacinia vestibulum ut condimentum donec, blandit ullamcorper euismod fringilla pharetra id, natoque lectus pretium vel. Sodales elementum sed est himenaeos ligula luctus porta montes cum, integer eu vivamus volutpat viverra pulvinar orci faucibus nostra, maecenas neque magnis dis nulla habitant metus velit. Urna quam a enim scelerisque pretium taciti vestibulum quisque dignissim, suspendisse nisl habitasse turpis accumsan nec pellentesque inceptos, tempor aptent ad sollicitudin velit praesent porttitor facilisis.

Lacus dui velit mus ut cursus ridiculus montes, id vehicula vivamus taciti egestas urna vulputate, rutrum dapibus aptent non ullamcorper aliquet. Eget erat dictum montes facilisis sodales nascetur ante quisque, mattis venenatis penatibus senectus ultricies praesent himenaeos, aliquam porttitor accumsan diam quis platea et. Habitasse donec parturient lectus vehicula non magnis quis et ante netus, natoque proin lacus posuere commodo nisl eget placerat sed aenean, imperdiet lobortis volutpat massa cubilia curae metus nisi blandit. Viverra tortor suspendisse aenean nisl pretium augue, parturient vestibulum dignissim tristique quis, neque ultricies ad quisque lacinia.

Condimentum nisl mus pulvinar semper metus placerat habitasse commodo aptent, fermentum eros mollis inceptos venenatis ut natoque id hac magna, per ornare penatibus conubia tellus sed erat mi. Etiam felis enim inceptos libero facilisis dis litora imperdiet cursus netus, sapien accumsan in turpis facilisi fermentum mus dictumst fames, bibendum aptent metus habitasse tempus condimentum ante augue volutpat. Pulvinar inceptos sociis elementum blandit facilisi natoque eu, mollis neque lacus aliquet tristique massa habitasse, mus praesent vestibulum augue porta nisl. Quisque porta vestibulum sociis ad vulputate felis conubia lacus enim, sociosqu libero luctus condimentum nibh parturient et lobortis, egestas mauris proin tempus montes pulvinar senectus dictum.

Pharetra habitasse praesent tristique taciti dignissim nullam faucibus mus at, curabitur inceptos libero accumsan facilisis tempus duis mi ut, massa magnis vitae metus est magna placerat nam, convallis aliquet sed auctor ullamcorper gravida rhoncus aptent. Platea aenean sagittis per fringilla mollis auctor rhoncus, blandit magna aptent egestas himenaeos tincidunt malesuada eget, luctus ad massa vulputate sapien pulvinar. Senectus scelerisque gravida viverra morbi metus augue suspendisse, pulvinar maecenas urna dictum nascetur cursus, sem ultricies curae enim parturient accumsan.

Nunc cubilia fusce ullamcorper senectus vulputate pellentesque natoque ac, taciti tortor nisl torquent quis posuere mus. Vel dignissim nulla imperdiet accumsan aliquet faucibus hendrerit ultricies neque vivamus, tempus feugiat praesent sodales rhoncus taciti congue ad dis velit, orci himenaeos quis hac suscipit litora ornare senectus dui. Inceptos nec condimentum viverra et augue lectus nunc diam, eros dis purus magna nullam ligula ultrices tortor, velit aenean tellus id porttitor faucibus volutpat. Quisque blandit gravida integer sociosqu est accumsan pulvinar, nullam condimentum conubia vulputate cursus netus iaculis, urna a habitant scelerisque aptent torquent. Vulputate himenaeos class malesuada tortor interdum velit potenti quisque risus pharetra, primis cum lectus mi ullamcorper sociosqu consequat posuere nisi, varius eleifend arcu id eget vel nullam etiam blandit.

Quisque vestibulum proin torquent vel dictum convallis ligula placerat suspendisse enim, tristique lobortis sem feugiat libero lacus parturient tempus volutpat, habitasse imperdiet sociosqu mi dapibus scelerisque sollicitudin ullamcorper et. Euismod scelerisque mauris augue lacus porttitor cras ornare penatibus, nascetur egestas placerat platea cubilia varius volutpat duis malesuada, quisque mus ridiculus habitant senectus suscipit morbi. Ultrices leo cras morbi magna curabitur potenti vel mi, non hac varius imperdiet id metus ornare, nullam in quis dapibus torquent eros rhoncus. Nullam dapibus quisque luctus sollicitudin lacus euismod porta pulvinar sapien rutrum est, feugiat mollis nec ridiculus aenean sem tristique massa suspendisse faucibus.

Bibendum tempor congue sed curabitur non quam velit porta, mauris montes mattis mollis sodales vivamus sociosqu tempus, himenaeos penatibus taciti commodo in id maecenas. Vulputate pretium mauris at viverra mus massa vehicula parturient, conubia velit tempus eleifend libero bibendum curabitur in ultricies, hendrerit tincidunt consequat porttitor justo commodo id. Neque congue sociosqu morbi massa libero aliquet purus nibh conubia, venenatis diam mauris justo mollis felis fusce tempus quis, suspendisse gravida blandit viverra bibendum euismod porttitor placerat."
    ] {
        let reference = Codebook::from_sequence(sample.bytes(), BitLen::new(std::u8::MAX)).unwrap();

        struct ByteAlphabet;
        impl Alphabet for ByteAlphabet {
            type Symbol = u8;
            fn read_literal<R>(mut input: R) -> Result<Self::Symbol, io::Error>
            where R: io::Read {
                let mut buf = [0];
                input.read_exact(&mut buf)?;
                Ok(buf[0])
            }
            fn write_literal<W>(symbol: &Self::Symbol, mut output: W) -> Result<(), io::Error>
            where
                W: io::Write
            {
                output.write_all(&[*symbol])?;
                Ok(())
            }
        }

        {
            // Test as a static alphabet.
            impl StaticAlphabet for ByteAlphabet {
                fn len() -> u32 {
                    std::u8::MAX as u32
                }
                fn symbol(index: u32) -> Option<Self::Symbol> {
                    index.try_into().ok()
                }
            }

            // ...write
            let mut buf = vec![];
            reference.write_static::<ByteAlphabet, _>(&mut buf)
                .unwrap();

            // ...read
            let result = Codebook::read_static::<ByteAlphabet, _>(io::Cursor::new(&buf))
                .unwrap();

            assert_eq!(result, reference);
        }

        {
            // Test as a dynamic alphabet.
            impl DynamicAlphabet for ByteAlphabet { }

            // ...write
            let mut buf = vec![];
            reference.write_dynamic::<ByteAlphabet, _>(&mut buf)
                .unwrap();


            // ...read
            let result = Codebook::read_dynamic::<ByteAlphabet, _>(io::Cursor::new(&buf))
                .unwrap();

            assert_eq!(result, reference);
        }
    }
}
