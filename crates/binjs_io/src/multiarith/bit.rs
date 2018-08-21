//! Bit-level implementation of entropy coding.

use multiarith::Segment;

use std;
use std::io::{ Read, Write };

/*
pub struct SymbolEncoder<W> where W: Write {
    passed: u32,
    low: u32,
    range: u32,
    writer: W,
}

impl<W> SymbolEncoder<W> where W: Write {
    pub fn new(writer: W) -> Self {
        Self {
            passed: 0,
            low: 0,
            range: std::u32::MAX,
            writer
        }
    }

    pub fn done(self) -> Result<W, std::io::Error> {
        self.writer.write_all(&[self.low >> 24])?;
        self.low <<= 8;
        self.writer.write_all(&[self.low >> 24])?;
        self.low <<= 8;
        self.writer.write_all(&[self.low >> 24])?;
        self.low <<= 8;
        self.writer.write_all(&[self.low >> 24])?;
        self.low <<= 8;
        Ok(())
    }

    pub fn encode(&mut self, symbol: Segment) -> Result<W, std::io::Error> {
        self.range /= symbol.context_length;
        self.low += ? * self.range;
        self.range *= symbol.length;
    }
}

*/

/// An implementation of range encoding (decoder).
///
/// See https://en.wikipedia.org/wiki/Range_encoding
///
/// This decoder is meant to be driven by an external model.
pub struct SymbolDecoder<R> where R: Read {
    low: u32,
    length: u32,

    /// Bits pending decoding.
    ///
    /// Next bit is always the lowest weight in `buffer`.
    buffer: u8,

    /// Number of bits currently waiting in `buffer`.
    bits_available: u8,
    pending: u32,
    reader: R,
}
impl<R> SymbolDecoder<R> where R: Read {
    pub fn new(reader: R) -> Self {
        Self {
            low: 0,
            length: std::u32::MAX,
            buffer: 0,
            bits_available: 0,
            recentering_bits: 0,
            reader,
        }
    }

    fn pull_bit(&mut self) -> Result<(), std::io::Error> {
        // FIXME: We need a way to find out that we have
        // read too many bits.
        // If necessary, load the next byte.
        if self.bits_available == 0 as u8 {
            debug_assert_eq!(self.buffer, 0);

            let mut buf = [0];
            self.reader.read_exact(&mut buf)?;

            self.bits_available = std::mem::size_of_val(&self.buffer) * 8;
            self.buffer = buf[0];
        }

        // Read next bit.
        self.pending = (self.pending << 1) | (self.buffer & 1);
        self.buffer >>= 1;
        self.bits_available -= 1;

        // Now divide the interval by two.
        self.length /= 2;
        if !bit {
            // First half of the interval.
            // Keep the same value of `self.low`.
        } else {
            // Second half of the interval.
            self.low += self.length;
        }
        Ok(())
    }

    /// Get the next symbol as a representative number to be mached
    /// with a probability segment.
    ///
    /// Pre-requirements:
    ///
    /// - no intersection between the segments of `symbols`.
    /// - all must share the same `context_total`.
    pub fn next(&mut self, symbols: &[Segment]) -> Result<u32, std::io::Error> {
        // FIXME: Handle the case in which `symbols` is empty.

        let result = loop {
            let symbols : Vec<_> = symbols.iter()
                .filter(|segment| {
                    (segment.low <= self.low
                    && segment.low + segment.length >= self.low + self.length)
                })
                .collect();

            match symbols.len() {
                0 => {
                    // We haven't converged yet
                    self.pull_bits()?;
                },
                1 => {
                    // We have converged.
                    break symbols[0].1;
                }
                _ => {
                    unimplemented!("FIXME: Handle error when two segments intersect");;
                }
            }
        };

        // At this stage, we have a result.
        // Zoom in.
        let context_length = result.context_length as u64;
        self.low = self.low + (((self.length as u64 * result.low as u64) / context_length) as u32);
        self.length = ((self.length as u64 * segment.length as u64) / context_length) as u32;
            // Assuming that `segment.length <= context_length`, this is decreasing,
            // so we can't overflow in the `as u32` conversion.

        // Now attempt to workaround finite precision by
        // recovering the first few digits.
        let interval_quarter = 1u32.rotate_right(2);
        let interval_half    = 1u32.rotate_right(1);
        let interval_three_quarters = interval_half + interval_quarter;
        'recover_digits: loop {
            if self.low + self.length < interval_half {
                // We're in the first half of the interval.
                // Do nothing.
                // FIXME: Why?
            } else if self.low >= interval_half {
                // We're in the second half of the interval.
                self.low -= interval_half; // FIXME: Why?
                unimplemented!()
            } else if self.low >= interval_quarter && self.low + self.length < interval_three_quarters {
                self.low -= interval_quarter; // FIXME: Why?
                // Information: we're in the half of the interval around the center.
                unimplemented!()
            } else {
                break 'recover_digits
            }

            self.low = self.low << 1;
            self.length = u32::max(self.length << 1, 1);
        }

        Ok(result)
    }
}

/// An implementation of range encoding (encoder).
///
/// See https://en.wikipedia.org/wiki/Range_encoding
///
/// This encoder is meant to be driven by an external model.
pub struct SymbolEncoder<W> where W: Write {
    low: u32,
    /// Length of the current segment.
    length: u32,
    /// Bits waiting to be written.
    buffer: u8,
    /// Number of bits of `buffer` waiting to be written.
    ///
    /// Invariant: < sizeof(buffer)
    bits_ready: u8,
    recentering_bits: u8,
    writer: W,
}
impl<W> SymbolEncoder<W> where W: Write {
    pub fn new(writer: W) -> Self {
        Self {
            low: 0,
            buffer: 0,
            bits_ready: 0,
            recentering_bits: 0,
            writer,
            length: std::u32::MAX,
        }
    }

    /// Append a new symbol, as represented by its probability segment.
    ///
    /// Note that it may take an unbounded amount of operations between
    /// the call to `append_segment` and the actual write of the symbol
    /// to the underlying writer.
    ///
    /// Consider the extreme case of a succession of symbols of probability 1
    /// (`symbol.low == 0`, `symbol.length == symbol.context_length`). As each
    /// of these symbols has a probability 1, they will be coded as 0 bits,
    /// and will therefore cause no write to the underlying writer.
    ///
    /// If you need to ensure that a symbol is written, you may want to call
    /// `flush_symbols()`.
    pub fn append_segment(&mut self, segment: &Segment) -> Result<(), std::io::Error> {
        debug!(target: "multiarith", "Adding segment {:?}", segment);

        // Update segment.
        assert!(self.length != 0);
        let context_length = segment.context_length as u64;
        self.low = self.low + (((self.length as u64 * segment.low as u64) / context_length) as u32);
        self.length = ((self.length as u64 * segment.length as u64) / context_length) as u32;
            // Assuming that `segment.length <= context_length`, this is decreasing,
            // so we can't overflow in the `as u32` conversion.

        // Since `length` is decreasing, it will eventually become smaller than `u32::max/2`,
        // which means that we have (at least) one bit of information. As soon as this
        // happens, we need to flush the bits and regrow `length`, to ensure that we
        // won't lose precision.

        let interval_quarter = 1u32.rotate_right(2);
        let interval_half    = 1u32.rotate_right(1);
        let interval_three_quarters = interval_half + interval_quarter;

        'one_bit: loop {
            if self.low + self.length < interval_half {
                // Information: we're in the first half of the interval.
                self.append_bit(false)?;
            } else if self.low >= interval_half {
                // Information: we're in the second half of the interval.
                self.append_bit(true)?;
            } else if self.low >= interval_quarter && self.low + self.length < interval_three_quarters {
                // Information: we're in the half of the interval around the center.
                self.low  -= interval_quarter;
                self.recentering_bits += 1;
            } else {
                break 'one_bit
            };
            self.low = self.low << 1;
            self.length = u32::max(self.length << 1, 1);
        }

        Ok(())
    }

    /// Flush all the symbols that haven't been written yet.
    pub fn flush_symbols(&mut self) -> Result<bool, std::io::Error> {
        // Write the digits of `self.low`.
        let high_bit: u32 = 1u32.rotate_right(1);
        let mut wrote = false;
        for _ in 0.. std::mem::size_of_val(&self.low) * 8 {
            let bit = self.low & high_bit != 0;
            wrote |= self.append_bit(bit)?;
            self.low = self.low << 1;
        }
        assert_eq!(self.low, 0);
        self.length = std::u32::MAX;
        wrote |= self.flush_bits_always()?;
        Ok(wrote)
    }

    pub fn flush(&mut self) -> Result<(), std::io::Error> {
        self.flush_symbols()?;
        self.flush_bits_always()?;
        self.writer.flush()?;
        Ok(())
    }

    pub fn done(mut self) -> W {
        if self.length != std::u32::MAX {
            self.flush()
                .expect("Could not flush SymbolEncoder");
        }
        self.writer
    }

    fn append_bit(&mut self, bit: bool) -> Result<bool, std::io::Error> {
        debug!(target: "multiarith", "append_bit with {} bits ready", self.bits_ready);
        assert!((self.bits_ready as usize) < std::mem::size_of_val(&self.buffer) * 8);
        let mut wrote = false;

        self.bits_ready += 1;
        self.buffer = self.buffer * 2 + if bit { 1 } else { 0 };
        wrote |= self.flush_bits_if_necessary()?;
        for _ in 0 .. self.recentering_bits {
            self.bits_ready += 1;
            self.buffer = self.buffer * 2 + if bit { 0 } else { 1 };
            wrote |= self.flush_bits_if_necessary()?;
        }
        self.recentering_bits = 0;
        Ok(wrote)
    }

    fn flush_bits_if_necessary(&mut self) -> Result<bool, std::io::Error> {
        debug!(target: "multiarith", "flush_bits_if_necessary with {} bits ready", self.bits_ready);
        if self.bits_ready as usize == std::mem::size_of_val(&self.buffer) * 8 {
            self.flush_bits_internal()?;
            return Ok(true)
        }
        Ok(false)
    }

    fn flush_bits_always(&mut self) -> Result<bool, std::io::Error> {
        if self.bits_ready == 0 {
            return Ok(false)
        }
        for _ in self.bits_ready..std::mem::size_of_val(&self.buffer) as u8 * 8 {
            self.bits_ready += 1;
            self.buffer *= 2;
        }
        self.flush_bits_internal()?;
        Ok(true)
    }

    /// Flush `self.buffer`.
    ///
    /// Does NOT flush `self.writer`, `self.low` or `self.high`.
    fn flush_bits_internal(&mut self) -> Result<(), std::io::Error> {
        assert!(self.bits_ready as usize == std::mem::size_of_val(&self.buffer) * 8);
        self.writer.write(&[self.buffer])?;
        self.bits_ready = 0;
        self.buffer = 0;
        Ok(())
    }
}
