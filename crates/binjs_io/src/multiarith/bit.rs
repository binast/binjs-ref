//! Arithmetic encoding.

use multiarith::Segment;

use std;
use std::io::Write;

pub struct SymbolEncoder<W> where W: Write {
    low: u32,
    /// Length of the current segment.
    length: u32,
    /// Bits waiting to be written.
    buffer: u8,
    /// Number of bits waiting to be written.
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

        let mut high : u32 = self.low + self.length;
        'one_bit: loop {
            if high < interval_half {
                // Information: we're in the first half of the interval.
                self.append_bit(false)?;
            } else if self.low >= interval_half {
                // Information: we're in the second half of the interval.
                self.append_bit(true)?;
            } else if self.low >= interval_quarter && high < interval_three_quarters {
                // Information: we're in the half of the interval around the center.
                self.low  -= interval_quarter;
                high -= interval_quarter;
                self.recentering_bits += 1;
            } else {
                break 'one_bit
            };
            // Renormalize low (last digit must always be 0)
            self.low = self.low << 1;
            // Renormalize high (last digit must always be 1)
            high = high << 1 | 1u32;
            assert!(high > self.low);
        }

        // At this stage, we can't have high = low:
        // - if we have not entered the loop
        //    - by definition, high - low > interval/4, so high != low
        // - otherwise
        //    - at the end of the loop, we have set a different last digit for low and high, so high != low

        // Convert back to `self.length`.
        debug!(target: "multiarith", "Converting back to length: [{}, {}(", self.low, high);
        self.length = high - self.low;
        assert!(self.length != 0); // In the loop, we ensure that the last digit of high and low is different, so it can't be 0.
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
