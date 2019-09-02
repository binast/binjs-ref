//! Implementation of varnums used in Context-0.x.
//!
//! Note that this is NOT the same implementation as used in other experiments.
//!
//! # Format
//!
//! ```
//! varu32 ::= [0b1nnnnnnn]{0..4} 0b0nnnnnnn
//! ```
//!
//! Bits are interpreted as little-endian.
//! A first bit of `1` indicates a non-last byte.
//! A first bit of `0` indicates a last byte.

use std::io::{self, Error, Read, Result};

/// A result of reading data from a byte-oriented stream.
pub struct ByteValue<T> {
    /// The value read.
    value: T,

    /// The number of bytes consumed.
    byte_len: usize,
}

/// A reader that may read varu32 values from a stream.
pub trait ReadVaru32 {
    /// Read a single varu32.
    ///
    /// Note that this operation may return denormalized 0 values, e.g.
    /// `ByteValue { value: 0, byte_len: 5 }`. Such values may be used
    /// to represent exceptional cases.
    fn read_varu32_no_normalization(&mut self) -> Result<ByteValue<u32>>;
}

impl<T> ReadVaru32 for T
where
    T: Read,
{
    fn read_varu32_no_normalization(&mut self) -> Result<ByteValue<u32>> {
        let mut result: u32 = 0;
        let mut shift: u8 = 0;
        loop {
            debug_assert!(shift < 32);
            let mut bytes = [0];
            self.read_exact(&mut bytes)?;

            let byte = bytes[0];
            let new_result = result | (((byte & 0b01111111) as u32) << shift);
            if new_result < result {
                return Err(Error::new(
                    io::ErrorKind::InvalidData,
                    "Overflow during read_varu32_no_normalization",
                ));
            }

            result = new_result;
            shift += 7;

            if byte & 0b10000000 == 0 {
                return Ok(ByteValue {
                    value: result,
                    byte_len: (shift / 7) as usize,
                });
            }

            if shift >= 32 {
                return Err(Error::new(
                    io::ErrorKind::InvalidData,
                    "Overflow during read_varu32_no_normalization (too many digits)",
                ));
            }
        }
    }
}

#[test]
fn test_read_varu32_no_normalization_must_work() {
    use std::io::Cursor;
    for (input, expected) in &[
        // Various versions of 0
        (vec![0], 0),
        (vec![0b10000000, 0b00000000], 0),
        (vec![0b10000000, 0b10000000, 0b00000000], 0),
        // Various numbers in [0, 128)
        (vec![0b00000001], 1),
        (vec![0b00000010], 2),
        (vec![0b00000100], 4),
        (vec![0b00001000], 8),
        (vec![0b00010000], 16),
        (vec![0b00100000], 32),
        (vec![0b01000000], 64),
        (vec![0b01111111], 127),
        // Various numbers in [128, 16384(
        (vec![0b10000000, 0b00000001], 128),
        (vec![0b10000000, 0b00000010], 256),
        (vec![0b10000000, 0b00000100], 512),
        (vec![0b10000000, 0b00001000], 1024),
        (vec![0b10000000, 0b00010000], 2048),
        (vec![0b10000000, 0b00100000], 4096),
        (vec![0b10000000, 0b01000000], 8192),
        (vec![0b10000001, 0b00000001], 128 + 1),
        (vec![0b10000010, 0b00000001], 128 + 2),
        (vec![0b10000100, 0b00000001], 128 + 4),
        (vec![0b10001000, 0b00000001], 128 + 8),
        (vec![0b10010000, 0b00000001], 128 + 16),
        (vec![0b10100000, 0b00000001], 128 + 32),
        (vec![0b11000000, 0b00000001], 128 + 64),
        (vec![0b11111111, 0b01111111], 0b00111111_11111111),
        (vec![0b11111111, 0b00000000], 0b00000000_01111111),
        (vec![0b10000000, 0b01111111], 0b00111111_10000000),
    ] {
        // Read from the start of the vector.
        let mut cursor = Cursor::new(&input);
        let result = cursor
            .read_varu32_no_normalization()
            .unwrap_or_else(|e| panic!("Could not read from {:?}: {:?}", input, e));

        // Check value.
        assert_eq!(result.value, *expected);

        // Check that entire input was consumed.
        assert_eq!(result.byte_len, input.len());
    }
}
