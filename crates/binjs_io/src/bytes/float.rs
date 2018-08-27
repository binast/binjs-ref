use bytes::varnum::*;

use std;
use std::io::Write;

/// The representation of "no float", used for `float | null`.
const NONE_FLOAT_REPR: u64 = 0x7FF0000000000001;
const VARNUM_PREFIX_FLOAT: [u8; 2] = VARNUM_INVALID_ZERO_1;
const VARNUM_NULL: [u8; 3] = VARNUM_INVALID_ZERO_2;

pub fn varbytes_of_float(value: Option<f64>) -> Vec<u8> {
    let mut buf = Vec::with_capacity(4);
    buf.write_maybe_varfloat(value)
        .unwrap(); // The write cannot fail on a Vec<>
    buf
}

/// Encode a f64 | null, little-endian
pub fn bytes_of_float(value: Option<f64>) -> [u8; 8] {
    let mut as_u64 : u64 = match value {
        None => NONE_FLOAT_REPR,
        Some(value) => unsafe { std::mem::transmute::<f64, u64>(value) }
    };
    let mut buf: [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
    for i in 0..8 {
        buf[i] = (as_u64 % 256) as u8;
        as_u64 >>= 8;
    }
    buf
}

pub trait WriteVarFloat {
    fn write_maybe_varfloat(&mut self, value: Option<f64>) -> Result<usize, std::io::Error>;
    fn write_varfloat(&mut self, num: f64) -> Result<usize, std::io::Error>;
}

impl<T> WriteVarFloat for T where T: Write {
    fn write_maybe_varfloat(&mut self, value: Option<f64>) -> Result<usize, std::io::Error> {
        match value {
            None => {
                // Magic constant NULL.
                self.write_all(&VARNUM_NULL)?;
                Ok(VARNUM_NULL.len())
            }
            Some(v) => {
                self.write_varfloat(v)
            }
        }
    }

    fn write_varfloat(&mut self, value: f64) -> Result<usize, std::io::Error> {
        let signed = value as i32;
        if signed as f64 == value {
            // This is an i32. We can fit it in at most 5 7bit bytes.
            let unsigned : u32 = unsafe { std::mem::transmute(signed) };
            return self.write_varnum(unsigned)
        }
        // Encode as a float prefixed by 0b00000001 0b00000000 (which is an invalid integer).
        let bytes = bytes_of_float(Some(value));
        self.write_all(&VARNUM_PREFIX_FLOAT)?;
        self.write_all(&bytes)?;
        Ok(bytes.len() + VARNUM_PREFIX_FLOAT.len())
    }
}


/// Decode a f64 | null, little-endian
pub fn float_of_bytes(buf: &[u8; 8]) -> Option<f64> {
    let as_u64 =
          ((buf[0] as u64) << 0)
        | ((buf[1] as u64) << 8)
        | ((buf[2] as u64) << 16)
        | ((buf[3] as u64) << 24)
        | ((buf[4] as u64) << 32)
        | ((buf[5] as u64) << 40)
        | ((buf[6] as u64) << 48)
        | ((buf[7] as u64) << 56);
    if as_u64 == NONE_FLOAT_REPR {
        None
    } else {
        let as_f64 = unsafe { std::mem::transmute::<_, f64>(as_u64) };
        Some(as_f64)
    }
}

#[test]
fn test_floats() {
    use std::f64::*;
    for x in &[0., 100., 10., 1000., INFINITY, MIN, MAX, NEG_INFINITY] {
        let value = Some(*x);
        let encoded = bytes_of_float(value);
        let decoded = float_of_bytes(&encoded);
        println!("Encoded {:?} as {:?}, decoded as {:?}", x, encoded, decoded);
        assert_eq!(decoded, value);
    }

    assert_eq!(float_of_bytes(&bytes_of_float(None)), None);
}