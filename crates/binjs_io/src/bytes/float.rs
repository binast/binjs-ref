use std;

/// The representation of "no float", used for `float | null`.
const NONE_FLOAT_REPR: u64 = 0x7FF0000000000001;

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