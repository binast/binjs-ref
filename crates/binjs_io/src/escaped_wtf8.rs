/// An utility to convert between WTF-8 and UTF-8 + special escape sequence.
///
/// The escape sequence has the following syntax:
///   \x7F (single delete character) + XXXX (4 hex digits in ASCII)
/// where XXXX is either 007F or lone surrogate's code unit.
/// All code units in that range should be escaped, and no other code units
/// are allowed to be escaped.
///
/// This escape sequence is supposed to be used in encoder/decoder's internal
/// representation of any kind of string, in order to use str/String type for
/// WTF-8 strings.
///
/// Deserializers are supposed to escape input WTF-8 string to get internal
/// escaped-UTF-8 string, and serializers are supposed to unescape the internal
/// escaped UTF-8 string to generate WTF-8 string.
///
/// \x7F is chosen because it's representable in single byte without escape
/// in JSON, and most likely unused in actual JS code.
use std::borrow::Cow;

use binjs_shared::SharedString;

const LONE_SURROGATE_ESCAPE_CHAR: u8 = 0x7F;

const LONE_SURROGATE_UNIT_1: u8 = 0xED;
const LONE_SURROGATE_UNIT_2_MIN: u8 = 0xA0;
const LONE_SURROGATE_UNIT_2_MAX: u8 = 0xBF;
const LONE_SURROGATE_UNIT_3_MIN: u8 = 0x80;
const LONE_SURROGATE_UNIT_3_MAX: u8 = 0xBF;

const LEAD_SURROGATE_MIN: u16 = 0xD800;
const LEAD_SURROGATE_MAX: u16 = 0xDBFF;
const TRAIL_SURROGATE_MIN: u16 = 0xDC00;
const TRAIL_SURROGATE_MAX: u16 = 0xDFFF;

fn is_lead_surrogate(n: u16) -> bool {
    return LEAD_SURROGATE_MIN <= n && n <= LEAD_SURROGATE_MAX;
}

fn is_trail_surrogate(n: u16) -> bool {
    return TRAIL_SURROGATE_MIN <= n && n <= TRAIL_SURROGATE_MAX;
}

/// Convert 0-F number to ASCII char.
fn encode_hex_char(n: u8) -> u8 {
    match n {
        0...9 => b'0' + n,
        0xa...0xf => b'A' + (n - 10),
        _ => panic!("unexpected input"),
    }
}

/// Returns true if the given code is 0-9A-Fa-f chars.
/// This uses u16 instead of u8 because the callsite uses u16.
fn is_hex_char(n: u16) -> bool {
    if b'0' as u16 <= n && n <= b'9' as u16 {
        true
    } else if b'A' as u16 <= n && n <= b'F' as u16 {
        true
    } else if b'a' as u16 <= n && n <= b'f' as u16 {
        true
    } else {
        false
    }
}

/// Convert 0-9A-Fa-f chars to number.
fn decode_hex_char(n: u8) -> u16 {
    match n {
        b'0'...b'9' => (n - b'0') as u16,
        b'A'...b'F' => (n - b'A' + 10) as u16,
        b'a'...b'f' => (n - b'a' + 10) as u16,
        _ => panic!("unexpected char"),
    }
}

/// True if the given byte is the 2nd code unit of lone surrogate in WTF-8.
fn is_unit_2(c: u8) -> bool {
    c >= LONE_SURROGATE_UNIT_2_MIN && c <= LONE_SURROGATE_UNIT_2_MAX
}

/// True if the given byte is the 3rd code unit of lone surrogate in WTF-8.
fn is_unit_3(c: u8) -> bool {
    c >= LONE_SURROGATE_UNIT_3_MIN && c <= LONE_SURROGATE_UNIT_3_MAX
}

/// If the given `bytes` is WTF-8 which contains lone surrogate, escape the
/// lone surrogate with \x7F + XXXX (4 hex digits) and return the byte array.
/// If not, return the given `bytes`.
///
/// This assumes the input is well-formed WTF-8, and panics otherwise.
pub fn escape(bytes: Vec<u8>) -> Vec<u8> {
    let pos = bytes
        .as_slice()
        .iter()
        .position(|&c| c == LONE_SURROGATE_ESCAPE_CHAR || c == LONE_SURROGATE_UNIT_1);

    //   ...... \x7F XXXX ......
    //   ^      ^
    //   |      |
    // bytes   end
    //
    //   ...... \xED \xA0 \x80 ......
    //   ^      ^
    //   |      |
    // bytes   end
    let mut end = if let Some(end) = pos {
        end
    } else {
        return bytes;
    };

    let mut input = bytes.as_slice();

    let mut buf: Vec<u8> = Vec::with_capacity(input.len().next_power_of_two());
    loop {
        let head = &input[..end];
        buf.extend_from_slice(head);

        let tail = if input[end] == LONE_SURROGATE_ESCAPE_CHAR {
            buf.extend_from_slice(b"\x7F007F");

            end + 1
        } else {
            if is_unit_2(input[end + 1]) && is_unit_3(input[end + 2]) {
                let codepoint = (((input[end] as u16) & 0x0F) << 12)
                    | (((input[end + 1] & 0x3F) as u16) << 6)
                    | ((input[end + 2] & 0x3F) as u16);

                buf.push(LONE_SURROGATE_ESCAPE_CHAR);
                buf.push(encode_hex_char(((codepoint >> 12) & 0xf) as u8));
                buf.push(encode_hex_char(((codepoint >> 8) & 0xf) as u8));
                buf.push(encode_hex_char(((codepoint >> 4) & 0xf) as u8));
                buf.push(encode_hex_char((codepoint & 0xf) as u8));
            } else {
                buf.push(input[end]);
                buf.push(input[end + 1]);
                buf.push(input[end + 2]);
            }

            end + 3
        };

        // ...... \xED \xA0 \x80 ......
        //                       ^
        //                       |
        //                     input
        input = &input[tail..];

        let pos = input.iter().position(|&c| c == LONE_SURROGATE_ESCAPE_CHAR);

        // ...... \xED \xA0 \x80 ...... \xED \xA0 \x80 ......
        //                       ^      ^
        //                       |      |
        //                     input   end
        end = if let Some(end) = pos {
            end
        } else {
            buf.extend_from_slice(&input);
            break;
        };
    }

    buf
}

/// If the given `bytes` contains any escaped lone surropgate, unescape all
/// escaped lone surrogate and returns the byte array.
/// If not, return None.
///
/// This assumes the input is well-formed escaped WTF-8, which is the result of
/// escape function, and panics otherwise.
pub fn unescape(bytes: &[u8]) -> Cow<[u8]> {
    let pos = bytes.iter().position(|&c| c == LONE_SURROGATE_ESCAPE_CHAR);

    //   ...... \x7F XXXX ......
    //   ^      ^
    //   |      |
    // bytes   end
    let mut end = if let Some(end) = pos {
        end
    } else {
        return Cow::from(bytes);
    };

    let mut input = bytes;

    let mut buf: Vec<u8> = Vec::with_capacity(input.len().next_power_of_two());
    loop {
        let head = &input[..end];
        buf.extend_from_slice(head);

        let codepoint: u16 = (decode_hex_char(input[end + 1]) << 12)
            | (decode_hex_char(input[end + 2]) << 8)
            | (decode_hex_char(input[end + 3]) << 4)
            | decode_hex_char(input[end + 4]);
        if codepoint == LONE_SURROGATE_ESCAPE_CHAR as u16 {
            buf.push(LONE_SURROGATE_ESCAPE_CHAR);
        } else {
            assert!(codepoint >= LEAD_SURROGATE_MIN &&
                    codepoint <= TRAIL_SURROGATE_MAX,
                    "escaped codepoint should be either escape character {:04x} or lone surrogate ({:04x}...{:04x})",
                    LONE_SURROGATE_ESCAPE_CHAR,
                    LEAD_SURROGATE_MIN,
                    TRAIL_SURROGATE_MAX);

            buf.push(LONE_SURROGATE_UNIT_1);
            buf.push((0x80 | ((codepoint >> 6) & 0x3F)) as u8);
            buf.push((0x80 | (codepoint & 0x3F)) as u8);
        }

        // ...... \x7F XXXX ......
        //                  ^
        //                  |
        //                input
        input = &input[end + 5..];

        let pos = input.iter().position(|&c| c == LONE_SURROGATE_ESCAPE_CHAR);

        // ...... \x7F XXXX ...... \x7F XXXX ......
        //                  ^      ^
        //                  |      |
        //                input   end
        end = if let Some(end) = pos {
            end
        } else {
            buf.extend_from_slice(&input);
            break;
        };
    }

    Cow::from(buf)
}

/// If the given `bytes` contains any escaped lone surropgate, convert it to
/// \uXXXX format, just for debug print.
/// This does unrecoverable conversion, the result shouldn't be used outside
/// of debug print.
///
/// This assumes the input is well-formed escaped WTF-8, which is the result of
/// escape function, and panics otherwise.
pub fn for_print(s: &SharedString) -> SharedString {
    let pos = s.find(char::from(LONE_SURROGATE_ESCAPE_CHAR));

    //   ...... \x7F XXXX ......
    //   ^      ^
    //   |      |
    //   s     end
    let mut end = if let Some(end) = pos {
        end
    } else {
        return s.clone();
    };

    let mut input = s.as_str().as_bytes();

    let mut buf: Vec<u8> = Vec::with_capacity(input.len().next_power_of_two());
    loop {
        let head = &input[..end];
        buf.extend_from_slice(head);

        let codepoint: u16 = (decode_hex_char(input[end + 1]) << 12)
            | (decode_hex_char(input[end + 2]) << 8)
            | (decode_hex_char(input[end + 3]) << 4)
            | decode_hex_char(input[end + 4]);
        if codepoint == LONE_SURROGATE_ESCAPE_CHAR as u16 {
            buf.push(LONE_SURROGATE_ESCAPE_CHAR);
        } else {
            assert!(
                codepoint >= LEAD_SURROGATE_MIN && codepoint <= TRAIL_SURROGATE_MAX,
                "escaped codepoint should be either {:04x} or lone surrogate ({:04x}...{:04x})",
                LONE_SURROGATE_ESCAPE_CHAR,
                LEAD_SURROGATE_MIN,
                TRAIL_SURROGATE_MAX
            );

            buf.extend_from_slice(b"\\u");
            buf.push(input[end + 1]);
            buf.push(input[end + 2]);
            buf.push(input[end + 3]);
            buf.push(input[end + 4]);
        }

        // ...... \x7F XXXX ......
        //                  ^
        //                  |
        //                input
        input = &input[end + 5..];

        let pos = input.iter().position(|&c| c == LONE_SURROGATE_ESCAPE_CHAR);

        // ...... \x7F XXXX ...... \x7F XXXX ......
        //                  ^      ^
        //                  |      |
        //                input   end
        end = if let Some(end) = pos {
            end
        } else {
            buf.extend_from_slice(&input);
            break;
        };
    }

    SharedString::from_string(String::from_utf8(buf).expect("Escaped string should be valid UTF-8"))
}

/// Convert the given escaped_wtf8 string to JSON.  If the given string contains
/// any escaped lone surrogate, it's converted to unicode escape sequence.
///
/// This is not optimized.
pub fn to_json(s: &SharedString) -> SharedString {
    let input = s.as_str().as_bytes();

    let mut i = 0;
    let mut buf: Vec<u8> = Vec::with_capacity((input.len() + 2).next_power_of_two());
    buf.push(b'\"');
    'per_byte: while i < input.len() {
        let unit = input[i];

        if unit != LONE_SURROGATE_ESCAPE_CHAR {
            match unit {
                b'\\' | b'\"' => {
                    buf.push(b'\\');
                    buf.push(unit);
                }
                0x08 => {
                    buf.push(b'\\');
                    buf.push(b'b');
                }
                0x0C => {
                    buf.push(b'\\');
                    buf.push(b'f');
                }
                b'\n' => {
                    buf.push(b'\\');
                    buf.push(b'n');
                }
                b'\r' => {
                    buf.push(b'\\');
                    buf.push(b'r');
                }
                b'\t' => {
                    buf.push(b'\\');
                    buf.push(b't');
                }
                _ => {
                    buf.push(unit);
                }
            }
            i += 1;
            continue 'per_byte;
        }

        let codepoint: u16 = (decode_hex_char(input[i + 1]) << 12)
            | (decode_hex_char(input[i + 2]) << 8)
            | (decode_hex_char(input[i + 3]) << 4)
            | decode_hex_char(input[i + 4]);

        if codepoint == LONE_SURROGATE_ESCAPE_CHAR as u16 {
            buf.push(LONE_SURROGATE_ESCAPE_CHAR);
        } else {
            assert!(
                codepoint >= LEAD_SURROGATE_MIN && codepoint <= TRAIL_SURROGATE_MAX,
                "escaped codepoint should be either {:04x} or lone surrogate ({:04x}...{:04x})",
                LONE_SURROGATE_ESCAPE_CHAR,
                LEAD_SURROGATE_MIN,
                TRAIL_SURROGATE_MAX
            );

            buf.extend_from_slice(b"\\u");
            buf.push(input[i + 1]);
            buf.push(input[i + 2]);
            buf.push(input[i + 3]);
            buf.push(input[i + 4]);
        }

        i += 5;
    }
    buf.push(b'\"');
    SharedString::from_string(String::from_utf8(buf).expect("Escaped string should be valid UTF-8"))
}

/// Parse UTF-16 encoded JSON string and return UTF-16 string,
/// which can contain raw lone surrogate.
fn parse_json(input: &[u16]) -> Result<Vec<u16>, ()> {
    assert!(input.len() >= 2);
    assert!(input[0] == b'"' as u16);
    assert!(input[input.len() - 1] == b'"' as u16);

    let mut buf: Vec<u16> = Vec::with_capacity(input.len());
    let mut i = 1;
    let end = input.len() - 1;
    while i < end {
        let c = input[i];
        if c == b'\\' as u16 {
            // Handle escape sequence.

            i += 1;
            if i >= end {
                return Err(());
            }

            let next = input[i];
            if next == b'"' as u16 || next == b'/' as u16 || next == b'\\' as u16 {
                buf.push(input[i]);
                i += 1;
            } else if next == b'b' as u16 {
                buf.push(0x08);
                i += 1;
            } else if next == b'f' as u16 {
                buf.push(0x0C);
                i += 1;
            } else if next == b'n' as u16 {
                buf.push(b'\n' as u16);
                i += 1;
            } else if next == b'r' as u16 {
                buf.push(b'\r' as u16);
                i += 1;
            } else if next == b't' as u16 {
                buf.push(b'\t' as u16);
                i += 1;
            } else if next == b'u' as u16 {
                i += 1;
                if end - i < 4
                    || !is_hex_char(input[i])
                    || !is_hex_char(input[i + 1])
                    || !is_hex_char(input[i + 2])
                    || !is_hex_char(input[i + 3])
                {
                    return Err(());
                }
                let code = (decode_hex_char(input[i] as u8) << 12)
                    | (decode_hex_char(input[i + 1] as u8) << 8)
                    | (decode_hex_char(input[i + 2] as u8) << 4)
                    | decode_hex_char(input[i + 3] as u8);
                i += 4;
                buf.push(code);
            } else {
                return Err(());
            }
        } else {
            buf.push(c);
            i += 1;
        }
    }
    Ok(buf)
}

/// Escape lone surrogate in the given UTF-16 string.
fn escape_utf16(input: &[u16]) -> Vec<u16> {
    let mut buf: Vec<u16> = Vec::with_capacity(input.len());
    let mut i = 0;
    let end = input.len();
    while i < end {
        let unit = input[i];

        if is_lead_surrogate(unit) {
            i += 1;

            if i >= end {
                // Lead surrogate at the end of string.
                buf.push(LONE_SURROGATE_ESCAPE_CHAR as u16);
                buf.push(encode_hex_char(((unit >> 12) & 0xf) as u8) as u16);
                buf.push(encode_hex_char(((unit >> 8) & 0xf) as u8) as u16);
                buf.push(encode_hex_char(((unit >> 4) & 0xf) as u8) as u16);
                buf.push(encode_hex_char((unit & 0xf) as u8) as u16);
            }
            let trail = input[i];

            if is_trail_surrogate(trail) {
                // Surrogate pair.
                buf.push(unit);
                buf.push(trail);
                i += 1;
            } else {
                // Lead surrogate without trail surrogate in the next unit.
                buf.push(LONE_SURROGATE_ESCAPE_CHAR as u16);
                buf.push(encode_hex_char(((unit >> 12) & 0xf) as u8) as u16);
                buf.push(encode_hex_char(((unit >> 8) & 0xf) as u8) as u16);
                buf.push(encode_hex_char(((unit >> 4) & 0xf) as u8) as u16);
                buf.push(encode_hex_char((unit & 0xf) as u8) as u16);
            }
        } else if is_trail_surrogate(unit) {
            // Trail surrogate without lead surrogate in the previous unit.
            buf.push(LONE_SURROGATE_ESCAPE_CHAR as u16);
            buf.push(encode_hex_char(((unit >> 12) & 0xf) as u8) as u16);
            buf.push(encode_hex_char(((unit >> 8) & 0xf) as u8) as u16);
            buf.push(encode_hex_char(((unit >> 4) & 0xf) as u8) as u16);
            buf.push(encode_hex_char((unit & 0xf) as u8) as u16);
            i += 1;
        } else if unit == LONE_SURROGATE_ESCAPE_CHAR as u16 {
            buf.push(LONE_SURROGATE_ESCAPE_CHAR as u16);
            buf.push(b'0' as u16);
            buf.push(b'0' as u16);
            buf.push(b'7' as u16);
            buf.push(b'F' as u16);
            i += 1;
        } else {
            buf.push(unit);
            i += 1;
        }
    }

    buf
}

/// Parse the given JSON string.  If the given JSON string contains lone
/// surrogate, it's escaped with \x7F + XXXX (4 hex digits).
///
/// This is not optimized and only for testing.
pub fn from_json(s: &SharedString) -> Result<SharedString, ()> {
    // To simplify the surrogate pair handling, do the following:
    //   1. convert to UTF-16,
    //   2. unescape JSON string escape sequence
    //   3. convert lone surrogate to special escape sequence
    //   3. convert to UTF-8

    let input: Vec<u16> = s.as_str().encode_utf16().collect();
    let raw_utf16 = parse_json(input.as_slice())?;
    let escaped_utf16 = escape_utf16(raw_utf16.as_slice());
    Ok(SharedString::from_string(
        String::from_utf16(escaped_utf16.as_slice()).expect("Escaped string should be valid UTF-8"),
    ))
}
