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

/// Parses 4 hex digits in the input string and returns its numeric value.
fn parse_4_hex_digits(input: &[u8]) -> u16 {
    return (decode_hex_char(input[0]) << 12)
        | (decode_hex_char(input[1]) << 8)
        | (decode_hex_char(input[2]) << 4)
        | decode_hex_char(input[3]);
}

/// If the given `bytes` is WTF-8 which contains lone surrogate, escape the
/// lone surrogate with \x7F + XXXX (4 hex digits) and return the byte array.
/// If not, return the given `bytes`.
///
/// # Failures
///
/// If the input is not well-formed escaped WTF-8, this function causes a panic.
///
/// # Examples
///
/// Lone lead surrogate is escaped.
///
/// ```
/// let input: Vec<u8>    = vec!(0xED, 0xA0, 0xBE);
/// let expected: Vec<u8> = vec!(0x7F, b'D', b'8', b'3', b'E');
/// assert_eq!(binjs_io::escaped_wtf8::escape(input), expected);
/// ```
///
/// Lone trail surrogate is escaped.
///
/// ```
/// let input: Vec<u8>    = vec!(0xED, 0xB6, 0x9D);
/// let expected: Vec<u8> = vec!(0x7F, b'D', b'D', b'9', b'D');
/// assert_eq!(binjs_io::escaped_wtf8::escape(input), expected);
/// ```
///
/// Escape character is escaped
///
/// ```
/// let input: Vec<u8>    = vec!(0x7F);
/// let expected: Vec<u8> = vec!(0x7F, b'0', b'0', b'7', b'F');
/// assert_eq!(binjs_io::escaped_wtf8::escape(input), expected);
/// ```
///
/// Anything else are not converted.
///
/// ```
/// let input: Vec<u8> = vec!(b'A', b'\n', 0xE3, 0x81, 0x82, 0xED, 0x83, 0xBF);
/// let expected: Vec<u8> = input.clone();
/// assert_eq!(binjs_io::escaped_wtf8::escape(input), expected);
/// ```
///
/// If the input is ill-formed, causes a panic.
///
/// ```should_panic
/// // Input is clamped
/// let input: Vec<u8>  = vec!(0xED, 0xA0);
/// binjs_io::escaped_wtf8::escape(input);
/// ```
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
/// # Failures
///
/// If the input is not well-formed escaped WTF-8, this function causes a panic.
///
/// # Examples
///
/// Lone lead surrogate is unescaped.
///
/// ```
/// use std::ops::Deref;
/// let input: [u8; 5]    = [0x7F, b'D', b'8', b'3', b'E'];
/// let expected: [u8; 3] = [0xED, 0xA0, 0xBE];
/// let output = binjs_io::escaped_wtf8::unescape(&input);
/// let actual: &[u8] = output.deref();
/// assert_eq!(actual, expected);
/// ```
///
/// Lone trail surrogate is unescaped.
///
/// ```
/// use std::ops::Deref;
/// let input: [u8; 5]    = [0x7F, b'D', b'D', b'9', b'D'];
/// let expected: [u8; 3] = [0xED, 0xB6, 0x9D];
/// let output = binjs_io::escaped_wtf8::unescape(&input);
/// let actual: &[u8] = output.deref();
/// assert_eq!(actual, expected);
/// ```
///
/// Escape character is unescaped.
///
/// ```
/// use std::ops::Deref;
/// let input: [u8; 5]    = [0x7F, b'0', b'0', b'7', b'F'];
/// let expected: [u8; 1] = [0x7F];
/// let output = binjs_io::escaped_wtf8::unescape(&input);
/// let actual: &[u8] = output.deref();
/// assert_eq!(actual, expected);
/// ```
///
/// Anything else are not converted.
///
/// ```
/// use std::ops::Deref;
/// let input: [u8; 8] = [b'A', b'\n', 0xE3, 0x81, 0x82, 0xED, 0x83, 0xBF];
/// let expected: [u8; 8] = input.clone();
/// let output = binjs_io::escaped_wtf8::unescape(&input);
/// let actual: &[u8] = output.deref();
/// assert_eq!(actual, expected);
/// ```
///
/// If the input is ill-formed, causes a panic.
///
/// ```should_panic
/// // Input is clamped
/// let input: [u8; 4]    = [0x7F, b'0', b'0', b'7'];
/// binjs_io::escaped_wtf8::unescape(&input);
/// ```
///
/// ```should_panic
/// // Codeunit is not in the surrogate pair range.
/// let input: [u8; 5]    = [0x7F, b'3', b'0', b'4', b'2'];
/// binjs_io::escaped_wtf8::unescape(&input);
/// ```
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

        let codepoint = parse_4_hex_digits(&input[end + 1..]);
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
/// unicode escape \uXXXX.
///
/// # Failures
///
/// If the input is not well-formed escaped WTF-8, this function causes a panic.
///
/// # Examples
///
/// Lone lead surrogate is converted to unicode escape.
///
/// ```
/// let input    = "\x7FD83E".to_string();
/// let expected = "\\uD83E".to_string();
/// assert_eq!(binjs_io::escaped_wtf8::to_unicode_escape(input), expected);
/// ```
///
/// Lone trail surrogate is converted to unicode escape.
///
/// ```
/// let input    = "\x7FDD9D".to_string();
/// let expected = "\\uDD9D".to_string();
/// assert_eq!(binjs_io::escaped_wtf8::to_unicode_escape(input), expected);
/// ```
///
/// Escape char (\x7F) is unescaped.
///
/// ```
/// let input    = "\x7F007F".to_string();
/// let expected = "\x7F".to_string();
/// assert_eq!(binjs_io::escaped_wtf8::to_unicode_escape(input), expected);
/// ```
///
/// If the input is ill-formed, causes a panic.
///
/// ```should_panic
/// // Not enough character after escape character.
/// let input = "\x7F000".to_string();
/// binjs_io::escaped_wtf8::to_unicode_escape(input);
/// ```
///
/// ```should_panic
/// // Invalid character in the codeunit
/// let input = "\x7F012X".to_string();
/// binjs_io::escaped_wtf8::to_unicode_escape(input);
/// ```
///
/// ```should_panic
/// // Codeunit is not in the surrogate pair range.
/// let input = "\x7F3042".to_string();
/// binjs_io::escaped_wtf8::to_unicode_escape(input);
/// ```
pub fn to_unicode_escape(s: String) -> String {
    let pos = s.find(|c| c == char::from(LONE_SURROGATE_ESCAPE_CHAR));

    //   ...... \x7F XXXX ......
    //   ^      ^
    //   |      |
    //   s     end
    let mut end = if let Some(end) = pos {
        end
    } else {
        // There's no escaped lone surrogate.
        return s;
    };

    let mut input = s.as_str().as_bytes();

    let mut buf = Vec::with_capacity(input.len().next_power_of_two());
    loop {
        let head = &input[..end];
        buf.extend_from_slice(head);

        let codepoint = parse_4_hex_digits(&input[end + 1..]);
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

    String::from_utf8(buf).expect("Unescaped string should be valid UTF-8")
}

/// The length of an escape sequence \? and unicode escape \uXXXX.
const SINGLE_ESCAPE_LEN: usize = 2;
const UNICODE_ESCAPE_LEN: usize = 6;

/// If the given `bytes` contains any unicode escape \uXXXX and that is lone
/// surrogate, convert it to escaped lone surropgate.
///
/// # Failures
///
/// If the input is not well-formed JSON, this function causes a panic.
///
/// # Examples
///
/// Escape character is escaped.
///
/// ```
/// let input    = "\x7F".to_string();
/// let expected = "\x7F007F".to_string();
/// assert_eq!(binjs_io::escaped_wtf8::from_unicode_escape(input), expected);
/// ```
///
/// Lone lead surrogate is escaped.
///
/// ```
/// let input    = "\\uD83E".to_string();
/// let expected = "\x7FD83E".to_string();
/// assert_eq!(binjs_io::escaped_wtf8::from_unicode_escape(input), expected);
/// ```
///
/// Lone trail surrogate is escaped.
///
/// ```
/// let input    = "\\uDD9D".to_string();
/// let expected = "\x7FDD9D".to_string();
/// assert_eq!(binjs_io::escaped_wtf8::from_unicode_escape(input), expected);
/// ```
///
/// If the input is escaped surrogate pair, does nothing.
///
/// ```
/// let input = "\\uD83E\\uDD9D".to_string();
/// let expected = input.clone();
/// assert_eq!(binjs_io::escaped_wtf8::from_unicode_escape(input), expected);
/// ```
///
/// If the leading backslash is also escaped, does nothing.
///
/// ```
/// let input = "\\\\uD83E".to_string();
/// let expected = input.clone();
/// assert_eq!(binjs_io::escaped_wtf8::from_unicode_escape(input), expected);
/// ```
///
/// Anything else are not converted.
///
/// ```
/// let input = "\t\\u3042\\r\\n".to_string();
/// let expected = input.clone();
/// assert_eq!(binjs_io::escaped_wtf8::from_unicode_escape(input), expected);
/// ```
///
/// If the input is ill-formed, causes a panic.
///
/// ```should_panic
/// // Not enough character after \\u.
/// let input = "\\u000".to_string();
/// binjs_io::escaped_wtf8::from_unicode_escape(input);
/// ```
///
/// ```should_panic
/// // Invalid character in the codeunit
/// let input  = "\\u012X".to_string();
/// binjs_io::escaped_wtf8::from_unicode_escape(input);
/// ```
pub fn from_unicode_escape(s: String) -> String {
    let pos = s.find(|c| c == char::from(b'\\') || c == char::from(LONE_SURROGATE_ESCAPE_CHAR));

    //   ...... \uXXXX ......
    //   ^      ^
    //   |      |
    //   s     end
    let mut end = if let Some(end) = pos {
        end
    } else {
        // There's no escape sequence or escape character.
        return s;
    };

    let mut input = s.as_str().as_bytes();

    let mut buf = Vec::with_capacity(input.len().next_power_of_two());
    loop {
        let head = &input[..end];
        buf.extend_from_slice(head);

        let c = input[end];

        if c == LONE_SURROGATE_ESCAPE_CHAR {
            // Escape the escape character.
            buf.extend_from_slice(b"\x7F007F");

            input = &input[end + 1..];
        } else {
            // Below, we access characters after `\\` without checking the input
            // length, given we assume the input is well-formed JSON.

            let escaped = input[end + 1];
            if escaped == b'u' {
                // ...... \uXXXX ......
                //        ^ ^
                //        | |
                //        | lead_digits
                //        |
                //        end
                let lead_digits = &input[end + SINGLE_ESCAPE_LEN..];
                let codepoint = parse_4_hex_digits(lead_digits);
                if is_lead_surrogate(codepoint) {
                    // If we find lead surrogate, it can be surrogate pair.
                    // Check if there's trailing surrogate t.
                    //
                    // ...... \uXXXX \uXXXX ......
                    //        ^      ^
                    //        |      |
                    //        end    next
                    let next = end + UNICODE_ESCAPE_LEN;

                    if next < input.len() && input[next] == b'\\' && input[next + 1] == b'u' {
                        // ...... \uXXXX \uXXXX ......
                        //        ^      ^ ^
                        //        |      | |
                        //        end    | maybe_trail_digits
                        //               |
                        //               next
                        let maybe_trail_digits = &input[next + SINGLE_ESCAPE_LEN..];
                        let maybe_trail = parse_4_hex_digits(maybe_trail_digits);
                        if is_trail_surrogate(maybe_trail) {
                            // This is escaped surrogate pair, do not use
                            // escaped-wtf8.

                            buf.push(b'\\');
                            buf.push(b'u');
                            buf.push(lead_digits[0]);
                            buf.push(lead_digits[1]);
                            buf.push(lead_digits[2]);
                            buf.push(lead_digits[3]);

                            buf.push(b'\\');
                            buf.push(b'u');
                            buf.push(maybe_trail_digits[0]);
                            buf.push(maybe_trail_digits[1]);
                            buf.push(maybe_trail_digits[2]);
                            buf.push(maybe_trail_digits[3]);

                            // ...... \uXXXX \uXXXX ......
                            //        ^      ^      ^
                            //        |      |      |
                            //        end    next   input

                            input = &input[next + UNICODE_ESCAPE_LEN..];
                        } else {
                            // This is lone lead surrogate.
                            buf.push(LONE_SURROGATE_ESCAPE_CHAR);
                            buf.push(lead_digits[0]);
                            buf.push(lead_digits[1]);
                            buf.push(lead_digits[2]);
                            buf.push(lead_digits[3]);

                            // ...... \uXXXX \uXXXX ......
                            //        ^      ^
                            //        |      |
                            //        end    input
                            input = &input[end + UNICODE_ESCAPE_LEN..];
                        }
                    } else {
                        // This is lone lead surrogate.
                        buf.push(LONE_SURROGATE_ESCAPE_CHAR);
                        buf.push(lead_digits[0]);
                        buf.push(lead_digits[1]);
                        buf.push(lead_digits[2]);
                        buf.push(lead_digits[3]);

                        // ...... \uXXXX ......
                        //        ^      ^
                        //        |      |
                        //        end    input
                        input = &input[end + UNICODE_ESCAPE_LEN..];
                    }
                } else if is_trail_surrogate(codepoint) {
                    // This is lone trail surrogate.
                    buf.push(LONE_SURROGATE_ESCAPE_CHAR);
                    buf.push(lead_digits[0]);
                    buf.push(lead_digits[1]);
                    buf.push(lead_digits[2]);
                    buf.push(lead_digits[3]);

                    // ...... \uXXXX ......
                    //        ^      ^
                    //        |      |
                    //        end    input
                    input = &input[end + UNICODE_ESCAPE_LEN..];
                } else {
                    // Not lone surrogate, don't change.
                    buf.push(b'\\');
                    buf.push(b'u');
                    buf.push(lead_digits[0]);
                    buf.push(lead_digits[1]);
                    buf.push(lead_digits[2]);
                    buf.push(lead_digits[3]);

                    // ...... \uXXXX ......
                    //        ^      ^
                    //        |      |
                    //        end   input
                    input = &input[end + UNICODE_ESCAPE_LEN..];
                }
            } else {
                buf.push(b'\\');
                buf.push(escaped);

                // ...... \? ......
                //        ^  ^
                //        |  |
                //        |  input
                //        |
                //        end
                input = &input[end + SINGLE_ESCAPE_LEN..];
            }
        }

        let pos = input
            .iter()
            .position(|&c| c == b'\\' || c == LONE_SURROGATE_ESCAPE_CHAR);

        // ...... \uXXXX ...... \uXXXX ......
        //               ^      ^
        //               |      |
        //               input   end
        //
        // or
        //
        // ...... \n ...... \uXXXX ......
        //           ^      ^
        //           |      |
        //           input   end
        end = if let Some(end) = pos {
            end
        } else {
            buf.extend_from_slice(&input);
            break;
        };
    }

    String::from_utf8(buf).expect("Escaped string should be valid UTF-8")
}
