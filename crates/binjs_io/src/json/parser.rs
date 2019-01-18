/// JSON parser.
/// This is manual translation of the JSONParser class in:
///   https://hg.mozilla.org/mozilla-central/file/tip/js/src/vm/JSONParser.cpp
///   https://hg.mozilla.org/mozilla-central/file/tip/js/src/vm/JSONParser.h
use TokenReaderError;

use binjs_shared::SharedString;

use super::value::Value;

use std::collections::HashMap;
use std::io::Read;
use std::rc::Rc;

use escaped_wtf8;

/// Token read by Parser::advance* methods.
///
/// Converted from JSONParserBase::Token.
enum Token {
    String(SharedString),
    Number(f64),
    True,
    False,
    Null,
    ArrayOpen,
    ArrayClose,
    ObjectOpen,
    ObjectClose,
    Colon,
    Comma,
}

/// Property item inside object.
/// This struct is used while parsing, and when a single object is parsed,
/// the list of Property is converted to HashMap for Value::Object.
struct Property {
    /// Property key.
    key: SharedString,

    /// Property value.
    value: Rc<Value>,
}

/// Possible states the parser can be in between values.
///
/// Converted from JSONParserBase::{ParserState, StackEntry}.
enum State {
    /// Parsing an object.
    /// Each property is pushed to the vector.
    FinishObjectMember(Vec<Property>),

    /// Parsing an array.
    /// Each element is pushed to the vector.
    FinishArrayElement(Vec<Rc<Value>>),

    /// At the start of the parse, before any values have been processed.
    JSONValue,
}

/// The state of the next iteration, with value from previous iteration.
struct NextState {
    /// The value parsed in the previous iteration.
    prev_value: Rc<Value>,

    /// The state of the next iteration.
    state: State,
}

/// The label of goto used in JSONParser:::parse.
enum NextLabel {
    /// An object property value has just been parsed.
    FinishObjectMember,

    /// At the start of parsing an object property.
    JSONMember(Token),

    /// An array element has just being parsed.
    FinishArrayElement,

    /// At the start of the parse, before any values have been processed.
    JSONValue,

    /// After reading a token for any value.
    JSONValueSwitch(Token),
}

/// No-op function just for readability.
/// Designates goto in the original code.
fn goto(label: NextLabel) -> NextLabel {
    label
}

/// JSON Parser.
///
/// Converted from JSONParser.
pub struct Parser<R: Read> {
    /// Input source.
    source: R,

    /// Current UTF-8 code unit.
    current: u8,

    /// True if the source hits EOF.
    is_eof: bool,

    /// State stack.
    stack: Vec<State>,
}
impl<R: Read> Parser<R> {
    pub fn new(source: R) -> Self {
        Parser {
            source,
            current: 0,
            is_eof: false,
            stack: Vec::new(),
        }
    }

    /// Read a byte from the stream and store the result to `self.current`.
    /// Returns `true` if it successfully read, and `false` is it reaches EOF.
    fn advance_char(&mut self) -> Result<bool, TokenReaderError> {
        if self.is_eof {
            return Ok(false);
        }

        let mut buf = [0; 1];

        let size = self
            .source
            .read(&mut buf)
            .map_err(TokenReaderError::ReadError)?;
        if size == 0 {
            self.is_eof = true;
            return Ok(false);
        }
        self.current = buf[0];
        Ok(true)
    }

    /// Read a byte and checks if it matches to `expected`.
    fn match_keyword_part(&mut self, expected: u8) -> Result<(), TokenReaderError> {
        if !self.advance_char()? {
            return Err(TokenReaderError::GenericError(
                "unexpected keyword".to_string(),
            ));
        }
        if self.current != expected {
            return Err(TokenReaderError::GenericError(
                "unexpected keyword".to_string(),
            ));
        }
        Ok(())
    }

    /// Read a string from stream.
    ///
    /// Corresponds to JSONParser::readString, but not direct conversion,
    /// because of WTF-8 handling.
    fn read_string(&mut self) -> Result<Token, TokenReaderError> {
        debug_assert!(self.current == b'"');

        // Escape sequences are parsed in escaped_wtf8.
        // Just read until the end of string.

        let mut buffer: Vec<u8> = Vec::new();
        buffer.push(self.current);
        loop {
            if !self.advance_char()? {
                return Err(TokenReaderError::GenericError(
                    "unterminated string".to_string(),
                ));
            }

            match self.current {
                b'"' => {
                    buffer.push(self.current);
                    self.advance_char()?;
                    break;
                }
                b'\\' => {
                    // Minimum handling of escape sequence.
                    // Read the next unit here to ignore '"' there.
                    buffer.push(self.current);
                    if !self.advance_char()? {
                        return Err(TokenReaderError::GenericError(
                            "bad escaped character".to_string(),
                        ));
                    }
                    buffer.push(self.current);
                }
                _ => {
                    buffer.push(self.current);
                }
            }
        }

        let s = SharedString::from_string(
            String::from_utf8(buffer).expect("Escaped string should be valid UTF-8"),
        );
        let decoded = escaped_wtf8::from_json(&s)
            .map_err(|_| TokenReaderError::GenericError("bad escaped character".to_string()))?;
        Ok(Token::String(decoded))
    }

    /// Read a number from stream.
    ///
    /// Converted from JSONParser::readNumber.
    fn read_number(&mut self) -> Result<Token, TokenReaderError> {
        // JSONNumber:
        //   /^-?(0|[1-9][0-9]+)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?$/

        let negative = self.current == b'-';

        let mut buffer = String::new();

        // -?
        if negative {
            buffer.push(self.current as char);
            if !self.advance_char()? {
                return Err(TokenReaderError::GenericError(
                    "no number after minus sign".to_string(),
                ));
            }
        }

        // 0|[1-9][0-9]+
        if !Self::is_ascii_digit(self.current) {
            return Err(TokenReaderError::GenericError(
                "unexpected non-digit".to_string(),
            ));
        }
        buffer.push(self.current as char);

        if self.current != b'0' {
            loop {
                if !self.advance_char()? {
                    let d = buffer
                        .as_str()
                        .parse::<f64>()
                        .expect("should be valid number");
                    return Ok(Token::Number(if negative { -d } else { d }));
                }
                if !Self::is_ascii_digit(self.current) {
                    break;
                }
                buffer.push(self.current as char);
            }
        };

        // Fast path: no fractional or exponent part.
        if self.current != b'.' && self.current != b'e' && self.current != b'E' {
            let d = buffer
                .as_str()
                .parse::<f64>()
                .expect("should be valid number");
            return Ok(Token::Number(if negative { -d } else { d }));
        }

        // (\.[0-9]+)?
        if self.current == b'.' {
            buffer.push(self.current as char);

            if !self.advance_char()? {
                return Err(TokenReaderError::GenericError(
                    "missing digits after decimal point".to_string(),
                ));
            }
            if !Self::is_ascii_digit(self.current) {
                return Err(TokenReaderError::GenericError(
                    "unterminated fractional number".to_string(),
                ));
            }
            buffer.push(self.current as char);

            loop {
                if !self.advance_char()? {
                    let d = buffer
                        .as_str()
                        .parse::<f64>()
                        .expect("should be valid number");
                    return Ok(Token::Number(if negative { -d } else { d }));
                }
                if !Self::is_ascii_digit(self.current) {
                    break;
                }
                buffer.push(self.current as char);
            }
        }

        // ([eE][\+\-]?[0-9]+)?
        if self.current == b'e' || self.current == b'E' {
            if !self.advance_char()? {
                return Err(TokenReaderError::GenericError(
                    "missing digits after exponent indicator".to_string(),
                ));
            }
            if self.current == b'+' || self.current == b'-' {
                buffer.push(self.current as char);

                if !self.advance_char()? {
                    return Err(TokenReaderError::GenericError(
                        "missing digits after exponent sign".to_string(),
                    ));
                }
            }
            if !Self::is_ascii_digit(self.current) {
                return Err(TokenReaderError::GenericError(
                    "exponent part is missing a number".to_string(),
                ));
            }
            buffer.push(self.current as char);

            loop {
                if !self.advance_char()? {
                    break;
                }
                if !Self::is_ascii_digit(self.current) {
                    break;
                }
                buffer.push(self.current as char);
            }
        }

        let d = buffer
            .as_str()
            .parse::<f64>()
            .expect("should be valid number");
        Ok(Token::Number(if negative { -d } else { d }))
    }

    /// Returns true if the given code unit is whitespace.
    ///
    /// Converted from IsJSONWhitespace.
    fn is_json_whitespace(c: u8) -> bool {
        return c == b'\t' || c == b'\r' || c == b'\n' || c == b' ';
    }

    /// Returns true if the given code unit is ASCII decimal digit.
    ///
    /// Converted from mozilla::IsAsciiDigit.
    fn is_ascii_digit(c: u8) -> bool {
        return b'0' <= c && c <= b'9';
    }

    /// Read bytes from stream until it find non-whitespace.
    /// Returns `true` if it successfully found non-whitespace, and `false` is
    /// it reaches EOF.
    ///
    /// Corresponds to the beginning of JSONParser::advance* methods.
    fn skip_whitespace(&mut self) -> Result<bool, TokenReaderError> {
        loop {
            if !Self::is_json_whitespace(self.current) {
                return Ok(true);
            }
            if !self.advance_char()? {
                return Ok(false);
            }
        }
    }

    /// Read a token.
    ///
    /// Converted from JSONParser::advance.
    fn advance(&mut self) -> Result<Token, TokenReaderError> {
        if !self.skip_whitespace()? {
            return Err(TokenReaderError::GenericError(
                "unexpected end of data".to_string(),
            ));
        }

        match self.current {
            b'"' => self.read_string(),
            b'-' | b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' => {
                self.read_number()
            }
            b't' => {
                self.match_keyword_part(b'r')?;
                self.match_keyword_part(b'u')?;
                self.match_keyword_part(b'e')?;
                self.advance_char()?;
                Ok(Token::True)
            }
            b'f' => {
                self.match_keyword_part(b'a')?;
                self.match_keyword_part(b'l')?;
                self.match_keyword_part(b's')?;
                self.match_keyword_part(b'e')?;
                self.advance_char()?;
                Ok(Token::False)
            }
            b'n' => {
                self.match_keyword_part(b'u')?;
                self.match_keyword_part(b'l')?;
                self.match_keyword_part(b'l')?;
                self.advance_char()?;
                Ok(Token::Null)
            }
            b'[' => {
                self.advance_char()?;
                Ok(Token::ArrayOpen)
            }
            b']' => {
                self.advance_char()?;
                Ok(Token::ArrayClose)
            }
            b'{' => {
                self.advance_char()?;
                Ok(Token::ObjectOpen)
            }
            b'}' => {
                self.advance_char()?;
                Ok(Token::ObjectClose)
            }
            b',' => {
                self.advance_char()?;
                Ok(Token::Comma)
            }
            b':' => {
                self.advance_char()?;
                Ok(Token::Colon)
            }
            _ => Err(TokenReaderError::GenericError(
                "unexpected character".to_string(),
            )),
        }
    }

    /// Read a token after '{'.
    ///
    /// Converted from JSONParser::advanceAfterObjectOpen.
    fn advance_after_object_open(&mut self) -> Result<Token, TokenReaderError> {
        if !self.skip_whitespace()? {
            return Err(TokenReaderError::GenericError(
                "end of data while reading object contents".to_string(),
            ));
        }

        match self.current {
            b'"' => self.read_string(),
            b'}' => {
                self.advance_char()?;
                Ok(Token::ObjectClose)
            }
            _ => Err(TokenReaderError::GenericError(
                "expected property name or '}'".to_string(),
            )),
        }
    }

    /// Read a token after array element.
    ///
    /// Converted from JSONParser::advanceAfterArrayElement.
    fn advance_after_array_element(&mut self) -> Result<Token, TokenReaderError> {
        if !self.skip_whitespace()? {
            return Err(TokenReaderError::GenericError(
                "end of data when ',' or ']' was expected".to_string(),
            ));
        }

        match self.current {
            b',' => {
                self.advance_char()?;
                Ok(Token::Comma)
            }
            b']' => {
                self.advance_char()?;
                Ok(Token::ArrayClose)
            }
            _ => Err(TokenReaderError::GenericError(
                "expected ',' or ']' after array element".to_string(),
            )),
        }
    }

    /// Read a token after property name.
    ///
    /// Converted from JSONParser::advancePropertyName.
    fn advance_property_name(&mut self) -> Result<Token, TokenReaderError> {
        if !self.skip_whitespace()? {
            return Err(TokenReaderError::GenericError(
                "end of data when property name was expected".to_string(),
            ));
        }

        match self.current {
            b'"' => self.read_string(),
            _ => Err(TokenReaderError::GenericError(
                "expected double-quoted property name".to_string(),
            )),
        }
    }

    /// Read a token after ':'.
    ///
    /// Converted from JSONParser::advancePropertyColon.
    fn advance_property_colon(&mut self) -> Result<Token, TokenReaderError> {
        if !self.skip_whitespace()? {
            return Err(TokenReaderError::GenericError(
                "end of data after property name when ':' was expected".to_string(),
            ));
        }

        match self.current {
            b':' => {
                self.advance_char()?;
                Ok(Token::Colon)
            }
            _ => Err(TokenReaderError::GenericError(
                "expected ':' after property name in object".to_string(),
            )),
        }
    }

    /// Read a token after object property.
    ///
    /// Converted from JSONParser::advanceAfterProperty.
    fn advance_after_property(&mut self) -> Result<Token, TokenReaderError> {
        if !self.skip_whitespace()? {
            return Err(TokenReaderError::GenericError(
                "end of data after property value in object".to_string(),
            ));
        }

        match self.current {
            b',' => {
                self.advance_char()?;
                Ok(Token::Comma)
            }
            b'}' => {
                self.advance_char()?;
                Ok(Token::ObjectClose)
            }
            _ => Err(TokenReaderError::GenericError(
                "expected ',' or '}' after property value in object".to_string(),
            )),
        }
    }

    /// Pops an object from the stack and convert it to a value.
    ///
    /// Corresponds to JSONParser::finishObject.
    fn finish_object(&mut self) -> Rc<Value> {
        let properties = match self.stack.pop() {
            Some(State::FinishObjectMember(properties)) => properties,
            _ => {
                panic!("there should be an object on the stack");
            }
        };

        let mut properties_map = HashMap::new();
        for prop in properties {
            properties_map.insert(prop.key, prop.value);
        }
        Rc::new(Value::Object(Rc::new(properties_map)))
    }

    /// Pops an array from the stack and convert it to a value.
    ///
    /// Corresponds to JSONParser::finishArray.
    fn finish_array(&mut self) -> Rc<Value> {
        let elements = match self.stack.pop() {
            Some(State::FinishArrayElement(elements)) => elements,
            _ => {
                panic!("there should be an array on the stack");
            }
        };

        Rc::new(Value::Array(Rc::new(elements)))
    }

    /// Parse the stream and returns a value.
    ///
    /// Converted from JSONParser::parse.
    pub fn parse(&mut self) -> Result<Rc<Value>, TokenReaderError> {
        let mut state = NextState {
            prev_value: Rc::new(Value::Null),
            state: State::JSONValue,
        };

        // Populate the current code unit.
        self.advance_char()?;

        // Loop for stack.
        let value = loop {
            // Corresponds to switch-case jump in the original code.
            let mut next_label = match state {
                NextState {
                    prev_value,
                    state: State::FinishArrayElement(mut elements),
                } => {
                    elements.push(prev_value);
                    self.stack.push(State::FinishArrayElement(elements));

                    goto(NextLabel::FinishArrayElement)
                }
                NextState {
                    prev_value,
                    state: State::FinishObjectMember(mut properties),
                } => {
                    properties
                        .last_mut()
                        .expect("there should already be a property with key populated")
                        .value = prev_value;
                    self.stack.push(State::FinishObjectMember(properties));

                    goto(NextLabel::FinishObjectMember)
                }
                NextState {
                    state: State::JSONValue,
                    ..
                } => goto(NextLabel::JSONValue),
            };

            // Loop which emulates switch and goto in the original code.
            let value = loop {
                next_label = match next_label {
                    NextLabel::FinishObjectMember => {
                        let token = self.advance_after_property()?;
                        match token {
                            Token::ObjectClose => {
                                break self.finish_object();
                            }
                            Token::Comma => {
                                let next_token = self.advance_property_name()?;
                                goto(NextLabel::JSONMember(next_token))
                            }
                            _ => {
                                return Err(TokenReaderError::GenericError(
                                    "expected ',' or '}' after property-value pair in object literal".to_string()));
                            }
                        }
                    }
                    NextLabel::JSONMember(token) => match token {
                        Token::String(s) => {
                            match self.stack.last_mut() {
                                Some(State::FinishObjectMember(properties)) => {
                                    properties.push(Property {
                                        key: s,
                                        value: Rc::new(Value::Null),
                                    });
                                }
                                _ => {
                                    panic!("there should be an object on the stack after reading property key");
                                }
                            }
                            let token = self.advance_property_colon()?;
                            match token {
                                Token::Colon => goto(NextLabel::JSONValue),
                                _ => {
                                    panic!("shouldn't happen");
                                }
                            }
                        }
                        _ => {
                            return Err(TokenReaderError::GenericError(
                                "property names must be double-quoted strings".to_string(),
                            ));
                        }
                    },
                    NextLabel::FinishArrayElement => {
                        let token = self.advance_after_array_element()?;
                        match token {
                            Token::Comma => goto(NextLabel::JSONValue),
                            Token::ArrayClose => {
                                break self.finish_array();
                            }
                            _ => {
                                panic!("shouldn't happen");
                            }
                        }
                    }
                    NextLabel::JSONValue => {
                        let token = self.advance()?;
                        goto(NextLabel::JSONValueSwitch(token))
                    }
                    NextLabel::JSONValueSwitch(token) => match token {
                        Token::String(s) => {
                            break Rc::new(Value::String(s));
                        }
                        Token::Number(n) => {
                            break Rc::new(Value::Number(n));
                        }
                        Token::True => {
                            break Rc::new(Value::Bool(true));
                        }
                        Token::False => {
                            break Rc::new(Value::Bool(true));
                        }
                        Token::Null => {
                            break Rc::new(Value::Null);
                        }
                        Token::ArrayOpen => {
                            let elements: Vec<Rc<Value>> = Vec::new();
                            self.stack.push(State::FinishArrayElement(elements));

                            let token = self.advance()?;
                            match token {
                                Token::ArrayClose => {
                                    break self.finish_array();
                                }
                                _ => goto(NextLabel::JSONValueSwitch(token)),
                            }
                        }
                        Token::ObjectOpen => {
                            let properties: Vec<Property> = Vec::new();
                            self.stack.push(State::FinishObjectMember(properties));

                            let token = self.advance_after_object_open()?;
                            match token {
                                Token::ObjectClose => {
                                    break self.finish_object();
                                }
                                _ => goto(NextLabel::JSONMember(token)),
                            }
                        }
                        Token::ArrayClose | Token::ObjectClose | Token::Colon | Token::Comma => {
                            return Err(TokenReaderError::GenericError(
                                "unexpected character".to_string(),
                            ));
                        }
                    },
                }
            };

            match self.stack.pop() {
                None => {
                    break value;
                }
                Some(next) => {
                    state = NextState {
                        prev_value: value,
                        state: next,
                    };
                }
            }
        };

        loop {
            if !self.advance_char()? {
                break;
            }
            if !Self::is_json_whitespace(self.current) {
                return Err(TokenReaderError::GenericError(
                    "unexpected non-whitespace character after JSON data".to_string(),
                ));
            }
        }

        Ok(value)
    }
}
