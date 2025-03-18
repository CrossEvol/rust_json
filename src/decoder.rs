use std::collections::HashMap;

use crate::node::{
    ArrayNode, FalseNode, INode, NullNode, NumberNode, ObjectNode, StringNode, TrueNode,
};

enum Literal {
    NULL,
    TRUE,
    FALSE,
}

#[derive(Debug)]
pub enum DecoderError {
    ExpectValue,
    InvalidValue,
    RootNotSingular,
    NumberTooBig,
    MissQuotationMark,
    InvalidStringEscape,
    InvalidStringChar,
    InvalidUnicodeHex,
    InvalidUnicodeSurrogate,
    InvalidUtf8Sequence,
    MissCommaOrSquareBracket,
    MissKey,
    MissColon,
    MissCommaOrCurlyBracket,
}

#[derive(Debug)]
pub struct JsonDecoder {
    stack: Vec<u8>,
    text: String,
    pos: usize,
}

impl JsonDecoder {
    pub fn new() -> JsonDecoder {
        JsonDecoder {
            stack: vec![],
            text: String::from(""),
            pos: 0,
        }
    }

    fn expect(&mut self, ch: u8) {
        assert!(self.ch() == ch);
        self.advance();
    }

    fn advance(&mut self) {
        self.pos += 1
    }

    fn is_digit_1_to_9(&self) -> bool {
        return self.ch().is_ascii_digit() && self.ch() != b'0';
    }

    fn is_digit(&self) -> bool {
        return self.ch().is_ascii_digit();
    }

    fn eat(&mut self) {
        self.push(self.ch());
        self.advance();
    }

    fn push(&mut self, ch: u8) {
        self.stack.push(ch);
    }

    fn ch(&self) -> u8 {
        self.text.as_bytes().get(self.pos).copied().unwrap_or(b'\0')
    }

    pub fn decode(&mut self, text: String) -> Result<Box<dyn INode>, DecoderError> {
        self.text = text;
        self.skip_whitespace();
        let res = self.parse();
        match res {
            Ok(node) => {
                self.skip_whitespace();
                if self.ch() != b'\0' {
                    Err(DecoderError::RootNotSingular)
                } else {
                    Ok(node)
                }
            }
            Err(err) => Err(err),
        }
    }

    fn parse(&mut self) -> Result<Box<dyn INode>, DecoderError> {
        let ch: u8 = self.ch();
        match ch {
            b't' => self.parse_literal(Literal::TRUE),
            b'f' => self.parse_literal(Literal::FALSE),
            b'n' => self.parse_literal(Literal::NULL),
            b'[' => self.parse_array(),
            b'{' => self.parse_object(),
            b'"' => {
                // Convert Box<StringNode> to Box<dyn INode>
                let string_result = self.parse_string();
                string_result.map(|node| node as Box<dyn INode>)
            }
            b'\0' => Err(DecoderError::ExpectValue),
            _ => self.parse_number(),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch().is_ascii_whitespace() {
            self.advance();
        }
    }

    fn parse_string(&mut self) -> Result<Box<StringNode>, DecoderError> {
        self.expect(b'"');
        loop {
            match self.ch() {
                b'"' => {
                    self.advance();
                    let str_value = String::from_utf8(std::mem::take(&mut self.stack))
                        .map_err(|_| DecoderError::InvalidUtf8Sequence)?;
                    return Ok(Box::new(StringNode { str_value }));
                }
                b'\\' => {
                    self.advance();
                    let ch = self.ch();
                    self.advance();
                    match ch {
                        b'"' => self.push(b'"'),
                        b'\\' => self.push(b'\\'),
                        b'/' => self.push(b'/'),
                        b'b' => self.push(0x08),
                        b'f' => self.push(0x0C),
                        b'n' => self.push(b'\n'),
                        b'r' => self.push(b'\r'),
                        b't' => self.push(b'\t'),
                        b'u' => {
                            let u32 = self.parse_hex4().ok_or(DecoderError::InvalidUnicodeHex)?;
                            let u = if (0xD800..=0xDBFF).contains(&u32) {
                                if self.ch() != b'\\' {
                                    return Err(DecoderError::InvalidUnicodeSurrogate);
                                }
                                self.advance();
                                if self.ch() != b'u' {
                                    return Err(DecoderError::InvalidUnicodeSurrogate);
                                }
                                self.advance();
                                let u32_2 =
                                    self.parse_hex4().ok_or(DecoderError::InvalidUnicodeHex)?;
                                if !(0xDC00..=0xDFFF).contains(&u32_2) {
                                    return Err(DecoderError::InvalidUnicodeSurrogate);
                                }
                                (((u32 - 0xD800) << 10) | (u32_2 - 0xDC00)) + 0x10000
                            } else {
                                u32
                            };
                            self.encode_utf(u);
                        }
                        _ => return Err(DecoderError::InvalidStringEscape),
                    }
                }
                b'\0' => return Err(DecoderError::MissQuotationMark),
                ch if ch < 0x20 => return Err(DecoderError::InvalidStringChar),
                _ => self.eat(),
            }
        }
    }

    fn parse_hex4(&mut self) -> Option<u32> {
        let mut u: u32 = 0;
        for _ in 0..4 {
            let ch = self.ch();
            self.advance();
            u <<= 4;
            match ch {
                b'0'..=b'9' => u |= (ch - b'0') as u32,
                b'A'..=b'F' => u |= (ch - (b'A' - 10)) as u32,
                b'a'..=b'f' => u |= (ch - (b'a' - 10)) as u32,
                _ => return None,
            }
        }
        Some(u)
    }

    fn encode_utf(&mut self, u: u32) {
        match u {
            0..=0x7F => {
                self.push((u & 0xFF) as u8);
            }
            0x80..=0x7FF => {
                self.push((0xC0 | ((u >> 6) & 0xFF)) as u8);
                self.push((0x80 | (u & 0x3F)) as u8);
            }
            0x800..=0xFFFF => {
                self.push((0xE0 | ((u >> 12) & 0xFF)) as u8);
                self.push((0x80 | ((u >> 6) & 0x3F)) as u8);
                self.push((0x80 | (u & 0x3F)) as u8);
            }
            0x10000..=0x10FFFF => {
                self.push((0xF0 | ((u >> 18) & 0xFF)) as u8);
                self.push((0x80 | ((u >> 12) & 0x3F)) as u8);
                self.push((0x80 | ((u >> 6) & 0x3F)) as u8);
                self.push((0x80 | (u & 0x3F)) as u8);
            }
            _ => panic!("Invalid Unicode code point: {}", u),
        }
    }

    fn parse_number(&mut self) -> Result<Box<dyn INode>, DecoderError> {
        // Clear the stack before parsing a new number
        self.stack.clear();

        if self.ch() == b'-' {
            self.eat();
        }
        if self.is_digit() {
            if self.ch() == b'0' {
                self.eat();
            } else {
                if !self.is_digit_1_to_9() {
                    return Err(DecoderError::InvalidValue);
                }
                while self.is_digit() {
                    self.eat();
                }
            }
            if self.ch() == b'.' {
                self.eat();
                if !self.is_digit() {
                    return Err(DecoderError::InvalidValue);
                }
                while self.is_digit() {
                    self.eat();
                }
            }
            if self.ch() == b'e' || self.ch() == b'E' {
                self.eat();
                if self.ch() == b'+' || self.ch() == b'-' {
                    self.eat();
                }
                if !self.is_digit() {
                    return Err(DecoderError::InvalidValue);
                }
                while self.is_digit() {
                    self.eat();
                }
            }

            // Use the stack directly without cloning
            let number_string = String::from_utf8(std::mem::take(&mut self.stack))
                .expect("Invalid UTF-8 sequence when parse_number");
            let number: f64 = number_string
                .parse()
                .expect("Unable to parse string to number");
            Ok(Box::new(NumberNode { num: number }))
        } else {
            Err(DecoderError::InvalidValue)
        }
    }

    fn parse_literal(&mut self, literal: Literal) -> Result<Box<dyn INode>, DecoderError> {
        let literal_string: &[u8] = match literal {
            Literal::NULL => b"null",
            Literal::TRUE => b"true",
            Literal::FALSE => b"false",
        };

        let mut pos = self.pos;
        for index in 0..literal_string.len() {
            if self.text.as_bytes().get(pos).copied().unwrap_or(b'\0') != literal_string[index] {
                return Err(DecoderError::InvalidValue);
            }
            pos += 1;
        }

        self.pos += literal_string.len();
        match literal {
            Literal::NULL => Ok(Box::new(NullNode {})),
            Literal::TRUE => Ok(Box::new(TrueNode {})),
            Literal::FALSE => Ok(Box::new(FalseNode {})),
        }
    }

    fn parse_array(&mut self) -> Result<Box<dyn INode>, DecoderError> {
        self.expect(b'[');
        self.skip_whitespace();
        if self.ch() == b']' {
            self.advance();
            Ok(Box::new(ArrayNode { list: vec![] }))
        } else {
            let mut list: Vec<Box<dyn INode>> = vec![];
            loop {
                let element = self.parse()?;
                list.push(element);
                self.skip_whitespace();
                if self.ch() == b',' {
                    self.advance();
                    self.skip_whitespace();
                } else if self.ch() == b']' {
                    self.advance();
                    return Ok(Box::new(ArrayNode { list }));
                } else {
                    return Err(DecoderError::MissCommaOrSquareBracket);
                }
            }
        }
    }

    fn parse_object(&mut self) -> Result<Box<dyn INode>, DecoderError> {
        self.expect(b'{');
        self.skip_whitespace();
        if self.ch() == b'}' {
            self.advance();
            Ok(Box::new(ObjectNode {
                map: HashMap::new(),
            }))
        } else {
            let mut map = HashMap::new();
            loop {
                if self.ch() != b'"' {
                    return Err(DecoderError::MissKey);
                }
                let key_node = self.parse_string()?;
                self.skip_whitespace();
                if self.ch() != b':' {
                    return Err(DecoderError::MissColon);
                } else {
                    self.advance();
                }
                self.skip_whitespace();
                let value_node = self.parse()?;
                map.insert(key_node, value_node);
                self.skip_whitespace();
                if self.ch() == b',' {
                    self.advance();
                    self.skip_whitespace();
                } else if self.ch() == b'}' {
                    self.advance();
                    return Ok(Box::new(ObjectNode { map }));
                } else {
                    return Err(DecoderError::MissCommaOrCurlyBracket);
                }
            }
        }
    }
}
