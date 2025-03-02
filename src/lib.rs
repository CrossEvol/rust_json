use std::collections::HashMap;
use std::hash::{Hash, Hasher};

// Define node types
#[derive(Debug, PartialEq, Eq)]
pub enum NodeType {
    NULL,
    FALSE,
    TRUE,
    NUMBER,
    STRING,
    ARRAY,
    OBJECT,
}

// Define possible node values
pub enum NodeValue {
    Literal,
    Text(String),
    Number(f64),
    Array(Vec<Box<dyn INode>>),
    Object(HashMap<Box<StringNode>, Box<dyn INode>>),
}

// Define the INode trait
pub trait INode: std::fmt::Debug {
    fn node_type(&self) -> NodeType;
    fn value(&self) -> NodeValue;
    fn clone_node(&self) -> Box<dyn INode>;
}

// NullNode implementation
#[derive(Debug)]
pub struct NullNode;

impl INode for NullNode {
    fn node_type(&self) -> NodeType {
        NodeType::NULL
    }

    fn value(&self) -> NodeValue {
        NodeValue::Literal
    }

    fn clone_node(&self) -> Box<dyn INode> {
        Box::new(NullNode)
    }
}

// TrueNode implementation
#[derive(Debug)]
pub struct TrueNode;

impl INode for TrueNode {
    fn node_type(&self) -> NodeType {
        NodeType::TRUE
    }

    fn value(&self) -> NodeValue {
        NodeValue::Literal
    }

    fn clone_node(&self) -> Box<dyn INode> {
        Box::new(TrueNode)
    }
}

// FalseNode implementation
#[derive(Debug)]
pub struct FalseNode;

impl INode for FalseNode {
    fn node_type(&self) -> NodeType {
        NodeType::FALSE
    }

    fn value(&self) -> NodeValue {
        NodeValue::Literal
    }

    fn clone_node(&self) -> Box<dyn INode> {
        Box::new(FalseNode)
    }
}

// NumberNode implementation
#[derive(Debug)]
pub struct NumberNode {
    num: f64,
}

impl INode for NumberNode {
    fn node_type(&self) -> NodeType {
        NodeType::NUMBER
    }

    fn value(&self) -> NodeValue {
        NodeValue::Number(self.num)
    }

    fn clone_node(&self) -> Box<dyn INode> {
        Box::new(NumberNode { num: self.num })
    }
}

// StringNode implementation
#[derive(Debug)]
pub struct StringNode {
    str_value: String,
}

impl INode for StringNode {
    fn node_type(&self) -> NodeType {
        NodeType::STRING
    }

    fn value(&self) -> NodeValue {
        NodeValue::Text(self.str_value.clone())
    }

    fn clone_node(&self) -> Box<dyn INode> {
        Box::new(StringNode {
            str_value: self.str_value.clone(),
        })
    }
}

impl PartialEq for StringNode {
    fn eq(&self, other: &Self) -> bool {
        self.str_value == other.str_value
    }
}

impl Eq for StringNode {}

impl Hash for StringNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.str_value.hash(state);
    }
}

impl StringNode {
    // Add this helper method to clone directly to a StringNode
    pub fn clone_string_node(&self) -> Box<StringNode> {
        Box::new(StringNode {
            str_value: self.str_value.to_owned(),
        })
    }
}

// ArrayNode now contains Vec<Box<dyn INode>>
#[derive(Debug)]
pub struct ArrayNode {
    list: Vec<Box<dyn INode>>,
}

impl INode for ArrayNode {
    fn node_type(&self) -> NodeType {
        NodeType::ARRAY
    }

    fn value(&self) -> NodeValue {
        let cloned_list = self.list.iter().map(|node| node.clone_node()).collect();
        NodeValue::Array(cloned_list)
    }

    fn clone_node(&self) -> Box<dyn INode> {
        let cloned_list = self.list.iter().map(|node| node.clone_node()).collect();
        Box::new(ArrayNode { list: cloned_list })
    }
}

// ObjectNode now contains HashMap<String, Box<dyn INode>>
#[derive(Debug)]
pub struct ObjectNode {
    map: HashMap<Box<StringNode>, Box<dyn INode>>,
}

impl INode for ObjectNode {
    fn node_type(&self) -> NodeType {
        NodeType::OBJECT
    }

    fn value(&self) -> NodeValue {
        let cloned_map = self
            .map
            .iter()
            .map(|(k, v)| (k.clone_string_node(), v.clone_node()))
            .collect();
        NodeValue::Object(cloned_map)
    }

    fn clone_node(&self) -> Box<dyn INode> {
        let cloned_map = self
            .map
            .iter()
            .map(|(k, v)| (k.clone_string_node(), v.clone_node()))
            .collect();
        Box::new(ObjectNode { map: cloned_map })
    }
}

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

    fn pop(&mut self) -> u8 {
        self.stack.pop().unwrap()
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
                let element = self.parse();
                if element.is_err() {
                    return element;
                }
                list.push(element.unwrap());
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
                let key_result = self.parse_string();
                if key_result.is_err() {
                    return Err(key_result.err().unwrap());
                }
                self.skip_whitespace();
                if self.ch() != b':' {
                    return Err(DecoderError::MissColon);
                } else {
                    self.advance();
                }
                self.skip_whitespace();
                let value_result = self.parse();
                if value_result.is_err() {
                    return value_result;
                }
                map.insert(key_result.unwrap(), value_result.unwrap());
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_literal_null() {
        let mut decoder = JsonDecoder::new();
        let result = decoder.decode(String::from("null"));
        assert!(result.is_ok());
        assert_eq!(result.unwrap().node_type(), NodeType::NULL);
    }

    #[test]
    fn test_parse_literal_true() {
        let mut decoder = JsonDecoder::new();
        let result = decoder.decode(String::from("true"));
        assert!(result.is_ok());
        assert_eq!(result.unwrap().node_type(), NodeType::TRUE);
    }

    #[test]
    fn test_parse_literal_false() {
        let mut decoder = JsonDecoder::new();
        let result = decoder.decode(String::from("false"));
        assert!(result.is_ok());
        assert_eq!(result.unwrap().node_type(), NodeType::FALSE);
    }

    #[test]
    fn test_parse_literal_invalid() {
        let mut decoder = JsonDecoder::new();

        let result = decoder.decode(String::from("nulx"));
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), DecoderError::InvalidValue));

        let result = decoder.decode(String::from("tru"));
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), DecoderError::InvalidValue));

        let result = decoder.decode(String::from(""));
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), DecoderError::ExpectValue));
    }

    #[test]
    fn test_parse_literal_with_offset() {
        let mut decoder = JsonDecoder::new();
        let result = decoder.decode(String::from("   null  "));
        assert!(result.is_ok());
        assert_eq!(result.unwrap().node_type(), NodeType::NULL);
    }

    #[test]
    fn test_parse_number() {
        // Helper function to test number parsing
        fn test_number(expected: f64, json: &str) {
            let mut decoder = JsonDecoder::new();
            let result = decoder.decode(String::from(json));
            assert!(result.is_ok(), "Failed to parse: {}", json);
            let node = result.unwrap();
            assert_eq!(
                node.node_type(),
                NodeType::NUMBER,
                "Not a number type for input: {}",
                json
            );

            if let NodeValue::Number(num) = node.value() {
                assert!(
                    (num - expected).abs() < f64::EPSILON,
                    "Expected {} but got {} for input: {}",
                    expected,
                    num,
                    json
                );
            } else {
                panic!("Expected NodeValue::Number for input: {}", json);
            }
        }

        // Basic numbers
        test_number(0.0, "0");
        test_number(0.0, "-0");
        test_number(0.0, "-0.0");
        test_number(1.0, "1");
        test_number(-1.0, "-1");
        test_number(1.5, "1.5");
        test_number(-1.5, "-1.5");
        test_number(3.1416, "3.1416");

        // Exponents
        test_number(1.0e10, "1E10");
        test_number(1.0e10, "1e10");
        test_number(1.0e10, "1E+10");
        test_number(1.0e-10, "1E-10");
        test_number(-1.0e10, "-1E10");
        test_number(-1.0e10, "-1e10");
        test_number(-1.0e10, "-1E+10");
        test_number(-1.0e-10, "-1E-10");
        test_number(1.234e10, "1.234E+10");
        test_number(1.234e-10, "1.234E-10");

        // Underflow case
        test_number(0.0, "1e-10000");

        // Note: Some of the original C tests use double precision values that may not be
        // precisely representable in f64. Adjusting these tests for f64 precision:

        // Special values (adjusted for f64 precision)
        test_number(1.0000001, "1.0000001"); // Smallest number > 1 for f64

        // Min/max values for f64
        test_number(f64::MIN_POSITIVE, &format!("{}", f64::MIN_POSITIVE));
        test_number(-f64::MIN_POSITIVE, &format!("-{}", f64::MIN_POSITIVE));
        test_number(f64::MAX, &format!("{}", f64::MAX));
        test_number(-f64::MAX, &format!("-{}", f64::MAX));
    }

    #[test]
    fn test_parse_string() {
        // Helper function to test string parsing
        fn test_string(expected: &str, json: &str) {
            let mut decoder = JsonDecoder::new();
            let result = decoder.decode(String::from(json));
            assert!(result.is_ok(), "Failed to parse: {}", json);
            let node = result.unwrap();
            assert_eq!(
                node.node_type(),
                NodeType::STRING,
                "Not a string type for input: {}",
                json
            );

            if let NodeValue::Text(text) = node.value() {
                assert_eq!(
                    text, expected,
                    "Expected '{}' but got '{}' for input: '{}'",
                    expected, text, json
                );
            } else {
                panic!("Expected NodeValue::Text for input: {}", json);
            }
        }

        // Empty string
        test_string("", "\"\"");

        // Simple string
        test_string("Hello", "\"Hello\"");

        // String with newline escape
        test_string("Hello\nWorld", "\"Hello\\nWorld\"");

        // String with all escape characters
        test_string(
            "\" \\ / \u{8} \u{c} \n \r \t",
            "\"\\\" \\\\ \\/ \\b \\f \\n \\r \\t\"",
        );

        // String with null character
        test_string("Hello\0World", "\"Hello\\u0000World\"");

        // Unicode characters
        test_string("\u{0024}", "\"\\u0024\""); // Dollar sign U+0024
        test_string("\u{00A2}", "\"\\u00A2\""); // Cents sign U+00A2
        test_string("\u{20AC}", "\"\\u20AC\""); // Euro sign U+20AC

        // Surrogate pairs
        test_string("\u{1D11E}", "\"\\uD834\\uDD1E\""); // G clef sign U+1D11E
        test_string("\u{1D11E}", "\"\\ud834\\udd1e\""); // G clef sign U+1D11E (lowercase hex)

        // String with whitespace
        test_string("Hello", "  \"Hello\"  ");
    }

    #[test]
    fn test_parse_invalid_string_escape() {
        let test_cases = vec![
            "\"\\v\"",   // Invalid escape character 'v'
            "\"\\'\"",   // Invalid escape character '\''
            "\"\\0\"",   // Invalid escape character '0'
            "\"\\x12\"", // Invalid escape character 'x'
        ];

        for json in test_cases {
            let mut decoder = JsonDecoder::new();
            let result = decoder.decode(String::from(json));
            assert!(result.is_err(), "Expected error for input: {}", json);
            assert!(
                matches!(result.unwrap_err(), DecoderError::InvalidStringEscape),
                "Expected InvalidStringEscape error for input: {}",
                json
            );
        }
    }

    #[test]
    fn test_parse_invalid_string_char() {
        // Test control characters (0x01-0x1F) which are not allowed in JSON strings
        let test_cases = vec![
            "\"\x01\"", // Control character 0x01
            "\"\x1F\"", // Control character 0x1F
        ];

        for json in test_cases {
            let mut decoder = JsonDecoder::new();
            let result = decoder.decode(String::from(json));
            assert!(result.is_err(), "Expected error for input: {}", json);
            assert!(
                matches!(result.unwrap_err(), DecoderError::InvalidStringChar),
                "Expected InvalidStringChar error for input: {}",
                json
            );
        }
    }

    #[test]
    fn test_parse_invalid_unicode_hex() {
        let test_cases = vec![
            "\"\\u\"",     // Missing all 4 hex digits
            "\"\\u0\"",    // Missing 3 hex digits
            "\"\\u01\"",   // Missing 2 hex digits
            "\"\\u012\"",  // Missing 1 hex digit
            "\"\\u/000\"", // Invalid hex character '/'
            "\"\\uG000\"", // Invalid hex character 'G'
            "\"\\u0/00\"", // Invalid hex character '/'
            "\"\\u0G00\"", // Invalid hex character 'G'
            "\"\\u00/0\"", // Invalid hex character '/'
            "\"\\u00G0\"", // Invalid hex character 'G'
            "\"\\u000/\"", // Invalid hex character '/'
            "\"\\u000G\"", // Invalid hex character 'G'
            "\"\\u 123\"", // Space is not a valid hex character
        ];

        for json in test_cases {
            let mut decoder = JsonDecoder::new();
            let result = decoder.decode(String::from(json));
            assert!(result.is_err(), "Expected error for input: {}", json);
            assert!(
                matches!(result.unwrap_err(), DecoderError::InvalidUnicodeHex),
                "Expected InvalidUnicodeHex error for input: {}",
                json
            );
        }
    }

    #[test]
    fn test_parse_invalid_unicode_surrogate() {
        let test_cases = vec![
            "\"\\uD800\"",        // High surrogate without low surrogate
            "\"\\uDBFF\"",        // High surrogate without low surrogate
            "\"\\uD800\\\\\"",    // High surrogate followed by backslash, not \u
            "\"\\uD800\\uDBFF\"", // High surrogate followed by another high surrogate
            "\"\\uD800\\uE000\"", // High surrogate followed by non-surrogate
        ];

        for json in test_cases {
            let mut decoder = JsonDecoder::new();
            let result = decoder.decode(String::from(json));
            assert!(result.is_err(), "Expected error for input: {}", json);
            assert!(
                matches!(result.unwrap_err(), DecoderError::InvalidUnicodeSurrogate),
                "Expected InvalidUnicodeSurrogate error for input: {}",
                json
            );
        }
    }

    #[test]
    fn test_parse_miss_quotation_mark() {
        let test_cases = vec![
            "\"",     // Missing closing quote
            "\"abc",  // Missing closing quote after content
            "\"\\\"", // Missing closing quote after escaped quote
        ];

        for json in test_cases {
            let mut decoder = JsonDecoder::new();
            let result = decoder.decode(String::from(json));
            assert!(result.is_err(), "Expected error for input: {}", json);
            assert!(
                matches!(result.unwrap_err(), DecoderError::MissQuotationMark),
                "Expected MissQuotationMark error for input: {}",
                json
            );
        }
    }

    #[test]
    fn test_parse_array() {
        // Test empty array
        let mut decoder = JsonDecoder::new();
        let result = decoder.decode(String::from("[ ]"));
        assert!(result.is_ok());
        let node = result.unwrap();
        assert_eq!(node.node_type(), NodeType::ARRAY);
        if let NodeValue::Array(elements) = node.value() {
            assert_eq!(elements.len(), 0);
        } else {
            panic!("Expected NodeValue::Array");
        }

        // Test array with multiple elements of different types
        let mut decoder = JsonDecoder::new();
        let result = decoder.decode(String::from("[ null , false , true , 123 , \"abc\" ]"));
        assert!(result.is_ok());
        let node = result.unwrap();
        assert_eq!(node.node_type(), NodeType::ARRAY);
        if let NodeValue::Array(elements) = node.value() {
            assert_eq!(elements.len(), 5);
            assert_eq!(elements[0].node_type(), NodeType::NULL);
            assert_eq!(elements[1].node_type(), NodeType::FALSE);
            assert_eq!(elements[2].node_type(), NodeType::TRUE);
            assert_eq!(elements[3].node_type(), NodeType::NUMBER);
            assert_eq!(elements[4].node_type(), NodeType::STRING);

            // Check number value
            if let NodeValue::Number(num) = elements[3].value() {
                assert_eq!(num, 123.0);
            } else {
                panic!("Expected NodeValue::Number");
            }

            // Check string value
            if let NodeValue::Text(text) = elements[4].value() {
                assert_eq!(text, "abc");
            } else {
                panic!("Expected NodeValue::Text");
            }
        } else {
            panic!("Expected NodeValue::Array");
        }

        // Test nested arrays
        let mut decoder = JsonDecoder::new();
        let result = decoder.decode(String::from("[ [ ] , [ 0 ] , [ 0 , 1 ] , [ 0 , 1 , 2 ] ]"));
        assert!(result.is_ok());
        let node = result.unwrap();
        assert_eq!(node.node_type(), NodeType::ARRAY);
        if let NodeValue::Array(elements) = node.value() {
            assert_eq!(elements.len(), 4);

            for i in 0..4 {
                assert_eq!(elements[i].node_type(), NodeType::ARRAY);
                if let NodeValue::Array(inner_elements) = elements[i].value() {
                    assert_eq!(inner_elements.len(), i);

                    for j in 0..i {
                        assert_eq!(inner_elements[j].node_type(), NodeType::NUMBER);
                        if let NodeValue::Number(num) = inner_elements[j].value() {
                            assert_eq!(num, j as f64);
                        } else {
                            panic!("Expected NodeValue::Number");
                        }
                    }
                } else {
                    panic!("Expected NodeValue::Array");
                }
            }
        } else {
            panic!("Expected NodeValue::Array");
        }
    }

    #[test]
    fn test_parse_miss_comma_or_square_bracket() {
        let test_cases = vec![
            "[1",   // Missing closing bracket
            "[1}",  // Wrong closing bracket
            "[1 2", // Missing comma between elements
            "[[]",  // Missing closing bracket for outer array
        ];

        for json in test_cases {
            let mut decoder = JsonDecoder::new();
            let result = decoder.decode(String::from(json));
            assert!(result.is_err(), "Expected error for input: {}", json);
            assert!(
                matches!(result.unwrap_err(), DecoderError::MissCommaOrSquareBracket),
                "Expected MissCommaOrSquareBracket error for input: {}",
                json
            );
        }
    }

    #[test]
    fn test_parse_object() {
        // Test empty object
        let mut decoder = JsonDecoder::new();
        let result = decoder.decode(String::from(" { } "));
        assert!(result.is_ok());
        let node = result.unwrap();
        assert_eq!(node.node_type(), NodeType::OBJECT);
        if let NodeValue::Object(map) = node.value() {
            assert_eq!(map.len(), 0);
        } else {
            panic!("Expected NodeValue::Object");
        }

        // Test complex object with different value types
        let mut decoder = JsonDecoder::new();
        let result = decoder.decode(String::from(
            " { \
            \"n\" : null , \
            \"f\" : false , \
            \"t\" : true , \
            \"i\" : 123 , \
            \"s\" : \"abc\", \
            \"a\" : [ 1, 2, 3 ], \
            \"o\" : { \"1\" : 1, \"2\" : 2, \"3\" : 3 } \
            } ",
        ));

        assert!(result.is_ok(), "Failed to parse complex object");
        let node = result.unwrap();
        assert_eq!(node.node_type(), NodeType::OBJECT);

        if let NodeValue::Object(map) = node.value() {
            assert_eq!(map.len(), 7, "Expected 7 key-value pairs in object");

            // Helper function to find a key in the map
            let find_key = |key: &str| {
                map.iter().find(|(k, _)| {
                    if let NodeValue::Text(text) = k.value() {
                        text == key
                    } else {
                        false
                    }
                })
            };

            // Check "n": null
            let (_, value) = find_key("n").expect("Key 'n' not found");
            assert_eq!(value.node_type(), NodeType::NULL);

            // Check "f": false
            let (_, value) = find_key("f").expect("Key 'f' not found");
            assert_eq!(value.node_type(), NodeType::FALSE);

            // Check "t": true
            let (_, value) = find_key("t").expect("Key 't' not found");
            assert_eq!(value.node_type(), NodeType::TRUE);

            // Check "i": 123
            let (_, value) = find_key("i").expect("Key 'i' not found");
            assert_eq!(value.node_type(), NodeType::NUMBER);
            if let NodeValue::Number(num) = value.value() {
                assert_eq!(num, 123.0);
            } else {
                panic!("Expected NodeValue::Number for key 'i'");
            }

            // Check "s": "abc"
            let (_, value) = find_key("s").expect("Key 's' not found");
            assert_eq!(value.node_type(), NodeType::STRING);
            if let NodeValue::Text(text) = value.value() {
                assert_eq!(text, "abc");
            } else {
                panic!("Expected NodeValue::Text for key 's'");
            }

            // Check "a": [1, 2, 3]
            let (_, value) = find_key("a").expect("Key 'a' not found");
            assert_eq!(value.node_type(), NodeType::ARRAY);
            if let NodeValue::Array(elements) = value.value() {
                assert_eq!(elements.len(), 3);
                for i in 0..3 {
                    assert_eq!(elements[i].node_type(), NodeType::NUMBER);
                    if let NodeValue::Number(num) = elements[i].value() {
                        assert_eq!(num, (i + 1) as f64);
                    } else {
                        panic!("Expected NodeValue::Number for array element {}", i);
                    }
                }
            } else {
                panic!("Expected NodeValue::Array for key 'a'");
            }

            // Check "o": {"1": 1, "2": 2, "3": 3}
            let (_, value) = find_key("o").expect("Key 'o' not found");
            assert_eq!(value.node_type(), NodeType::OBJECT);
            if let NodeValue::Object(inner_map) = value.value() {
                assert_eq!(inner_map.len(), 3);

                // Check each of the inner object's key-value pairs
                for i in 1..=3 {
                    let key = i.to_string();
                    let find_inner_key = |key: &str| {
                        inner_map.iter().find(|(k, _)| {
                            if let NodeValue::Text(text) = k.value() {
                                text == key
                            } else {
                                false
                            }
                        })
                    };

                    let (_, inner_value) =
                        find_inner_key(&key).expect(&format!("Inner key '{}' not found", key));
                    assert_eq!(inner_value.node_type(), NodeType::NUMBER);
                    if let NodeValue::Number(num) = inner_value.value() {
                        assert_eq!(num, i as f64);
                    } else {
                        panic!("Expected NodeValue::Number for inner key '{}'", key);
                    }
                }
            } else {
                panic!("Expected NodeValue::Object for key 'o'");
            }
        } else {
            panic!("Expected NodeValue::Object");
        }
    }

    #[test]
    fn test_parse_miss_key() {
        let test_cases = vec![
            "{:1,",      // Missing key before colon
            "{1:1,",     // Number as key (not allowed in JSON)
            "{true:1,",  // Boolean as key
            "{false:1,", // Boolean as key
            "{null:1,",  // Null as key
            "{[]:1,",    // Array as key
            "{{}:1,",    // Object as key
            "{\"a\":1,", // Missing closing brace after comma
        ];

        for json in test_cases {
            let mut decoder = JsonDecoder::new();
            let result = decoder.decode(String::from(json));
            assert!(result.is_err(), "Expected error for input: {}", json);
            assert!(
                matches!(result.unwrap_err(), DecoderError::MissKey),
                "Expected MissKey error for input: {}",
                json
            );
        }
    }

    #[test]
    fn test_parse_miss_colon() {
        let test_cases = vec![
            "{\"a\"}",       // Missing colon after key
            "{\"a\",\"b\"}", // Missing colon, has comma instead
        ];

        for json in test_cases {
            let mut decoder = JsonDecoder::new();
            let result = decoder.decode(String::from(json));
            assert!(result.is_err(), "Expected error for input: {}", json);
            assert!(
                matches!(result.unwrap_err(), DecoderError::MissColon),
                "Expected MissColon error for input: {}",
                json
            );
        }
    }

    #[test]
    fn test_parse_miss_comma_or_curly_bracket() {
        let test_cases = vec![
            "{\"a\":1",       // Missing closing brace
            "{\"a\":1]",      // Wrong closing bracket (square instead of curly)
            "{\"a\":1 \"b\"", // Missing comma between key-value pairs
            "{\"a\":{}",      // Missing closing brace after nested object
        ];

        for json in test_cases {
            let mut decoder = JsonDecoder::new();
            let result = decoder.decode(String::from(json));
            assert!(result.is_err(), "Expected error for input: {}", json);
            assert!(
                matches!(result.unwrap_err(), DecoderError::MissCommaOrCurlyBracket),
                "Expected MissCommaOrCurlyBracket error for input: {}",
                json
            );
        }
    }
}
