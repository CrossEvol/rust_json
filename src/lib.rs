use std::{collections::HashMap, fmt::Error, string::ParseError};

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
    Number(f32),
    Array(Vec<Box<dyn INode>>),
    Object(HashMap<String, Box<dyn INode>>),
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
    num: f32,
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
    map: HashMap<String, Box<dyn INode>>,
}

impl INode for ObjectNode {
    fn node_type(&self) -> NodeType {
        NodeType::OBJECT
    }

    fn value(&self) -> NodeValue {
        let cloned_map = self
            .map
            .iter()
            .map(|(k, v)| (k.clone(), v.clone_node()))
            .collect();
        NodeValue::Object(cloned_map)
    }

    fn clone_node(&self) -> Box<dyn INode> {
        let cloned_map = self
            .map
            .iter()
            .map(|(k, v)| (k.clone(), v.clone_node()))
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
        self.parse_whitespace();
        let res = self.parse();
        match res {
            Ok(node) => {
                self.parse_whitespace();
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
            b'"' => self.parse_string(),
            b'\0' => Err(DecoderError::ExpectValue),
            _ => self.parse_number(),
        }
    }

    fn parse_whitespace(&mut self) {
        while self.ch().is_ascii_whitespace() {
            self.pos += 1
        }
    }

    fn parse_string(&self) -> Result<Box<dyn INode>, DecoderError> {
        todo!()
    }

    fn parse_number(&self) -> Result<Box<dyn INode>, DecoderError> {
        todo!()
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

    fn parse_array(&self) -> Result<Box<dyn INode>, DecoderError> {
        todo!()
    }

    fn parse_object(&self) -> Result<Box<dyn INode>, DecoderError> {
        todo!()
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
}
