use std::collections::HashMap;

// Define node types
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
pub trait INode {
    fn node_type(&self) -> NodeType;
    fn value(&self) -> NodeValue;
    fn clone_node(&self) -> Box<dyn INode>;
}

// NullNode implementation
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

    pub fn decode(&mut self, text: String) -> Box<dyn INode> {
        self.text = text;
        return self.parse();
    }

    fn parse(&self) -> Box<dyn INode> {
        let ch: u8 = self.ch();
        match ch {
            b't' | b'f' | b'n' => self.parse_literal(),
            b'[' => self.parse_array(),
            b'{' => self.parse_object(),
            b'"' => self.parse_string(),
            b'\0' => panic!(),
            _ => self.parse_number(),
        }
    }

    fn parse_string(&self) -> Box<dyn INode> {
        todo!()
    }

    fn parse_number(&self) -> Box<dyn INode> {
        todo!()
    }

    fn parse_literal(&self) -> Box<dyn INode> {
        todo!()
    }

    fn parse_array(&self) -> Box<dyn INode> {
        todo!()
    }

    fn parse_object(&self) -> Box<dyn INode> {
        todo!()
    }
}
