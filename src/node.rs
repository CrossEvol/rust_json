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
    pub num: f64,
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
    pub str_value: String,
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
    pub list: Vec<Box<dyn INode>>,
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
    pub map: HashMap<Box<StringNode>, Box<dyn INode>>,
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
