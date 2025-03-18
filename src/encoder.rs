use crate::node::{INode, NodeType, NodeValue, StringNode};

#[derive(Debug)]
pub struct JsonEncoder {
    stack: Vec<u8>,
}

const HEX_DIGITS: [u8; 16] = [
    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E', b'F',
];

impl JsonEncoder {
    pub fn new() -> JsonEncoder {
        JsonEncoder { stack: vec![] }
    }

    pub fn encode(&mut self, node: &Box<dyn INode>) -> String {
        self.stringify_inode(node);
        String::from_utf8(std::mem::take(&mut self.stack)).unwrap()
    }

    fn push(&mut self, ch: u8) {
        self.stack.push(ch);
    }

    fn pop(&mut self) {
        self.stack.pop();
    }

    fn put_string(&mut self, str: String) {
        for ch in str.as_bytes() {
            self.push(*ch);
        }
    }

    fn format_double_to_string(&self, value: f64) -> String {
        // Handle special cases first
        if value == 0.0 {
            return "0".to_string();
        }

        if value.is_infinite() || value.is_nan() {
            // JSON doesn't support Infinity or NaN, but we'll handle them anyway
            return "null".to_string();
        }

        // For very small or very large numbers, use scientific notation
        if value.abs() < 1e-6 || value.abs() >= 1e10 {
            // Use scientific notation with enough precision
            let s = format!("{:.16e}", value);

            // Normalize the exponent format to match JSON standard
            if s.contains('e') {
                let parts: Vec<&str> = s.split('e').collect();
                let base = parts[0].trim_end_matches('0').trim_end_matches('.');
                let exp = parts[1];

                // Format the exponent with explicit + sign if positive
                let exp_value = exp.parse::<i32>().unwrap_or(0);
                let formatted_exp = if exp_value >= 0 {
                    format!("e+{}", exp_value)
                } else {
                    format!("e{}", exp_value)
                };

                return format!("{}{}", base, formatted_exp);
            }
            return s;
        } else {
            // For regular numbers, use decimal notation with enough precision
            let s = format!("{:.16}", value);

            // Trim trailing zeros and decimal point if it becomes an integer
            s.trim_end_matches('0').trim_end_matches('.').to_string()
        }
    }

    fn stringify_inode(&mut self, node: &Box<dyn INode>) {
        match node.as_ref().node_type() {
            NodeType::NULL => self.put_string(String::from("null")),
            NodeType::FALSE => self.put_string(String::from("false")),
            NodeType::TRUE => self.put_string(String::from("true")),
            NodeType::NUMBER => {
                let node_value = node.value();
                if let NodeValue::Number(num) = node_value {
                    let num_string = self.format_double_to_string(num);
                    self.put_string(num_string);
                }
            }
            NodeType::STRING => {
                let node_value = node.value();
                if let NodeValue::Text(text) = node_value {
                    self.stringify_string_node(&Box::new(StringNode { str_value: text }));
                } else {
                    panic!();
                }
            }
            NodeType::ARRAY => {
                self.push(b'[');
                let node_value = node.value();
                if let NodeValue::Array(elements) = node_value {
                    let size = elements.len();
                    for ele in elements {
                        self.stringify_inode(&ele);
                        self.push(b',');
                    }
                    if size > 0 {
                        self.pop();
                    }
                } else {
                    panic!();
                }
                self.push(b']');
            }
            NodeType::OBJECT => {
                self.push(b'{');
                let node_value = node.value();
                if let NodeValue::Object(map) = node_value {
                    let size = map.len();
                    for (key, value) in map.iter() {
                        self.stringify_string_node(key);
                        self.push(b':');
                        self.stringify_inode(value);
                        self.push(b',');
                    }
                    if size > 0 {
                        self.pop();
                    }
                }
                self.push(b'}');
            }
        }
    }

    fn stringify_string_node(&mut self, node: &Box<StringNode>) {
        self.push(b'"');
        for byte in node.str_value.as_bytes() {
            match byte {
                b'\"' => {
                    self.push(b'\\');
                    self.push(b'\"');
                }
                b'\\' => {
                    self.push(b'\\');
                    self.push(b'\\');
                }
                0x08 => {
                    self.push(b'\\');
                    self.push(b'b');
                }
                0x0c => {
                    self.push(b'\\');
                    self.push(b'f');
                }
                b'\n' => {
                    self.push(b'\\');
                    self.push(b'n');
                }
                b'\r' => {
                    self.push(b'\\');
                    self.push(b'r');
                }
                b'\t' => {
                    self.push(b'\\');
                    self.push(b't');
                }
                _ => {
                    if *byte < 0x20 {
                        self.push(b'\\');
                        self.push(b'u');
                        self.push(b'0');
                        self.push(b'0');
                        self.push(HEX_DIGITS[(*byte >> 4) as usize]);
                        self.push(HEX_DIGITS[(*byte & 15) as usize]);
                    } else {
                        self.push(*byte);
                    }
                }
            }
        }
        self.push(b'"');
    }
}
