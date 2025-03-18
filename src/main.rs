use rust_json::{self, decoder::JsonDecoder, encoder::JsonEncoder};

fn main() {
    let ch = '5';

    if ch.is_digit(10) {
        println!("'{}' is a digit", ch);
    }

    let ch = 'a';

    if ch.is_alphabetic() {
        println!("'{}' is an alphabetic character", ch);
    }

    let ch = '1';

    if ch.is_alphanumeric() {
        println!("'{}' is an alphanumeric character", ch);
    }

    let ch = ' ';

    if ch.is_whitespace() {
        println!("'{}' is a whitespace character", ch);
    }

    let mut decoder = JsonDecoder::new();
    println!("{decoder:?}");

    let node = decoder.decode(String::from("[ null , false , true , 123 , \"abc\" ]"));
    println!("{node:?}");

    println!("----------------------------------------");
    let u32: u32 = 255255;
    println!("{}", format!("{:b}", u32));
    let u16: u16 = u32 as u16;
    println!("{}", format!("{:b}", u16));

    let my_u8: u8 = 65;
    let my_u8_2: u8 = 200;

    println!("{}", char::from_u32(my_u8 as u32).unwrap());
    println!("{}", char::from_u32(my_u8_2 as u32).unwrap());

    let small_number = 0.00012345;
    let large_number = 1234567.89;
    println!("Small number in general format: {:.3}", small_number);
    println!("Large number in general format: {:.3}", large_number);

    fn format_double_to_string(value: f64) -> String {
        if value.abs() < 1e-4 || value.abs() >= 1e4 {
            // Use scientific notation for very small or large numbers
            format!("{:.17e}", value)
        } else {
            // Use fixed notation, trim trailing zeros
            let s = format!("{:.17}", value);
            s.trim_end_matches('0').trim_end_matches('.').to_string()
        }
    }

    let values = [123.4567890123456789, 0.000000123, 1234567890.123456];
    for &v in &values {
        let result = format_double_to_string(v);
        println!("{}", result);
    }

    let mut decoder = JsonDecoder::new();
    let res = decoder.decode("0".to_owned()).unwrap();
    let mut encoder = JsonEncoder::new();
    let res = encoder.encode(&res);
    println!("{res:?}");

    // Example usage:
}
