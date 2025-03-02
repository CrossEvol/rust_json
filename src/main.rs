use rust_json::JsonDecoder;

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

    let node = decoder.decode(String::from("\"text\""));
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
}
