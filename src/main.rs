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

    let decoder = JsonDecoder::new();
    println!("{decoder:?}");
}
