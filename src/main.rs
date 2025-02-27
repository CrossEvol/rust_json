use rust_json::{JsonDecoder, NullNode};

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

    let nullNode = NullNode {};
    println!("{nullNode:?}");

    let max_f32 = f32::MAX;
    let min_f32 = f32::MIN;
    let min_positive_f32 = f32::MIN_POSITIVE;

    println!("Max f32: {:.16e}", max_f32);
    println!("Min f32: {:.16e}", min_f32);
    println!("Min positive f32: {:.16e}", min_positive_f32);

    let max_f64 = f64::MAX;
    let min_f64 = f64::MIN;
    let min_positive_f64 = f64::MIN_POSITIVE;

    println!("Max f64: {:.16e}", max_f64);
    println!("Min f64: {:.16e}", min_f64);
    println!("Min positive f64: {:.16e}", min_positive_f64);

    // Sample Vec<u8>
    let vec: Vec<u8> = vec![49, 50, 51, 52]; // Represents the bytes for "1234"

    // Convert Vec<u8> to String
    let string = String::from_utf8(vec).expect("Invalid UTF-8 sequence");

    // Convert String to number (in this case, u32)
    let number: f32 = string.parse().expect("Unable to parse string to number");

    println!("The number is: {}", number);
}
