use std::fs;

fn main() {
    let input = fs::read_to_string("1.input").unwrap();
    let mut count = 0;
    let mut pos = 0;
    for c in input.chars() {
        match c {
            '(' => count = count + 1,
            ')' => count = count - 1,
            _ => println!("Unknown Character: {}", c)
        }
        pos = pos + 1;
        if count == -1 {
            println!("-1 at {}", pos);
        }
    }
    println!("Count: {}", count)
}
