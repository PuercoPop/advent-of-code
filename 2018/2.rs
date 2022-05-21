use std::{collections::HashMap, fs};

fn frequency_table(word: String) -> HashMap<char, i32> {
    let mut freq = HashMap::new(); // Character -> count
    for character in word.chars() {
        if !freq.contains_key(&character) {
            freq.insert(character.clone(), 0);
        }
        let count = freq.get_mut(&character).unwrap();
        *count += 1;
    }
    freq
}

fn analyze_word(word: String) -> (i32, i32) {
    let frequency_table = frequency_table(word);
    let two_count = if frequency_table.values().any(|val| *val == 2) { 1 } else { 0 };
    let three_count = if frequency_table.values().any(|val| *val == 3) { 1 } else { 0 };
    return (two_count, three_count);
}

pub fn main() {
    let result = fs::read_to_string("2.input")
        .unwrap()
        .lines()
        .map(|line| analyze_word(line.to_string()))
        .fold((0,0), |(xt, yt), (x, y)| (xt + x, yt + y));

    println!("{:?}", result.0 * result.1);
}
