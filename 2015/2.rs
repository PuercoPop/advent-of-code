use std::fs;
use std::cmp;

fn surface_area(l:u64, w:u64, h:u64) -> u64 {
    return 2*l*w + 2*w*h + 2*h*l
}

fn area_smallest_side(l:u64, w:u64, h:u64) -> u64 {
    return cmp::min(h*w, cmp::min(l*w, l*h))
}

fn paper_requires(l:u64, w:u64, h:u64) -> u64 {
    return surface_area(l, w, h) + area_smallest_side(l, w, h)

}

fn parse_line(line:&str) -> (u64, u64, u64) {
    let sides:Vec<&str> = line.split('x').collect();
    return (sides[0].parse::<u64>().unwrap(),
            sides[1].parse::<u64>().unwrap(),
            sides[2].parse::<u64>().unwrap())
}

fn main() {
    let input = fs::read_to_string("2.input").unwrap();
    let mut tot:u64 = 0;
    for line in input.lines() {
        let (length, width, height) = parse_line(line);
        tot = tot + paper_requires(length, width, height);
    }
    println!("Total: {}", tot)
}
