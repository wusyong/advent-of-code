use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/03.txt")?;
    let buf = BufReader::new(file);
    let inputs: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();
    let mut list = Vec::new();

    for input in inputs {
        let route: Vec<bool> = input.chars().map(|c| if c == '#' { true } else { false }).collect();
        list.push(route);
    }

    let result = part1(&list, 3, 1);
    println!("{}", result);

    let result = part2(&list);
    println!("{}", result);

    Ok(())
}

fn part1(list: &[Vec<bool>], right: usize, down: usize) -> usize {
    let len = list[0].len();
    let mut pos = 0;
    let mut tree = 0;
    for route in list.iter().step_by(down) {
        if route[pos] == true { tree += 1; }
        pos += right;
        if pos >= len { pos -= len; }
    }
    tree
}

fn part2(list: &[Vec<bool>]) -> usize {
    part1(list, 1, 1) *
    part1(list, 3, 1) *
    part1(list, 5, 1) *
    part1(list, 7, 1) *
    part1(list, 1, 2)
}

