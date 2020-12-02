use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

struct Log {
    min: usize,
    max: usize,
    char: char,
    passowrd: String,
}

fn main() -> Result<()> {
    let file = File::open("../../input/02.txt")?;
    let buf = BufReader::new(file);
    let inputs: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();
    let mut list = Vec::new();

    for input in inputs {
        let pair: Vec<&str> = input.split(' ').collect();
        let bound: Vec<&str> = pair[0].split('-').collect();
        let char: Vec<&str> = pair[1].split(':').collect();
        list.push(Log {
            min: bound[0].parse()?,
            max: bound[1].parse()?,
            char: char[0].chars().next().unwrap(),
            passowrd: pair[2].to_string(),
        });
    }

    let result = part1(&list);
    println!("{}", result);

    let result = part2(&list);
    println!("{}", result);

    Ok(())
}

fn part1(list: &[Log]) -> usize {
    let mut result = 0;
    for log in list {
        let mut count = 0;
        log.passowrd.chars().for_each(|c| if c == log.char { count += 1; });
        if count >= log.min && count <= log.max { result += 1; }
    }

    result
}

fn part2(list: &[Log]) -> usize {
    let mut result = 0;
    for log in list {
        let min = log.passowrd.chars().nth(log.min - 1).unwrap_or(' ');
        let max = log.passowrd.chars().nth(log.max - 1).unwrap_or(' ');
        if (log.char == min) ^ (log.char == max) { result += 1; }
    }

    result
}

