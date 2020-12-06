use std::collections::HashSet;
use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/06.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();

    let result = part1(&list);
    println!("{}", result);

    let result = part2(&list);
    println!("{:?}", result);

    Ok(())
}

fn part1(list: &[String]) -> usize {
    let mut result = 0;
    let mut set = HashSet::new();
    for line in list {
        match line.as_str() {
            "" => {
                result += set.len();
                set.clear();
            },
            l => l.chars().for_each(|c| { set.insert(c); }),
        }
    }

    result
}

fn part2(list: &[String]) -> usize {
    let mut result = 0;
    let mut set = HashSet::new();
    let mut first = true;
    for line in list {
        match line.as_str() {
            "" => {
                result += set.len();
                set.clear();
                first = true;
            },
            l => if first {
                l.chars().for_each(|c| { set.insert(c); });
                first = false;
            } else {
                let dif: HashSet<char> = l.chars().collect();
                set = set.intersection(&dif).cloned().collect();
            }
        }
    }

    result
}

