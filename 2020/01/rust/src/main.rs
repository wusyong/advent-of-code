use std::collections::HashSet;
use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/01.txt")?;
    let buf = BufReader::new(file);
    let mut list: HashSet<usize> = HashSet::new();

    for line in buf.lines() {
        let num: usize = line?.parse()?;
        list.insert(num);
    }

    let result = part1(&list);
    println!("{}", result);

    let result = part2(&list);
    println!("{}", result);

    Ok(())
}

fn part1(list: &HashSet<usize>) -> usize {
    for i in list {
        let j = 2020 - i;
        if list.contains(&j) {
            return i * j;
        }
    }
    0
}

fn part2(list: &HashSet<usize>) -> usize {
    for i in list {
        for j in list {
            let k = if i + j <= 2020 {
                2020 - i - j
            } else {
                continue;
            };
            if list.contains(&k) {
                return i * j * k;
            }
        }
    }
    0
}
