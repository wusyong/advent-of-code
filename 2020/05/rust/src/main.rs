use std::collections::HashSet;
use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/05.txt")?;
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
    for line in list {
        let mut row = 0;
        let mut col = 0;
        for sign in line.chars() {
            match sign {
                'F' => row = row << 1,
                'B' => row = (row << 1) + 1,
                'L' => col = col << 1,
                'R' => col = (col << 1) + 1,
                _ => unreachable!(),
            }
        }
        result = result.max(row * 8 + col);
    }

    result
}

fn part2(list: &[String]) -> Option<usize> {
    let mut set = HashSet::new();
    for line in list {
        let mut row = 0;
        let mut col = 0;
        for sign in line.chars() {
            match sign {
                'F' => row = row << 1,
                'B' => row = (row << 1) + 1,
                'L' => col = col << 1,
                'R' => col = (col << 1) + 1,
                _ => unreachable!(),
            }
        }
        set.insert(row * 8 + col);
    }

    set.iter().find_map(|s| {
        if !set.contains(&(s + 1)) && set.contains(&(s + 2)) {
            Some(s + 1)
        } else {
            None
        }
    })
}
