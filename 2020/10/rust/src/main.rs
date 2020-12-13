use std::collections::HashMap;
use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/10.txt")?;
    let buf = BufReader::new(file);
    let mut list: Vec<usize> = buf
        .lines()
        .map(|l| l.unwrap().parse().unwrap())
        .collect();
    list.push(0);
    list.sort();
    let last = *list.last().unwrap();
    list.push(last);

    let result = part1(&list);
    println!("{}", result);

    let result = part2(&list);
    println!("{}", result);
    Ok(())
}

fn part1(list: &[usize]) -> usize {
    let mut jolt = HashMap::new();
    for i in 1..list.len() {
        let counter = jolt.entry(list[i] - list[i - 1]).or_insert(0);
        *counter += 1;
    }

    jolt.get(&1).unwrap() * (jolt.get(&3).unwrap() + 1)
}

fn part2(list: &[usize]) -> usize {
    let mut dp = [0usize; 50];
    dp[0] = 1;
    for i in 0..40 {
        dp[i + 1] += dp[i];
        dp[i + 2] += dp[i];
        dp[i + 3] += dp[i];
    }
    let mut choice = 0;
    let mut result = 1;
    for i in 1..list.len() {
        if list[i] - list[i - 1] == 1 {
            choice += 1;
        } else {
            result *= dp[choice];
            choice = 0;
        }
    }

    result
}

