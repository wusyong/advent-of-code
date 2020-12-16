use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::{collections::HashMap, vec};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let result = part1();
    println!("{}", result);
    Ok(())
}

fn part1() -> usize {
    let mut prev: HashMap<usize, (usize, usize)> = HashMap::new();

    prev.insert(0, (0,0));
    prev.insert(14, (1,1));
    prev.insert(1, (2,2));
    prev.insert(3, (3, 3));
    prev.insert(7, (4, 4));
    prev.insert(9, (5, 5));
    let mut n = 6;
    let mut seen = 9;
    while n < 30000000 {
        let num = prev.get(&seen).unwrap();
        let num = if num.0 == num.1 {
            0
        } else {
            num.1 - num.0
        };
        prev.entry(num).and_modify(|e| *e = (e.1,n)).or_insert((n,n));
        n += 1;
        seen = num;
    }
    seen
}
