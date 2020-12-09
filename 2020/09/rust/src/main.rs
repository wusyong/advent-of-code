use std::collections::VecDeque;
use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/09.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<isize> = buf
        .lines()
        .map(|l| l.unwrap().parse().unwrap())
        .collect();

    let result = part1(&list);
    println!("{}", result);

    let result = part2(&list);
    println!("{}", result);
    Ok(())
}

fn part1(list: &[isize]) -> isize {
    let mut queue = VecDeque::new();
    for i in 0..25 {
        queue.push_back(list[i]);
    }

    let mut bug = true;
    let mut result = 0;
    for i in &list[25..] {
        for j in 0..25 {
            for k in j..25 {
                if queue[j] + queue[k] == *i { bug = false; break; }
            }
            if !bug { break; }
        }
        if bug { result = *i; break; }
        bug = true;
        queue.pop_front();
        queue.push_back(*i);
    }

    result
}

fn part2(list: &[isize]) -> isize {
    let result = part1(list);

    let mut s = 0;
    let mut t = 0;

    for (i, val) in list.iter().enumerate() {
        s = i;
        let mut acc = result - val;
        if acc == 0 { continue; }
        for j in i+1..list.len() {
            acc -= list[j];
            if acc <= 0 { t = j + 1; break; }
        }
        if acc == 0 { break; }
    }
    let max = list[s..t].iter().max().unwrap();
    let min = list[s..t].iter().min().unwrap();
    max + min
}


