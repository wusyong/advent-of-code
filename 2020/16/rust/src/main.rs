use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::{collections::HashMap};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/16.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<Vec<usize>> = buf.lines().map(|l| l.unwrap().split(",").map(|s| s.parse().unwrap()).collect()).collect();

    let result = part1(&list);
    println!("{}", result);
    let result = part2(list);
    println!("{}", result);
    Ok(())
}

fn part1(list: &[Vec<usize>]) -> usize {
    let range = 30..975;
    let mut error = Vec::new();
    for ticket in list {
        for n in ticket {
            if !range.contains(n) {
                error.push(n);
            }
        }
    }
    error.into_iter().sum()
}

fn part2(list: Vec<Vec<usize>>) -> usize {
    let range = 30..975;
    let list: Vec<Vec<usize>> = list.into_iter().filter(|ticket| {
        let mut valid = true;
        for n in ticket {
            if !range.contains(n) { valid = false; }
        }
        valid
    }).collect();
    let range = [
        (45..423, 444..951),
        (36..742, 752..957),
        (46..789, 806..968),
        (46..58, 70..951),
        (35..100, 108..975),
        (42..884, 903..963),
        (47..84, 95..954),
        (31..228, 240..971),
        (48..841, 853..965),
        (49..488, 499..965),
        (33..364, 381..960),
        (35..510, 522..952),
        (38..591, 601..951),
        (41..267, 285..963),
        (44..403, 419..963),
        (41..616, 634..957),
        (47..157, 178..955),
        (44..314, 338..965),
        (30..111, 133..971),
        (38..542, 550..966),
    ];
    let mine = [109,199,223,179,97,227,197,151,73,79,211,181,71,139,53,149,137,191,83,193];

    let mut result = 0;
    for r in range.iter() {
        for idx in 0..list[0].len() {
            let mut valid = true;
            for n in &list {
                if !(r.0.contains(&n[idx]) || r.1.contains(&n[idx])) {
                    valid = false;
                    break;
                }
            }
            if valid {
                // Too lazy to write the code, so I decided to count the combination myself.
                print!("{}, ",idx);
            }
        }
        println!("\n");
    }
    result
}
