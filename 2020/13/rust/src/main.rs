use std::fs::File;
use std::io::{prelude::*, BufReader};
use modinverse::modinverse;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/13.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<String> = buf
        .lines()
        .map(|l| l.unwrap())
        .collect();


    let result = part1(&list);
    println!("{}", result);
    let result = part2(&list);
    println!("{}", result);
    Ok(())
}

fn part1(list: &[String]) -> usize {
    let timestamp: usize = list[0].parse().unwrap();
    let schedule:Vec<usize> = list[1].replace(",", " ").replace("x", " ").split_whitespace().map(|s| s.parse().unwrap()).collect();
    let mut min = usize::MAX;
    let mut result = 0;
    for bus in schedule {
        let next = ((timestamp / bus) + 1) * bus;
        let interval = next - timestamp;
        if interval < min {
            min = interval;
            result = interval * bus;
        }
    }

    result
}

fn part2(list: &[String]) -> isize {
    let mut mods:Vec<(isize, isize)> = Vec::new();
    list[1].replace(",", " ").split_whitespace().enumerate().for_each(|(i,s)| if s != "x" {
        let prime: isize = s.parse().unwrap();
        mods.push(((prime - i as isize).rem_euclid(prime), prime));
    });
    let product: isize = mods.iter().map(|r| r.1).product();
    let product_each: Vec<isize> = mods.iter().map(|r| product / r.1 ).collect();
    let inverse_each: Vec<isize> = product_each.iter().zip(mods.iter()).map(|(p, m)| modinverse(*p, m.1).unwrap()).collect();
    let sum: isize = inverse_each.iter().zip(mods.iter()).zip(product_each).map(|((x, r), p)| *x * r.0 * p).sum();

    sum % product
}

