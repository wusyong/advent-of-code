use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::{collections::HashMap, vec};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/14.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();

    let result = part1(&list);
    println!("{}", result);
    let result = part2(&list);
    println!("{}", result);
    Ok(())
}

fn part1(list: &[String]) -> u64 {
    let mut mask = String::new();
    let mut map = HashMap::new();
    for line in list {
        let split: Vec<&str> = line.split(" = ").collect();
        if split[0] == "mask" {
            mask = split[1].to_string();
        } else {
            let mem: u64 = split[0]
                .replace("[", " ")
                .replace("]", " ")
                .split(" ")
                .nth(1)
                .unwrap()
                .parse()
                .unwrap();
            let value = format!("{:036b}", split[1].parse::<u64>().unwrap());
            let value: String = value
                .chars()
                .zip(mask.chars())
                .map(|(v, m)| if m == 'X' { v } else { m })
                .collect();
            map.insert(mem, value);
        }
    }
    map.iter()
        .map(|(_, b)| u64::from_str_radix(b, 2).unwrap())
        .sum()
}

fn part2(list: &[String]) -> u64 {
    let mut mask = String::new();
    let mut map = HashMap::new();
    for line in list {
        let split: Vec<&str> = line.split(" = ").collect();
        if split[0] == "mask" {
            mask = split[1].to_string();
            dbg!(&mask);
        } else {
            let mem: u64 = split[0]
                .replace("[", " ")
                .replace("]", " ")
                .split(" ")
                .nth(1)
                .unwrap()
                .parse()
                .unwrap();
            let mem = format!("{:036b}", mem);
            let value = split[1].parse::<u64>().unwrap();
            dbg!(&mem);
            let mem: String = mem
                .chars()
                .zip(mask.chars())
                .map(|(v, m)| if m == '0' { v } else { m })
                .collect();
            dbg!(&mem);
            let mut combination = vec![mem];
            loop {
                if let Some(pos) = combination[0].find('X') {
                    let mut more = vec![];
                    combination.iter_mut().for_each(|s| {
                        let mut n = s.clone();
                        s.replace_range(pos..pos + 1, "0");
                        n.replace_range(pos..pos + 1, "1");
                        more.push(n);
                    });
                    combination.append(&mut more);
                } else {
                    break;
                }
            }
            dbg!(&combination);
            combination.iter().for_each(|b| {
                map.insert(u64::from_str_radix(b, 2).unwrap(), value);
            });
        }
    }
    map.iter().map(|(_, v)| v).sum()
}
