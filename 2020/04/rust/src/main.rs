use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::collections::HashSet;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/04.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();

    let result = part1(&list);
    println!("{}", result);

    let result = part2(&list)?;
    println!("{}", result);

    Ok(())
}

fn part1(list: &[String]) -> usize {
    let valid: HashSet<&str> = ["byr","iyr", "eyr", "hgt", "hcl", "ecl", "pid"].iter().cloned().collect();
    let mut record = HashSet::new();
    let mut result = 0;
    for line in list {
        match line.is_empty() {
            true => {
                record.remove("cid");
                if valid == record { result += 1; }
                record.clear();
            },
            false => {
                let fields: Vec<&str> = line.split(' ').collect();
                for field in fields {
                    let kv: Vec<&str> = field.split(':').collect();
                    record.insert(kv[0]);
                }
            },
        }
    }

    result
}

fn part2(list: &[String]) -> Result<usize> {
    let valid: HashSet<&str> = ["byr","iyr", "eyr", "hgt", "hcl", "ecl", "pid"].iter().cloned().collect();
    let mut record = HashSet::new();
    let mut result = 0;
    for line in list {
        match line.is_empty() {
            true => {
                if valid == record { result += 1; }
                record.clear();
            },
            false => {
                let fields: Vec<&str> = line.split(' ').collect();
                for field in fields {
                    let kv: Vec<&str> = field.split(':').collect();
                    match kv[0] {
                        "byr" => { 
                            match (1920..2003).contains(&kv[1].parse::<usize>()?) {
                                true => { record.insert(kv[0]); },
                                false => break,
                            }
                        },
                        "iyr" => {
                            match (2010..2021).contains(&kv[1].parse::<usize>()?) {
                                true => { record.insert(kv[0]); },
                                false => break,
                            }
                        },
                        "eyr" => {
                            match (2020..2031).contains(&kv[1].parse::<usize>()?) {
                                true => { record.insert(kv[0]); },
                                false => break,
                            }
                        },
                        "hgt" => {
                            let len = kv[1].len();
                            match &kv[1][len-2..] {
                                "cm" => match (150..194).contains(&kv[1][..len-2].parse::<usize>()?) {
                                    true => { record.insert(kv[0]); },
                                    false => break,
                                },
                                "in" => match (59..77).contains(&kv[1][..len-2].parse::<usize>()?) {
                                    true => { record.insert(kv[0]); },
                                    false => break,
                                },
                                _ => break,
                            }
                        },
                        "hcl" => {
                            match &kv[1][..1] == "#" && kv[1].len() == 7 {
                                true => {
                                    match &kv[1][1..].chars().all(|c| c.is_digit(16)) {
                                        true => { record.insert(kv[0]); },
                                        false => break,
                                    }
                                },
                                false => break,
                            }
                        },
                        "ecl" => {
                            match kv[1] {
                                "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => { record.insert(kv[0]); },
                                _ => break,
                            }
                        },
                        "pid" => {
                            match kv[1].chars().all(|c| c.is_digit(10)) && kv[1].len() == 9 {
                                true => { record.insert(kv[0]); },
                                false => break,
                            }
                        },
                        _ => ()
                    }
                    
                }
            },
        }
    }

    Ok(result)
}

