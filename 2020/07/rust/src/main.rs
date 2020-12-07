use std::collections::{HashMap, VecDeque, HashSet};
use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/07.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<_> = buf
        .lines()
        .map(|l| l.unwrap().replace("bags", "").replace("bag", "").replace("no other", "0 none").replace("contain", ",").replace(".", ""))
        .collect();

    let result = part1(&list);
    println!("{}", result);

    let result = part2(&list);
    println!("{}", result);
    Ok(())
}

fn part1(list: &[String]) -> usize {
    let mut bags: HashMap<&str, Vec<(&str, usize)>> = HashMap::new();
    for line in list {
        let rule: Vec<_> = line.split(",").map(|s| s.trim()).collect();
        for i in 1..rule.len() {
            let (n, s) = rule[i].split_at(2);
            let n: usize = n.trim().parse().unwrap();
            match bags.get_mut(s) {
                Some(v) => v.push((rule[0], n)),
                None => {
                    bags.insert(s, vec![(rule[0], n)]);
                }
            }
        }
    }

    let mut result = HashSet::new();
    let mut search = VecDeque::new();
    search.push_back("shiny gold");
    while let Some(k) = search.pop_front() {
        if let Some(v) = bags.remove(k) {
            v.into_iter().for_each(|i| {
                    search.push_back(i.0);
                    result.insert(i.0);
            });
        }
    }

    result.len()
}

fn part2(list: &[String]) -> usize {
    let mut bags: HashMap<&str, Vec<(&str, usize)>> = HashMap::new();
    for line in list {
        let rule: Vec<_> = line.split(",").map(|s| s.trim()).collect();
        for i in 1..rule.len() {
            let (n, s) = rule[i].split_at(2);
            let n: usize = n.trim().parse().unwrap();
            match bags.get_mut(rule[0]) {
                Some(v) => v.push((s, n)),
                None => {
                    bags.insert(rule[0], vec![(s, n)]);
                }
            }
        }
    }
    
    let mut result = 0;
    let mut search = VecDeque::new();
    search.push_back(("shiny gold", 1));
    while let Some(k) = search.pop_front() {
        result += k.1;
        if let Some(v) = bags.get_mut(k.0) {
            v.into_iter().for_each(|i| {
                search.push_back((i.0, i.1 * k.1));
            });
        }
    }

    result - 1
}
