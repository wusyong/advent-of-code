use std::collections::HashSet;
use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/08.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<_> = buf
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
    let mut set = HashSet::new();
    let mut cp = 0;
    let mut output = 0;
    loop {
        set.insert(cp);
        let line: Vec<_> = list[cp].split(" ").collect();
        let sign = &line[1][0..1];
        let num: usize = line[1][1..].parse().unwrap();
        match line[0] {
            "acc" => {
                if sign == "+" {
                    output += num;
                } else {
                    output -= num;
                }
                cp += 1;
            }
            "jmp" => {
                if sign == "+" {
                    cp += num;
                } else {
                    cp -= num;
                }
            }
            _ => cp += 1,
        }
        if set.contains(&cp) {
            break;
        }
    }

    output
}

fn part2(list: &[String]) -> usize {
    let mut sus = vec![];
    let mut ins = vec![];
    for (i, line) in list.iter().enumerate() {
        let line: Vec<_> = line.split(" ").collect();
        let sign = &line[1][0..1];
        let num: usize = line[1][1..].parse().unwrap();

        ins.push((line[0], sign, num));
        if line[0] == "nop" || line[0] == "jmp" {
            sus.push(i);
        }
    }

    let mut set = HashSet::new();
    let mut cp = 0;
    let mut output = 0;
    let mut n = sus.pop().unwrap();
    while cp < ins.len() {
        set.insert(cp);
        match ins[cp].0 {
            "acc" => {
                if ins[cp].1 == "+" {
                    output += ins[cp].2;
                } else {
                    output -= ins[cp].2;
                }
                cp += 1;
            }
            "jmp" => {
                if ins[cp].1 == "+" {
                    cp += ins[cp].2;
                } else {
                    cp -= ins[cp].2;
                }
            }
            _ => cp += 1,
        }
        if set.contains(&cp) {
            cp = 0;
            set.clear();
            output = 0;
            match ins[n].0 {
                "nop" => ins[n].0 = "jmp",
                "jmp" => ins[n].0 = "nop",
                _ => unreachable!()
            }
            n = sus.pop().unwrap();
            match ins[n].0 {
                "nop" => ins[n].0 = "jmp",
                "jmp" => ins[n].0 = "nop",
                _ => unreachable!()
            }
        }
    }

    output
}

