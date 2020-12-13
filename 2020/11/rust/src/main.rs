use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/11.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<Vec<Block>> = buf
        .lines()
        .map(|l| l.unwrap().chars().map(|c| match c {
            '.' => Block::Florr,
            'L' => Block::Empty(false),
            '#' => Block::Occupied(false),
            _ => unreachable!(),
        }).collect())
        .collect();

    let result = part1(list.clone());
    println!("{}", result);
    let result = part2(list.clone());
    println!("{}", result);
    Ok(())
}

#[derive(Debug, Clone)]
enum Block {
    Florr,
    Empty(bool),
    Occupied(bool),
}

fn part1(mut list: Vec<Vec<Block>>) -> usize {
    let row = list.len();
    let col = list[0].len();
    let mut stop = false;
    while !stop {
        stop = true;
        for i in 0..row {
            for j in 0..col {
                match list[i][j] {
                    Block::Empty(_) => if around(i, j, &list) == 0 { list[i][j] = Block::Empty(true); stop = false; },
                    Block::Occupied(_) => if around(i, j, &list) >= 4 { list[i][j] = Block::Occupied(true); stop = false; },
                    _ => ()
                };
            }
        }

        for i in 0..row {
            for j in 0..col {
                match list[i][j] {
                    Block::Empty(x) => if x { list[i][j] = Block::Occupied(false) },
                    Block::Occupied(x) => if x { list[i][j] = Block::Empty(false) },
                    _ => ()
                };
            }
        }
    }

    let mut result = 0;
    for i in 0..row {
        for j in 0..col {
            if let Block::Occupied(_) = list[i][j] {
                result += 1;
            };
        }
    }

    result
}

fn around(i: usize, j: usize, list: &[Vec<Block>]) -> usize {
    let dir = [(1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1)];
    let mut sum = 0;
    let mut x = i as isize;
    let mut y = j as isize;
    
    for d in dir.iter() {
        if x == 0 && d.0 == -1 { continue; }
        if x == list.len() as isize - 1 && d.0 == 1 { continue; }
        if y == 0 && d.1 == -1 { continue; }
        if y == list[0].len() as isize - 1 && d.1 == 1 { continue; }
        if let Block::Occupied(_) = list[(x + d.0) as usize][(y + d.1) as usize] {
            sum += 1;
        }
    }

    sum
}

fn part2(mut list: Vec<Vec<Block>>) -> usize {
    let row = list.len();
    let col = list[0].len();
    let mut stop = false;
    while !stop {
        stop = true;
        for i in 0..row {
            for j in 0..col {
                match list[i][j] {
                    Block::Empty(_) => if around2(i, j, &list) == 0 { list[i][j] = Block::Empty(true); stop = false; },
                    Block::Occupied(_) => if around2(i, j, &list) >= 5 { list[i][j] = Block::Occupied(true); stop = false; },
                    _ => ()
                };
            }
        }

        for i in 0..row {
            for j in 0..col {
                match list[i][j] {
                    Block::Empty(x) => if x { list[i][j] = Block::Occupied(false) },
                    Block::Occupied(x) => if x { list[i][j] = Block::Empty(false) },
                    _ => ()
                };
            }
        }
    }

    let mut result = 0;
    for i in 0..row {
        for j in 0..col {
            if let Block::Occupied(_) = list[i][j] {
                result += 1;
            };
        }
    }

    result
}

fn around2(i: usize, j: usize, list: &[Vec<Block>]) -> usize {
    let dir = [(1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1)];
    let mut sum = 0;
    
    for d in dir.iter() {
        let mut x = i as isize;
        let mut y = j as isize;
        loop {
            if x == 0 && d.0 == -1 { break; }
            if x == list.len() as isize - 1 && d.0 == 1 { break; }
            if y == 0 && d.1 == -1 { break; }
            if y == list[0].len() as isize - 1 && d.1 == 1 { break; }
            x += d.0;
            y += d.1;
            match list[x as usize][y as usize] {
                Block::Occupied(_) => {
                    sum += 1;
                    break;
                }
                Block::Empty(_) => break,
                Block::Florr => continue,
            }
        }
    }

    sum
}
