use std::collections::HashSet;
use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/17.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<Vec<bool>> = buf
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|s| if s == '.' { false } else { true })
                .collect()
        })
        .collect();

    let mut cube = HashSet::new();
    for (i, n) in list.iter().enumerate() {
        for (j, m) in n.iter().enumerate() {
            if *m {
                cube.insert(((i + 6) as i32, (j + 6) as i32, 6, 6));
            }
        }
    }

    let result = part1(cube.clone());
    println!("{}", result);

    let result = part2(cube);
    println!("{}", result);
    Ok(())
}

fn part1(mut cube: HashSet<(i32, i32, i32, i32)>) -> usize {
    for _ in 0..6 {
        let mut new = HashSet::new();
        for x in 0..20 {
            for y in 0..20 {
                for z in 0..13 {
                    let mut neighbor = 0;
                    for px in -1..2 {
                        for py in -1..2 {
                            for pz in -1..2 {
                                if !(px == 0 && py == 0 && pz == 0) {
                                    if let Some(_) = cube.get(&(x + px, y + py, z + pz, 6)) {
                                        neighbor += 1;
                                    }
                                }
                            }
                        }
                    }
                    if let Some(_) = cube.get(&(x, y, z, 6)) {
                        if neighbor == 2 || neighbor == 3 {
                            new.insert((x, y, z, 6));
                        }
                    } else {
                        if neighbor == 3 {
                            new.insert((x, y, z, 6));
                        }
                    }
                }
            }
        }
        cube = new;
    }
    cube.len()
}


fn part2(mut cube: HashSet<(i32, i32, i32, i32)>) -> usize {
    // Hahaa! O(n^9) goes brrrrrrrrrrrr
    for _ in 0..6 {
        let mut new = HashSet::new();
        for x in 0..20 {
            for y in 0..20 {
                for z in 0..13 {
                    for w in 0..13 {
                        let mut neighbor = 0;
                        for px in -1..2 {
                            for py in -1..2 {
                                for pz in -1..2 {
                                    for pw in -1..2 {
                                        if !(px == 0 && py == 0 && pz == 0 && pw == 0) {
                                            if let Some(_) = cube.get(&(x + px, y + py, z + pz, w + pw)) {
                                                neighbor += 1;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        if let Some(_) = cube.get(&(x, y, z, w)) {
                            if neighbor == 2 || neighbor == 3 {
                                new.insert((x, y, z, w));
                            }
                        } else {
                            if neighbor == 3 {
                                new.insert((x, y, z, w));
                            }
                        }
                    }
                }
            }
        }
        cube = new;
    }
    cube.len()
}
