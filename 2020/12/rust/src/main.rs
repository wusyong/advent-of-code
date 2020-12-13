use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(Debug)]
enum Dir {
    N,
    E,
    W,
    S
}

impl Dir {
    fn get(&self) -> (isize, isize) {
        match self {
            Dir::N => (0, 1),
            Dir::E => (1, 0),
            Dir::W => (-1, 0),
            Dir::S => (0, -1),
        }
    }

    fn left(self, mut deg: isize) -> Dir {
        let mut dir = self;
        while deg > 0 {
            dir = match dir {
                Dir::N => Dir::W,
                Dir::E => Dir::N,
                Dir::W => Dir::S,
                Dir::S => Dir::E,
            };
            deg -= 90;
        }
        dir
    }

    fn right(self, mut deg: isize) -> Dir {
        let mut dir = self;
        while deg > 0 {
            dir = match dir {
                Dir::N => Dir::E,
                Dir::E => Dir::S,
                Dir::W => Dir::N,
                Dir::S => Dir::W,
            };
            deg -= 90;
        }
        dir
    }
}

#[derive(Debug)]
struct Waypoint(isize, isize);

impl Waypoint {
    fn left(self, mut deg: isize) -> Self {
        let mut dir = self;
        while deg > 0 {
            dir = Waypoint(-dir.1, dir.0);
            deg -= 90;
        }
        dir
    }

    fn right(self, mut deg: isize) -> Self {
        let mut dir = self;
        while deg > 0 {
            dir = Waypoint(dir.1, -dir.0);
            deg -= 90;
        }
        dir
    }
}

enum Mov {
    N(isize),
    E(isize),
    W(isize),
    S(isize),
    F(isize),
    L(isize),
    R(isize),
}

fn main() -> Result<()> {
    let file = File::open("../../input/12.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<Mov> = buf
        .lines()
        .map(|l| {
            let l = l.unwrap();
            let (mov, val) = l.split_at(1);
            let val = val.parse().unwrap();
            match mov {
                "N" => Mov::N(val),
                "E" => Mov::E(val),
                "W" => Mov::W(val),
                "S" => Mov::S(val),
                "F" => Mov::F(val),
                "L" => Mov::L(val),
                "R" => Mov::R(val),
                _ => unreachable!()

            }
        })
        .collect();


    let result = part1(&list);
    println!("{}", result);

    let result = part2(&list);
    println!("{}", result);

    Ok(())
}

fn part1(list: &[Mov]) -> isize {
    let mut pos = (0, 0);
    let mut dir = Dir::E;
    for mov in list {
        match mov {
            Mov::N(v) => pos.1 += v,
            Mov::E(v) => pos.0 += v,
            Mov::W(v) => pos.0 -= v,
            Mov::S(v) => pos.1 -= v,
            Mov::F(v) => { let d = dir.get(); pos.0 += d.0 * v; pos.1 += d.1 * v; },
            Mov::L(v) => dir = dir.left(*v),
            Mov::R(v) => dir = dir.right(*v),
        }
    }
    pos.0.abs() + pos.1.abs()
}

fn part2(list: &[Mov]) -> isize {
    let mut way = Waypoint(10, 1);
    let mut pos = (0, 0);
    for mov in list {
        match mov {
            Mov::N(v) => way.1 += v,
            Mov::E(v) => way.0 += v,
            Mov::W(v) => way.0 -= v,
            Mov::S(v) => way.1 -= v,
            Mov::F(v) => {
                pos.0 += v * way.0;
                pos.1 += v * way.1;
            },
            Mov::L(v) => way = way.left(*v),
            Mov::R(v) => way = way.right(*v),
        }
    }
    dbg!(pos);
    dbg!(way);
    pos.0.abs() + pos.1.abs()
}
