
use std::fs::File;
use std::io::{prelude::*, BufReader};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let file = File::open("../../input/18.txt")?;
    let buf = BufReader::new(file);
    let list: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();

    let result = part1(&list);
    println!("{}", result);

    Ok(())
}

fn part1(list: &[String]) -> usize {
    let mut result = 0;
    for line in list {
        let mut val = Vec::new();
        let mut op = Vec::new();
        for c in line.chars() {
            match c {
                num if num.is_digit(10) => {
                    match op.pop() {
                        Some('*') => {
                            let n = num.to_digit(10).unwrap() as usize * val.pop().unwrap();
                            val.push(n);
                        }
                        Some('+') => {
                            let n = num.to_digit(10).unwrap() as usize + val.pop().unwrap();
                            val.push(n);
                        }
                        Some('(') => {
                            op.push('(');
                            val.push(num.to_digit(10).unwrap() as usize); 
                        }
                        _ => val.push(num.to_digit(10).unwrap() as usize),
                    }
                    
                },
                '*' => op.push('*'),
                '+' => op.push('+'),
                '(' => op.push('('),
                ')' => { 
                    op.pop();
                    if let Some(p) = op.pop() {
                        match p {
                            '*' => {
                                let v1 = val.pop().unwrap();
                                let v2 = val.pop().unwrap();
                                val.push(v1 * v2);

                            }
                            '+' => {
                                let v1 = val.pop().unwrap();
                                let v2 = val.pop().unwrap();
                                val.push(v1 + v2);

                            }
                            p => op.push(p),
                        }
                    }
                },
                _ => ()
            }
        }
        dbg!(&val);
        result += val.pop().unwrap();
    }
    result
}
