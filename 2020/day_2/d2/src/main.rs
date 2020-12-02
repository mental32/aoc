#![feature(str_split_once)]

use std::{convert::TryInto, ops::BitXor, time::Duration};

const INPUT: &str = include_str!("../../two.txt");

#[derive(Debug)]
struct Policy {
    min: usize,
    max: usize,
    letter: char,
}

impl Policy {
    #[inline]
    fn approves(&self, st: &str) -> usize {
        let count = bytecount::count(st.as_bytes(), self.letter as u8);
        if (self.min..(self.max + 1)).contains(&count) {
            1
        } else {
            0
        }
    }

    #[inline]
    fn toboggan_approves(&self, st: &str) -> usize {
        let st = st.as_bytes();
        let letter = self.letter as u8;

        let a = st[self.min - 1];
        let b = st[self.max - 1];

        ((a == letter) != (b == letter)) as usize
    }
}

#[derive(Debug)]
struct Scanner<'a> {
    string: std::str::Lines<'a>,
}

impl<'a> Iterator for Scanner<'a> {
    type Item = (Policy, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        // use std::time::Instant;

        // let start = Instant::now();

        let subslice = self.string.next()?;

        let (min, max, letter, password): (usize, usize, char, &str) =
            if option_env!("SCANNER") == Some("serde") {
                serde_scan::scan!("{}-{} {}: {}" <- subslice).ok()?
            } else {
                let (policy, password) = subslice.split_once(":")?;

                let (min, glob) = policy.split_once("-")?;

                let min = min.parse().ok()?;

                let (max, glob) = glob.split_once(" ")?;

                let max = max.parse().ok()?;

                assert_eq!(glob.len(), 1);

                let letter = glob.chars().nth(0)?;
                (min, max, letter, password.trim())
            };

        // eprintln!("{:?}", start.elapsed());

        let policy = Policy { min, max, letter };

        Some((policy, password))
    }
}

fn parse<'a>(input: &'a str) -> Scanner<'a> {
    Scanner {
        string: input.lines(),
    }
}

fn part1<'a>(input: Scanner<'a>) -> usize {
    input
        .map(|(policy, password)| policy.approves(password))
        .sum()
}

fn part2<'a>(input: Scanner<'a>) -> usize {
    input
        .map(|(policy, password)| policy.toboggan_approves(password))
        .sum()
}

fn main() {
    use std::time::Instant;

    fn time<'a>(f: fn(Scanner<'a>) -> usize, expected: usize) -> Duration {
        let start = Instant::now();
        let it = parse(INPUT);
        assert_eq!(f(it), expected);
        start.elapsed()
    }

    eprintln!("Part 1 {:?}", time(part1, 458));
    eprintln!("Part 2 {:?}", time(part2, 342));
}
