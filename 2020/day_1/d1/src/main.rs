#![feature(is_sorted)]
#[inline]
fn crop_search(inp: &[i32]) -> Option<i32> {
    let is_sorted = inp.is_sorted();
    let contains = |z| {
        if is_sorted {
            inp.binary_search(&z).is_ok()
        } else {
            inp.contains(&z)
        }
    };

    for (i, x) in inp.iter().enumerate() {
        for y in &inp[i + 1..] {
            let z = 2020 - x - y;
            if contains(z) {
                return Some(x * y * z);
            }
        }
    }

    None
}

const NEEDLE: i32 = 138233720;
const CHALLANGE: &str = include_str!("../../one_inp.txt");

fn main() {
    use std::time::{Duration, Instant};

    let mut n = vec![];
    for _ in 0..1 {
        let start = Instant::now();
        let mut buf: [i32; 200] = [0; 200];

        {
            let mut parsed: Vec<_> = CHALLANGE
                .split("\n")
                .filter_map(|part| part.parse().ok())
                .collect();

            parsed.sort_unstable();

            buf.copy_from_slice(parsed.as_slice());
        }

        assert_eq!(crop_search(&buf), Some(NEEDLE));
        n.push(Instant::now() - start);
    }

    eprintln!(
        "{:?} (fastest) : {:?} (slowest) : {:?} (avg)",
        n.iter().min(),
        n.iter().max(),
        n.iter().sum::<Duration>() / n.len() as u32
    );
}
