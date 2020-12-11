use std::time::Instant;

const INPUT: &str = include_str!("../../ten.txt");


#[inline]
fn parse(input: &str, buf: &mut [u8]) -> usize {
    let mut n = 0;

    for (idx, line) in input.lines().enumerate() {
        buf[idx] = line.parse().unwrap();
        n = idx;
    }

    n
}

#[inline]
fn part2(input: &[u8]) -> usize {
    let mut paths = [0; 255];

    paths[0] = 1usize;
    paths[1] = 1usize;
    paths[2] = 2usize;
    paths[3] = 4usize;

    for jolt in &input[3..] {
        let jolt = *jolt as usize;

        let sum = {
            let a = unsafe { paths.get_unchecked(jolt - 1) };
            let b = unsafe { paths.get_unchecked(jolt - 2) };
            let c = unsafe { paths.get_unchecked(jolt - 3) };

            a + b + c
        };

        paths[jolt] = sum;
    }

    let last = input[input.len() - 1] as usize;

    paths[last]
}

fn main() {
    let mut buf = [0; 1024];

    let start = Instant::now();

    let input = {
        let n = buf.len() - parse(INPUT, &mut buf) - 1;
        buf.sort();
        &buf[n..]
    };

    eprintln!("Parsing: {:?}", start.elapsed());

    let start = Instant::now();
    let result = part2(&input);
    let elapsed = start.elapsed();

    eprintln!("Part2: {:?} ({:?})", result, elapsed);
}
