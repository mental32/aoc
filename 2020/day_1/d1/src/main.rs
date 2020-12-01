use crossbeam_utils::CachePadded;

#[inline]
fn crop_search(inp: &[isize]) -> Option<isize> {
    for (i, x) in inp.iter().enumerate() {
        for y in &inp[i + 1..] {
            let z = 2020 - x - y;
            if inp.binary_search(&z.into()).is_ok() {
                return Some(x * y * z);
            }
        }
    }

    None
}

const CHALLANGE: &str = include_str!("../../one_inp.txt");

fn main() {
    use std::time::Instant;

    let mut cache_parsed: CachePadded<[isize; 200]> = CachePadded::from([0isize; 200]);
    {
        let mut parsed: Vec<isize> = CHALLANGE
            .trim()
            .split("\n")
            .map(|part| part.parse().unwrap())
            .collect::<Vec<_>>();

        parsed.sort_unstable();

        cache_parsed.copy_from_slice(parsed.as_slice());
    }

    let start = Instant::now();
    assert_eq!(crop_search(&*cache_parsed), Some(138233720));
    dbg!(Instant::now() - start);
}
