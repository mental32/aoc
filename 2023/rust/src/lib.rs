#[cfg(test)]
mod day_1 {
    fn part_one(st: &str) -> usize {
        let it = st
            .lines()
            .map(|st| st.chars().filter(|c| c.is_digit(10)).collect::<String>());

        let mut sum = 0;

        for line in it {
            let first = line.chars().next().unwrap();
            let last = line.chars().last().unwrap();
            let both = format!("{}{}", first, last);

            sum += both.parse::<usize>().unwrap();
        }

        sum
    }

    fn part_two(st: &str) -> usize {
        let words = [
            ("one", 1u8),
            ("two", 2),
            ("three", 3),
            ("four", 4),
            ("five", 5),
            ("six", 6),
            ("seven", 7),
            ("eight", 8),
            ("nine", 9),
        ];

        let mut sum = 0;

        for line in st.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            } else {
                assert!(!line.is_empty());
            }

            let mut buf = &line[..];
            assert!(!buf.is_empty());

            let mut first = 0u8;
            let mut last = 0u8;

            while !buf.is_empty() {
                // try and match a numeric digit 1..=9
                if let Some(ch) = buf.chars().next() {
                    if ch.is_digit(10) {
                        buf = &buf[1..];
                        let n = ch.to_digit(10).unwrap() as u8;
                        if first == 0 {
                            first = n;
                            last = n;
                        } else {
                            last = n;
                        }
                        continue;
                    }
                }

                // try and match a word from the list "one" to "nine"
                match words.iter().find(|(p, _)| buf.starts_with(p)) {
                    Some((prefix, n)) => {
                        if first == 0 {
                            first = *n;
                            last = *n;
                        } else {
                            last = *n;
                        }

                        // remove the prefix from the buffer except for the last character as words can share letters
                        buf = &buf[prefix.len().saturating_sub(1)..];
                    }
                    None => {
                        buf = &buf[1..];
                    }
                }
            }

            assert_ne!(first, 0);
            assert_ne!(last, 0);

            let parse = format!("{first}{last}").parse::<usize>().unwrap();

            sum += parse;
        }

        sum
    }

    #[test]
    fn test_part_one() {
        let st = "1abc2
            pqr3stu8vwx
            a1b2c3d4e5f
            treb7uchet";

        assert_eq!(part_one(st), 142);
        assert_eq!(part_one(include_str!("./data/day1.txt")), 53921);
    }

    #[test]
    fn test_part_two() {
        let st = "
        two1nine
        eightwothree
        abcone2threexyz
        xtwone3four
        4nineeightseven2
        zoneight234
        7pqrstsixteen";

        assert_eq!(part_two(st), 281);
        assert_eq!(part_two("twone"), 21);
        assert_eq!(part_two("sixrthreeseven74oneightssl"), 68);
        assert_eq!(part_two(include_str!("./data/day1.txt")), 54676);
    }
}

#[cfg(test)]
mod day_2 {
    fn part_one(st: &str) -> usize {
        fn is_valid(st: &str) -> Option<usize> {
            let (n, rest) = st.split_once(":").unwrap();
            let n = n.split_once(" ").unwrap().1.parse::<usize>().unwrap();

            for set in rest.split(";") {
                let mut r = 0;
                let mut g = 0;
                let mut b = 0;

                for n_color in set.split(",") {
                    // {n} {color}
                    let (n, color) = n_color.trim().split_once(" ").unwrap();
                    match color {
                        "red" => r += n.parse::<usize>().unwrap(),
                        "green" => g += n.parse::<usize>().unwrap(),
                        "blue" => b += n.parse::<usize>().unwrap(),
                        _ => panic!("unknown color"),
                    }
                }

                if r > 12 || g > 13 || b > 14 {
                    return None;
                }
            }

            Some(n)
        }

        st.lines().filter_map(is_valid).sum()
    }

    fn part_two(st: &str) -> usize {
        fn is_valid(st: &str) -> Option<usize> {
            let (n, rest) = st.split_once(":").unwrap();
            let n = n.split_once(" ").unwrap().1.parse::<usize>().unwrap();

            let mut r = 0;
            let mut g = 0;
            let mut b = 0;

            for set in rest.split(";") {
                for n_color in set.split(",") {
                    // {n} {color}
                    let (n, color) = n_color.trim().split_once(" ").unwrap();
                    let n = n.parse::<usize>().unwrap();
                    match color {
                        "red" => r = std::cmp::max(r, n),
                        "green" => g = std::cmp::max(g, n),
                        "blue" => b = std::cmp::max(b, n),
                        _ => panic!("unknown color"),
                    }
                }
            }

            // dbg!((r, g, b));

            Some(r * g * b)
        }

        st.lines().filter_map(is_valid).sum()
    }

    #[test]
    fn test_part_one() {
        let example = "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
";

        assert_eq!(part_one(example.trim()), 8);
        assert_eq!(part_one(include_str!("./data/day2.txt")), 2105);
        assert_eq!(part_two(example.trim()), 2286);
        assert_eq!(part_two(include_str!("./data/day2.txt")), 72422);
    }
}

#[cfg(test)]
mod day_3 {
    fn part_one(input: &str) -> usize {
        let it = input.lines().enumerate().flat_map(|(y, line)| {
            line.chars().enumerate().filter_map(move |(x, ch)| {
                if ch == '.' || ch.is_digit(10) {
                    None
                } else {
                    Some((x, y))
                }
            })
        });

        let mut input: Vec<String> = input.lines().map(|st| st.to_owned()).collect();

        let mut sum = 0;
        for (x, y) in it {
            for (x, y) in [
                (x + 1, y),     // right
                (x - 1, y),     // left
                (x, y + 1),     // down
                (x, y - 1),     // up
                (x + 1, y + 1), // down right
                (x - 1, y - 1), // up left
                (x + 1, y - 1), // up right
                (x - 1, y + 1), // down left
            ] {
                let Some(line) = input.iter().nth(y).clone() else {
                    continue;
                };

                let Some(ch) = line.chars().nth(x) else {
                    continue;
                };

                if !ch.is_digit(10) {
                    continue;
                }

                let after = line
                    .chars()
                    .skip(x)
                    .take_while(|c| c.is_digit(10))
                    .collect::<String>();

                let before = line
                    .chars()
                    .take(x)
                    .collect::<Vec<_>>()
                    .into_iter()
                    .rev()
                    .take_while(|c| c.is_digit(10))
                    .collect::<String>()
                    .chars()
                    .rev()
                    .collect::<String>();

                let value = format!("{}{}", before, after);
                let pre = &line[..(x - before.len())];
                let post = &line[(x + after.len())..];
                let cen = format!("{}{}{}", pre, ".".repeat(value.len()), post);

                let value_n = value.parse::<usize>().unwrap();

                input[y] = cen;

                sum += value_n;
            }
        }

        sum
    }

    const fn coords(x: usize, y: usize) -> [(usize, usize); 8] {
        [
            (x + 1, y),     // right
            (x - 1, y),     // left
            (x, y + 1),     // down
            (x, y - 1),     // up
            (x + 1, y + 1), // down right
            (x - 1, y - 1), // up left
            (x + 1, y - 1), // up right
            (x - 1, y + 1), // down left
        ]
    }

    fn part_two(input: &str) -> usize {
        let it = input.lines().enumerate().flat_map(|(y, line)| {
            line.chars().enumerate().filter_map(
                move |(x, ch)| {
                    if ch == '*' {
                        Some((x, y))
                    } else {
                        None
                    }
                },
            )
        });

        let mut input: Vec<String> = input.lines().map(|st| st.to_owned()).collect();

        let mut sum = 0;
        for (x, y) in it {
            let mut nums = vec![];
            for (x, y) in coords(x, y) {
                let Some(line) = input.iter().nth(y).clone() else {
                    continue;
                };

                let Some(ch) = line.chars().nth(x) else {
                    continue;
                };

                if !ch.is_digit(10) {
                    continue;
                }

                let after = line
                    .chars()
                    .skip(x)
                    .take_while(|c| c.is_digit(10))
                    .collect::<String>();

                let before = line
                    .chars()
                    .take(x)
                    .collect::<Vec<_>>()
                    .into_iter()
                    .rev()
                    .take_while(|c| c.is_digit(10))
                    .collect::<String>()
                    .chars()
                    .rev()
                    .collect::<String>();

                let value = format!("{}{}", before, after);
                let pre = &line[..(x - before.len())];
                let post = &line[(x + after.len())..];
                let cen = format!("{}{}{}", pre, ".".repeat(value.len()), post);

                let value_n = value.parse::<usize>().unwrap();

                input[y] = cen;

                nums.push(value_n);
            }
            if let [one, two] = nums.as_slice() {
                sum += one * two;
            }
        }

        sum
    }

    #[test]
    fn test_part_one() {
        let ex = "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"
        .trim();

        assert_eq!(part_one(ex), 4361);
        assert_eq!(part_one(include_str!("./data/day3.txt")), 532445);
    }

    #[test]
    fn test_part_two() {
        assert_eq!(part_two(include_str!("./data/day3.txt")), 79842967);
    }
}
