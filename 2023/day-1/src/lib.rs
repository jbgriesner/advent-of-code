use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static!{
    static ref MAP: HashMap<&'static str, u32> = vec![
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9),
    ].into_iter().collect();
}

pub fn process_part1(s: &str) -> String {
    s.split('\n').map(|l| 
        {
            let x = get_first_digit(l);
            let l_rev = l.chars().rev().collect::<String>();
            let y = get_first_digit(&l_rev);
            
            let r = format!("{}{}", x.unwrap(), y.unwrap());
            r.parse::<u32>().unwrap()
        }
    ).sum::<u32>().to_string()
}

fn get_first_digit(s: &str) -> Option<char> {
    for c in s.chars() {
        if c.is_digit(10) {
            return Some(c);
        }
    }
    None
}

pub fn process_part2(s: &str) -> String {
    s.split('\n').map(|l| 
        {
            let x = get_first_digit_chars(l);
            let l_rev = l.chars().rev().collect::<String>();
            let y = get_first_digit_chars_rev(&l_rev);
            
            // println!("{}", l);
            // println!("{}---{}", x.unwrap(), y.unwrap());
            let r = format!("{}{}", x.unwrap(), y.unwrap());
            r.parse::<u32>().unwrap()
        }
    ).sum::<u32>().to_string()
}

fn get_first_digit_chars(s: &str) -> Option<u32> {
    for (i, c) in s.chars().enumerate() {
        if c.is_digit(10) {
            return c.to_digit(10);
        } else {
            for (&k, &v) in MAP.iter() {
                if s[i..].starts_with(&k) {
                    return Some(v);
                }
            }
        }
    }
    None
}

fn get_first_digit_chars_rev(s: &str) -> Option<u32> {
    for (i, c) in s.chars().enumerate() {
        if c.is_digit(10) {
            return c.to_digit(10);
        } else {
            for (&k, &v) in MAP.iter() {
                if s[i..].starts_with(&k.chars().rev().collect::<String>()) {
                    return Some(v);
                }
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT1: &str = "1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet";

    const INPUT2: &str = "two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen";

    #[test]
    fn part1() {
        let result = process_part1(INPUT1);
        assert_eq!(result, "142");
    }

    #[test]
    fn part2() {
        let result = process_part2(INPUT2);
        assert_eq!(result, "281");
    }
}
