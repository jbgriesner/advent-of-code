pub mod part1;
pub mod part2;

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT1: &str = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45";

    const INPUT2: &str = INPUT1;

    #[test]
    fn part1() {
        let result = part1::process(INPUT1);
        assert_eq!(result, "114");
    }

    #[test]
    fn part2() {
        let result = part2::process(INPUT2);
        assert_eq!(result, "2");
    }
}