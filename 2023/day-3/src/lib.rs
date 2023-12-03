pub mod part1;
pub mod part2;

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT1: &str = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";

    const INPUT2: &str = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";

    #[test]
    fn part1() {
        let result = part1::process(INPUT1);
        assert_eq!(result, "4361");
    }

    #[test]
    fn part2() {
        let result = part2::process(INPUT2);
        assert_eq!(result, "467835");
    }
}
