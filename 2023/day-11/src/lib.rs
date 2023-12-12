pub mod part1;
pub mod part2;

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT1: &str = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....";

    const INPUT2: &str = INPUT1;

    #[test]
    fn part1() {
        let result = part1::process(INPUT1);
        assert_eq!(result, "374");
    }

    #[test]
    fn part2() {
        let result = part2::process(INPUT2);
        assert_eq!(result, "");
    }
}
