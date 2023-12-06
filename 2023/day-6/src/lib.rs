pub mod part1;
pub mod part2;

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT1: &str = "Time:      7  15   30
Distance:  9  40  200";

    const INPUT2: &str = "";

    #[test]
    fn part1() {
        let result = part1::process(INPUT1);
        assert_eq!(result, "288");
    }

    #[test]
    fn part2() {
        let result = part2::process(INPUT2);
        assert_eq!(result, "71503");
    }
}
