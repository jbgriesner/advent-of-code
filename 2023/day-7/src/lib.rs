pub mod part1;
pub mod part2;

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT1: &str = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483";

    const INPUT2: &str = INPUT1;

    #[test]
    fn part1() {
        let result = part1::process(INPUT1);
        assert_eq!(result, "6440");
    }

    #[test]
    fn part2() {
        let result = part2::process(INPUT2);
        assert_eq!(result, "5905");
    }
}
