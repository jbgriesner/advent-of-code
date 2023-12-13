pub mod part1;
pub mod part2;

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT1: &str = "???.### 1,1,3";
    // .??..??...?##. 1,1,3
    // ?#?#?#?#?#?#?#? 1,3,1,6
    // ????.#...#... 4,1,1
    // ????.######..#####. 1,6,5
    // ?###???????? 3,2,1";

    const INPUT2: &str = "";

    #[test]
    fn part1() {
        let result = part1::process(INPUT1);
        assert_eq!(result, "21");
    }

    #[test]
    fn part2() {
        let result = part2::process(INPUT2);
        // assert_eq!(result, "");
    }
}
