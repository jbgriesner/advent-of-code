pub mod part1;
pub mod part2;

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT1: &str = "..F7.
.FJ|.
SJ.L7
|F--J
LJ...";

    // ..F7.
    // .FJ|.
    // SJ.L7
    // |F--J
    // LJ...

    const INPUT2: &str = "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........";

    const INPUT3: &str = "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L";

    // "FF7FSF7F7F7F7F7F---7
    //  L|LJ||||||||||||F--J
    //  FL-7LJLJ||||||LJL-77
    //  F--JF--7||LJLJ7F7FJ-
    //  L---JF-JLJ.||-FJLJJ7
    //  |F|F-JF---7F7-L7L|7|
    //  |FFJF7L7F-JF7|JL---7
    //  7-L-JL7||F7|L7F-7F7|
    //  L.L7LFJ|||||FJL7||LJ
    //  L7JLJL-JLJLJL--JLJ.L"

    #[test]
    fn part1() {
        let result = part1::process(INPUT1);
        assert_eq!(result, "8");
    }

    #[test]
    fn part2() {
        let result = part2::process(INPUT3);
        // assert_eq!(result, "10");
        // assert_eq!(result, "4");
        assert_eq!(result, "101");
    }
}
