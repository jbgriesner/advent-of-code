pub fn process_part1(s: &str) -> String {
    let rez = "one";
    rez.to_string()
}

pub fn process_part2(s: &str) -> String {
    let rez = "two";
    rez.to_string()
}


#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "";

    #[test]
    fn part1() {
        let result = process_part1(INPUT);
        // assert_eq!(result, "");
    }

    #[test]
    fn part2() {
        let result = process_part2(INPUT);
        // assert_eq!(result, "");
    }
}
