use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, newline, space1},
    multi::separated_list1,
    sequence::preceded,
    IResult,
};

fn negative_digit1(input: &str) -> IResult<&str, i64> {
    let (input, n) = preceded(tag("-"), digit1)(input)?;
    Ok((input, ["-", n].concat().parse().unwrap()))
}

fn positive_digit1(input: &str) -> IResult<&str, i64> {
    let (input, n) = digit1(input)?;
    Ok((input, n.parse().unwrap()))
}

fn neg_or_pos(input: &str) -> IResult<&str, i64> {
    alt((positive_digit1, negative_digit1))(input)
}

fn line(input: &str) -> IResult<&str, Vec<i64>> {
    separated_list1(space1, neg_or_pos)(input)
}

fn parse(input: &str) -> Vec<Vec<i64>> {
    separated_list1(newline, line)(input).unwrap().1
}

fn solve(sequence: &Vec<i64>) -> i64 {
    fn close(seq: &Vec<i64>, firsts: &mut Vec<i64>) -> i64 {
        firsts.push(*seq.first().unwrap());
        let next_seq = &seq[..]
            .windows(2)
            .map(|pair| pair[1] - pair[0])
            .collect::<Vec<i64>>();
        if next_seq.iter().all(|&x| x == 0) {
            firsts.iter().rev().fold(0, |acc, x| x - acc)
        } else {
            close(next_seq, firsts)
        }
    }
    let mut init = vec![];
    close(sequence, &mut init)
}

pub fn process(s: &str) -> String {
    let lines = parse(s);
    lines.iter().map(solve).sum::<i64>().to_string()
}
