use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, newline},
    multi::separated_list1,
    sequence::delimited,
    IResult,
};
use std::collections::HashMap;

fn from_char(c: char) -> usize {
    match c {
        'L' => 0,
        'R' => 1,
        _ => 3,
    }
}

fn line(input: &str) -> IResult<&str, (String, Vec<String>)> {
    let (input, curr_poz) = alpha1(input)?;

    let (input, v) = delimited(tag(" = ("), separated_list1(tag(", "), alpha1), tag(")"))(input)?;

    Ok((
        input,
        (
            curr_poz.to_string(),
            v.iter().map(|e| e.to_string()).collect(),
        ),
    ))
}

fn parse(input: &str) -> HashMap<String, Vec<String>> {
    let (_, lines) = separated_list1(newline, line)(input).unwrap();

    lines.iter().fold(HashMap::new(), |mut acc, (id, v)| {
        acc.insert(id.clone(), v.clone());
        acc
    })
}

pub fn process(s: &str) -> String {
    let lines: Vec<&str> = s.split("\n\n").collect();

    let moves = lines[0].chars().map(from_char).collect::<Vec<usize>>();
    let map = parse(lines[1]);

    moves
        .iter()
        .cycle()
        .scan("AAA".to_string(), |state, &current_move| {
            match state.as_str() {
                "ZZZ" => None,
                _ => {
                    let m = map.get(state).expect("next move should exist");
                    *state = m.get(current_move).unwrap().to_string();
                    Some("".to_string())
                }
            }
        })
        .count()
        .to_string()
}
