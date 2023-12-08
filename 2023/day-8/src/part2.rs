use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha0, digit0, newline},
    multi::separated_list1,
    sequence::{delimited, tuple},
    IResult,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::collections::HashMap;

fn from_char(c: char) -> usize {
    match c {
        'L' => 0,
        'R' => 1,
        _ => 3,
    }
}

fn line(input: &str) -> IResult<&str, (String, (String, String))> {
    let (input, curr_poz) = alt((tuple((digit0, alpha0)), tuple((alpha0, digit0))))(input)?;
    let (input, v) = delimited(
        tag(" = ("),
        separated_list1(
            tag(", "),
            alt((tuple((digit0, alpha0)), tuple((alpha0, digit0)))),
        ),
        tag(")"),
    )(input)?;

    let e0 = v.get(0).unwrap();
    let e1 = v.get(1).unwrap();

    Ok((
        input,
        (
            format!("{}{}", curr_poz.0, curr_poz.1),
            (format!("{}{}", e0.0, e0.1), format!("{}{}", e1.0, e1.1)),
        ),
    ))
}

fn parse(input: &str) -> HashMap<String, (String, String)> {
    let (_, lines) = separated_list1(newline, line)(input).unwrap();

    lines
        .into_iter()
        .fold(HashMap::new(), |mut acc, (id, (v1, v2))| {
            acc.insert(id, (v1, v2));
            acc
        })
}

pub fn process(s: &str) -> String {
    let lines: Vec<&str> = s.split("\n\n").collect();

    let moves = lines[0].chars().map(from_char).collect::<Vec<usize>>();
    let map = parse(lines[1]);

    let start_values: Vec<String> = map.keys().filter(|s| s.ends_with("A")).cloned().collect();

    println!("start_values: {:?}", start_values);

    let cycles = start_values
        .into_par_iter()
        .map(|node| {
            let mut visited = vec![node.clone()];
            let mut current_node = node;

            moves
                .iter()
                .cycle()
                .enumerate()
                .find_map(|(idx, mve)| {
                    let dirs = map.get(&current_node).expect("next move should exist");

                    let next_node = match mve {
                        0 => dirs.0.clone(),
                        1 => dirs.1.clone(),
                        _ => "".to_string(),
                    };

                    if idx % 100 == 0 {
                        println!(
                            "- iteration {}:\n\t- current_node: {}\n\t- next_node: {}\n\t",
                            idx, current_node, next_node
                        )
                    }

                    if next_node.ends_with("Z") {
                        Some(idx + 1)
                    } else {
                        visited.push(next_node.clone());
                        current_node = next_node;
                        None
                    }
                })
                .expect("should find cycle")
        })
        .collect::<Vec<usize>>();

    let min_cycle = lcm(&cycles);

    min_cycle.to_string()

    // moves
    //     .iter()
    //     .cycle()
    //     .scan(start_values, |state, &current_move| {
    //         if finish(state.clone()) {
    //             None
    //         } else {
    //             // println!("state: {:?}, current_move: {}", state, current_move);
    //             *state = state
    //                 .par_iter()
    //                 .map(|x| {
    //                     let v = map.get(x).expect("next move should exist");
    //                     v.get(current_move).unwrap().clone()
    //                 })
    //                 .collect();
    //             // *state = state.iter().fold(vec![], |mut acc, x| {
    //             //     let v = map.get(x).expect("next move should exist");
    //             //     let next = v.get(current_move).unwrap().clone();
    //             //     acc.push(next);
    //             //     acc
    //             // });
    //             // *state = m.iter().fold(vec![], |mut acc, vv| {
    //             //     let s = vv.get(current_move).unwrap();
    //             //     acc.push(s.clone());
    //             //     acc
    //             // });
    //             Some("AAA".to_string())
    //         }
    //     })
    //     .count()
    //     .to_string()
}

fn lcm(cycles: &[usize]) -> usize {
    if cycles.len() == 1 {
        return cycles[0];
    }
    let a = cycles[0];
    let b = lcm(&cycles[1..]);
    a * b / gcd(a, b)
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        return a;
    }
    gcd(b, a % b)
}
