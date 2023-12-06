use std::cmp;

use nom::{
    bytes::complete::tag,
    character::complete::{digit1, line_ending, newline, space1},
    multi::separated_list1,
    sequence::{preceded, terminated, tuple},
    IResult,
};

fn ways_to_beat_record(time: Vec<u64>, distance: Vec<u64>) -> u64 {
    let mut total_ways = 1;

    for (t, d) in time.iter().zip(distance.iter()) {
        let mut ways_for_race = 0;

        for hold_time in 0..=*t {
            let speed = hold_time;
            let remaining_time = t - hold_time;
            // let distance_covered = cmp::min(hold_time, remaining_time) * speed;
            let distance_covered = remaining_time * speed;

            if distance_covered > *d {
                ways_for_race += 1;
            }
        }

        total_ways *= ways_for_race;
    }

    total_ways
}

fn parse_given_tag(current_tag: &str) -> impl Fn(&str) -> IResult<&str, Vec<u64>> + '_ {
    move |input: &str| {
        let (input, v) = preceded(
            tuple((tag(current_tag), space1)),
            separated_list1(space1, digit1),
        )(input)?;

        Ok((input, v.iter().map(|x| x.parse::<u64>().unwrap()).collect()))
    }
}

fn parse(input: &str) -> (Vec<u64>, Vec<u64>) {
    let (input, t) = terminated(parse_given_tag("Time:"), line_ending)(input).unwrap();
    let (_, d) = parse_given_tag("Distance:")(input).unwrap();

    (t, d)
}

pub fn process(input: &str) -> String {
    let (time, distance) = parse(input);

    let result = ways_to_beat_record(time, distance);

    result.to_string()
}
