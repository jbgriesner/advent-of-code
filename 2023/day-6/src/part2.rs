use nom::{
    bytes::complete::tag,
    character::complete::{digit1, line_ending, newline, space1},
    multi::separated_list1,
    sequence::{preceded, terminated, tuple},
    IResult,
};

fn ways_to_beat_record(time: u64, distance: u64) -> u64 {
    let mut total_ways = 0;

    for hold_time in 14..=time - 14 {
        let remaining_time = time - hold_time;
        let distance_covered = remaining_time * hold_time;

        if distance_covered > distance {
            total_ways += 1;
        }
    }

    total_ways
}

fn parse(input: &str) -> IResult<&str, (u64, u64)> {
    let (input, _) = tuple((tag("Time:"), space1))(input)?;
    let (input, t) = terminated(separated_list1(space1, digit1), newline)(input)?;
    println!("yy: {:?}", t);
    let (input, _) = tuple((tag("Distance:"), space1))(input)?;
    println!("iii: {}", input);
    let (input, d) = separated_list1(space1, digit1)(input)?;
    println!("iii: {}", input);

    Ok((
        input,
        (
            t.concat().parse::<u64>().unwrap(),
            d.concat().parse::<u64>().unwrap(),
        ),
    ))
}

pub fn process(input: &str) -> String {
    let (time, distance) = parse(input).unwrap().1;

    let result = ways_to_beat_record(time, distance);

    result.to_string()
}
