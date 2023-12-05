use indicatif::ParallelProgressIterator;
use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, digit1, newline, space1},
    multi::separated_list1,
    sequence::{preceded, tuple},
    IResult,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::ops::Range;

#[derive(Debug, PartialEq, Default, Clone)]
struct Maps {
    mappings: Vec<(Range<u64>, Range<u64>)>,
}

impl Maps {
    fn map(&self, v: u64) -> u64 {
        match self.mappings.iter().find(|(r1, _)| r1.contains(&v)) {
            Some((s_range, d_range)) => {
                let diff = v - s_range.start;
                d_range.start + diff
            }
            None => v,
        }
    }
}

fn parse_seeds(input: &str) -> IResult<&str, Vec<&str>> {
    preceded(tag("seeds: "), separated_list1(space1, digit1))(input)
}

fn parse_line(input: &str) -> IResult<&str, (Range<u64>, Range<u64>)> {
    let (input, d) = separated_list1(space1, digit1)(input)?;
    let destination = d[0].parse::<u64>().unwrap();
    let source = d[1].parse::<u64>().unwrap();
    let num = d[2].parse::<u64>().unwrap();
    Ok((
        input,
        (source..(source + num), destination..(destination + num)),
    ))
}

fn parse_map(input: &str) -> IResult<&str, Maps> {
    let (input, _) = take_until("-to-")(input)?;
    let (input, _) = preceded(tag("-to-"), alpha1)(input)?;
    let (input, _) = tuple((space1, tag("map:"), newline))(input)?;

    let (input, mappings) = separated_list1(newline, parse_line)(input)?;

    Ok((input, Maps { mappings }))
}

fn parse_maps(input: &str) -> IResult<&str, Vec<Maps>> {
    preceded(
        tuple((newline, newline)),
        separated_list1(tuple((newline, newline)), parse_map),
    )(input)
}

fn parse(input: &str) -> (Vec<u64>, Vec<Maps>) {
    let (input, seeds) = parse_seeds(input).expect("should parse seeds");

    let seeds: Vec<u64> = seeds.iter().map(|&s| s.parse::<u64>().unwrap()).collect();

    let (_, maps) = parse_maps(input).expect("should parse maps");

    (seeds, maps)
}

pub fn process(input: &str) -> String {
    let (seeds, maps) = parse(input);

    let seeds: Vec<Range<u64>> = seeds
        .chunks(2)
        .filter_map(|chunk| {
            if chunk.len() == 2 {
                Some(chunk[0]..(chunk[0] + chunk[1]))
            } else {
                None
            }
        })
        .collect();

    let locations = seeds
        .iter()
        .flat_map(|range| range.clone().into_iter())
        .collect::<Vec<u64>>();

    let locations = locations
        .into_par_iter()
        .progress()
        .map(|seed| maps.iter().fold(seed, |s, map| map.map(s)))
        .collect::<Vec<u64>>();

    locations.iter().min().unwrap().to_string()
}
