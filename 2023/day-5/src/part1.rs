use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, digit1, newline, space1},
    multi::separated_list1,
    sequence::{preceded, tuple},
    IResult,
};
use num_bigint::{BigUint, ToBigUint};

#[derive(Debug, PartialEq, Clone, Copy)]
enum Category {
    Seed,
    Soil,
    Fertilizer,
    Water,
    Light,
    Temperature,
    Humidity,
    Location,
    Error,
}

impl Default for Category {
    fn default() -> Self {
        Category::Seed
    }
}

impl From<&str> for Category {
    fn from(input: &str) -> Self {
        match input {
            "seed" => Category::Seed,
            "soil" => Category::Soil,
            "fertilizer" => Category::Fertilizer,
            "water" => Category::Water,
            "light" => Category::Light,
            "temperature" => Category::Temperature,
            "humidity" => Category::Humidity,
            "location" => Category::Location,
            _ => Category::Error,
        }
    }
}

#[derive(Debug, PartialEq, Default, Clone)]
struct Map {
    from: Category,
    to: Category,
    ranges: Vec<Range>,
}

impl Map {
    fn mapping(&self, v: BigUint) -> BigUint {
        for range in &self.ranges {
            if v > range.source_range_start.to_biguint().unwrap()
                && v < range.source_range_start.to_biguint().unwrap()
                    + range.range_length.to_biguint().unwrap()
            {
                let diff = v - range.source_range_start;
                return range.destination_range_start.to_biguint().unwrap() + diff;
            }
        }
        v
    }
}

#[derive(Debug, PartialEq, Default, Clone)]
struct Range {
    destination_range_start: u32,
    source_range_start: u32,
    range_length: u32,
}

#[derive(Debug, PartialEq, Default, Clone)]
struct Finder {
    current_category: Category,
    maps: Vec<Map>,
}

impl Finder {
    fn new(m: Vec<Map>) -> Self {
        Self {
            current_category: Category::Seed,
            maps: m,
        }
    }

    fn find(self, start: u32) -> BigUint {
        let mut curr = start.to_biguint().unwrap();
        for m in self {
            curr = m.mapping(curr);
            println!("     curr -> {}\n", curr);
        }
        curr
    }
}

impl Iterator for Finder {
    type Item = Map;

    fn next(&mut self) -> Option<Self::Item> {
        match self
            .maps
            .iter()
            .filter(|&maps| maps.from == self.current_category)
            .collect::<Vec<&Map>>()
            .get(0)
        {
            Some(&map) => {
                self.current_category = map.to;
                Some(map.clone())
            }
            None => None,
        }
    }
}

fn parse_seeds(input: &str) -> IResult<&str, Vec<&str>> {
    preceded(tag("seeds: "), separated_list1(space1, digit1))(input)
}

fn parse_map(input: &str) -> IResult<&str, Map> {
    let (input, from) = take_until("-to-")(input)?;
    let (input, to) = preceded(tag("-to-"), alpha1)(input)?;
    let (input, _) = tuple((space1, tag("map:"), newline))(input)?;

    let (input, v) = separated_list1(newline, separated_list1(space1, digit1))(input)?;

    let map = Map {
        from: from.into(),
        to: to.into(),
        ranges: v
            .iter()
            .map(|x| Range {
                destination_range_start: x[0].parse::<u32>().unwrap(),
                source_range_start: x[1].parse::<u32>().unwrap(),
                range_length: x[2].parse::<u32>().unwrap(),
            })
            .collect(),
    };

    Ok((input, map))
}

fn parse_maps(input: &str) -> IResult<&str, Vec<Map>> {
    preceded(
        tuple((newline, newline)),
        separated_list1(tuple((newline, newline)), parse_map),
    )(input)
}

fn parse(input: &str) -> (Vec<u32>, Finder) {
    let (input, seeds) = parse_seeds(input).expect("should parse seeds");

    let seeds: Vec<u32> = seeds.iter().map(|&s| s.parse::<u32>().unwrap()).collect();

    let maps = parse_maps(input).expect("should parse maps");

    (seeds, Finder::new(maps.1))
}

pub fn process(input: &str) -> String {
    let mut rez = vec![];
    let (seeds, finder) = parse(input);

    for seed in seeds {
        let location = finder.clone().find(seed);
        rez.push(location);
    }

    rez.into_iter().min().unwrap().to_string()
}
