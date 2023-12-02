use nom::character::complete::digit0;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, line_ending, space0},
    multi::{separated_list0, separated_list1},
    sequence::{preceded, terminated, tuple},
    IResult,
};
use std::{ops::Not, str::FromStr};

const VALID_GRAB: Grab = Grab {
    red: 12,
    green: 13,
    blue: 14,
};

#[derive(Debug)]
enum Color {
    Red,
    Green,
    Blue,
}

#[derive(Debug)]
struct ColorError;

impl FromStr for Color {
    type Err = ColorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            " blue" => Ok(Color::Blue),
            " red" => Ok(Color::Red),
            " green" => Ok(Color::Green),
            _ => Err(ColorError),
        }
    }
}

#[derive(Debug)]
struct NColor {
    n: u32,
    color: Color,
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Clone)]
struct Grab {
    red: u32,
    green: u32,
    blue: u32,
}

#[derive(Debug, Clone)]
struct Game {
    id: u32,
    grabs: Vec<Grab>,
}

impl Game {
    fn valid_game(&self) -> Option<u32> {
        self.grabs
            .iter()
            .any(|g| {
                g.red > VALID_GRAB.red || g.blue > VALID_GRAB.blue || g.green > VALID_GRAB.green
            })
            .not()
            .then_some(self.id)
    }
}

fn parse(color: &str) -> impl Fn(&str) -> IResult<&str, NColor> + '_ {
    move |input| {
        let (input, (_, id)) = terminated(tuple((space0, digit0)), tag(color))(input)?;
        Ok((
            input,
            NColor {
                n: id.parse::<u32>().unwrap(),
                color: Color::from_str(color).unwrap(),
            },
        ))
    }
}

fn parse_color(input: &str) -> IResult<&str, NColor> {
    alt((parse(" blue"), parse(" red"), parse(" green")))(input)
}

fn grab(input: &str) -> IResult<&str, Grab> {
    let mut g = Grab {
        red: 0,
        green: 0,
        blue: 0,
    };
    let (input, v) = separated_list0(tag(", "), parse_color)(input)?;
    for c in v {
        match c.color {
            Color::Blue => g.blue = g.blue + c.n,
            Color::Red => g.red = g.red + c.n,
            Color::Green => g.green = g.green + c.n,
        }
    }
    Ok((input, g))
}

fn game(input: &str) -> IResult<&str, Game> {
    let (input, id) = preceded(tag("Game "), digit1)(input)?;
    let (input, grabs) = preceded(tag(": "), separated_list1(tag("; "), grab))(input)?;

    Ok((
        input,
        Game {
            id: id.parse::<u32>().unwrap(),
            grabs,
        },
    ))
}

fn parse_games(input: &str) -> IResult<&str, Vec<Game>> {
    let (input, games) = separated_list1(line_ending, game)(input)?;
    Ok((input, games))
}

fn invalid_games(g: Vec<Game>) -> Vec<u32> {
    g.into_iter()
        .filter(|game| game.valid_game().map_or(true, |x| false))
        .map(|g| g.id)
        .collect()
}

pub fn process(s: &str) -> String {
    let g = parse_games(s).unwrap().1;
    // println!("GAMES: {:?}", invalid_games(g.clone()));
    g.iter()
        .filter_map(|game| game.valid_game())
        .sum::<u32>()
        .to_string()
}
