use nom::{
    bytes::complete::tag,
    character::complete::{digit1, line_ending, space1},
    multi::separated_list1,
    sequence::{preceded, tuple},
    IResult,
};

#[derive(Debug, PartialEq)]
struct Card {
    id: u32,
    win_numbers: Vec<u32>,
    numbers: Vec<u32>,
}

fn parse_card(input: &str) -> IResult<&str, Card> {
    let (input, id) = preceded(tuple((tag("Card"), space1)), digit1)(input)?;
    let (input, wins) =
        preceded(tuple((tag(":"), space1)), separated_list1(space1, digit1))(input)?;
    let (input, nums) = preceded(
        tuple((space1, tag("|"), space1)),
        separated_list1(space1, digit1),
    )(input)?;

    Ok((
        input,
        Card {
            id: id.parse::<u32>().unwrap(),
            win_numbers: wins.iter().map(|n| n.parse::<u32>().unwrap()).collect(),
            numbers: nums.iter().map(|n| n.parse::<u32>().unwrap()).collect(),
        },
    ))
}

fn parse_cards(input: &str) -> Vec<Card> {
    separated_list1(line_ending, parse_card)(input).unwrap().1
}

fn compute_card(acc: u32, card: &Card) -> u32 {
    let nums = &card.numbers;
    let rez: u32 = card
        .win_numbers
        .iter()
        .filter(|&i| nums.contains(i))
        .count() as u32;
    if rez == 0 {
        acc
    } else {
        acc + u32::pow(2, rez - 1)
    }
}

pub fn process(input: &str) -> String {
    let cards = parse_cards(input);
    println!("-- Cards: {:?}", cards);
    let rez: u32 = cards.iter().fold(0, compute_card);
    rez.to_string()
}
