use std::collections::BTreeMap;

use nom::{
    bytes::complete::tag,
    character::complete::{digit1, line_ending, space1},
    multi::separated_list1,
    sequence::{preceded, tuple},
    IResult,
};

#[derive(Debug, PartialEq, Clone)]
struct Card {
    id: u32,
    win_numbers: Vec<u32>,
    numbers: Vec<u32>,
}

#[derive(Debug, PartialEq, Clone)]
struct CardsMap {
    hmap: BTreeMap<u32, u32>,
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

fn compute_card(mut acc: CardsMap, card: &Card) -> CardsMap {
    let total_num_cards = acc.hmap.len() as u32;
    let current_id = card.id;
    let nums = &card.numbers;
    let current_card_count = acc.hmap.get(&current_id).unwrap() + 1;

    acc.hmap.insert(current_id, current_card_count);

    let matchs: u32 = card
        .win_numbers
        .iter()
        .filter(|&i| nums.contains(i))
        .count() as u32;

    for i in (current_id + 1)..(current_id + matchs + 1) {
        if i < total_num_cards + 1 {
            let old_value = acc.hmap.get(&i).unwrap();
            acc.hmap.insert(i, old_value + current_card_count);
        }
    }
    acc
}

pub fn process(input: &str) -> String {
    let cards = parse_cards(input);
    let num_cards = cards.len() as u32;

    let init_map: BTreeMap<u32, u32> = (1..num_cards + 1).map(|i| (i, 0)).collect();

    let rez: CardsMap = cards.iter().fold(CardsMap { hmap: init_map }, compute_card);

    rez.hmap.into_values().into_iter().sum::<u32>().to_string()
}
