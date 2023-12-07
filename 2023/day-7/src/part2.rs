use std::collections::HashMap;

fn card_value(card: char) -> usize {
    match card {
        'A' => 14,
        'K' => 13,
        'Q' => 12,
        'J' => 1,
        'T' => 10,
        _ => card.to_digit(10).unwrap() as usize,
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
enum HandType {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
struct Hand {
    hand_type: HandType,
    cards: [usize; 5],
    bid: u32,
}

impl Hand {
    fn new(input: &str) -> Hand {
        let v = input.split(' ').collect::<Vec<&str>>();
        let cards_str = v[0];

        let cards: Vec<usize> = cards_str.chars().map(|c| card_value(c)).collect();

        let mut card_count = HashMap::new();
        for &card in &cards {
            *card_count.entry(card).or_insert(0) += 1;
        }

        let max_count = *card_count.values().max().unwrap();
        let len = card_count.len();
 
        let hand_type = match card_count.get(&1).unwrap_or(&0) {
          0 => evaluate_hand(max_count, len),
          1 => match (max_count, len) {
            (1, 5) => evaluate_hand(2, 4),
            (2, 4) => evaluate_hand(3, 3),
            (2, 3) => evaluate_hand(3, 2),
            (3, 3) => evaluate_hand(4, 2),
            (4, 2) => evaluate_hand(5, 1),
              _ => evaluate_hand(max_count, len),
          },
          2 => {
            match (max_count, len) {
              (2, 3) => evaluate_hand(4, 2),
              (3, 2) => evaluate_hand(5, 1),
              (2, 4) => evaluate_hand(3, 3),
              _ => evaluate_hand(max_count, len),
            }
          },
          3 => {
            match (max_count, len) {
              (3, 3) => evaluate_hand(4, 2),
              (3, 2) => evaluate_hand(5, 1),
              (2, 4) => evaluate_hand(3, 3),
              _ => evaluate_hand(max_count, len),
            }
          },
          4 => {
            match (max_count, len) {
              (4, 2) => evaluate_hand(5, 1),
              _ => evaluate_hand(max_count, len),
            }
          },
          5 => HandType::FiveOfAKind,
          _ => evaluate_hand(max_count, len),
        };  

        Hand {
            hand_type,
            cards: cards.try_into().unwrap(),
            bid: v[1].parse::<u32>().unwrap(),
        }
    }
}

fn evaluate_hand(max_count: i32, len: usize) -> HandType {
  match (max_count, len) {
    (5, 1) => HandType::FiveOfAKind,
    (4, 2) => HandType::FourOfAKind,
    (3, 2) => HandType::FullHouse,
    (3, 3) => HandType::ThreeOfAKind,
    (2, 3) => HandType::TwoPair,
    (2, 4) => HandType::OnePair,
    _ => HandType::HighCard,
  }
}

fn compare_hands(hand1: &Hand, hand2: &Hand) -> std::cmp::Ordering {
    match hand1.hand_type.cmp(&hand2.hand_type) {
        std::cmp::Ordering::Equal => hand2.cards.cmp(&hand1.cards),
        ordering => ordering,
    }
}

fn calculate_winnings(lines: Vec<&str>) -> u32 {
    let mut ranked_hands: Vec<Hand> = lines.iter().map(|&h| Hand::new(h)).collect();
    ranked_hands.sort_by(|a, b| compare_hands(b, a));

    let mut total_winnings = 0;
    let mut rank = 1;

    for hand in ranked_hands {
      total_winnings += hand.bid * rank;
      rank += 1;
    }
    total_winnings
}

pub fn process(s: &str) -> String {
  let lines: Vec<&str> = s.lines().collect();

  calculate_winnings(lines).to_string()
}