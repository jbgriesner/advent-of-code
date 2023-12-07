use day_7::part1::process;
use std::fs;

fn main() {
  let file = fs::read_to_string("./input.txt").unwrap();
  println!("{}", process(&file))
}