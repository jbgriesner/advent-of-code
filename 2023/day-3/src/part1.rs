use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, line_ending, satisfy},
    multi::{many1, separated_list1},
    IResult,
};
use std::collections::HashSet;

#[derive(Debug, Clone)]
struct Grid {
    cells: Vec<Vec<Cell>>,
    single_cells: Vec<Vec<Cell>>,
    width: usize,
    height: usize,
}

fn remove_duplicates(vec: Vec<(u32, u32)>) -> Vec<(u32, u32)> {
    let mut unique_set: HashSet<_> = HashSet::new();

    vec.into_iter()
        .filter(|&element| unique_set.insert(element))
        .collect()
}

impl Grid {
    fn show(&self) -> String {
        let mut rez = String::from("\n");
        for r in self.single_cells.iter() {
            for c in r {
                match c {
                    Cell::Empty => rez.push('.'),
                    Cell::Symbol => rez.push('S'),
                    Cell::Num(x) => rez.push(x.to_string().chars().collect::<Vec<char>>()[0]),
                }
            }
            rez.push('\n');
        }
        rez
    }

    fn get_neighs(&self, x: u32, y: u32) -> Vec<(u32, u32)> {
        let mut rez = vec![];
        if x < (self.height - 1) as u32 {
            rez.push((x + 1, y));
            if y < (self.width - 1) as u32 {
                rez.push((x + 1, y + 1));
            }
        }
        if x > 0 {
            rez.push((x - 1, y));
            if y < (self.width - 1) as u32 {
                rez.push((x - 1, y + 1));
            }
            if y > 0 {
                rez.push((x - 1, y - 1));
            }
        }
        if y > 0 {
            rez.push((x, y - 1));
            if x < (self.height - 1) as u32 {
                rez.push((x + 1, y - 1));
            }
        }
        if y < (self.width - 1) as u32 {
            rez.push((x, y + 1));
        }
        rez
    }

    fn init(g: Vec<Vec<Cell>>) -> Self {
        let single_cells = g
            .clone()
            .iter()
            .map(|r| {
                let mut rez = vec![];
                for c in r {
                    let s = c.size();
                    if s == 1 {
                        rez.push(c.clone());
                    } else {
                        if let Cell::Num(x) = c {
                            let y: Vec<u32> = x
                                .to_string()
                                .chars()
                                .map(|d| d.to_digit(10).unwrap())
                                .collect();
                            for n in y {
                                rez.push(Cell::Num(n))
                            }
                        }
                    }
                }
                rez
            })
            .collect();
        Grid {
            height: g.len(),
            width: g[0].iter().fold(0, |acc, c| c.size() + acc),
            cells: g,
            single_cells,
        }
    }

    fn get_cell_at(&self, x: u32, y: u32) -> Cell {
        let row: &Vec<Cell> = &self.single_cells[x as usize];
        return row[y as usize].clone();
    }

    fn get_valid_numbers(&self) -> Vec<u32> {
        let mut rez: Vec<u32> = vec![];
        for (i, row) in self.cells.iter().enumerate() {
            // println!("-- Grid: {}", self.show());
            // println!("-- ROW-{}: {:?}", i, row);
            let mut buf = 0;
            for (j, cell) in row.iter().enumerate() {
                // println!("      - cell-{}: {:?}", j, cell);
                if let &Cell::Num(num) = cell {
                    println!("      - cell-{}[buf={}]: {:?}", j, buf, cell);
                    let mut positions_to_check: Vec<(u32, u32)> = vec![];
                    for k in 0..cell.size() {
                        positions_to_check
                            .push((i.try_into().unwrap(), (j + k + buf).try_into().unwrap()));
                    }
                    buf += cell.size() - 1;
                    let positions_to_check: Vec<(u32, u32)> = remove_duplicates(
                        positions_to_check
                            .iter()
                            .flat_map(|(x, y)| self.get_neighs(*x, *y))
                            .collect(),
                    );
                    println!(
                        "                       - positions_to_check: {:?}",
                        positions_to_check
                    );
                    if positions_to_check.iter().any(|(x, y)| {
                        if Cell::Symbol == self.get_cell_at(*x, *y) {
                            println!("true for ({}, {})", *x, *y);
                            true
                        } else {
                            false
                        }
                    }) {
                        rez.push(num);
                        continue;
                    }
                }
            }
        }
        rez
    }
}

fn parse_digit(input: &str) -> IResult<&str, Cell> {
    let (input, d) = digit1(input)?;
    Ok((input, Cell::Num(d.parse::<u32>().unwrap())))
}

fn parse_empty(input: &str) -> IResult<&str, Cell> {
    let (input, _) = tag(".")(input)?;
    Ok((input, Cell::Empty))
}

fn parse_symbol(input: &str) -> IResult<&str, Cell> {
    let (input, _) = satisfy(|c| c != '\n')(input)?;
    Ok((input, Cell::Symbol))
}

fn cell(input: &str) -> IResult<&str, Cell> {
    let (input, r) = alt((parse_digit, parse_empty, parse_symbol))(input)?;
    Ok((input, r))
}

fn line(input: &str) -> IResult<&str, Vec<Cell>> {
    many1(cell)(input)
}

pub fn process(input: &str) -> String {
    let r = separated_list1(line_ending, line)(input).unwrap().1;
    let g = Grid::init(r);

    println!("Grid: {}", g.show());
    // println!("0, 2: {:?}", g.get_cell_at(0, 2));
    // println!("0, 3: {:?}", g.get_cell_at(0, 3));
    // println!("2, 3: {:?}", g.get_cell_at(2, 3));
    let valid_numbers = g.get_valid_numbers();
    println!("valid_numbers: {:?}", valid_numbers);
    // println!("Cell at 1, 3: {:?}", g.get_cell_at(1, 3));
    valid_numbers.iter().sum::<u32>().to_string()
}

#[derive(Debug, Clone, PartialEq)]
enum Cell {
    Empty,
    Symbol,
    Num(u32),
}

impl Cell {
    fn size(&self) -> usize {
        match self {
            Cell::Empty => 1,
            Cell::Symbol => 1,
            Cell::Num(x) => (0..).take_while(|i| 10u32.pow(*i) <= *x).count(),
        }
    }
}
