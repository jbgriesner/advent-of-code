use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, line_ending, satisfy},
    multi::{many1, separated_list1},
    IResult,
};
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, std::hash::Hash)]
enum Cell {
    Empty,
    Gear,
    Symbol,
    Num(u32),
}

impl Cell {
    fn size(&self) -> usize {
        match self {
            Cell::Empty => 1,
            Cell::Symbol => 1,
            Cell::Gear => 1,
            Cell::Num(x) => (0..).take_while(|i| 10u32.pow(*i) <= *x).count(),
        }
    }
}

#[derive(Debug, Clone)]
struct Grid {
    cells: Vec<Vec<Cell>>,
    single_cells: Vec<Vec<(Cell, usize, usize)>>,
    width: usize,
    height: usize,
}

fn remove_duplicates<T: Eq + std::hash::Hash + Clone>(vec: Vec<T>) -> Vec<T> {
    let mut unique_set: HashSet<_> = HashSet::new();

    vec.into_iter()
        .filter(|element| unique_set.insert(element.clone()))
        .collect()
}

impl Grid {
    fn get_gears_poz(&self) -> Vec<(usize, usize)> {
        let mut rez = vec![];
        for (i, r) in self.single_cells.iter().enumerate() {
            for (j, (c, _, _)) in r.iter().enumerate() {
                if let Cell::Gear = c {
                    rez.push((i, j));
                }
            }
        }
        rez
    }

    fn _show(&self) -> String {
        let mut rez = String::from("\n");
        for r in self.single_cells.iter() {
            for c in r {
                match c {
                    (Cell::Empty, _, _) => rez.push('.'),
                    (Cell::Symbol, _, _) => rez.push('S'),
                    (Cell::Gear, _, _) => rez.push('*'),
                    (Cell::Num(x), _, _) => {
                        rez.push(x.to_string().chars().collect::<Vec<char>>()[0])
                    }
                }
            }
            rez.push('\n');
        }
        rez
    }

    fn get_neighs(&self, x: usize, y: usize) -> Vec<(usize, usize)> {
        let mut rez = vec![];
        if x < self.height - 1 {
            rez.push((x + 1, y));
            if y < self.width - 1 {
                rez.push((x + 1, y + 1));
            }
        }
        if x > 0 {
            rez.push((x - 1, y));
            if y < self.width - 1 {
                rez.push((x - 1, y + 1));
            }
            if y > 0 {
                rez.push((x - 1, y - 1));
            }
        }
        if y > 0 {
            rez.push((x, y - 1));
            if x < self.height - 1 {
                rez.push((x + 1, y - 1));
            }
        }
        if y < self.width - 1 {
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
                let mut buf = 0;
                for (i, c) in r.iter().enumerate() {
                    let s = c.size();
                    if s == 1 {
                        rez.push((c.clone(), i + buf, i));
                    } else {
                        if let Cell::Num(x) = c {
                            let y: Vec<u32> = x
                                .to_string()
                                .chars()
                                .map(|d| d.to_digit(10).unwrap())
                                .collect();
                            for n in y {
                                rez.push((Cell::Num(n), buf + i, i))
                            }
                            buf += c.size() - 1;
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

    fn get_actual_num(&self, x: usize, y: usize) -> Cell {
        // println!("        get_actual_num({}, {}) called", x, y);

        let row: &Vec<Cell> = &self.cells[x];
        // println!("            row returned: {:?}", &row);
        let rez = row[y].clone();
        // println!("                 rez: {:?}", rez);
        rez
    }

    fn get_cell_at(&self, x: usize, y: usize) -> (Cell, usize, usize) {
        let row: &Vec<(Cell, usize, usize)> = &self.single_cells[x];
        row[y].clone()
    }
}

fn parse_gear(input: &str) -> IResult<&str, Cell> {
    let (input, _) = tag("*")(input)?;
    Ok((input, Cell::Gear))
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
    alt((parse_digit, parse_empty, parse_gear, parse_symbol))(input)
}

fn line(input: &str) -> IResult<&str, Vec<Cell>> {
    many1(cell)(input)
}

pub fn process(input: &str) -> String {
    let r = separated_list1(line_ending, line)(input).unwrap().1;
    let g = Grid::init(r);

    let gears_poz = g.get_gears_poz();

    let mut rez: Vec<u32> = vec![];

    // println!("Grid: {}", g._show());
    for (row, col) in gears_poz {
        let voisins = g.get_neighs(row, col);
        // println!("  voisins at: {:?}", &voisins);
        let selected_cells: Vec<Cell> = voisins
            .iter()
            .map(|(r, c)| (g.get_cell_at(*r, *c), (*r, *c)))
            .filter(|((c, _, _), _)| match c {
                Cell::Num(_) => true,
                _ => false,
            })
            // .inspect(|x| println!("    ----- INSPECT{:?}", x))
            .map(|((_, _, iii), (ii, _))| g.get_actual_num(ii, iii))
            .collect();
        // println!("selected_cells: {:?}", selected_cells);
        let sel = remove_duplicates(selected_cells);
        // println!("sel: {:?}", sel);
        if sel.len() == 2 {
            if let (Cell::Num(a), Cell::Num(b)) = (sel[0].clone(), sel[1].clone()) {
                rez.push(a * b);
            }
        }
    }
    // println!("Grid: {}", g._show());
    // println!("Grid: {:?}", g);
    // println!("  gears at: {:?}", gears_poz);
    rez.iter().sum::<u32>().to_string()
}
