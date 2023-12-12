use std::collections::{HashMap, HashSet, VecDeque};

use indicatif::ParallelProgressIterator;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Galaxy {
    x: usize,
    y: usize,
    id: usize,
}

fn get_column(image: &Vec<Vec<char>>, x: usize) -> Vec<char> {
    image.iter().fold(vec![], |mut acc, row| {
        acc.push(row[x]);
        acc
    })
}

fn insert_column(column: &Vec<char>, x: usize, new_image: &mut Vec<Vec<char>>) {
    for i in 0..(column.len()) {
        let char_to_insert = column[i];
        new_image[i].insert(x, char_to_insert);
    }
}

fn expand_universe(image: &mut Vec<Vec<char>>) -> Vec<Galaxy> {
    let mut rez = vec![];
    let mut new_image = Vec::new();

    print!("GERERERE");
    for row in image.iter() {
        if row.iter().all(|c| *c == '.') {
            // new_image.push(vec![char::from_digit(6, 10).unwrap(); row.len()]);

            let mut idx = 0;
            // loop {
            new_image.push(row.clone());
            idx += 1;
            if idx == 9 {
                break;
            }
            // }
        } else {
            new_image.push(row.clone());
        }
    }
    print!("GERERERE 22222");

    let mut x = 0;
    while x < new_image[0].len() {
        let column = get_column(&new_image, x);
        if column.iter().all(|x| *x == '.' || *x == '6') {
            // let ncolumn: Vec<char> = column
            //     .iter()
            //     .map(|c| if *c == '6' { '8' } else { '7' })
            //     .collect();
            // insert_column(&ncolumn, x, &mut new_image);

            let mut idx = 0;
            // loop {
            insert_column(&column, x, &mut new_image);
            idx += 1;
            if idx == 9 {
                break;
            }
            x += 1;
            // }
        }
        x += 1;
    }

    for (j, row) in new_image.iter().enumerate() {
        for (i, el) in row.iter().enumerate() {
            if *el == '#' {
                rez.push(Galaxy {
                    x: i,
                    y: j,
                    id: rez.len(),
                })
            }
        }
    }
    *image = new_image;
    rez
}

fn shortest_path(image: &Vec<Vec<char>>, start: &Galaxy, end: &Galaxy) -> usize {
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();

    queue.push_back((start.x, start.y, 0));
    visited.insert((start.x, start.y));

    let mut previous = '.';

    while let Some((x, y, steps)) = queue.pop_front() {
        if x == end.x && y == end.y {
            return steps;
        }

        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)].iter() {
            let nx = x as isize + *dx;
            let ny = y as isize + *dy;

            if nx >= 0 && ny >= 0 && nx < image[0].len() as isize && ny < image.len() as isize {
                let nx = nx as usize;
                let ny = ny as usize;

                if visited.get(&(nx, ny)).is_none() {
                    let visiting = image[ny][nx];
                    match visiting {
                        '6' => {
                            if previous == '6' {
                                queue.push_back((nx, ny, steps + 1))
                            } else {
                                previous = '6';
                                queue.push_back((nx, ny, steps + 100))
                            }
                        }
                        '7' => {
                            if previous == '7' {
                                queue.push_back((nx, ny, steps + 1))
                            } else {
                                previous = '7';
                                queue.push_back((nx, ny, steps + 100))
                            }
                        }
                        '8' => {
                            if previous == '8' {
                                queue.push_back((nx, ny, steps + 1))
                            } else {
                                previous = '8';
                                queue.push_back((nx, ny, steps + 100))
                            }
                        }
                        _ => {
                            previous = visiting;
                            queue.push_back((nx, ny, steps + 1))
                        }
                    }
                    visited.insert((nx, ny));
                }
            }
        }
    }
    0
}

fn show_image(s: &Vec<Vec<char>>, gal: &Vec<Galaxy>) -> String {
    let mut rez = "".to_string();
    for (j, line) in s.iter().enumerate() {
        rez.push_str("\n");
        for (i, c) in line.iter().enumerate() {
            if let Some(g) = gal.iter().find(|&g| g.x == i && g.y == j) {
                if g.id.to_string().chars().collect::<Vec<char>>().len() == 1 {
                    rez.push(char::from_digit(g.id as u32, 10).unwrap());
                } else {
                    rez.push(c.clone());
                }
            } else {
                rez.push(c.clone());
            }
        }
    }
    rez
}

pub fn process(input: &str) -> String {
    let mut image: Vec<Vec<char>> = input.lines().map(|s| s.chars().collect()).collect();

    println!("before expansion: \n{}", show_image(&image, &vec![]));
    let galaxies = expand_universe(&mut image);
    println!("after expansion: \n{}", show_image(&image, &galaxies));
    // println!("galaxies: \n{:?}", galaxies);

    // let mut pairs: Vec<(Galaxy, Galaxy)> = vec![];
    // for (i, start) in galaxies.iter().enumerate() {
    //     for end in galaxies.iter().skip(i + 1) {
    //         pairs.push((start.clone(), end.clone()));
    //     }
    // }

    // println!("pairs: {:?}", pairs.sort());

    // let total_length = pairs
    //     .into_iter()
    //     // .progress()
    //     .map(|(g1, g2)| {
    //         let s = shortest_path(&image, &g1, &g2);
    //         println!("shortest path between {} and {} is {}", g1.id, g2.id, s);
    //         s
    //     })
    //     .sum::<usize>();

    // println!("Sum of lengths: {}", total_length);
    0.to_string()
}
