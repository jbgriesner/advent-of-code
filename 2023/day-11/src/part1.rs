use std::collections::{HashMap, VecDeque};

use indicatif::ParallelProgressIterator;
use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};

#[derive(Debug, Clone)]
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

    for row in image.iter() {
        if row.iter().all(|c| *c == '.') {
            new_image.push(row.clone());
            new_image.push(row.clone());
        } else {
            new_image.push(row.clone());
        }
    }

    let mut x = 0;
    while x < new_image[0].len() {
        let column = get_column(&new_image, x);
        if column.iter().all(|x| *x == '.') {
            insert_column(&column, x, &mut new_image);
            x += 1;
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
    let mut visited = HashMap::new();

    queue.push_back((start.x, start.y, 0));
    visited.insert((start.x, start.y), true);

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
                    queue.push_back((nx, ny, steps + 1));
                    visited.insert((nx, ny), true);
                }
            }
        }
    }
    0
}

fn show_image(s: &Vec<Vec<char>>) -> String {
    let mut rez = "".to_string();
    for line in s.iter() {
        rez.push_str("\n");
        for c in line.iter() {
            rez.push(c.clone());
        }
    }
    rez
}

pub fn process(input: &str) -> String {
    let mut image: Vec<Vec<char>> = input.lines().map(|s| s.chars().collect()).collect();

    let galaxies = expand_universe(&mut image);
    println!("after expansion: \n{}", show_image(&image));
    println!("galaxies: \n{:?}", galaxies);

    let mut pairs: Vec<(Galaxy, Galaxy)> = vec![];
    for (i, start) in galaxies.iter().enumerate() {
        for end in galaxies.iter().skip(i + 1) {
            pairs.push((start.clone(), end.clone()));
        }
    }

    let total_length = pairs
        .into_par_iter()
        .progress()
        .map(|(g1, g2)| shortest_path(&image, &g1, &g2))
        .sum::<usize>();

    println!("Sum of lengths: {}", total_length);
    total_length.to_string()
}
