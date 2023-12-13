fn count_arrangements(row: &str, groups: &[usize]) -> usize {
    println!("count_arrangements({}, {:?}) -> ", row, groups);
    if let Some((first, rest)) = groups.split_first() {
        let mut count = 0;

        for i in 0..=row.len().checked_sub(*first).unwrap_or(0) {
            let (prefix, suffix) = row.split_at(i);
            let mut new_row = String::with_capacity(row.len());

            let to_remove_from_suffix = match suffix.len().checked_sub(*first + 1) {
                None => match suffix.len().checked_sub(*first) {
                    None => break,
                    Some(_) => *first,
                },
                Some(_) => *first + 1,
            };
            // print!("\ti: {}, first: {}", i, first);
            // println!("\tprefix: {}, suffix: {}", prefix, suffix);
            // println!("\t\tto_remove_from_suffix: {}", to_remove_from_suffix);

            // println!(
            //     "\t&suffix[to_remove_from_suffix..]: {}",
            //     &suffix[to_remove_from_suffix..]
            // );
            new_row.push_str(prefix);
            new_row.push_str(&".".repeat(*first));
            new_row.push_str(".");
            new_row.push_str(&suffix[to_remove_from_suffix..]);

            // println!("\t\tnew_row: {}", new_row);
            count += count_arrangements(&new_row, rest);
            // println!("count: {}", count);
        }
        count
    } else {
        // If there are no more groups, check if the row is a valid arrangement
        if row.chars().all(|c| c == '.') {
            println!("\t\t\t WOOOOOW -> groups: {:?}, last row: {}", groups, row);
            1
        } else {
            0
        }
    }
}

fn process_line(line: &str) -> usize {
    let parts: Vec<&str> = line.split_whitespace().collect();
    let row = parts[0];
    let groups: Vec<usize> = parts[1].split(',').map(|s| s.parse().unwrap()).collect();

    println!("row: {}", row);
    println!("groups: {:?}", groups);

    count_arrangements(row, &groups)
}

pub fn process(s: &str) -> String {
    let total_arrangements: usize = s.lines().map(process_line).sum();
    total_arrangements.to_string()
}
