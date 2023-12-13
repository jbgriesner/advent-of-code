fn count_arrangements(row: &str, groups: &[usize]) -> usize {
    if let Some((first, rest)) = groups.split_first() {
        let mut count = 0;

        // Try placing the first group in different positions
        for i in 0..=row.len().checked_sub(*first).unwrap_or(0) {
            let (prefix, suffix) = row.split_at(i);
            let mut new_row = String::with_capacity(row.len());
            new_row.push_str(prefix);
            new_row.push_str(&".".repeat(*first));
            new_row.push_str(suffix);

            count += count_arrangements(&new_row, rest);
        }

        count
    } else {
        // If there are no more groups, check if the row is a valid arrangement
        if row.chars().all(|c| c == '.' || c == '#') {
            1
        } else {
            0
        }
    }
}

pub fn process(s: &str) -> String {
    let total_arrangements: usize = s
        .lines()
        .map(|line| {
            let parts: Vec<&str> = line.split_whitespace().collect();
            let row = parts[0];
            let groups: Vec<usize> = parts[1].split(',').map(|s| s.parse().unwrap()).collect();
            count_arrangements(row, &groups)
        })
        .sum();
    total_arrangements.to_string()
}
