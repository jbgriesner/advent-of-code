pub trait MyParser<T, E> {
    fn run_parser(input: &str) -> Result<T, E>;
}

pub struct AnotherParser<T> {
    pub run_parser: fn(input: &str) -> T,
}
