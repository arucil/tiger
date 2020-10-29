
mod parser;

pub fn parse(input: &str) {
    parser::with_input(input).parse_program();
}
