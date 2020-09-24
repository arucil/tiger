
parsergens::parsergen!(mod parser: LALR("parser.pg"));

pub fn parse(input: &str) {
    parser::with_input(input).parse_program();
}
