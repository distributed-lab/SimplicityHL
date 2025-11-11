#![cfg_attr(fuzzing, no_main)]

#[cfg(any(fuzzing, test))]
fn do_test(parse_program: simplicityhl::parse::Program) {
    use simplicityhl::parse::{ParseFromStr, Program};

    let program_text = parse_program.to_string();
    let restored_parse_program = Program::parse_from_str(program_text.as_str())
        .expect("Output of fmt::Display should be parseable");
    assert_eq!(
        parse_program, restored_parse_program,
        "Output of fmt::Display should parse to original program"
    );
}

#[cfg(not(fuzzing))]
fn main() {}

#[cfg(fuzzing)]
libfuzzer_sys::fuzz_target!(|data: simplicityhl::parse::Program| {
    do_test(data);
});

#[cfg(test)]
mod test {

    use simplicityhl::parse::{ParseFromStr, Program};
    #[test]
    fn test() {
        let program_test = r#"fn main() {
            assert!(jet::eq_32(witness::A, witness::A));
        }"#;

        let program = Program::parse_from_str(program_test)
            .expect("expected conversion to Program to be successfull");
        super::do_test(program);
    }
}
