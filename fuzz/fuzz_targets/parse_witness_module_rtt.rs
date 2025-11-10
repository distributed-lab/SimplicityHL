#![cfg_attr(fuzzing, no_main)]

#[cfg(any(fuzzing, test))]
fn do_test(witness_values: simplicityhl::WitnessValues) {
    use simplicityhl::parse::ParseFromStr;
    use simplicityhl::WitnessValues;

    let witness_text = witness_values.to_string();
    let parsed_witness_text =
        WitnessValues::parse_from_str(&witness_text).expect("Witness module should be parseable");
    assert_eq!(
        witness_values, parsed_witness_text,
        "Witness module should parse to original witness values"
    );
}

#[cfg(not(fuzzing))]
fn main() {}

#[cfg(fuzzing)]
libfuzzer_sys::fuzz_target!(|data: simplicityhl::WitnessValues| do_test(data));

#[cfg(test)]
mod test {
    use simplicityhl::{parse::ParseFromStr, WitnessValues};
    #[test]
    fn test() {
        let witness_text = r#"mod witness {
            const A: u32 = 1;
            const B: u32 = 2;
            const C: u32 = 3;
        }"#;

        let witness_values = WitnessValues::parse_from_str(witness_text)
            .expect("parsing of valid string should work");
        super::do_test(witness_values);
    }
}
