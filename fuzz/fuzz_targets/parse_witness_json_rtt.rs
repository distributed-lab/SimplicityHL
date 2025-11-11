#![cfg_attr(fuzzing, no_main)]

#[cfg(any(fuzzing, test))]
fn do_test(witness_values: simplicityhl::WitnessValues) {
    let witness_text = serde_json::to_string(&witness_values)
        .expect("Witness map should be convertible into JSON");
    let parsed_witness_text =
        serde_json::from_str(&witness_text).expect("Witness JSON should be parseable");
    assert_eq!(
        witness_values, parsed_witness_text,
        "Witness JSON should parse to original witness map"
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
