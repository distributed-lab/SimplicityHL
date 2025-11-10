#![cfg_attr(fuzzing, no_main)]

#[cfg(any(fuzzing, test))]
fn do_test(value: simplicityhl::value::Value) {
    use simplicityhl::value::Value;

    let value_string = value.to_string();
    let parsed_value =
        Value::parse_from_str(&value_string, value.ty()).expect("Value string should be parseable");
    assert_eq!(
        value, parsed_value,
        "Value string should parse to original value"
    );
}

#[cfg(not(fuzzing))]
fn main() {}

#[cfg(fuzzing)]
libfuzzer_sys::fuzz_target!(|data: simplicityhl::value::Value| {
    do_test(data);
});

#[cfg(test)]
mod test {
    use simplicityhl::{types::TypeConstructible, value::Value, ResolvedType};

    use crate::do_test;
    #[test]
    fn test() {
        let value = Value::parse_from_str("true", &ResolvedType::boolean())
            .expect("should parse a valid value");

        do_test(value);
    }
}
