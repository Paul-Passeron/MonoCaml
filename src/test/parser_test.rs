use std::fs;

use crate::parsing::parser::MonoCamlParser;

pub fn test_filename(file_name: &str) {
    let src = fs::read_to_string(file_name).expect("Failed to read file");
    let p = MonoCamlParser::new(file_name, &src);
    p.parse_ocaml_source().unwrap();
}

#[test]
pub fn ex_hw() {
    test_filename("examples/hw.ml")
}

#[test]
pub fn ex_let_test() {
    test_filename("examples/let_test.ml")
}

#[test]
pub fn ex_test1() {
    test_filename("examples/test1.ml")
}

#[test]
pub fn ex_test2() {
    test_filename("examples/test2.ml")
}

#[test]
pub fn ex_test3() {
    test_filename("examples/test3.ml")
}

#[test]
pub fn ex_test4() {
    test_filename("examples/test4.ml")
}

#[test]
pub fn ex_test5() {
    test_filename("examples/test5.ml")
}

#[test]
pub fn ex_test6() {
    test_filename("examples/test6.ml")
}

#[test]
pub fn ex_test7() {
    test_filename("examples/test7.ml")
}

#[test]
pub fn ex_test8() {
    test_filename("examples/test8.ml")
}
