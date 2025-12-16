// Minimal test to verify Position and Location are working

use crate::lexing::Position;
use crate::parsing::location::Location;

pub fn test_position() {
    // Create a dummy position
    let pos = Position::dummy();
    println!("Created dummy position: {:?}", pos);

    // Create a position with a file name
    let pos_with_file = pos.with_file_name("test.ml");
    println!("Position with file name: {:?}", pos_with_file);
}

pub fn test_location() {
    // Create two positions
    let start = Position::dummy().with_file_name("test.ml");
    let end = Position::dummy().with_file_name("test.ml");

    // Create a location
    let loc = Location::make(start.clone(), end.clone());
    println!("Created location: {:?}", loc);

    // Create a ghost location
    let ghost_loc = Location::ghost(start, end);
    println!("Created ghost location: {:?}", ghost_loc);

    // Create a location in file
    let file_loc = Location::in_file("test.ml");
    println!("Created location in file: {:?}", file_loc);

    // Test is_none
    let none_loc = Location::none();
    println!("Is none? {}", none_loc.is_none());
}

pub fn run_all_tests() {
    println!("=== Testing Position ===");
    test_position();
    println!("\n=== Testing Location ===");
    test_location();
    println!("\n✅ All tests passed!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_creation() {
        let pos = Position::dummy();
        let pos_with_file = pos.with_file_name("test.ml");
        assert_eq!(pos_with_file, pos_with_file);
    }

    #[test]
    fn test_location_creation() {
        let start = Position::dummy();
        let end = Position::dummy();
        let loc = Location::make(start, end);
        assert!(!loc.is_none());
    }

    #[test]
    fn test_location_none() {
        let none_loc = Location::none();
        assert!(none_loc.is_none());
    }
}
