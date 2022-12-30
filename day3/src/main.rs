mod part_two;

use std::convert::TryInto;
use std::io::{self, BufRead};
use std::collections::HashSet;


fn get_common_items(first_half: Vec<char>, second_half: Vec<char>) -> Vec<char>{
    let a :HashSet<char>= HashSet::from_iter(first_half);
    let b :HashSet<char>= HashSet::from_iter(second_half);
    a.intersection(&b).into_iter().copied().collect()


}

fn calculate_priority(c: char) -> u32 {
    let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".chars();
    for (i, f) in alphabet.enumerate() {
        if f == c {
            let val:u32 = match (i + 1).try_into() {
                Ok(v) => v,
                Err(err) => panic!("{}", err)
            };
            return val
        } 
    }
    panic!("Char not found")
}

fn calculate_line(line:String) -> u32{
    let line_chars = line.char_indices();
    let line_length = line.len();
    let mut first_half = Vec::new();
    let mut second_half = Vec::new();
    for (index, char) in line_chars {
        if index >= line_length / 2 {
            first_half.push(char)
        } else {
            second_half.push(char)
        }
    }

    let common_items = get_common_items(first_half, second_half);
    common_items.iter().fold(0, |acc, v| {
        acc + calculate_priority(*v)
    })

}


fn calculate_all() -> u32{

    let stdin = io::stdin();
    let line_iterator = stdin.lock().lines();

    line_iterator.fold(0, |sum, line| {
        let line_priority = match line {
            Err(error) => panic!("Problem opening the file: {:?}", error),
            Ok(line_string) =>  calculate_line(line_string)
        };
        sum + line_priority
    })

}

fn main() {
    part_two::part_2_calculate()
}

