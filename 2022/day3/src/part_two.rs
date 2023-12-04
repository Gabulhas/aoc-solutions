use std::convert::TryInto;
use std::io::{self, BufRead};
use std::collections::HashSet;


fn get_common_items(first: Vec<char>, second: Vec<char>, third: Vec<char>) -> Vec<char>{
    let a :HashSet<char>= HashSet::from_iter(first);
    let b :HashSet<char>= HashSet::from_iter(second);
    let c :HashSet<char>= HashSet::from_iter(third);
    let a_i_b:HashSet<char> = a.intersection(&b).into_iter().copied().collect();
    a_i_b.intersection(&c).into_iter().copied().collect()




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

fn calculate_badge(first: String, second: String, third: String) -> u32{
    let common = get_common_items(first.chars().collect(), second.chars().collect(), third.chars().collect());
    common.into_iter().fold(0, |acc, v| acc + calculate_priority(v))

}


fn calculate_all() -> u32{

    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    let mut sum = 0;

    while let (Some(line1), Some(line2), Some(line3)) = (lines.next(), lines.next(), lines.next()) {
        sum += calculate_badge(line1.unwrap(), line2.unwrap(), line3.unwrap())
    }
    sum

}

pub fn part_2_calculate() {
    let res = calculate_all();
    println!("{}", res)
}
