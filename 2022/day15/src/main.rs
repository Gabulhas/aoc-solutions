use std::io::{self, BufRead};

fn main() {}

fn read_sensors() {
    let stdin = io::stdin();
    let reader = stdin.lock();

    for line in reader.lines() {
        if let Ok(line) = line {
            // Process each line here
            println!("Read line: {}", line);
        } else {
            // Handle error if reading line fails
            eprintln!("Error reading line");
        }
    }
}
