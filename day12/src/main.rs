use std::env;
mod part_one;

fn main() {
    let mut args = env::args();

    let selection = match args.nth(1) {
        Some(a) => a,
        None => "1".to_string(),
    };
    part_one::run(selection)
}
