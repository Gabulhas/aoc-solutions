use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{prelude::*, BufReader, Error};

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct Point {
    x: usize,
    y: usize,
}

#[derive(PartialEq)]
enum Version {
    One,
    Two,
}

type Map = Vec<Vec<u32>>;

pub fn run(part: String) {
    let part_e = if part == "1" {
        Version::One
    } else {
        Version::Two
    };

    let mut args = env::args();

    let file_name = match args.nth(2) {
        Some(a) => a,
        None => String::from("/dev/stdin"),
    };

    let (map, start, end) = read_from_file(file_name, part_e).unwrap();
    let steps = walk_it_through(map, start, end);
    println!("{}", steps.unwrap())
}

fn read_from_file(path: String, part: Version) -> Result<(Map, Vec<Point>, Point), Error> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut starting_points = Vec::new();
    let mut end_point = Point { x: 0, y: 0 };
    let mut result: Vec<Vec<u32>> = Vec::new();

    if part == Version::One {
        starting_points.push(Point { x: 0, y: 0 });
    }

    for (y, line) in reader.lines().enumerate() {
        let mut current_line: Vec<u32> = Vec::new();
        for (x, char) in line?.chars().enumerate() {
            let this_point = Point { x, y };
            let val = match char {
                'S' => {
                    if part == Version::One {
                        starting_points[0] = this_point
                    } else {
                        starting_points.push(this_point)
                    }
                    u32::from('a')
                }
                'a' => {
                    if part == Version::Two {
                        starting_points.push(this_point);
                    }
                    u32::from('a')
                }
                'E' => {
                    end_point = this_point;
                    u32::from('z')
                }
                _ => u32::from(char),
            };
            current_line.push(val)
        }
        result.push(current_line)
    }

    Ok((result, starting_points, end_point))
}

fn walk_it_through(map: Map, starting_points: Vec<Point>, end_point: Point) -> Option<u32> {
    let mut visited = HashMap::new();

    starting_points
        .into_iter()
        .filter_map(|start_point| recurse_walk(&map, &mut visited, &start_point, &end_point, 0))
        .min()
}
fn point_in_map(map: &Map, point: &Point) -> bool {
    point.y < map.len() && point.x < map.first().unwrap().len()
}

fn value_of_point(map: &Map, point: &Point) -> Option<u32> {
    let y: usize = point.y;
    let x: usize = point.x;
    let row: Option<&Vec<u32>> = map.get(y);
    row?.get(x).copied()
}

fn possible_next_points(map: &Map, point: Point) -> Vec<Point> {
    // Define relative movements: right, up, left, down
    let movements = [(1, 0), (0, 1), (-1, 0), (0, -1)];

    let mut next_points = Vec::new();
    let point_value = value_of_point(map, &point).unwrap_or_default();

    for (dx, dy) in movements {
        let new_x = point.x.wrapping_add(dx as usize);
        let new_y = point.y.wrapping_add(dy as usize);
        let next_point = Point { x: new_x, y: new_y };
        if point_in_map(map, &next_point) && (new_x != point.x || new_y != point.y) {
            if let Some(next_point_value) = value_of_point(map, &next_point) {
                if next_point_value <= point_value + 1 {
                    next_points.push(next_point);
                }
            }
        }
    }

    next_points
}

fn recurse_walk(
    map: &Map,
    visited: &mut HashMap<Point, u32>,
    this_point: &Point,
    end_point: &Point,
    steps: u32,
) -> Option<u32> {
    // If we've reached the end, return the steps taken.
    if this_point == end_point {
        return Some(steps);
    }

    // Check if we've visited this point with fewer steps before.
    if let Some(&previous_steps) = visited.get(this_point) {
        if previous_steps <= steps {
            return None; // We've been here with fewer or equal steps, no need to continue.
        }
    }

    // Insert the current point and steps into the visited HashMap.
    visited.insert(*this_point, steps);

    // Try to walk to each possible next point and choose the one with the minimum steps.
    possible_next_points(map, *this_point)
        .into_iter()
        .filter_map(|next_point| recurse_walk(map, visited, &next_point, end_point, steps + 1))
        .min()
}
