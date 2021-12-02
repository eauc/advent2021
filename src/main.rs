use std::{
    env,
    fs::File,
    io::{prelude::*, BufReader},
    path::Path,
};

fn day01_from_file(filename: impl AsRef<Path>) -> Vec<i32> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .map(|s| s.parse::<i32>().expect("Could not parse int"))
        .collect()
}

fn day02_from_file(filename: impl AsRef<Path>) -> Vec<(String, i32)> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .map(|l| l.split_whitespace().map(|l| l.to_string()).collect())
        .map(|p: Vec<String>| (p[0].to_string(), p[1].parse::<i32>().expect("Could not parse int")))
        .collect()
}

fn count_increments(data: &Vec<i32>) -> usize {
    data.windows(2)
        .filter(|p| p[0] < p[1])
        .count()
}

struct SubmarineState {
    x: i32,
    depth: i32,
    aim: i32,
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // DAY 01
    let day01 = day01_from_file(&args[1]);

    let increments2: usize = count_increments(&day01);
    println!("Day01 with window=2 increments={:?}", increments2);

    let increments3 = count_increments(
        &day01.windows(3)
            .map(|p| p[0] + p[1] + p[2])
            .collect()
    );
    println!("Day01 with window=3 increments={:?}", increments3);

    // DAY 02
    let day02 = day02_from_file(&args[2]);
    let x = day02.iter().filter(|p| p.0 == "forward").fold(0i32, |sum, val| sum + val.1);
    let down = day02.iter().filter(|p| p.0 == "down").fold(0i32, |sum, val| sum + val.1);
    let up = day02.iter().filter(|p| p.0 == "up").fold(0i32, |sum, val| sum + val.1);
    let depth = down - up;
    println!("Day02 x={:?} depth={:?} res={:?}", x, depth, x * depth);
    let final_state = day02.iter().fold(SubmarineState { x: 0, depth: 0, aim: 0 }, |state, input| match input.0.as_str() {
        "forward" => SubmarineState { x: state.x + input.1, depth: state.depth + state.aim * input.1, aim: state.aim },
        "down" => SubmarineState { x: state.x, depth: state.depth, aim: state.aim + input.1 },
        "up" => SubmarineState { x: state.x, depth: state.depth, aim: state.aim - input.1 },
        _ => state,
    });
    println!("Day02 x={:?} depth={:?} res={:?}", final_state.x, final_state.depth, final_state.x * final_state.depth);
}
