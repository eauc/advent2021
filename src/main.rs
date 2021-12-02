use std::{
    env,
    fs::File,
    io::{prelude::*, BufReader},
    path::Path,
};

fn data_from_file(filename: impl AsRef<Path>) -> Vec<i32> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .map(|s| s.parse::<i32>().expect("Could not parse int"))
        .collect()
}

fn count_increments(data: &Vec<i32>) -> usize {
    data.windows(2)
        .filter(|p| p[0] < p[1])
        .count()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let day01 = data_from_file(&args[1]);

    let increments2: usize = count_increments(&day01);
    println!("with window=2 increments={:?}", increments2);

    let increments3 = count_increments(
        &day01.windows(3)
            .map(|p| p[0] + p[1] + p[2])
            .collect()
    );
    println!("with window=3 increments={:?}", increments3);
}
