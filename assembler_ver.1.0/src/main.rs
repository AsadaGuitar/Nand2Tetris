use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader, Write};



fn main() -> Result<(), Box<dyn std::error::Error>> {

    let args: Vec<String> = env::args().collect();
    let args_head: Option<&str> = args.get(1).map(|x| x.as_str());

    if let Some(filepath) = args_head {
        if validate_filepath(filepath) {

            let output_filepath = "./output.hack";

            // file lines
            let lines_as_str: Result<Vec<String>, std::io::Error> = BufReader::new(File::open(filepath)?)
                    .lines().into_iter().collect();

            let command_types =
                lines_as_str.unwrap().iter()
                    .map(|line| if is_a_command(line) {
                        "A\n"
                    } else {
                        if is_label(line) {
                            "L\n"
                        } else {
                            if is_jump(line) {
                                "J\n"
                            } else {
                                "C\n"
                            }
                        }
                    }).collect::<Vec<&str>>();

            // write output data
            write_to_file_path(output_filepath, command_types)?

        } else {
            println!("Invalid extension.")
        }
    } else {
        println!("Invalid program arguments.")
    }
    Ok(())
}

fn is_a_command(line: &String) -> bool {
    let head = line.chars().next().unwrap();
    head == '@' || head == 'A'
}

fn is_label(line: &String) -> bool {
    let head = line.chars().next().unwrap();
    head == '('
}

fn is_jump(line: &String) -> bool {
    line.contains(";")
}

fn validate_filepath(filepath: &str) -> bool {
    match filepath.split(".").last() {
        Some(x) if (x == "asm") => true,
        _ => false
    }
}

fn write_to_file_path(filepath: &str, data_list: Vec<&str>) -> Result<(), std::io::Error> {
    let mut file = File::create(filepath)?;

    for data in data_list {
        file.write(data.as_bytes())?;
    }
    file.flush()
}

