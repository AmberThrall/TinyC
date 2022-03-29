extern crate pest;
#[macro_use]
extern crate pest_derive;

mod parser;
mod eval;

use std::env;
use std::fs;
use std::process;
use parser::*;
use eval::*;

fn main() {
    let filename = match env::args().nth(1) {
        Some(x) => x,
        None => {
            println!("Usage: {} <filename>", env::args().nth(0).unwrap());
            process::exit(1);
        }
    };

    let unparsed_string = match fs::read_to_string(filename) {
        Ok(x) => x,
        Err(e) => {
            println!("File error: {}", e);
            process::exit(1);
        }
    };

    let ast = match parse(&unparsed_string) {
        Ok(ast) => ast,
        Err(e) => {
            println!("Parsing error: {}", e);
            process::exit(1);
        }
    };

    let mut evaluator = Evaluator::new();
    match evaluator.eval(&ast) {
        Ok(_) => {},
        Err(e) => println!("Error: {}", e)
    }
}
