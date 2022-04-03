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
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(name = "tinyc")]
#[clap(author = "Amber Thrall <amber@thrall.me>")]
#[clap(version = "1.0")]
#[clap(about = "Tiny-C interpreter", long_about = None)]
struct Cli {
    /// File to run
    filename: String,

    /// Print out graphviz code for the ast
    #[clap(short, long)]
    graphviz: bool,
}

fn main() {
    let cli = Cli::parse();

    let unparsed_string = match fs::read_to_string(cli.filename) {
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

    if cli.graphviz {
        println!("{}\n", ast.graphviz("ast"));
    }

    let mut evaluator = Evaluator::new();
    match evaluator.eval(&ast) {
        Ok(_) => {},
        Err(e) => println!("Error: {}", e)
    }
}
