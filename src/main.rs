extern crate pest;
#[macro_use]
extern crate pest_derive;

mod parser;
mod eval;

use std::fs;
use parser::*;
use eval::*;

fn main() {
    let unparsed_string = fs::read_to_string("fib.tinyc").expect("cannot read file");
    match parse(&unparsed_string) {
        Err(e) => println!("Parsing error: {}", e),
        Ok(ast) => {
            //println!("AST: {:#?}", ast);
            //println!("{}", ast.graphviz("ast"));
            let mut evaluator = Evaluator::new();
            match evaluator.eval(&ast) {
                Err(e) => println!("Error occured: {}", e),
                Ok(_) => {}
            }
        }
    }
}
