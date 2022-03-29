use crate::parser::*;
use pest::iterators::Pair;
use std::collections::HashMap;

pub struct Evaluator {
    stack: Vec<HashMap<String, i64>>
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            stack: Vec::new()
        }
    }

    pub fn clear_stack(&mut self) {
        self.stack.clear();
    }

    pub fn push_scope(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.stack.pop();
    }

    pub fn write_stack(&mut self, ident: String, value: i64) {
        if self.stack.len() == 0 {
            self.push_scope();
        }
        let mut scope = self.stack.last_mut().unwrap();
        scope.insert(ident, value);
    }

    pub fn read_stack(&self, ident: String) -> Option<&i64> {
        // Search the stack backwards.
        for scope in self.stack.iter().rev() {
            if let Some(x) = scope.get(&ident) {
                return Some(x);
            }
        }
        None
    }

    pub fn eval(&mut self, pair: Pair<Rule>) -> Result<(), String> {
        match pair.as_rule() {
            Rule::program => self.eval_program(pair),
            Rule::statement => self.eval_statement(pair),
            _ => Err(format!("Unsupported rule '{:?}'", pair.as_rule()))
        }
    }

    fn eval_program(&mut self, pair: Pair<Rule>) -> Result<(), String> {
        for record in pair.into_inner() {
            match record.as_rule() {
                Rule::statement => self.eval_statement(record)?,
                Rule::EOI => return Ok(()),
                _ => return Err(format!("Invalid record '{:?}' in program", record.as_rule()))
            }
        }
        Err("Never reached end of input in program".to_string())
    }

    fn eval_statement(&mut self, pair: Pair<Rule>) -> Result<(), String> {
        let stmt = pair.into_inner().next().unwrap();
        println!("Entered statement: {:?}", stmt.as_rule());

        match stmt.as_rule() {
            Rule::block_statement => {
                for s in stmt.into_inner() {
                    self.eval_statement(s)?;
                }
                Ok(())
            },
            Rule::expr_statement => self.eval_expr(stmt.into_inner().next().unwrap()),
            _ => return Err(format!("Unsupported statement of type '{:?}'", stmt.as_rule()))
        }
    }

    fn eval_expr(&mut self, pair: Pair<Rule>) -> Result<(), String> {
        println!("Expr: {:?}", pair);
        let mut records = pair.into_inner();
        let lhs = records.next();
        let op = records.next();
        let rhs = records.next();

        println!("{:?} {:?} {:?}", lhs, op, rhs);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stack() {
        let mut e = Evaluator::new();
        e.write_stack("x".to_string(), 14);
        e.write_stack("y".to_string(), 5);
        assert_eq!(e.read_stack("x".to_string()), Some(&14));
        assert_eq!(e.read_stack("y".to_string()), Some(&5));

        e.push_scope();
        e.write_stack("z".to_string(), -2);
        assert_eq!(e.read_stack("x".to_string()), Some(&14));
        assert_eq!(e.read_stack("y".to_string()), Some(&5));
        assert_eq!(e.read_stack("z".to_string()), Some(&-2));

        e.pop_scope();
        assert_eq!(e.read_stack("x".to_string()), Some(&14));
        assert_eq!(e.read_stack("y".to_string()), Some(&5));
        assert_eq!(e.read_stack("z".to_string()), None);
    }
}
