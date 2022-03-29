use crate::parser::*;
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

        // Search the stack backwards.
        for mut scope in self.stack.iter_mut().rev() {
            if let Some(x) = scope.get(&ident) {
                scope.insert(ident, value);
                return;
            }
        }

        // Not already in the stack, add to top scope.
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

    pub fn eval(&mut self, node: &Node) -> Result<Option<i64>, String> {
        match node {
            Node::Program { statement } => match statement {
                Some(s) => self.eval(&s),
                None => Ok(None)
            },
            Node::IfStatement { paren_expr, statement } => {
                let condition = self.eval(&paren_expr)?.unwrap();
                if condition > 0 {
                    self.eval(&statement)
                } else {
                    Ok(None)
                }
            },
            Node::IfElseStatement { paren_expr, true_statement, false_statement } => {
                let condition = self.eval(&paren_expr)?.unwrap();
                if condition > 0 {
                    self.eval(&true_statement)
                } else {
                    self.eval(&false_statement)
                }
            },
            Node::WhileStatement { paren_expr, statement } => {
                loop {
                    let condition = self.eval(&paren_expr)?.unwrap();
                    if condition > 0 {
                        self.eval(&statement)?;
                    } else {
                        break;
                    }
                }
                Ok(None)
            },
            Node::DoStatement { paren_expr, statement } => {
                loop {
                    self.eval(&statement)?;

                    let condition = self.eval(&paren_expr)?.unwrap();
                    if condition > 0 {
                    } else {
                        break;
                    }
                }
                Ok(None)
            },
            Node::BlockStatement { statements } => {
                for s in statements {
                    self.eval(&s)?;
                }
                Ok(None)
            },
            Node::PrintStatement { paren_expr } => {
                println!("{}", self.eval(&paren_expr)?.unwrap());
                Ok(None)
            },
            Node::EmptyStatement => Ok(None),
            Node::Int(x) => Ok(Some(x.clone())),
            Node::Id(x) => match self.read_stack(x.clone()) {
                Some(v) => Ok(Some(v.clone())),
                None => Err(format!("No variable named '{}' found in stack", x))
            },
            Node::BinaryOp { op, lhs, rhs } => {
                let r = self.eval(&rhs)?.unwrap();

                match op {
                    Operator::Assign => {
                        match &**lhs {
                            Node::Id(id) => {
                                self.write_stack(id.clone(), r);
                                Ok(Some(r))
                            },
                            _ => Err("Expected identifier to the left of '='".to_string())
                        }
                    },
                    Operator::Plus => Ok(Some(self.eval(&lhs)?.unwrap() + r)),
                    Operator::Minus => Ok(Some(self.eval(&lhs)?.unwrap() - r)),
                    Operator::Times => Ok(Some(self.eval(&lhs)?.unwrap() * r)),
                    Operator::Divide => Ok(Some(self.eval(&lhs)?.unwrap() / r)),
                    Operator::GreaterThen => Ok(Some(if self.eval(&lhs)?.unwrap() > r { 1 } else { 0 })),
                    Operator::LessThen => Ok(Some(if self.eval(&lhs)?.unwrap() < r { 1 } else { 0 })),
                    Operator::GreaterThenOrEqual => Ok(Some(if self.eval(&lhs)?.unwrap() >= r { 1 } else { 0 })),
                    Operator::LessThenOrEqual => Ok(Some(if self.eval(&lhs)?.unwrap() <= r { 1 } else { 0 })),
                    Operator::Equals => Ok(Some(if self.eval(&lhs)?.unwrap() == r { 1 } else { 0 })),
                    _ => Err(format!("Unknown operator {:?}", op))
                }
            },
            _ => Err(format!("Unsupported node '{:?}'", node))
        }
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

        e.write_stack("x".to_string(), 3);
        assert_eq!(e.read_stack("x".to_string()), Some(&3));

        e.pop_scope();
        assert_eq!(e.read_stack("x".to_string()), Some(&3));
        assert_eq!(e.read_stack("y".to_string()), Some(&5));
        assert_eq!(e.read_stack("z".to_string()), None);
    }
}
