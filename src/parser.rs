use pest::Parser;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};

#[derive(Parser)]
#[grammar = "tinyc.pest"]
pub struct TinyCParser;

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    Divide,
    LessThen,
    GreaterThen,
    LessThenOrEqual,
    GreaterThenOrEqual,
    Equals,
    Assign
}

impl Operator {
    pub fn from(s: &str) -> Option<Operator> {
        match s {
            "+" => Some(Operator::Plus),
            "-" => Some(Operator::Minus),
            "*" => Some(Operator::Times),
            "/" => Some(Operator::Divide),
            "<" => Some(Operator::LessThen),
            ">" => Some(Operator::GreaterThen),
            "<=" => Some(Operator::LessThenOrEqual),
            ">=" => Some(Operator::GreaterThenOrEqual),
            "==" => Some(Operator::Equals),
            "=" => Some(Operator::Assign),
            _ => None
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            Operator::Plus => 1,
            Operator::Minus => 1,
            Operator::Times => 2,
            Operator::Divide => 2,
            Operator::LessThen => 3,
            Operator::GreaterThen => 3,
            Operator::LessThenOrEqual => 3,
            Operator::GreaterThenOrEqual => 3,
            Operator::Equals => 3,
            Operator::Assign => 0,
        }
    }
}

#[derive(Debug)]
pub enum Node {
    Program {
        statement: Option<Box<Node>>
    },
    IfStatement {
        paren_expr: Box<Node>,
        statement: Box<Node>
    },
    IfElseStatement {
        paren_expr: Box<Node>,
        true_statement: Box<Node>,
        false_statement: Box<Node>
    },
    WhileStatement {
        paren_expr: Box<Node>,
        statement: Box<Node>
    },
    DoStatement {
        paren_expr: Box<Node>,
        statement: Box<Node>
    },
    BlockStatement {
        statements: Vec<Box<Node>>
    },
    PrintStatement {
        expr: Box<Node>
    },
    EmptyStatement,
    Int(i64),
    Id(String),
    BinaryOp {
        op: Operator,
        lhs: Box<Node>,
        rhs: Box<Node>
    }
}

pub fn parse(s: &str) -> Result<Node, Error<Rule>> {
    let res = TinyCParser::parse(Rule::program, s)?.next().unwrap();
    Ok(build_ast(res))
}

fn build_ast(pair: Pair<Rule>) -> Node {
    match pair.as_rule() {
        Rule::program => {
            for record in pair.into_inner() {
                match record.as_rule() {
                    Rule::statement => return Node::Program { statement: Some(Box::new(build_ast(record))) },
                    _ => ()
                }
            }
            Node::Program { statement: None }
        },
        Rule::statement => {
            let child = pair.into_inner().next().unwrap();
            build_ast(child)
        },
        Rule::if_statement => {
            let mut pairs = pair.into_inner();
            Node::IfStatement {
                paren_expr: Box::new(build_ast(pairs.next().unwrap())),
                statement: Box::new(build_ast(pairs.next().unwrap()))
            }
        },
        Rule::if_else_statement => {
            let mut pairs = pair.into_inner();
            Node::IfElseStatement {
                paren_expr: Box::new(build_ast(pairs.next().unwrap())),
                true_statement: Box::new(build_ast(pairs.next().unwrap())),
                false_statement: Box::new(build_ast(pairs.next().unwrap()))
            }
        },
        Rule::while_statement => {
            let mut pairs = pair.into_inner();
            Node::WhileStatement {
                paren_expr: Box::new(build_ast(pairs.next().unwrap())),
                statement: Box::new(build_ast(pairs.next().unwrap()))
            }
        },
        Rule::do_statement => {
            let mut pairs = pair.into_inner();
            Node::DoStatement {
                paren_expr: Box::new(build_ast(pairs.next().unwrap())),
                statement: Box::new(build_ast(pairs.next().unwrap()))
            }
        },
        Rule::block_statement => {
            let mut stmts = Vec::new();
            for record in pair.into_inner() {
                stmts.push(Box::new(build_ast(record)));
            }
            Node::BlockStatement { statements: stmts }
        },
        Rule::print_statement => Node::PrintStatement { expr: Box::new(build_ast(pair.into_inner().next().unwrap())) },
        Rule::paren_expr => build_ast(pair.into_inner().next().unwrap()),
        Rule::expr_statement => build_ast(pair.into_inner().next().unwrap()),
        Rule::semicolon => Node::EmptyStatement,
        Rule::term => build_ast(pair.into_inner().next().unwrap()),
        Rule::expr => build_expr_ast(pair.into_inner()),
        Rule::term => build_ast(pair.into_inner().next().unwrap()),
        Rule::int => Node::Int(pair.as_str().parse().unwrap()),
        Rule::id => Node::Id(String::from(pair.as_str())),
        _ => panic!("Unknown rule: {:?}", pair.as_rule())
    }
}

fn build_expr_ast(mut pairs: Pairs<Rule>) -> Node {
    let lhs = build_ast(pairs.next().unwrap());
    build_expr_ast_climber(&mut pairs, lhs, 0)
}

// lookahead := peek next token
//     while lookahead is a binary operator whose precedence is >= min_precedence
//         op := lookahead
//         advance to next token
//         rhs := parse_primary ()
//         lookahead := peek next token
//         while lookahead is a binary operator whose precedence is greater
//                  than op's, or a right-associative operator
//                  whose precedence is equal to op's
//             rhs := parse_expression_1 (rhs, precedence of op + (1 if lookahead precedence is greater, else 0))
//             lookahead := peek next token
//         lhs := the result of applying op with operands lhs and rhs
//     return lhs

fn build_expr_ast_climber(pairs: &mut Pairs<Rule>, mut lhs: Node, min_precedence: u8) -> Node {
    let mut peekable = pairs.clone().peekable();
    let mut peek = peekable.peek();
    while peek != None {
        let operator = Operator::from(peek.unwrap().as_str()).unwrap();
        if operator.precedence() < min_precedence {
            break;
        }

        pairs.next();
        let lookahead = pairs.next();
        let mut rhs = build_ast(lookahead.clone().unwrap());

        peekable = pairs.clone().peekable();
        peek = peekable.peek();
        while peek != None {
            let next_operator = Operator::from(peek.unwrap().as_str()).unwrap();
            if next_operator.precedence() <= operator.precedence() {
                break;
            }

            rhs = build_expr_ast_climber(pairs, rhs, operator.precedence() + 1);
            peekable = pairs.clone().peekable();
            peek = peekable.peek();
        }

        lhs = Node::BinaryOp {
            op: operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs)
        };
    }
    lhs
}
