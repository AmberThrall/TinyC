use pest::Parser;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};

#[derive(Parser)]
#[grammar = "tinyc.pest"]
pub struct TinyCParser;

#[derive(Debug, PartialEq)]
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
    Assign,
    And,
    Or
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
            "&&"=> Some(Operator::And),
            "||" => Some(Operator::Or),
            _ => None
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            Operator::Plus => 3,
            Operator::Minus => 3,
            Operator::Times => 4,
            Operator::Divide => 4,
            Operator::LessThen => 2,
            Operator::GreaterThen => 2,
            Operator::LessThenOrEqual => 2,
            Operator::GreaterThenOrEqual => 2,
            Operator::Equals => 2,
            Operator::And => 1,
            Operator::Or => 1,
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
        paren_expr: Box<Node>
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

impl Node {
    pub fn graphviz(&self, graphname: &str) -> String {
        format!("graph {} {{\n  // Nodes:\n{}\n\n  // Edges:\n{}\n}}",
            graphname,
            self.graphviz_build_nodes(vec![0]),
            self.graphviz_build_edges(vec![0])
        )
    }

    fn graphviz_build_nodes(&self, indices: Vec<usize>) -> String {
        let name = format!("node_{}", indices.iter().map(|x| format!("{}", x)).collect::<Vec<_>>().join("_"));

        let new_indices = |i: usize| -> Vec<usize> {
            let mut new_indices = indices.clone();
            new_indices.push(i);
            new_indices
        };

        match self {
            Node::Program { statement } => match statement {
                Some(n) => format!("  {} [label=\"Program\"];\n{}", name, n.graphviz_build_nodes(new_indices(0))),
                None => format!("  {} [label=\"Program\"];", name)
            },
            Node::IfStatement { paren_expr, statement } => format!("  {} [label=\"IfStatement\"];\n{}\n{}", name,
                 paren_expr.graphviz_build_nodes(new_indices(0)), statement.graphviz_build_nodes(new_indices(1))),
            Node::IfElseStatement { paren_expr, true_statement, false_statement } => format!("  {} [label=\"IfElseStatement\"];\n{}\n{}\n{}",
                name,
                paren_expr.graphviz_build_nodes(new_indices(0)),
                true_statement.graphviz_build_nodes(new_indices(1)),
                false_statement.graphviz_build_nodes(new_indices(2))),
            Node::WhileStatement { paren_expr, statement } => format!("  {} [label=\"WhileStatement\"];\n{}\n{}", name,
                paren_expr.graphviz_build_nodes(new_indices(0)), statement.graphviz_build_nodes(new_indices(1))),
            Node::DoStatement { paren_expr, statement } => format!("  {} [label=\"DoStatement\"];\n{}\n{}", name,
                paren_expr.graphviz_build_nodes(new_indices(0)), statement.graphviz_build_nodes(new_indices(1))),
            Node::BlockStatement { statements } => format!("  {} [label=\"BlockStatement\"];\n{}", name,
                    statements.iter().enumerate().map(|(i, s)| s.graphviz_build_nodes(new_indices(i))).collect::<Vec<_>>().join("\n")
                ),
            Node::PrintStatement { paren_expr } => format!("  {} [label=\"PrintStatement\"];\n{}", name,
                paren_expr.graphviz_build_nodes(new_indices(0))),
            Node::EmptyStatement => format!("  {} [label=\"EmptyStatement\"];", name),
            Node::Int(x) => format!("  {} [label=\"Int({})\"];", name, x),
            Node::Id(x) => format!("  {} [label=\"Id({})\"];", name, x),
            Node::BinaryOp { op, lhs, rhs } => format!("  {} [label=\"BinaryOp ({:?})\"];\n{}\n{}",
                name, op,
                lhs.graphviz_build_nodes(new_indices(0)),
                rhs.graphviz_build_nodes(new_indices(1))),
            _ => format!("  {};", name)
        }
    }

    fn graphviz_build_edges(&self, indices: Vec<usize>) -> String {
        let name = format!("node_{}", indices.iter().map(|x| format!("{}", x)).collect::<Vec<_>>().join("_"));

        let new_indices = |i: usize| -> Vec<usize> {
            let mut new_indices = indices.clone();
            new_indices.push(i);
            new_indices
        };

        let edge = |i: usize| -> String {
            format!("{} -- {}_{}", name, name, i)
        };

        match self {
            Node::Program { statement } => match statement {
                Some(n) => format!("  {} [label=\"statement\"];\n{}", edge(0), n.graphviz_build_edges(new_indices(0))),
                None => "".to_string()
            },
            Node::IfStatement { paren_expr, statement } => format!(
                "  {} [label=\"paren_expr\"];\n  {} [label=\"statement\"];\n{}{}",
                edge(0), edge(1),
                paren_expr.graphviz_build_edges(new_indices(0)),
                statement.graphviz_build_edges(new_indices(1))
            ),
            Node::IfElseStatement { paren_expr, true_statement, false_statement } => format!(
                "  {} [label=\"paren_expr\"];\n  {} [label=\"true_statement\"];\n  {} [label=\"false_statement\"];\n{}{}{}",
                edge(0), edge(1), edge(2),
                paren_expr.graphviz_build_edges(new_indices(0)),
                true_statement.graphviz_build_edges(new_indices(1)),
                false_statement.graphviz_build_edges(new_indices(2))
            ),
            Node::WhileStatement { paren_expr, statement } => format!(
                "  {} [label=\"paren_expr\"];\n  {} [label=\"statement\"];\n{}{}",
                edge(0), edge(1),
                paren_expr.graphviz_build_edges(new_indices(0)),
                statement.graphviz_build_edges(new_indices(1))
            ),
            Node::DoStatement { paren_expr, statement } => format!(
                "  {} [label=\"paren_expr\"];\n  {} [label=\"statement\"];\n{}{}",
                edge(0), edge(1),
                paren_expr.graphviz_build_edges(new_indices(0)),
                statement.graphviz_build_edges(new_indices(1))
            ),
            Node::BlockStatement { statements } => format!("{}\n{}",
                statements.iter().enumerate().map(|(i, _)| format!("  {} [label=\"statement\"];\n", edge(i))).collect::<Vec<_>>().join(""),
                statements.iter().enumerate().map(|(i, s)| s.graphviz_build_edges(new_indices(i))).collect::<Vec<_>>().join("")
            ),
            Node::PrintStatement { paren_expr } => format!(
                "  {} [label=\"paren_expr\"];\n{}",
                edge(0),
                paren_expr.graphviz_build_edges(new_indices(0))
            ),
            Node::EmptyStatement => "".to_string(),
            Node::Int(_) => "".to_string(),
            Node::Id(_) => "".to_string(),
            Node::BinaryOp { op, lhs, rhs } => format!(
                "  {} [label=\"lhs\"];\n  {} [label=\"rhs\"];\n{}{}",
                edge(0), edge(1),
                lhs.graphviz_build_edges(new_indices(0)),
                rhs.graphviz_build_edges(new_indices(1))
            ),
            _ => format!("  // Unknown node: {:?}\n", self)
        }
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
                statement: Box::new(build_ast(pairs.next().unwrap())),
                paren_expr: Box::new(build_ast(pairs.next().unwrap()))
            }
        },
        Rule::block_statement => {
            let mut stmts = Vec::new();
            for record in pair.into_inner() {
                stmts.push(Box::new(build_ast(record)));
            }
            Node::BlockStatement { statements: stmts }
        },
        Rule::print_statement => Node::PrintStatement { paren_expr: Box::new(build_ast(pair.into_inner().next().unwrap())) },
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
