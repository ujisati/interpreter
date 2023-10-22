use macros::Node;

use crate::lexer::{Token, TokenType};

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait DebugString {
    fn repr(&self) -> String;
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Expression {
    None,
    Identifier(Identifier),
    Integer(Integer),
    Prefix(Prefix),
    Infix(Infix),
    Boolean(Boolean),
    If(If)
}

#[derive(Node, Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[derive(Node, Debug)]
pub struct Integer {
    pub token: Token,
    pub value: i64,
}

#[derive(Node, Debug)]
pub struct Prefix {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Node, Debug)]
pub struct Infix {
    pub token: Token,
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Node, Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

#[derive(Node, Debug)]
pub struct If {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: Box<Statement>,
    pub alternative: Box<Statement>
}

#[derive(Debug)]
pub enum Statement {
    Let(Let),
    Return(Return),
    ExpressionStmt(ExpressionStmt),
    Block(Block)
}

#[derive(Node, Debug)]
pub struct Let {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Node, Debug)]
pub struct Return {
    pub token: Token,
    pub return_value: Expression,
}

#[derive(Node, Debug)]
pub struct ExpressionStmt {
    pub token: Token,
    pub expression: Expression,
}

#[derive(Node, Debug)]
pub struct Block {
    token: Token,
    statements: Vec<Statement>
}

impl DebugString for Program {
    fn repr(&self) -> String {
        let mut output = String::from("");
        for s in &self.statements {
            output.push_str(&s.repr())
        }
        output
    }
}

impl DebugString for Statement {
    fn repr(&self) -> String {
        match self {
            Self::Let(s) => s.repr(),
            Self::Return(s) => s.repr(),
            Self::ExpressionStmt(s) => s.repr(),
            Self::Block(s) => s.repr()
        }
    }
}

impl DebugString for Infix {
    fn repr(&self) -> String {
        let mut output = String::new();
        output.push('(');
        output.push_str(&self.left.repr());
        output.push(' ');
        output.push_str(&self.operator);
        output.push(' ');
        output.push_str(&self.right.repr());
        output.push(')');
        output
    }
}

impl DebugString for Prefix {
    fn repr(&self) -> String {
        let mut output = String::new();
        output.push('(');
        output.push_str(&self.operator);
        output.push_str(&self.right.repr());
        output.push(')');
        output
    }
}

impl DebugString for Expression {
    fn repr(&self) -> String {
        match self {
            Self::Identifier(s) => s.repr(),
            Self::Prefix(p) => p.repr(),
            Self::Infix(i) => i.repr(),
            Self::Integer(i) => i.repr(),
            Self::Boolean(b) => b.repr(),
            _ => panic!("Expression not found: {:?}", self),
        }
    }
}

impl DebugString for ExpressionStmt {
    fn repr(&self) -> String {
        self.expression.repr()
    }
}

impl DebugString for Let {
    fn repr(&self) -> String {
        self.token_literal() + " " + &self.name.repr() + " = " + &self.value.repr() + ";"
    }
}

impl DebugString for Return {
    fn repr(&self) -> String {
        self.token_literal() + &self.return_value.repr()
    }
}

impl DebugString for Integer {
    fn repr(&self) -> String {
        self.value.clone().to_string()
    }
}

impl DebugString for Identifier {
    fn repr(&self) -> String {
        self.value.clone()
    }
}

impl DebugString for Boolean {
    fn repr(&self) -> String {
        self.value.to_string()
    }
}

impl DebugString for Block {
    fn repr(&self) -> String {
        let mut output = String::new(); 
        for s in &self.statements {
            output.push_str(s.repr().as_str());
        }
        output
    }
}

impl DebugString for If {
    fn repr(&self) -> String {
        let mut output = String::new();
        output.push_str("if");
        output.push_str(&self.condition.repr());
        output.push(' ');
        output.push_str(&self.consequence.repr());
        match self.alternative.as_ref() {
            Statement::Block(b) => {
                if b.statements.len() > 0 {
                    output.push_str("else");
                    output.push_str(&self.alternative.repr());
                }
            }
            _ => panic!("Expected block")
        }
        output
    }
}


#[cfg(test)]
mod tests {
    use crate::lexer::TokenType;

    use super::*;

    #[test]
    fn test_debug_string() {
        let program = Program {
            statements: vec![Statement::Let(Let {
                token: Token::new(TokenType::LET, "let"),
                name: Identifier {
                    token: Token::new(TokenType::IDENT, "my_var"),
                    value: String::from("my_var"),
                },
                value: Expression::Identifier(Identifier {
                    token: Token::new(TokenType::IDENT, "another_var"),
                    value: String::from("another_var"),
                }),
            })],
        };
        println!("{}", &program.repr());
        assert!(&program.repr() == "let my_var = another_var;");
    }
}
