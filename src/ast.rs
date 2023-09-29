use macros::Node;

use crate::lexer::Token;

pub struct Program {
    pub statements: Vec<Statement>,
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

pub enum Statement {
    Let(Let),
    Return(Return),
    ExpressionStmt(ExpressionStmt),
}

impl DebugString for Statement {
    fn repr(&self) -> String {
        match self {
            Self::Let(s) => s.repr(),
            Self::Return(s) => s.repr(),
            Self::ExpressionStmt(s) => s.repr(),
        }
    }
}

pub enum Expression {
    None, //FIXME: remove this
    Identifier(Identifier),
}

impl DebugString for Expression {
    fn repr(&self) -> String {
        match self {
            Self::Identifier(s) => s.repr(),
            _ => panic!("fix this"),
        }
    }
}

#[derive(Node)]
pub struct ExpressionStmt {
    pub token: Token,
    pub expression: Expression,
}

impl DebugString for ExpressionStmt {
    fn repr(&self) -> String {
        self.expression.repr()
    }
}

#[derive(Node)]
pub struct Let {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl DebugString for Let {
    fn repr(&self) -> String {
        self.token_literal() + " " + &self.name.repr() + " = " + &self.value.repr() + ";"
    }
}

#[derive(Node)]
pub struct Return {
    pub token: Token,
    pub return_value: Expression,
}

impl DebugString for Return {
    fn repr(&self) -> String {
        self.token_literal() + &self.return_value.repr()
    }
}

#[derive(Node)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl DebugString for Identifier {
    fn repr(&self) -> String {
        self.value.clone()
    }
}

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait DebugString {
    fn repr(&self) -> String;
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
