use macros::Node;

use crate::lexer::Token;

pub enum Statement {
    Let(Let),
    Return(Return),
}

pub enum Expression {
    None
}

#[derive(Node)]
pub struct Let {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Node)]
pub struct Return {
    pub token: Token,
    pub return_value: Expression
}


#[derive(Node)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}


pub trait Node {
    fn token_literal(&self) -> String;
}
