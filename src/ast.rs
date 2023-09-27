use crate::lexer::Token;

pub enum Statement {
    Let(Let),
}

pub struct Let {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl Node for Let {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

pub struct Expression {}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

pub trait Node {
    fn token_literal(&self) -> String;
}
