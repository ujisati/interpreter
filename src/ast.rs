use crate::lexer::Token;

pub struct Statement {}

impl Node for Let {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
struct Expression {}

pub struct Let {
    pub token: Token,
    name: Identifier,
    value: Expression,
}

pub struct Identifier {
    token: Token,
    value: String,
}

pub trait Node {
    fn token_literal(&self) -> String;
}
