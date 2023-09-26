use crate::lexer::{Token, Lexer};
use std::mem;

struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let curr_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Self {
            lexer,
            curr_token,
            peek_token,
        }
    }

    pub fn next_token(&mut self) {
        self.curr_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
    }
}
