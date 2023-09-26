use anyhow::Result;

use crate::ast::{Node, Statement};
use crate::lexer::{Lexer, Token};
use std::mem;

struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
}

struct Program {
    statements: Vec<Statement>,
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

    pub fn parse(self) -> Result<Program> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";
        let lexer = Lexer::new(input.into());
        let parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        assert!(program.statements.len() != 3);
        let expected_identifier = ["x", "y", "foobar"];
        for (i, e) in expected_identifier.iter().enumerate() {
            let stmt = &program.statements[i];
            check_let_statement(stmt, e)
        }
    }

    fn check_let_statement(stmt: &Statement, expected: &str) {
    }
}
