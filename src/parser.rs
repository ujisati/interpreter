use anyhow::Result;

use crate::ast::{Expression, Identifier, Let, Node, Statement, Return};
use crate::lexer::{Lexer, Token, TokenType};
use std::mem;

struct Program {
    statements: Vec<Statement>,
}

struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let curr_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Self {
            lexer,
            curr_token,
            peek_token,
            errors: Vec::new(),
        }
    }

    pub fn next_token(&mut self) {
        self.curr_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut program = Program {
            statements: Vec::new(),
        };
        while self.curr_token.token_type != TokenType::EOF {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                program.statements.push(s);
            }
            self.next_token();
        }
        Ok(program)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token.token_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => None
            
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        // let stmt = Statement::Let(Let {
        //     token: self.curr_token,
        //     name
        // });
        let token = self.curr_token.clone();

        if !self.peek_then_next(TokenType::IDENT) {
            return None;
        }

        let identifier = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        if !self.peek_then_next(TokenType::ASSIGN) {
            return None;
        }

        // TODO: don't skip expressions
        while !self.is_curr_token_expected(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::Let(Let {
            token,
            name: identifier,
            value: Expression::None,
        }))
    }

    fn is_curr_token_expected(&self, token_type: TokenType) -> bool {
        self.curr_token.token_type == token_type
    }

    fn is_peek_token_expected(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn peek_then_next(&mut self, token_type: TokenType) -> bool {
        if self.is_peek_token_expected(token_type) {
            self.next_token();
            return true;
        }
        self.errors.push(format!(
            "Expect next token to be {:?}, got {:?} instead",
            token_type, self.peek_token.token_type
        ));
        return false;
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone();
        self.next_token();
        while !self.is_curr_token_expected(TokenType::SEMICOLON) {
            self.next_token(); 
        }
        return Some(Statement::Return(Return {
            token,
            return_value: Expression::None // TODO: make expression
        }))
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
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        check_errors(parser);
        assert!(program.statements.len() == 3);
        let expected_identifier = ["x", "y", "foobar"];
        for (i, e) in expected_identifier.iter().enumerate() {
            let stmt = &program.statements[i];
            check_let_statement(stmt, e)
        }
    }

    #[test]
    #[should_panic]
    fn test_parse_erros() {
        let input = "
            let x  5;
        ";
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        check_errors(parser);
    }

    fn check_let_statement(stmt: &Statement, expected: &str) {
        let stmt = match stmt {
            Statement::Let(stmt) => stmt,
            _ => panic!("This shouldn't happen"),
        };
        assert_eq!(stmt.token_literal(), "let");
        assert_eq!(stmt.name.value, expected);
        assert_eq!(stmt.name.token_literal(), expected);
    }

    fn check_errors(parser: Parser) {
        for error in parser.errors {
            assert!(false, "Parser error: {}", error)
        }
    }

    #[test]
    fn test_return_statement() {
        let input = "
            return 5;
            return 10;
            return 993322;
        ";
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();

        assert!(program.statements.len() == 3);
        for stmt in program.statements {
            let stmt = match stmt {
                Statement::Return(s) => s,
                _ => panic!("This shouldn't happen"),
            };
            assert_eq!(stmt.token_literal(), "return")
        }
    }
}
