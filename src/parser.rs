use anyhow::Result;

use crate::ast::{
    Expression, ExpressionStmt, Identifier, Integer, Let, Node, Program, Return, Statement,
};
use crate::lexer::{Lexer, Token, TokenType};
use std::collections::HashMap;
use std::hash::Hash;
use std::mem;

enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser {
    lexer: Lexer,
    pub curr_token: Token,
    peek_token: Token,
    prefix_parse_fns: HashMap<TokenType, fn(&mut Parser) -> Expression>,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let curr_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let mut prefix_parse_fns = HashMap::<TokenType, fn(&mut Parser) -> Expression>::new();
        prefix_parse_fns.insert(TokenType::IDENT, parse_fns::parse_identifier);
        prefix_parse_fns.insert(TokenType::INT, parse_fns::parse_integer);
        Self {
            lexer,
            curr_token,
            peek_token,
            prefix_parse_fns,
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
            _ => self.parse_expression_statement(),
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
            return_value: Expression::None, // TODO: make expression
        }));
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.curr_token.clone();
        let expression = self.parse_expression(Precedence::Lowest);

        // WARNING: this is the root of all evil
        if self.is_peek_token_expected(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::ExpressionStmt(ExpressionStmt {
            token,
            expression,
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Expression {
        let prefix = match self.prefix_parse_fns.get(&self.curr_token.token_type) {
            Some(f) => f,
            None => return Expression::None,
        };
        prefix(self)
    }
}

mod parse_fns {
    use super::*;

    pub fn parse_identifier(p: &mut Parser) -> Expression {
        return Expression::Identifier(Identifier {
            token: p.curr_token.clone(),
            value: p.curr_token.literal.clone(),
        });
    }

    pub fn parse_integer(p: &mut Parser) -> Expression {
        let value = match p.curr_token.literal.parse::<i64>() {
            Ok(v) => v,
            Err(v) => {
                p.errors.push(format!(
                    "Could not parse {} as integer",
                    p.curr_token.literal
                ));
                return Expression::None;
            }
        };

        Expression::Integer(Integer {
            token: p.curr_token.clone(),
            value,
        })
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

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        check_errors(parser);
        assert!(program.statements.len() == 1);
        let exp_stmt = match &program.statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression statement"),
        };
        let ident = match &exp_stmt.expression {
            Expression::Identifier(i) => i,
            _ => panic!("Expected identifier expression"),
        };

        assert!(ident.value == "foobar");
        assert!(ident.token_literal() == "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        check_errors(parser);
        assert!(program.statements.len() == 1);
        let exp_stmt = match &program.statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression statement"),
        };
        let ident = match &exp_stmt.expression {
            Expression::Integer(i) => i,
            _ => panic!("Expected integer expression"),
        };

        assert!(ident.value == 5);
        assert!(ident.token_literal() == "5");
    }
}
