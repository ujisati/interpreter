use anyhow::Result;

use crate::ast::{
    DebugString, Expression, ExpressionStmt, Identifier, Integer, Let, Node, Program, Return,
    Statement,
};
use crate::lexer::{Lexer, Token, TokenType};
use log::info;
use std::collections::HashMap;
use std::hash::Hash;
use std::mem;

#[derive(PartialEq, PartialOrd, Copy, Clone, Debug)]
enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

pub struct Parser {
    lexer: Lexer,
    pub curr_token: Token,
    peek_token: Token,
    prefix_parse_fns: HashMap<TokenType, fn(&mut Parser) -> Expression>,
    infix_parse_fns: HashMap<TokenType, fn(&mut Parser, Expression) -> Expression>,
    precedences: HashMap<TokenType, Precedence>,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let curr_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let mut prefix_parse_fns = HashMap::<TokenType, fn(&mut Parser) -> Expression>::new();
        prefix_parse_fns.insert(TokenType::IDENT, parse_fns::parse_identifier);
        prefix_parse_fns.insert(TokenType::INT, parse_fns::parse_integer);
        prefix_parse_fns.insert(TokenType::BANG, parse_fns::parse_prefix_expression);
        prefix_parse_fns.insert(TokenType::MINUS, parse_fns::parse_prefix_expression);
        prefix_parse_fns.insert(TokenType::TRUE, parse_fns::parse_boolean_expression);
        prefix_parse_fns.insert(TokenType::FALSE, parse_fns::parse_boolean_expression);
        prefix_parse_fns.insert(TokenType::LPAREN, parse_fns::parse_grouped_expression);
        let mut infix_parse_fns =
            HashMap::<TokenType, fn(&mut Parser, Expression) -> Expression>::new();
        infix_parse_fns.insert(TokenType::PLUS, parse_fns::parse_infix_expression);
        infix_parse_fns.insert(TokenType::MINUS, parse_fns::parse_infix_expression);
        infix_parse_fns.insert(TokenType::SLASH, parse_fns::parse_infix_expression);
        infix_parse_fns.insert(TokenType::ASTERISK, parse_fns::parse_infix_expression);
        infix_parse_fns.insert(TokenType::EQUAL, parse_fns::parse_infix_expression);
        infix_parse_fns.insert(TokenType::NOTEQUAL, parse_fns::parse_infix_expression);
        infix_parse_fns.insert(TokenType::LT, parse_fns::parse_infix_expression);
        infix_parse_fns.insert(TokenType::GT, parse_fns::parse_infix_expression);
        let precedences = HashMap::from([
            (TokenType::EQUAL, Precedence::Equals),
            (TokenType::NOTEQUAL, Precedence::Equals),
            (TokenType::LT, Precedence::LessGreater),
            (TokenType::GT, Precedence::LessGreater),
            (TokenType::PLUS, Precedence::Sum),
            (TokenType::MINUS, Precedence::Sum),
            (TokenType::SLASH, Precedence::Product),
            (TokenType::ASTERISK, Precedence::Product),
        ]);
        Self {
            lexer,
            curr_token,
            peek_token,
            prefix_parse_fns,
            infix_parse_fns,
            precedences,
            errors: Vec::new(),
        }
    }

    pub fn next_token(&mut self) {
        self.curr_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
        info!("curr token: {}", self.curr_token.literal);
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut program = Program {
            statements: Vec::new(),
        };
        while self.curr_token.token_type != TokenType::EOF {
            info!("parsing {}", self.curr_token.literal);
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
        info!(
            "is peek token expected: {} == {:?}",
            self.peek_token.literal, token_type
        );
        self.peek_token.token_type == token_type
    }

    fn peek_then_next(&mut self, token_type: TokenType) -> bool {
        info!(
            "peek then next: {} == {:?}",
            self.peek_token.literal, token_type
        );
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
        info!("parsing {} {}", token.literal, expression.repr());

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
        info!(
            "parsing expression: {} {}",
            self.curr_token.literal, self.peek_token.literal
        );
        info!("curr precedence: {:?}", precedence);
        let prefix = match self.prefix_parse_fns.get(&self.curr_token.token_type) {
            Some(f) => *f,
            None => {
                // TODO: impl display for token type and add failure to errors
                return Expression::None;
            }
        };
        let mut left_exp = prefix(self);
        info!("prefix left expression: {}", left_exp.repr());

        while !self.is_peek_token_expected(TokenType::SEMICOLON)
            && precedence < self.peek_precedence()
        {
            let infix = match self.infix_parse_fns.get(&self.peek_token.token_type) {
                Some(f) => *f,
                None => {
                    // TODO: impl display for token type and add failure to errors
                    return left_exp;
                }
            };
            self.next_token();
            left_exp = infix(self, left_exp);
            info!("infix left expression: {}", left_exp.repr());
        }
        left_exp
    }

    fn peek_precedence(&self) -> Precedence {
        match self.precedences.get(&self.peek_token.token_type) {
            Some(p) => {
                info!("peek precedence: {:?}", p);
                *p
            }
            None => Precedence::Lowest,
        }
    }

    fn curr_precedence(&self) -> Precedence {
        match self.precedences.get(&self.curr_token.token_type) {
            Some(p) => {
                info!("curr precedence: {:?}", p);
                *p
            }
            None => Precedence::Lowest,
        }
    }
}

mod parse_fns {
    use crate::ast::{Boolean, Infix, Prefix};

    use super::*;

    pub fn parse_grouped_expression(p: &mut Parser) -> Expression {
        p.next_token();
        let expression = p.parse_expression(Precedence::Lowest);
        if !p.is_peek_token_expected(TokenType::RPAREN) {
            return Expression::None;
        }
        return expression;
    }

    pub fn parse_boolean_expression(p: &mut Parser) -> Expression {
        Expression::Boolean(Boolean {
            token: p.curr_token.clone(),
            value: p.is_curr_token_expected(TokenType::TRUE),
        })
    }

    pub fn parse_identifier(p: &mut Parser) -> Expression {
        info!("parsing identifier: {}", p.curr_token.literal);
        return Expression::Identifier(Identifier {
            token: p.curr_token.clone(),
            value: p.curr_token.literal.clone(),
        });
    }

    pub fn parse_integer(p: &mut Parser) -> Expression {
        info!("parsing integer: {}", p.curr_token.literal);
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

    pub fn parse_prefix_expression(p: &mut Parser) -> Expression {
        let token = p.curr_token.clone();
        info!("parsing prefix expression: {}", token.literal);
        p.next_token();
        info!("\tparsing right");
        let right = p.parse_expression(Precedence::Prefix);
        info!("\tright: {}", right.repr());

        Expression::Prefix(Prefix {
            operator: token.literal.clone(),
            token,
            right: Box::new(right),
        })
    }

    pub fn parse_infix_expression(p: &mut Parser, left: Expression) -> Expression {
        let token = p.curr_token.clone();
        info!("parsing infix expression: {}", token.literal);
        let precedence = p.curr_precedence();
        p.next_token();
        info!("\tparsing right");
        let right = p.parse_expression(precedence);
        info!("\tright: {}", right.repr());
        Expression::Infix(Infix {
            left: Box::new(left),
            right: Box::new(right),
            operator: token.literal.clone(),
            token,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    enum Type {
        Integer(i64),
        Identifier(String),
        Bool(bool),
    }

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

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
    fn test_parse_errors() {
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

    #[test]
    fn test_parsing_prefix_expression() {
        let prefix_tests = [
            ("!5;", "!", Type::Integer(5)),
            ("-15;", "-", Type::Integer(15)),
            ("!true;", "!", Type::Bool(true)),
            ("!false;", "!", Type::Bool(false)),
        ];
        for (input, op, val) in prefix_tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();
            check_errors(parser);
            assert!(program.statements.len() == 1);
            let exp_stmt = match &program.statements[0] {
                Statement::ExpressionStmt(s) => s,
                _ => panic!("Expected expression statement"),
            };
            let prefix = match &exp_stmt.expression {
                Expression::Prefix(o) => o,
                _ => panic!("Expected prefix expression"),
            };
            assert!(prefix.operator == op);
            check_literal_expression(prefix.right.as_ref(), val);
        }
    }

    fn check_integer_literal(exp: &Expression, value: i64) -> bool {
        let int = match exp {
            Expression::Integer(i) => i,
            _ => panic!("Expected integer expression"),
        };
        assert!(int.value == value);
        assert!(int.token_literal() == format!("{}", value));
        return true;
    }

    #[test]
    fn test_parse_infix_expression() {
        let infix_tests = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];
        for (input, left, op, right) in infix_tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();
            check_errors(parser);
            assert!(program.statements.len() == 1);
            let exp_stmt = match &program.statements[0] {
                Statement::ExpressionStmt(s) => s,
                _ => panic!("Expected expression statement"),
            };
            let infix = match &exp_stmt.expression {
                Expression::Infix(i) => i,
                _ => panic!("Expected infix expression"),
            };
            assert!(infix.operator == op);
            check_integer_literal(infix.left.as_ref(), left);
            check_integer_literal(infix.right.as_ref(), right);
        }

        let infix_tests_bool = [
            ("true == true;", true, "==", true),
            ("true != false;", true, "!=", false),
            ("false == false;", false, "==", false),
        ];
        for (input, left, op, right) in infix_tests_bool {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();
            check_errors(parser);
            assert!(program.statements.len() == 1);
            let exp_stmt = match &program.statements[0] {
                Statement::ExpressionStmt(s) => s,
                _ => panic!("Expected expression statement"),
            };
            let infix = match &exp_stmt.expression {
                Expression::Infix(i) => i,
                _ => panic!("Expected infix expression"),
            };
            assert!(infix.operator == op);
            assert!(check_literal_expression(
                infix.left.as_ref(),
                Type::Bool(left)
            ));
            assert!(check_literal_expression(
                infix.right.as_ref(),
                Type::Bool(right)
            ));
        }
    }

    #[test]
    fn test_op_parsing() {
        init();
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            // ("(5 + 5) * 2", "((5 + 5) * 2)"),
            // ("2 / (5 + 5)", "(2 / (5 + 5))"),
            // ("-(5 + 5)", "(-(5 + 5))"),
            // ("!(true == true)", "(!(true == true))"),
        ];

        for (input, expected) in tests.iter() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();
            check_errors(parser);

            let actual = program.repr();
            assert_eq!(actual, *expected, "{}, {}", actual, *expected);
        }
    }

    fn check_identifier(expression: &Expression, value: String) -> bool {
        let ident = match expression {
            Expression::Identifier(i) => i,
            _ => panic!("Expected identifier"),
        };
        assert!(ident.value == value);
        assert!(ident.token_literal() == value);
        return true;
    }

    fn check_literal_expression(expression: &Expression, expected_type: Type) -> bool {
        match expected_type {
            Type::Integer(i) => return check_integer_literal(expression, i),
            Type::Identifier(i) => return check_identifier(expression, i),
            Type::Bool(b) => return check_boolean_literal(expression, b),
        }
    }

    fn check_infix_expression(expression: Expression, left: Type, op: String, right: Type) -> bool {
        let exp = match expression {
            Expression::Infix(i) => i,
            _ => panic!("Expected infix"),
        };
        assert!(check_literal_expression(exp.left.as_ref(), left));
        assert!(exp.operator == op);
        assert!(check_literal_expression(exp.right.as_ref(), right));
        return true;
    }

    fn check_boolean_literal(expression: &Expression, value: bool) -> bool {
        let exp = match expression {
            Expression::Boolean(b) => b,
            _ => panic!("Expected infix"),
        };
        assert!(exp.value == value, "expected {} got {}", value, exp.value);
        assert!(exp.token_literal() == value.to_string());
        return true;
    }
}
