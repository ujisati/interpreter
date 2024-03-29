use anyhow::Result;

use crate::ast::{
    Block, DebugString, Expression, ExpressionStmt, Identifier, Index, Integer, Let, Node, Program,
    Return, Statement,
};
use crate::lexer::{Lexer, Token, TokenType};
use log::info;
use std::collections::HashMap;
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
    Index = 8,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    pub curr_token: Token,
    peek_token: Token,
    prefix_parse_fns: HashMap<TokenType, fn(&mut Parser) -> Expression>,
    infix_parse_fns: HashMap<TokenType, fn(&mut Parser, Expression) -> Expression>,
    precedences: HashMap<TokenType, Precedence>,
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser {
        let curr_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let prefix_parse_fns = HashMap::from([
            (
                TokenType::IDENT,
                parse_fns::parse_identifier as fn(&mut Parser) -> Expression,
            ),
            (TokenType::INT, parse_fns::parse_integer),
            (TokenType::BANG, parse_fns::parse_prefix_expression),
            (TokenType::MINUS, parse_fns::parse_prefix_expression),
            (TokenType::TRUE, parse_fns::parse_boolean_expression),
            (TokenType::FALSE, parse_fns::parse_boolean_expression),
            (TokenType::LPAREN, parse_fns::parse_grouped_expression),
            (TokenType::IF, parse_fns::parse_if_expression),
            (TokenType::FUNCTION, parse_fns::parse_function_literal),
            (TokenType::STRING, parse_fns::parse_string_literal),
            (TokenType::LBRACKET, parse_fns::parse_array_literal),
        ]);
        let infix_parse_fns = HashMap::from([
            (
                TokenType::PLUS,
                parse_fns::parse_infix_expression as fn(&mut Parser, Expression) -> Expression,
            ),
            (TokenType::MINUS, parse_fns::parse_infix_expression),
            (TokenType::SLASH, parse_fns::parse_infix_expression),
            (TokenType::ASTERISK, parse_fns::parse_infix_expression),
            (TokenType::EQUAL, parse_fns::parse_infix_expression),
            (TokenType::NOTEQUAL, parse_fns::parse_infix_expression),
            (TokenType::LT, parse_fns::parse_infix_expression),
            (TokenType::GT, parse_fns::parse_infix_expression),
            (TokenType::LPAREN, parse_fns::parse_call_expression),
            (TokenType::LBRACKET, parse_fns::parse_index_expression),
        ]);

        let precedences = HashMap::from([
            (TokenType::EQUAL, Precedence::Equals),
            (TokenType::NOTEQUAL, Precedence::Equals),
            (TokenType::LT, Precedence::LessGreater),
            (TokenType::GT, Precedence::LessGreater),
            (TokenType::PLUS, Precedence::Sum),
            (TokenType::MINUS, Precedence::Sum),
            (TokenType::SLASH, Precedence::Product),
            (TokenType::ASTERISK, Precedence::Product),
            (TokenType::LPAREN, Precedence::Call),
            (TokenType::LBRACKET, Precedence::Index),
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
            literal: "".into(),
            token: Token {
                token_type: TokenType::None,
                literal: "".into(),
            },
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
        let token = self.curr_token.clone();

        if !self.peek_then_next(TokenType::IDENT) {
            return None;
        }

        let name = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        if !self.peek_then_next(TokenType::ASSIGN) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest);

        if !self.peek_then_next(TokenType::SEMICOLON) {
            //TODO: add better error handling
            panic!("Expected semicolon");
        }

        Some(Statement::Let(Let { token, value, name }))
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

        let return_value = self.parse_expression(Precedence::Lowest);

        if !self.peek_then_next(TokenType::SEMICOLON) {
            //TODO: add better error handling
            panic!("Expected semicolon");
        }

        return Some(Statement::Return(Return {
            token,
            return_value,
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

    fn parse_block_statement(&mut self) -> Block {
        let mut block = Block {
            token: self.curr_token.clone(),
            statements: Vec::new(),
        };
        self.next_token();
        while !self.is_curr_token_expected(TokenType::RBRACE)
            && !self.is_curr_token_expected(TokenType::EOF)
        {
            let stmt = self.parse_statement();
            match stmt {
                Some(s) => block.statements.push(s),
                None => (),
            }
            self.next_token();
        }
        block
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

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = Vec::new();
        if self.is_peek_token_expected(TokenType::RPAREN) {
            self.next_token();
            return identifiers;
        }
        self.next_token();
        let identifier = Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };
        identifiers.push(identifier);
        while self.is_peek_token_expected(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let identifier = Identifier {
                token: self.curr_token.clone(),
                value: self.curr_token.literal.clone(),
            };
            identifiers.push(identifier);
        }
        if !self.peek_then_next(TokenType::RPAREN) {
            // TODO: there should be better error handling here
            panic!("Expected RPAREN")
        }

        identifiers
    }

    pub fn parse_call_args(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();

        if self.is_peek_token_expected(TokenType::RPAREN) {
            self.next_token();
            return args;
        }

        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);
        args.push(exp);
        while self.is_peek_token_expected(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest));
        }

        if !self.peek_then_next(TokenType::RPAREN) {
            todo!("Better error handling")
        }

        args
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
    use crate::ast::{Array, Boolean, Call, FnLit, If, Infix, Prefix, Str};

    use super::*;

    pub fn parse_index_expression(p: &mut Parser, array: Expression) -> Expression {
        let token = p.curr_token.clone();
        info!("parsing index expression: {}", token.literal);
        p.next_token();
        info!("\tparsing right");
        let right = p.parse_expression(Precedence::Lowest);
        info!("\tright: {}", right.repr());
        if !p.peek_then_next(TokenType::RBRACKET) {
            return Expression::None;
        }
        Expression::Index(Index {
            array: Box::new(array),
            index: Box::new(right),
            token,
        })
    }

    pub fn parse_array_literal(p: &mut Parser) -> Expression {
        let token = p.curr_token.clone();
        let mut elements = Vec::new();
        if p.is_peek_token_expected(TokenType::RBRACKET) {
            p.next_token();
            return Expression::Array(Array { token, elements });
        }

        p.next_token();

        let exp = p.parse_expression(Precedence::Lowest);
        elements.push(exp);
        while p.is_peek_token_expected(TokenType::COMMA) {
            p.next_token();
            p.next_token();
            elements.push(p.parse_expression(Precedence::Lowest));
        }

        if !p.peek_then_next(TokenType::RBRACKET) {
            todo!("Better error handling")
        }

        Expression::Array(Array { token, elements })
    }

    pub fn parse_string_literal(p: &mut Parser) -> Expression {
        Expression::String(Str {
            token: p.curr_token.clone(),
            value: p.curr_token.literal.clone(),
        })
    }

    pub fn parse_function_literal(p: &mut Parser) -> Expression {
        let token = p.curr_token.clone();
        if !p.peek_then_next(TokenType::LPAREN) {
            return Expression::None;
        }

        let parameters = p.parse_function_parameters();

        if !p.peek_then_next(TokenType::LBRACE) {
            return Expression::None;
        }

        let body = p.parse_block_statement();

        Expression::FnLit(FnLit {
            token,
            parameters,
            body,
        })
    }

    pub fn parse_if_expression(p: &mut Parser) -> Expression {
        let token = p.curr_token.clone();
        if !p.peek_then_next(TokenType::LPAREN) {
            return Expression::None;
        }
        p.next_token();
        let condition = Box::new(p.parse_expression(Precedence::Lowest));
        if !p.peek_then_next(TokenType::RPAREN) {
            return Expression::None;
        }

        if !p.peek_then_next(TokenType::LBRACE) {
            return Expression::None;
        }
        let consequence = Box::new(p.parse_block_statement());

        let mut alternative = None;
        if p.is_peek_token_expected(TokenType::ELSE) {
            p.next_token();
            if !p.peek_then_next(TokenType::LBRACE) {
                return Expression::None;
            }
            alternative = Some(Box::new(p.parse_block_statement()));
        }
        Expression::If(If {
            token,
            condition,
            consequence,
            alternative,
        })
    }

    pub fn parse_grouped_expression(p: &mut Parser) -> Expression {
        p.next_token();
        let expression = p.parse_expression(Precedence::Lowest);
        if !p.peek_then_next(TokenType::RPAREN) {
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

    pub fn parse_call_expression(p: &mut Parser, function: Expression) -> Expression {
        let token = p.curr_token.clone();
        let arguments = p.parse_call_args();
        Expression::Call(Call {
            token,
            arguments,
            function: Box::new(function),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    enum Type {
        Int(i64),
        Ident(String),
        Str(String),
        Bool(bool),
    }

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_let_statements() {
        let tests = [
            ("let x = 5;", "x", Type::Int(5)),
            ("let y = true;", "y", Type::Bool(true)),
            ("let foobar = y;", "foobar", Type::Ident("y".into())),
            (
                r#"let hello = "Hello world";"#,
                "hello",
                Type::Str("Hello world".to_string()),
            ),
        ];

        for (input, expected_ident, expected_val) in tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();
            check_errors(parser);
            assert!(program.statements.len() == 1);

            let stmt = &program.statements[0];
            check_let_statement(stmt, expected_ident);
            let val = match stmt {
                Statement::Let(stmt) => &stmt.value,
                _ => panic!("Expected Let statement"),
            };
            check_literal_expression(val, expected_val);
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
        let tests = [
            ("return 5;", Type::Int(5)),
            ("return true;", Type::Bool(true)),
            ("return y;", Type::Ident("y".into())),
        ];

        for (input, expected_val) in tests {
            let lexer = Lexer::new(input.into());
            let mut parser = Parser::new(lexer);
            let program = parser.parse().unwrap();
            check_errors(parser);
            assert!(program.statements.len() == 1);

            let stmt = &program.statements[0];
            let val = match stmt {
                Statement::Return(stmt) => &stmt.return_value,
                _ => panic!("Expected return statement"),
            };
            check_literal_expression(val, expected_val);
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
            ("!5;", "!", Type::Int(5)),
            ("-15;", "-", Type::Int(15)),
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

    fn check_string_literal(exp: &Expression, value: String) -> bool {
        let streeng = match exp {
            Expression::String(s) => s,
            _ => panic!("Expected string expression"),
        };
        assert!(streeng.value == value);
        assert!(streeng.token_literal() == format!("{}", value));
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
    fn test_operator_precedence_parsing() {
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
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
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
            Type::Int(i) => return check_integer_literal(expression, i),
            Type::Ident(i) => return check_identifier(expression, i),
            Type::Bool(b) => return check_boolean_literal(expression, b),
            Type::Str(s) => return check_string_literal(expression, s),
        }
    }

    fn check_infix_expression(
        expression: &Expression,
        left: Type,
        op: String,
        right: Type,
    ) -> bool {
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

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        check_errors(parser);
        assert!(program.statements.len() == 1);
        let exp_stmt = match &program.statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression statement"),
        };
        let if_stmt = match &exp_stmt.expression {
            Expression::If(i) => i,
            _ => panic!("Expected if expression"),
        };
        check_infix_expression(
            if_stmt.condition.as_ref(),
            Type::Ident("x".into()),
            "<".into(),
            Type::Ident("y".into()),
        );
        let statements = &if_stmt.consequence.as_ref().statements;
        assert!(statements.len() == 1);

        let consequence = match &statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression stmt"),
        };
        check_identifier(&consequence.expression, "x".into());
        if let Some(alt) = &if_stmt.alternative {
            match alt.as_ref().statements[0] {
                Statement::None => (),
                _ => panic!("Expected block, got: {:?}", if_stmt.alternative.as_ref()),
            };
        };
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        check_errors(parser);
        assert!(program.statements.len() == 1);
        let exp_stmt = match &program.statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression statement"),
        };
        let if_stmt = match &exp_stmt.expression {
            Expression::If(i) => i,
            _ => panic!("Expected if expression"),
        };
        check_infix_expression(
            if_stmt.condition.as_ref(),
            Type::Ident("x".into()),
            "<".into(),
            Type::Ident("y".into()),
        );
        let statements = &if_stmt.consequence.as_ref().statements;
        assert!(statements.len() == 1);

        let consequence = match &statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression stmt"),
        };
        check_identifier(&consequence.expression, "x".into());
        let alt_statements = &if_stmt.alternative.as_ref().unwrap().statements;
        assert!(alt_statements.len() == 1);

        let alternative = match &alt_statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression stmt"),
        };
        check_identifier(&alternative.expression, "y".into());
    }

    #[test]
    fn test_function_literal_parsing() {
        // TODO: might want to add more tests for this
        let input = "fn(x, y) { x + y }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        check_errors(parser);
        assert!(program.statements.len() == 1);

        let exp_stmt = match &program.statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression statement"),
        };
        let fn_lit = match &exp_stmt.expression {
            Expression::FnLit(f) => f,
            _ => panic!("Expected fn lit expression"),
        };

        assert!(fn_lit.parameters.len() == 2);
        check_identifier(
            &Expression::Identifier(fn_lit.parameters[0].clone()),
            "x".into(),
        );
        check_identifier(
            &Expression::Identifier(fn_lit.parameters[1].clone()),
            "y".into(),
        );
        assert!(fn_lit.body.statements.len() == 1);
        let body_stmt = match &fn_lit.body.statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression stmt"),
        };
        check_infix_expression(
            &body_stmt.expression,
            Type::Ident("x".into()),
            "+".into(),
            Type::Ident("y".into()),
        );
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        check_errors(parser);
        assert!(program.statements.len() == 1);

        let exp_stmt = match &program.statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression statement"),
        };
        let call_exp = match &exp_stmt.expression {
            Expression::Call(c) => c,
            _ => panic!("Expected call expression"),
        };

        check_identifier(&call_exp.function.as_ref(), "add".into());

        assert!(call_exp.arguments.len() == 3);
        check_literal_expression(&call_exp.arguments[0], Type::Int(1));
        check_infix_expression(
            &call_exp.arguments[1],
            Type::Int(2),
            "*".into(),
            Type::Int(3),
        );
        check_infix_expression(
            &call_exp.arguments[2],
            Type::Int(4),
            "+".into(),
            Type::Int(5),
        );
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world!\";";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        check_errors(parser);
        assert!(program.statements.len() == 1);

        let exp_stmt = match &program.statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression statement"),
        };
        let str_exp = match &exp_stmt.expression {
            Expression::String(s) => s,
            _ => panic!("Expected string expression, got {:?}", exp_stmt),
        };
        assert!(str_exp.value == "hello world!");
    }

    #[test]
    fn test_parse_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        check_errors(parser);
        assert!(program.statements.len() == 1);

        let exp_stmt = match &program.statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression statement"),
        };
        let arr_exp = match &exp_stmt.expression {
            Expression::Array(a) => a,
            _ => panic!("Expected array expression, got {:?}", exp_stmt),
        };
        check_integer_literal(&arr_exp.elements[0], 1);
        check_infix_expression(&arr_exp.elements[1], Type::Int(2), "*".into(), Type::Int(2));
        check_infix_expression(&arr_exp.elements[2], Type::Int(3), "+".into(), Type::Int(3));
    }

    #[test]
    fn test_parse_index_expression() {
        let input = "myArray[1 + 1];";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        check_errors(parser);
        assert!(program.statements.len() == 1);

        let exp_stmt = match &program.statements[0] {
            Statement::ExpressionStmt(s) => s,
            _ => panic!("Expected expression statement"),
        };
        let idx_exp = match &exp_stmt.expression {
            Expression::Index(i) => i,
            _ => panic!("Expected index expression, got {:?}", exp_stmt),
        };
        check_identifier(&*idx_exp.array, "myArray".into());
        check_infix_expression(&*idx_exp.index, Type::Int(1), "+".into(), Type::Int(1));
    }
}
