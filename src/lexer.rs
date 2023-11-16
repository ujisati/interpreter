use std::{collections::HashMap, fmt::Display};

/// Rust only allows enum discriminant to be an integer
/// Token::match_char handles matching TokenType to char
/// The Lexer keywords HashMap handles matching TokenType to keywords
/// I want to refactor that at somepoint, but it was the most simple
/// translation from Thorsten Ball's Go code for me at the time.
#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum TokenType {
    None,
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT,
    INT,

    // Operators
    ASSIGN,
    EQUAL,
    NOTEQUAL,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,

    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    //Types
    STRING,
    LBRACKET,
    RBRACKET,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new<T: Into<String>>(token_type: TokenType, literal: T) -> Self {
        Self {
            token_type,
            literal: literal.into(),
        }
    }

    fn match_char(ch: char, is_eof: bool) -> TokenType {
        if is_eof {
            return TokenType::EOF;
        }
        match ch {
            '=' => TokenType::ASSIGN,
            ';' => TokenType::SEMICOLON,
            '(' => TokenType::LPAREN,
            ')' => TokenType::RPAREN,
            ',' => TokenType::COMMA,
            '+' => TokenType::PLUS,
            '{' => TokenType::LBRACE,
            '}' => TokenType::RBRACE,
            '-' => TokenType::MINUS,
            '!' => TokenType::BANG,
            '*' => TokenType::ASTERISK,
            '/' => TokenType::SLASH,
            '>' => TokenType::GT,
            '<' => TokenType::LT,
            '"' => TokenType::STRING,
            '[' => TokenType::LBRACKET,
            ']' => TokenType::RBRACKET,
            _ => TokenType::ILLEGAL,
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,      // Current
    read_position: usize, // Current + 1
    ch: char,
    is_eof: bool,
    keywords: HashMap<String, TokenType>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let keywords = HashMap::from([
            ("fn".to_string(), TokenType::FUNCTION),
            ("let".to_string(), TokenType::LET),
            ("true".to_string(), TokenType::TRUE),
            ("false".to_string(), TokenType::FALSE),
            ("if".to_string(), TokenType::IF),
            ("else".to_string(), TokenType::ELSE),
            ("return".to_string(), TokenType::RETURN),
        ]);
        let mut l = Self {
            input,
            position: 0,
            read_position: 0,
            ch: 0 as char,
            is_eof: false,
            keywords,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.is_eof = true;
        } else {
            // Consume the next char
            // This is not 0(1), should have a Vec<char> instead
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> char {
        if self.read_position < self.input.len() {
            return self.input.chars().nth(self.read_position).unwrap();
        }
        panic!("Shouldn't have peeked char")
    }

    pub fn next_token(&mut self) -> Token {
        self.ignore_whitespace();
        let token = match Token::match_char(self.ch, self.is_eof) {
            TokenType::ASSIGN => match self.peek_char() {
                '=' => {
                    self.read_char();
                    Token::new(TokenType::EQUAL, "==")
                }
                _ => Token::new(TokenType::ASSIGN, self.ch),
            },
            TokenType::BANG => match self.peek_char() {
                '=' => {
                    self.read_char();
                    Token::new(TokenType::NOTEQUAL, "!=")
                }
                _ => Token::new(TokenType::BANG, self.ch),
            },
            TokenType::PLUS => Token::new(TokenType::PLUS, self.ch),
            TokenType::COMMA => Token::new(TokenType::COMMA, self.ch),
            TokenType::SEMICOLON => Token::new(TokenType::SEMICOLON, self.ch),
            TokenType::LPAREN => Token::new(TokenType::LPAREN, self.ch),
            TokenType::RPAREN => Token::new(TokenType::RPAREN, self.ch),
            TokenType::LBRACE => Token::new(TokenType::LBRACE, self.ch),
            TokenType::RBRACE => Token::new(TokenType::RBRACE, self.ch),
            TokenType::MINUS => Token::new(TokenType::MINUS, self.ch),
            TokenType::ASTERISK => Token::new(TokenType::ASTERISK, self.ch),
            TokenType::SLASH => Token::new(TokenType::SLASH, self.ch),
            TokenType::LT => Token::new(TokenType::LT, self.ch),
            TokenType::GT => Token::new(TokenType::GT, self.ch),
            TokenType::EOF => Token::new(TokenType::EOF, ""),
            TokenType::STRING => Token::new(TokenType::STRING, self.read_string()),
            TokenType::LBRACKET => Token::new(TokenType::LBRACKET, self.ch),
            TokenType::RBRACKET => Token::new(TokenType::RBRACKET, self.ch),
            _ => {
                if Lexer::is_letter(self.ch, self.is_eof) {
                    let literal = self.read_identifier();
                    let token_type = self.find_indentifier(&literal);
                    return Token::new(token_type, literal);
                } else if Lexer::is_digit(self.ch, self.is_eof) {
                    let literal = self.read_number();
                    let token_type = TokenType::INT;
                    return Token::new(token_type, literal);
                } else {
                    return Token::new(TokenType::ILLEGAL, self.ch as char);
                }
            }
        };
        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while Lexer::is_letter(self.ch, self.is_eof) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while Lexer::is_digit(self.ch, self.is_eof) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_string(&mut self) -> String {
        self.read_char();
        let position = self.position;
        while Lexer::is_string(self.ch, self.is_eof) {
            self.read_char();
        }
        return self.input[position..self.position].to_string();
    }

    fn is_string(ch: char, is_eof: bool) -> bool {
        if is_eof {
            return false;
        }
        return ch != '"';
    }

    fn is_letter(ch: char, is_eof: bool) -> bool {
        if is_eof {
            return false;
        }
        'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
    }

    fn is_digit(ch: char, is_eof: bool) -> bool {
        if is_eof {
            return false;
        }
        '0' <= ch && ch <= '9'
    }

    fn find_indentifier(&self, literal: &String) -> TokenType {
        if let Some(token_type) = self.keywords.get(literal) {
            return token_type.clone();
        }
        TokenType::IDENT
    }

    fn ignore_whitespace(&mut self) {
        while !self.is_eof
            && (self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r')
        {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token() {
        let tokens = [
            (TokenType::LET, "let"),
            (TokenType::IDENT, "five"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "5"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "ten"),
            (TokenType::ASSIGN, "="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "add"),
            (TokenType::ASSIGN, "="),
            (TokenType::FUNCTION, "fn"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "x"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "y"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::IDENT, "x"),
            (TokenType::PLUS, "+"),
            (TokenType::IDENT, "y"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LET, "let"),
            (TokenType::IDENT, "result"),
            (TokenType::ASSIGN, "="),
            (TokenType::IDENT, "add"),
            (TokenType::LPAREN, "("),
            (TokenType::IDENT, "five"),
            (TokenType::COMMA, ","),
            (TokenType::IDENT, "ten"),
            (TokenType::RPAREN, ")"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::BANG, "!"),
            (TokenType::MINUS, "-"),
            (TokenType::SLASH, "/"),
            (TokenType::ASTERISK, "*"),
            (TokenType::INT, "2"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::INT, "0"),
            (TokenType::LT, "<"),
            (TokenType::INT, "10"),
            (TokenType::GT, ">"),
            (TokenType::INT, "9"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::IF, "if"),
            (TokenType::LPAREN, "("),
            (TokenType::INT, "1"),
            (TokenType::LT, "<"),
            (TokenType::INT, "7"),
            (TokenType::RPAREN, ")"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::TRUE, "true"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::ELSE, "else"),
            (TokenType::LBRACE, "{"),
            (TokenType::RETURN, "return"),
            (TokenType::FALSE, "false"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::RBRACE, "}"),
            (TokenType::INT, "10"),
            (TokenType::EQUAL, "=="),
            (TokenType::INT, "10"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::INT, "10"),
            (TokenType::NOTEQUAL, "!="),
            (TokenType::INT, "9"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::STRING, "foobar"),
            (TokenType::STRING, "foo bar"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::LBRACKET, "["),
            (TokenType::INT, "1"),
            (TokenType::COMMA, ","),
            (TokenType::INT, "2"),
            (TokenType::RBRACKET, "]"),
            (TokenType::SEMICOLON, ";"),
            (TokenType::EOF, ""),
        ];
        let mut lexer = Lexer::new(
            r#"
            let five = 5; 
            let ten = 10; 
            let add = fn(x, y) { x + y; };
            let result = add(five, ten);
            !-/*2;
            0 < 10 > 9;
            if (1 < 7) {
                return true;
            } else {
                return false;
            }
            10 == 10;
            10 != 9;
            "foobar"
            "foo bar";
            [1, 2];
            "#,
        );
        for token in tokens.iter() {
            let tok = lexer.next_token();
            assert_eq!(tok.token_type, token.0);
            assert_eq!(tok.literal, token.1);
        }
    }

    #[test]
    fn test_is_letter() {
        assert_eq!(Lexer::is_letter('a', false), true);
        assert_eq!(Lexer::is_letter('z', false), true);
        assert_eq!(Lexer::is_letter('A', false), true);
        assert_eq!(Lexer::is_letter('Z', false), true);
        assert_eq!(Lexer::is_letter('_', false), true);
        assert_eq!(Lexer::is_letter('0', false), false);
        assert_eq!(Lexer::is_letter('9', false), false);
        assert_eq!(Lexer::is_letter(' ', false), false);
        assert_eq!(Lexer::is_letter('\t', false), false);
    }
}
