use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT, // add, foobar, x, y, ...
    INT,   // 1343456

    // Operators
    ASSIGN,
    PLUS,

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
}

pub struct Token {
    token_type: TokenType,
    literal: String,
}

impl Token {
    pub fn new<T: Into<String>>(token_type: TokenType, literal: T) -> Self
    {
        Self {
            token_type,
            literal: literal.into(),
        }
    }

    pub fn match_char(ch: char, is_eof: bool) -> TokenType {
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
            _ => TokenType::ILLEGAL,
        }
    }
}

pub struct Lexer {
    input: String,
    position: usize, // Current
    read_position: usize, // Current + 1
    ch: char,
    is_eof: bool,
    keywords: HashMap<String, TokenType>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let keywords = HashMap::from([
            ("fn".to_string(), TokenType::FUNCTION),
            ("let".to_string(), TokenType::LET),
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
            self.ch = self.input.chars().nth(self.read_position).unwrap(); // Consume the next char
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        self.ignore_whitespace();
        let token = match Token::match_char(self.ch, self.is_eof) {
            TokenType::ASSIGN => Token::new(TokenType::ASSIGN, self.ch),
            TokenType::PLUS => Token::new(TokenType::PLUS, self.ch),
            TokenType::COMMA => Token::new(TokenType::COMMA, self.ch),
            TokenType::SEMICOLON => Token::new(TokenType::SEMICOLON, self.ch),
            TokenType::LPAREN => Token::new(TokenType::LPAREN, self.ch),
            TokenType::RPAREN => Token::new(TokenType::RPAREN, self.ch),
            TokenType::LBRACE => Token::new(TokenType::LBRACE, self.ch),
            TokenType::RBRACE => Token::new(TokenType::RBRACE, self.ch),
            TokenType::EOF => Token::new(TokenType::EOF, ""),
            _ => {
                if Lexer::is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let token_type = self.find_indentifier(&literal);
                    return Token::new(token_type, literal);
                } else if Lexer::is_digit(self.ch) {
                    let literal = self.read_number();
                    let token_type = TokenType::INT;
                    return Token::new(token_type, literal);
                } else {
                    Token::new(TokenType::ILLEGAL, self.ch as char)
                }
            }
        };
        self.read_char();
        return token;
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while Lexer::is_letter(self.ch) {
            self.read_char();
        }
        return self.input[position..self.position].to_string();
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while Lexer::is_digit(self.ch) {
            self.read_char();
        }
        return self.input[position..self.position].to_string();
    }

    fn is_letter(ch: char) -> bool {
        return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_';
    }

    fn is_digit(ch: char) -> bool {
        return '0' <= ch && ch <= '9';
    }

    fn find_indentifier(&self, literal: &String) -> TokenType {
        if let Some(token_type) = self.keywords.get(literal) {
            return token_type.clone();
        }
        return TokenType::IDENT
    }

    fn ignore_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
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
            (TokenType::EOF, ""),
        ];
        let mut lexer = Lexer::new("let five = 5; let ten = 10; let add = fn(x, y) { x + y; }; let result = add(five, ten);".to_string());
        for token in tokens.iter() {
            let tok = lexer.next_token();
            assert_eq!(tok.token_type, token.0);
            assert_eq!(tok.literal, token.1);
        }
    }

    #[test]
    fn test_is_letter() {
        assert_eq!(Lexer::is_letter('a'), true);
        assert_eq!(Lexer::is_letter('z'), true);
        assert_eq!(Lexer::is_letter('A'), true);
        assert_eq!(Lexer::is_letter('Z'), true);
        assert_eq!(Lexer::is_letter('_'), true);
        assert_eq!(Lexer::is_letter('0'), false);
        assert_eq!(Lexer::is_letter('9'), false);
        assert_eq!(Lexer::is_letter(' '), false);
        assert_eq!(Lexer::is_letter('\t'), false);
    }
}
