extern crate lang;
use lang::{ast::DebugString, lexer::TokenType, parser::Parser};
use std::io;
use std::io::Write;

use crate::lang::lexer::Lexer;

fn main() -> io::Result<()> {
    println!("Hello, friend!");
    loop {
        print!(">> ");
        io::stdout().flush()?;
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        let input = &*buffer;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = match parser.parse() {
            Ok(p) => p,

            // TODO: improve improve parser error handling and printing
            Err(_) => panic!("Parser error"),
        };
        if parser.errors.len() > 0 {
            // TODO: improve improve parser error handling and printing
            println!("{:?}", parser.errors);
            continue;
        }
        println!("{}", program.repr());
    }
}
