extern crate lang;
use lang::evaluator::Eval;
use lang::objects::Environment;
use lang::{ast::DebugString, parser::Parser};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::io::Write;
use std::rc::Rc;

use crate::lang::lexer::Lexer;

fn main() -> io::Result<()> {
    println!("Cash lang v0.1\n(Sorry if it panics)");
    let env = Rc::new(RefCell::new(Environment::new()));
    loop {
        print!("\u{001b}[32m>>\u{001b}[37m ");
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
            println!(" \u{001b}[33m{:?}", parser.errors);
            continue;
        }
        // Enable this for ast printing
        // println!("{:?}", program);
        let evaluated = program.eval(env.clone()).borrow().inspect();
        if evaluated != "" {
            println!("{}", evaluated);
        }
    }
}
