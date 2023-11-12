use std::{fs, cell::RefCell, rc::Rc};

use lang::{objects::Environment, lexer::Lexer, parser::Parser, evaluator::Eval};

fn main() {
    let path = std::env::args().nth(1).expect("No path given");
    let input = fs::read_to_string(path).expect("Unable to read file");
    let env = Rc::new(RefCell::new(Environment::new()));
    let lexer = Lexer::new(&*input);
    let mut parser = Parser::new(lexer);
    let program = match parser.parse() {
        Ok(p) => p,

        // TODO: improve improve parser error handling and printing
        Err(_) => panic!("Parser error"),
    };
    // Enable this for ast printing
    // println!("{:?}", program);
    let evaluated = program.eval(env.clone()).borrow().inspect();
    println!("{}", evaluated);
}
