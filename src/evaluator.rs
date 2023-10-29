use crate::{ast::Node, objects::ObjectType};

fn eval(node: Box<dyn Node>) -> ObjectType {
    todo!()
}


#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn get_eval(input: &str) -> ObjectType {
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        eval(Box::new(program))
    }

    fn test_int_object(obj: ObjectType, expected: i64) -> bool {
    
    }

    #[test]
    fn test_eval_int_expression() {}

}

