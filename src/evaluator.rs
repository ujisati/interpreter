use crate::{
    ast::{Expression, ExpressionStmt, Integer, Node, Program, Statement, Boolean, Prefix},
    objects::{Integer as IntObj, Boolean as BoolObj, ObjectType},
};

pub trait Eval {
    fn eval(&self) -> ObjectType;
}

impl Eval for Program {
    fn eval(&self) -> ObjectType {
        let mut result = None;
        for stmt in &self.statements {
            result = Some(stmt.eval());
        }
        match result {
            Some(r) => return r,
            None => panic!("No statements where evaluated"), // TODO: better error handling
        }
    }
}

impl Eval for Integer {
    fn eval(&self) -> ObjectType {
        ObjectType::Integer(IntObj { value: self.value })
    }
}

impl Eval for Boolean {
    fn eval(&self) -> ObjectType {
        ObjectType::Boolean(BoolObj { value: self.value })
    }
}

impl Eval for Statement {
    fn eval(&self) -> ObjectType {
        match self {
            Statement::None => todo!(),
            Statement::Let(i) => todo!(),
            Statement::Return(i) => todo!(),
            Statement::ExpressionStmt(i) => i.eval(),
            Statement::Block(i) => todo!(),
        }
    }
}

impl Eval for ExpressionStmt {
    fn eval(&self) -> ObjectType {
        self.expression.eval()
    }
}

impl Eval for Expression {
    fn eval(&self) -> ObjectType {
        match self {
            Expression::Integer(i) => i.eval(),
            Expression::Prefix(i) => i.eval(),
            Expression::Boolean(i) => i.eval(),
            Expression::None => todo!(),
            Expression::Identifier(_) => todo!(),
            Expression::Infix(_) => todo!(),
            Expression::If(_) => todo!(),
            Expression::FnLit(_) => todo!(),
            Expression::Call(_) => todo!(),
        }
    }
}

impl Eval for Prefix {
    fn eval(&self) -> ObjectType {
        let right = self.right.eval();
        match self.operator.as_str() {
            "!" => util::eval_bang_operator(right),
            _ => todo!("Need to add null handling. NO NULL :) ")  
        }
    }
}

mod util {
    use super::*;

    pub fn eval_bang_operator(right: ObjectType) -> ObjectType {
        match right {
            ObjectType::Boolean(i) => {
                if i.value {
                    return ObjectType::Boolean(BoolObj { value: false })
                }
                return ObjectType::Boolean(BoolObj { value: true })
            }
            _ => ObjectType::Boolean(BoolObj { value: false })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn get_eval(input: &str) -> ObjectType {
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        program.eval()
    }

    fn assert_int_object(obj: ObjectType, expected: i64) {
        let int_obj = match obj {
            ObjectType::Integer(i) => i,
            _ => todo!(),
        };
        assert!(int_obj.value == expected);
    }

    fn assert_bool_object(obj: ObjectType, expected: bool) {
        let bool_obj = match obj {
            ObjectType::Boolean(i) => i,
            _ => todo!(),
        };
        assert!(bool_obj.value == expected);

    }

    #[test]
    fn test_eval_int_expression() {
        let tests = [("5", 5), ("10", 10)];
        for (input, expected) in tests {
            let evaluated = get_eval(input);
            assert_int_object(evaluated, expected)
        }
    }

    #[test]
    fn test_eval_bool_expression() {
        let tests = [("true", true), ("false", false)];
        for (input, expected) in tests {
            let evaluated = get_eval(input);
            assert_bool_object(evaluated, expected);
        }
    }
    
    #[test]
    fn test_bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true)
        ];
        for (input, expected) in tests {
            let evaluated = get_eval(input);
            assert_bool_object(evaluated, expected);
        }
    }
}
