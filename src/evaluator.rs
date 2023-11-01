use crate::{
    ast::{Boolean, Expression, ExpressionStmt, Infix, Integer, Node, Prefix, Program, Statement},
    objects::{Boolean as BoolObj, Integer as IntObj, ObjectType},
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
            Expression::Infix(i) => i.eval(),
            Expression::None => todo!(),
            Expression::Identifier(_) => todo!(),
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
            "-" => util::eval_minus_operator(right),
            _ => todo!("Need to add null handling. NO NULL :) "),
        }
    }
}

impl Eval for Infix {
    fn eval(&self) -> ObjectType {
        let left = self.left.eval();
        let right = self.right.eval();
        util::eval_infix_expression(&self.operator, left, right)
    }
}

mod util {
    use super::*;

    pub fn eval_bang_operator(right: ObjectType) -> ObjectType {
        match right {
            ObjectType::Boolean(i) => {
                if i.value {
                    return ObjectType::Boolean(BoolObj { value: false });
                }
                return ObjectType::Boolean(BoolObj { value: true });
            }
            _ => ObjectType::Boolean(BoolObj { value: false }),
        }
    }

    pub fn eval_minus_operator(right: ObjectType) -> ObjectType {
        match right {
            ObjectType::Integer(i) => return ObjectType::Integer(IntObj { value: -i.value }),
            _ => todo!("Better error handling"),
        }
    }

    pub fn eval_infix_expression(op: &String, left: ObjectType, right: ObjectType) -> ObjectType {
        match (left, right) {
            (ObjectType::Integer(i1), ObjectType::Integer(i2)) => eval_integer_infix_expression(op, i1, i2),
            (ObjectType::Integer(_), ObjectType::Boolean(_)) => todo!(),
            (ObjectType::Boolean(_), ObjectType::Integer(_)) => todo!(),
            (ObjectType::Boolean(_), ObjectType::Boolean(_)) => todo!(),
        }
    }

    fn eval_integer_infix_expression(op: &String, left: IntObj, right: IntObj) -> ObjectType {
        match op.as_str() {
            "+" => ObjectType::Integer(IntObj {
                value: left.value + right.value,
            }),
            "-" => ObjectType::Integer(IntObj { value: left.value - right.value }),
            "*" => ObjectType::Integer(IntObj { value: left.value * right.value }),
            "/" => ObjectType::Integer(IntObj { value: left.value / right.value }),
            _ => todo!("Better error handling"),
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
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];
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
            ("!!5", true),
        ];
        for (input, expected) in tests {
            let evaluated = get_eval(input);
            assert_bool_object(evaluated, expected);
        }
    }
}
