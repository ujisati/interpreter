use crate::{
    ast::{
        Block, Boolean, Expression, ExpressionStmt, If, Infix, Integer, Node, Prefix, Program,
        Statement, Return,
    },
    objects::{ ObjectType},
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
        ObjectType::Integer{ value: self.value }
    }
}

impl Eval for Boolean {
    fn eval(&self) -> ObjectType {
        ObjectType::Boolean { value: self.value }
    }
}

impl Eval for Statement {
    fn eval(&self) -> ObjectType {
        match self {
            Statement::None => todo!(),
            Statement::Let(i) => todo!(),
            Statement::Return(i) => i.eval(),
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
            Expression::If(i) => i.eval(),
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

impl Eval for If {
    fn eval(&self) -> ObjectType {
        let condition_is_true = match self.condition.eval() {
            ObjectType::Boolean { value } => value,
            _ => todo!("Better error handling"),
        };
        if condition_is_true {
            return self.consequence.as_ref().eval();
        }
        match &self.alternative {
            Some(i) => return i.as_ref().eval(),
            None => return ObjectType::None,
        }
    }
}

impl Eval for Block {
    fn eval(&self) -> ObjectType {
        let mut result = None;
        for stmt in &self.statements {
            result = Some(stmt.eval());
        }
        match result {
            Some(r) => return r,
            None => ObjectType::None,
        }
    }
}

impl Eval for Return {
    fn eval(&self) -> ObjectType {
        ObjectType::Return { obj_type: Box::new(self.return_value.eval())}
    }
}

mod util {
    use super::*;

    pub fn eval_bang_operator(right: ObjectType) -> ObjectType {
        match right {
            ObjectType::Boolean {value} => {
                if value {
                    return ObjectType::Boolean { value: false };
                }
                return ObjectType::Boolean { value: true };
            }
            _ => ObjectType::Boolean { value: false },
        }
    }

    pub fn eval_minus_operator(right: ObjectType) -> ObjectType {
        match right {
            ObjectType::None => todo!(),
            ObjectType::Integer { value } => todo!(),
            _ => todo!()

            // ObjectType::Integer(i) => return ObjectType::Integer{value: -i.value},
            // _ => todo!("Better error handling"),
        }
    }

    pub fn eval_infix_expression(op: &String, left: ObjectType, right: ObjectType) -> ObjectType {
        match (left, right) {
            (ObjectType::Integer { value: val1 }, ObjectType::Integer { value: val2 }) => eval_integer_infix_expression(op, val1, val2),
            (ObjectType::Boolean { value: val1 }, ObjectType::Boolean { value: val2 }) => eval_bool_infix_expression(op, val1, val2),
            _ => todo!("Better error handling")
            // (ObjectType::Integer(i1), ObjectType::Integer(i2)) => {
            //     eval_integer_infix_expression(op, i1, i2)
            // }
            // (ObjectType::Boolean(b1), ObjectType::Boolean(b2)) => {
            //     eval_bool_infix_expression(op, b1, b2)
            // }
            
        }
    }

    fn eval_integer_infix_expression(op: &String, left: i64, right: i64) -> ObjectType {
        match op.as_str() {
            "+" => ObjectType::Integer {
                value: left + right,
            },
            "-" => ObjectType::Integer {
                value: left - right,
            },
            "*" => ObjectType::Integer {
                value: left * right,
            },
            "/" => ObjectType::Integer {
                value: left / right,
            },
            "<" => ObjectType::Boolean {
                value: left < right,
            },
            ">" => ObjectType::Boolean {
                value: left > right,
            },
            "==" => ObjectType::Boolean {
                value: left == right,
            },
            "!=" => ObjectType::Boolean {
                value: left != right,
            },
            _ => todo!("Better error handling"),
        }
    }

    fn eval_bool_infix_expression(op: &String, left: bool, right: bool) -> ObjectType {
        match op.as_str() {
            "==" => ObjectType::Boolean {
                value: left == right,
            },
            "!=" => ObjectType::Boolean {
                value: left != right,
            },
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
            ObjectType::Integer { value } => value,
            _ => todo!()
        };
        assert!(int_obj == expected);
    }

    fn assert_bool_object(obj: ObjectType, expected: bool) {
        let bool_obj = match obj {
            ObjectType::Boolean { value } => value,
            _ => todo!(),
        };
        assert!(bool_obj == expected);
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
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
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

    #[test]
    fn test_if_else_expression() {
        let tests = [
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for (input, expected) in tests {
            let evaluated = get_eval(input);
            match expected {
                Some(i) => assert_int_object(evaluated, i),
                None => assert!(evaluated == ObjectType::None),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = [
            ("1; return 10;", 10),
            ("return 11;", 11),
            ("return 1; 9;", 1),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
        ];

        for (input, expected) in tests {
            let evaluated = get_eval(input);
            assert_int_object(evaluated, expected)
        }
    }
}
