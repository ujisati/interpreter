use std::{rc::Rc, cell::RefCell};
use crate::{
    ast::{
        Block, Boolean, Expression, ExpressionStmt, If, Infix, Integer, Node, Prefix, Program,
        Return, Statement, Let, Identifier,
    },
    objects::{ObjectType, Environment, Obj},
};



// Rc<RefCell> gives us interior mutability (refcell) without worrying about reference lifetimes (rc)

pub trait Eval {
    fn eval(&self, env: &mut Environment) -> Obj;
}

impl Eval for Program {
    fn eval(&self, env: &mut Environment) -> Obj {
        let mut result = env.get_none();
        for stmt in &self.statements {
            let eval_obj = stmt.eval(env);
            let eval_type = eval_obj.borrow();
            match &*eval_type {
                ObjectType::Return { obj } => return obj.clone(),
                _ => result = eval_obj.clone(),
            }
        }
        result
    }
}

impl Eval for Integer {
    fn eval(&self, env: &mut Environment) -> Obj {
        get_obj(ObjectType::Integer { value: self.value })
    }
}

impl Eval for Boolean {
    fn eval(&self, env: &mut Environment) -> Obj {
        get_obj(ObjectType::Boolean { value: self.value })
    }
}

impl Eval for Statement {
    fn eval(&self, env: &mut Environment) -> Obj {
        match self {
            Statement::None => todo!(),
            Statement::Let(i) => i.eval(env),
            Statement::Return(i) => i.eval(env),
            Statement::ExpressionStmt(i) => i.eval(env),
            Statement::Block(i) => todo!(),
        }
    }
}

impl Eval for ExpressionStmt {
    fn eval(&self, env: &mut Environment) -> Obj {
        self.expression.eval(env)
    }
}

impl Eval for Expression {
    fn eval(&self, env: &mut Environment) -> Obj {
        match self {
            Expression::Integer(i) => i.eval(env),
            Expression::Prefix(i) => i.eval(env),
            Expression::Boolean(i) => i.eval(env),
            Expression::Infix(i) => i.eval(env),
            Expression::None => todo!(),
            Expression::Identifier(i) => i.eval(env),
            Expression::If(i) => i.eval(env),
            Expression::FnLit(_) => todo!(),
            Expression::Call(_) => todo!(),
        }
    }
}

impl Eval for Prefix {
    fn eval(&self, env: &mut Environment) -> Obj {
        let right = self.right.eval(env);
        match self.operator.as_str() {
            "!" => eval_util::eval_bang_operator(right, env),
            "-" => eval_util::eval_minus_operator(right),
            _ => todo!("Need to add null handling. NO NULL :) "),
        }
    }
}

impl Eval for Infix {
    fn eval(&self, env: &mut Environment) -> Obj {
        let left = self.left.eval(env);
        let right = self.right.eval(env);
        eval_util::eval_infix_expression(&self.operator, left, right)
    }
}

impl Eval for If {
    fn eval(&self, env: &mut Environment) -> Obj {
        let condition_is_true = match *self.condition.eval(env).borrow() {
            ObjectType::Boolean { value } => value,
            _ => todo!("Better error handling"),
        };
        if condition_is_true {
            return self.consequence.as_ref().eval(env);
        }
        match &self.alternative {
            Some(i) => return i.as_ref().eval(env),
            None => return env.get_none()
        }
    }
}

impl Eval for Block {
    fn eval(&self, env: &mut Environment) -> Obj {
        let mut result = env.get_none();
        for stmt in &self.statements {
            let obj = stmt.eval(env);
            let obj_type = obj.borrow();
            match &*obj_type {
                ObjectType::Return { .. } => return obj.clone(),
                _ => result = obj.clone(),
            }
        }
        result
    }
}

impl Eval for Return {
    fn eval(&self, env: &mut Environment) -> Obj {
        get_obj(ObjectType::Return {
            obj: self.return_value.eval(env)
        })
    }
}

impl Eval for Let {
    fn eval(&self, env: &mut Environment) -> Obj {
        let value = self.value.eval(env);
        env.store.insert(self.name.value.clone(), value);
        env.get_none()
    }
}

impl Eval for Identifier {
    fn eval(&self, env: &mut Environment) -> Obj {
        match env.store.get(&self.value) {
            Some(value) => value.clone(),
            None => todo!("Identifier not defined")
        }
    }
}

fn get_obj(obj_type: ObjectType) -> Obj {
    Rc::new(RefCell::new(obj_type))
}

mod eval_util {
    use super::*;

    pub fn eval_bang_operator(right: Obj, env: &mut Environment) -> Obj {
        let obj_type = right.borrow();
        match *obj_type {
            ObjectType::Boolean { value } => {
                if value {
                    return env.get_false();
                }
                return env.get_true();
            }
            ObjectType::Integer { value } => {
                if value == 0 {
                    return env.get_true();
                }
                return env.get_false();
            }
            _ => todo!("Cannot negate object type {:?}", obj_type)
            
        }
    }

    pub fn eval_minus_operator(right: Obj) -> Obj {
        match *right.borrow() {
            ObjectType::Integer { value } => get_obj(ObjectType::Integer { value: -value }),
            _ => todo!()
        }
    }

    pub fn eval_infix_expression(op: &String, left: Obj, right: Obj) -> Obj {
        match (&*left.borrow(), &*right.borrow()) {
            (ObjectType::Integer { value: val1 }, ObjectType::Integer { value: val2 }) => {
                eval_integer_infix_expression(op, val1, val2)
            }
            (ObjectType::Boolean { value: val1 }, ObjectType::Boolean { value: val2 }) => {
                eval_bool_infix_expression(op, val1, val2)
            }
            _ => todo!("Better error handling"), 
        }
    }

    fn eval_integer_infix_expression(op: &String, left: &i64, right: &i64) -> Obj {
        let obj_type = match op.as_str() {
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
        };
        get_obj(obj_type)
    }

    fn eval_bool_infix_expression(op: &String, left: &bool, right: &bool) -> Obj {
        let obj_type = match op.as_str() {
            "==" => ObjectType::Boolean {
                value: left == right,
            },
            "!=" => ObjectType::Boolean {
                value: left != right,
            },
            _ => todo!("Better error handling"),
        };
        get_obj(obj_type)
    }

}


#[cfg(test)]
mod tests {

    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn get_eval(input: &str) -> Obj {
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse().unwrap();
        let env = &mut Environment::new();
        program.eval(env)
    }

    fn assert_int_object(obj: Obj, expected: i64) {
        let int_obj = match *obj.borrow() {
            ObjectType::Integer { value } => value,
            _ => panic!("Expected integer object, got {:?}", obj),
        };
        assert!(int_obj == expected);
    }

    fn assert_bool_object(obj: Obj, expected: bool) {
        let bool_obj = match *obj.borrow() {
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
                None => assert!(*evaluated.borrow() == ObjectType::None),
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
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }",
                10,
            ),
        ];

        for (input, expected) in tests {
            let evaluated = get_eval(input);
            assert_int_object(evaluated, expected)
        }
    }

    #[test]
    fn test_let_statement() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];
        for (input, expected) in tests {
            let evaluated = get_eval(input);
            assert_int_object(evaluated, expected)
        }
 
    }
}
