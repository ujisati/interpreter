use crate::{
    ast::{
        Block, Boolean, DebugString, Expression, ExpressionStmt, FnLit, Identifier, If, Infix,
        Integer, Let, Node, Prefix, Program, Return, Statement, Call, Str, Array, Index,
    },
    objects::{Env, Environment, Obj, ObjectType},
};
use std::{cell::RefCell, rc::Rc};

// Rc<RefCell> gives us interior mutability (refcell) without worrying about reference lifetimes (rc)

pub trait Eval {
    fn eval(&self, env: Env) -> Obj;
}

impl Eval for Program {
    fn eval(&self, env: Env) -> Obj {
        let mut result = env.borrow_mut().get_none();
        for stmt in &self.statements {
            let eval_obj = stmt.eval(env.clone());
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
    fn eval(&self, env: Env) -> Obj {
        get_obj(ObjectType::Integer { value: self.value })
    }
}

impl Eval for Boolean {
    fn eval(&self, env: Env) -> Obj {
        get_obj(ObjectType::Boolean { value: self.value })
    }
}

impl Eval for Statement {
    fn eval(&self, env: Env) -> Obj {
        match self {
            Statement::None => todo!(),
            Statement::Let(i) => i.eval(env),
            Statement::Return(i) => i.eval(env),
            Statement::ExpressionStmt(i) => i.eval(env),
            Statement::Block(i) => i.eval(env)
        }
    }
}

impl Eval for ExpressionStmt {
    fn eval(&self, env: Env) -> Obj {
        self.expression.eval(env)
    }
}

impl Eval for Expression {
    fn eval(&self, env: Env) -> Obj {
        match self {
            Expression::Integer(i) => i.eval(env),
            Expression::Prefix(i) => i.eval(env),
            Expression::Boolean(i) => i.eval(env),
            Expression::Infix(i) => i.eval(env),
            Expression::None => todo!(),
            Expression::Identifier(i) => i.eval(env),
            Expression::If(i) => i.eval(env),
            Expression::FnLit(i) => i.eval(env),
            Expression::Call(i) => i.eval(env),
            Expression::String(i) => i.eval(env),
            Expression::Array(i) => i.eval(env),
            Expression::Index(i) => i.eval(env),
        }
    }
}

impl Eval for Prefix {
    fn eval(&self, env: Env) -> Obj {
        let right = self.right.eval(env.clone());
        match self.operator.as_str() {
            "!" => eval_util::eval_bang_operator(right, env.clone()),
            "-" => eval_util::eval_minus_operator(right),
            _ => todo!("Need to add null handling. NO NULL :) "),
        }
    }
}

impl Eval for Infix {
    fn eval(&self, env: Env) -> Obj {
        let left = self.left.eval(env.clone());
        let right = self.right.eval(env);
        eval_util::eval_infix_expression(&self.operator, left, right)
    }
}

impl Eval for If {
    fn eval(&self, env: Env) -> Obj {
        let condition_is_true = match *self.condition.eval(env.clone()).borrow() {
            ObjectType::Boolean { value } => value,
            _ => todo!("Better error handling"),
        };
        if condition_is_true {
            return self.consequence.as_ref().eval(env);
        }
        match &self.alternative {
            Some(i) => return i.as_ref().eval(env),
            None => return env.borrow_mut().get_none(),
        }
    }
}

impl Eval for Block {
    fn eval(&self, env: Env) -> Obj {
        let mut result = env.borrow_mut().get_none();
        for stmt in &self.statements {
            let obj = stmt.eval(env.clone());
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
    fn eval(&self, env: Env) -> Obj {
        get_obj(ObjectType::Return {
            obj: self.return_value.eval(env),
        })
    }
}

impl Eval for Let {
    fn eval(&self, env: Env) -> Obj {
        let value = self.value.eval(env.clone());
        env.borrow_mut()
            .store
            .insert(self.name.value.clone(), value);
        env.borrow_mut().get_none()
    }
}

impl Eval for Identifier {
    fn eval(&self, env: Env) -> Obj {
        let value = env.borrow().get(&self.value);
        if let Some(v) = value {
            return v.clone();
        }
        if let Some(f) = env.borrow().builtin_fns.get(&self.value) {
            return get_obj(ObjectType::BuiltinFunction { name: self.value.clone(), function: f.clone() })
        };
        todo!("Identifier not found: {:?}", self.value)
    }
}

impl Eval for FnLit {
    fn eval(&self, env: Env) -> Obj {
        get_obj(ObjectType::Function {
            parameters: self.parameters.clone(),
            body: self.body.clone(),
            env: env.clone()
        })
    }
}

impl Eval for Call {
    fn eval(&self, env: Env) -> Obj {
        match &*self.function.eval(env.clone()).borrow() {
            ObjectType::Function { parameters, body, env: fn_env } => {
                let args = eval_util::eval_expressions(&self.arguments, env.clone());
                let mut inner_env = Environment::new_inner(fn_env.clone());
                for (i, arg) in args.iter().enumerate() {
                    inner_env.store.insert(parameters[i].value.clone(), arg.clone());
                }
                let evaluated = body.eval(Rc::new(RefCell::new(inner_env)));
                match &*evaluated.borrow() {
                    ObjectType::Return { obj } => return obj.clone(),
                    _ => (),
                }
                evaluated
            },
            ObjectType::BuiltinFunction { function, .. } => {
                let args = eval_util::eval_expressions(&self.arguments, env.clone());
                function(Some(args), env.clone())
            }
            _ => todo!("Expected function")
        }
    }
}

impl Eval for Str {
    fn eval(&self, env: Env) -> Obj {
       get_obj(ObjectType::Str { value: self.value.clone() }) 
    }
}

impl Eval for Array {
    fn eval(&self, env: Env) -> Obj {
        let mut evaluated = Vec::new();
        for elem in &self.elements {
            evaluated.push(elem.eval(env.clone()));
        }
        get_obj(ObjectType::Array { elements: evaluated })
    }
}

impl Eval for Index {
    fn eval(&self, env: Env) -> Obj {
        let arr = match *self.array {
            Expression::Array(_) => todo!(),
            _ => todo!("Better error handling")
        };
    }
}

pub fn get_obj(obj_type: ObjectType) -> Obj {
    Rc::new(RefCell::new(obj_type))
}

mod eval_util {
    use super::*;

    pub fn eval_bang_operator(right: Obj, env: Env) -> Obj {
        let obj_type = right.borrow();
        let mut env = env.borrow_mut();
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
            _ => todo!("Cannot negate object type {:?}", obj_type),
        }
    }

    pub fn eval_minus_operator(right: Obj) -> Obj {
        match *right.borrow() {
            ObjectType::Integer { value } => get_obj(ObjectType::Integer { value: -value }),
            _ => todo!(),
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
    
    pub fn eval_expressions(expressions: &Vec<Expression>, env: Env) -> Vec<Obj> {
        let mut evaluated = Vec::new();
        for expression in expressions {
            evaluated.push(expression.eval(env.clone()))
        }
        evaluated

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
        let env = Rc::new(RefCell::new(Environment::new()));
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
                None => match *evaluated.borrow() {
                    ObjectType::None => assert!(true),
                    _ => panic!("Expected None"),
                },
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

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = get_eval(input);
        match &*evaluated.borrow() {
            ObjectType::Function {
                parameters, body, ..
            } => {
                assert!(parameters.len() == 1);
                assert!(parameters[0].value == "x");
                assert!(body.repr() == "(x + 2)");
            }
            _ => panic!("Expected a function"),
        };
    }

    #[test]
    fn test_function_application() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];
        for (input, expected) in tests {
            let evaluated = get_eval(input);
            assert_int_object(evaluated, expected)
        }
    }

    #[test]
    fn test_closures() {
        let input = "
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(2);
        ";
        let evaluated = get_eval(input);
        assert_int_object(evaluated, 4);
    }

    #[test]
    fn test_first_class_function() {
        let input = "
            let add = fn(a, b) { a + b };
            let sub = fn(a, b) { a - b };
            let applyFunc = fn(a, b, func) { func(a, b) };
            applyFunc(10, 2, sub);
        ";
        let evaluated = get_eval(input);
        assert_int_object(evaluated, 8);
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello world!\"";
        let evaluated = get_eval(input);
        let hello = match &*evaluated.borrow() {
            ObjectType::Str { value } => value.clone(),
            _ => panic!("Expected string literal")
        };
        assert!(hello == "Hello world!");
    }

    #[test]
    #[should_panic]
    fn test_block_scope() {
        // TODO: blocks need their own environment
        let input = "
            if (true) { let x = 1; };
            let y = x;
        ";
        get_eval(input);
    }

    #[test]
    #[should_panic]
    fn test_function_scope() {
        let input = "
            let scope = fn() { let x = 1; };
            scope();
            let y = x;
        ";
        get_eval(input);
    }

    #[test]
    fn test_eval_array() {
        let input = "
            let x = 5;
            let arr = [1, 2 * 2, x];
            arr;
        ";
        let evaluated = get_eval(input);
        let elements = match &*evaluated.borrow() {
            ObjectType::Array { elements } => {
                elements.clone()
            }
            _ => panic!("Expected array obj, got {:?}", evaluated)
        };
        let val0 = elements[0].borrow();
        let val1 = elements[1].borrow();
        let val2 = elements[2].borrow();
        match *val0 {
            ObjectType::Integer { value } => assert!(value == 1),
            _ => panic!("Expected integer")
        }
        match *val1 {
            ObjectType::Integer { value } => assert!(value == 4),
            _ => panic!("Expected integer")
        }
        match *val2 {
            ObjectType::Integer { value } => assert!(value == 5),
            _ => panic!("Expected integer")
        }
    }

    // #[test]
    // fn test_builtin_len() {
    //     let tests = [
    //         (r#"len("hello!");"#, "hello"),
    //     ];
    //     for (input, expected) in tests {
    //         let evaluated = match *get_eval(input).borrow() {
    //
    //             _ => panic!("Expected builtin function")
    //         };
    //
    //         assert!(evaluated.name == "hello!");
    //     }
    // }
}
