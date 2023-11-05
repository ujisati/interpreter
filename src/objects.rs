use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast::{Block, Identifier};

pub type Obj = Rc<RefCell<ObjectType>>;
pub type Env = Rc<RefCell<Environment>>;

#[derive(Debug)]
pub enum ObjectType {
    None, // This is not a None object type, but akin to Option<ObjectType>
    Integer {
        value: i64,
    },
    Boolean {
        value: bool,
    },
    Function {
        parameters: Vec<Identifier>,
        body: Block,
        env: Environment,
    },
    Return {
        obj: Obj,
    },
}

impl ObjectType {
    pub fn inspect(&self) -> String {
        match self {
            ObjectType::None => "".into(),
            ObjectType::Integer { value } => value.to_string(),
            ObjectType::Boolean { value } => value.to_string(),
            ObjectType::Function {
                parameters,
                body,
                env,
            } => todo!(),
            ObjectType::Return { obj } => obj.borrow().inspect(),
        }
    }
}

#[derive(Debug)]
pub struct Environment {
    pub store: HashMap<String, Obj>,
    pub obj_pool: HashMap<String, Obj>,
    pub outer: Option<Env>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            obj_pool: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_inner(outer: Env) -> Self {
        let mut env = Environment::new();
        env.outer = Some(outer);
        env
    }

    pub fn get_true(&mut self) -> Obj {
        match self.obj_pool.get("true") {
            Some(i) => i.clone(),
            None => {
                let t = Rc::new(RefCell::new(ObjectType::Boolean { value: true }));
                self.obj_pool.insert("true".into(), t.clone());
                t
            }
        }
    }

    pub fn get_false(&mut self) -> Obj {
        match self.obj_pool.get("false") {
            Some(i) => i.clone(),
            None => {
                let f = Rc::new(RefCell::new(ObjectType::Boolean { value: false }));
                self.obj_pool.insert("false".into(), f.clone());
                f
            }
        }
    }

    pub fn get_none(&mut self) -> Obj {
        match self.obj_pool.get("none") {
            Some(i) => i.clone(),
            None => {
                let n = Rc::new(RefCell::new(ObjectType::None));
                self.obj_pool.insert("none".into(), n.clone());
                n
            }
        }
    }
}
