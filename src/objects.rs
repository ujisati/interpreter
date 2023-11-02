use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type Obj = Rc<RefCell<ObjectType>>;

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    None, // This is not a None object type, but akin to Option<ObjectType>
    Integer { value: i64 },
    Boolean { value: bool },
    Return { obj: Obj },
}

impl ObjectType {
    pub fn inspect(&self) -> String {
        match self {
            ObjectType::None => "".into(),
            ObjectType::Integer { value } => value.to_string(),
            ObjectType::Boolean { value } => value.to_string(),
            ObjectType::Return { obj } => obj.borrow().inspect(),
        }
    }
}

pub struct Environment {
    pub store: HashMap<String, Obj>,
    pub obj_pool: HashMap<String, Obj>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            obj_pool: HashMap::new(),
        }
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
