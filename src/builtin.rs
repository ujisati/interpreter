use crate::evaluator::get_obj;
use crate::objects::Env;
use crate::objects::Obj;
use crate::objects::ObjectType;

pub fn print(obj: Option<Vec<Obj>>, env: Env) -> Obj {
    let obj = match obj {
        Some(i) => i[0].clone(),
        None => todo!("Probably want shared utility for enforcing function signature"),
    };
    match &*obj.borrow() {
        ObjectType::Str { value } => println!("{}", value),
        _ => todo!("Better error handling"),
    }
    env.borrow_mut().get_none()
}

pub fn len(obj: Option<Vec<Obj>>, env: Env) -> Obj {
    let obj = match obj {
        Some(i) => i[0].clone(),
        None => todo!("Probably want shared utility for enforcing function signature"),
    };
    let len = match &*obj.borrow() {
        ObjectType::Str { value } => value.len() as i64,
        _ => todo!("Better error handling"),
    };
    get_obj(ObjectType::Integer { value: len })
}
