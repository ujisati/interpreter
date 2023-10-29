pub trait Object {
    fn inspect(&self) -> String;
    // fn object_type(&self) -> ObjectType;
}

pub enum ObjectType {
    Integer(Integer),
    Boolean(Boolean)
}

pub struct Integer {
    value: i64
} 

pub struct Boolean {
    value: bool
}

impl Object for Integer {
    fn inspect(&self) -> String {
        self.value.to_string()
    }

    // fn object_type(&self) -> ObjectType {
    //     ObjectType::Integer
    // }
}

impl Object for Boolean {
    fn inspect(&self) -> String {
        self.value.to_string()
    }

    // fn object_type(&self) -> ObjectType {
    //     ObjectType::Boolean
    // }
}
