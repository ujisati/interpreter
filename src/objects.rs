pub trait Object {
    fn inspect(&self) -> String;
    // fn object_type(&self) -> ObjectType;
}

#[derive(Debug)]
pub enum ObjectType {
    Integer(Integer),
    Boolean(Boolean),
}

#[derive(Debug)]
pub struct Integer {
    pub value: i64,
}

#[derive(Debug)]
pub struct Boolean {
    pub value: bool,
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
