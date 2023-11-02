pub trait Object {
    fn inspect(&self) -> String;
    // fn object_type(&self) -> ObjectType;
}

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    None, // This is not a None object type, but akin to Option<ObjectType>
    Integer { value: i64 },
    Boolean { value: bool },
    Return { obj_type: Box<ObjectType> },
}
