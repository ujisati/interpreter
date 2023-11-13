use macros::Node;

use crate::{
    lexer::{Token, TokenType},
    objects::ObjectType,
};

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait DebugString {
    fn repr(&self) -> String;
}

#[derive(Node, Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub token: Token,    // just to satisfy the interface
    pub literal: String, //just to satisfy the interface
}

#[derive(Debug, Clone)]
pub enum Expression {
    None,
    Identifier(Identifier),
    Integer(Integer),
    Prefix(Prefix),
    Infix(Infix),
    Boolean(Boolean),
    If(If),
    FnLit(FnLit),
    Call(Call),
    String(Str),
    Array(Array),
}

#[derive(Node, Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[derive(Node, Debug, Clone)]
pub struct Integer {
    pub token: Token,
    pub value: i64,
}

#[derive(Node, Debug, Clone)]
pub struct Prefix {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Node, Debug, Clone)]
pub struct Infix {
    pub token: Token,
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Node, Debug, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

#[derive(Node, Debug, Clone)]
pub struct If {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: Box<Block>,
    pub alternative: Option<Box<Block>>,
}

#[derive(Node, Debug, Clone)]
pub struct FnLit {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Block,
}

#[derive(Debug)]
pub enum FunctionName {
    Identifier(Identifier),
    FnLit(FnLit),
}

#[derive(Node, Debug, Clone)]
pub struct Call {
    pub token: Token,
    pub function: Box<Expression>, // Identifier or FnLit
    pub arguments: Vec<Expression>,
}

#[derive(Node, Debug, Clone)]
pub struct Str {
    pub token: Token,
    pub value: String
}

#[derive(Node, Debug, Clone)]
pub struct Array {
    pub token: Token,
    pub elements: Vec<Expression>
}

#[derive(Debug, Clone)]
pub enum Statement {
    None,
    Let(Let),
    Return(Return),
    ExpressionStmt(ExpressionStmt),
    Block(Block),
}

#[derive(Node, Debug, Clone)]
pub struct Let {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Node, Debug, Clone)]
pub struct Return {
    pub token: Token,
    pub return_value: Expression,
}

#[derive(Node, Debug, Clone)]
pub struct ExpressionStmt {
    pub token: Token,
    pub expression: Expression,
}

#[derive(Node, Debug, Clone)]
pub struct Block {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl DebugString for Program {
    fn repr(&self) -> String {
        let mut output = String::from("");
        for s in &self.statements {
            output.push_str(&s.repr())
        }
        output
    }
}

impl DebugString for Statement {
    fn repr(&self) -> String {
        match self {
            Self::Let(s) => s.repr(),
            Self::Return(s) => s.repr(),
            Self::ExpressionStmt(s) => s.repr(),
            Self::Block(s) => s.repr(),
            Self::None => "".into(),
        }
    }
}

impl DebugString for Infix {
    fn repr(&self) -> String {
        let mut output = String::new();
        output.push('(');
        output.push_str(&self.left.repr());
        output.push(' ');
        output.push_str(&self.operator);
        output.push(' ');
        output.push_str(&self.right.repr());
        output.push(')');
        output
    }
}

impl DebugString for Prefix {
    fn repr(&self) -> String {
        let mut output = String::new();
        output.push('(');
        output.push_str(&self.operator);
        output.push_str(&self.right.repr());
        output.push(')');
        output
    }
}

impl DebugString for Expression {
    fn repr(&self) -> String {
        match self {
            Self::Identifier(s) => s.repr(),
            Self::Prefix(p) => p.repr(),
            Self::Infix(i) => i.repr(),
            Self::Integer(i) => i.repr(),
            Self::Boolean(b) => b.repr(),
            Self::Call(c) => c.repr(),
            Self::FnLit(f) => f.repr(),
            _ => panic!("Expression not found: {:?}", self),
        }
    }
}

impl DebugString for ExpressionStmt {
    fn repr(&self) -> String {
        self.expression.repr()
    }
}

impl DebugString for Let {
    fn repr(&self) -> String {
        self.token_literal() + " " + &self.name.repr() + " = " + &self.value.repr() + ";"
    }
}

impl DebugString for Return {
    fn repr(&self) -> String {
        self.token_literal() + " " + &self.return_value.repr()
    }
}

impl DebugString for Integer {
    fn repr(&self) -> String {
        self.value.clone().to_string()
    }
}

impl DebugString for Identifier {
    fn repr(&self) -> String {
        self.value.clone()
    }
}

impl DebugString for Boolean {
    fn repr(&self) -> String {
        self.value.to_string()
    }
}

impl DebugString for Block {
    fn repr(&self) -> String {
        let mut output = String::new();
        for s in &self.statements {
            output.push_str(s.repr().as_str());
        }
        output
    }
}

impl DebugString for If {
    fn repr(&self) -> String {
        let mut output = String::new();
        output.push_str("if");
        output.push_str(&self.condition.repr());
        output.push(' ');
        output.push_str(&self.consequence.repr());
        if let Some(alt) = &self.alternative {
            if alt.statements.len() > 0 {
                output.push_str("else");
                output.push_str(&alt.repr());
            }
        }
        output
    }
}

impl DebugString for FnLit {
    fn repr(&self) -> String {
        let mut output = String::new();
        let mut params = Vec::new();
        for param in &self.parameters {
            params.push(param.repr())
        }
        output.push_str(&self.token_literal());
        output.push('(');
        output.push_str(&params.join(", "));
        self.body.repr();
        output.push(')');
        output.push_str(&self.body.repr());
        output
    }
}

impl DebugString for Call {
    fn repr(&self) -> String {
        let mut output = String::new();
        let mut args = Vec::new();
        for arg in &self.arguments {
            args.push(arg.repr());
        }
        output.push_str(&self.function.repr());
        output.push('(');
        output.push_str(&args.join(", "));
        output.push(')');
        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenType;

    #[test]
    fn test_debug_string() {
        let program = Program {
            statements: vec![Statement::Let(Let {
                token: Token::new(TokenType::LET, "let"),
                name: Identifier {
                    token: Token::new(TokenType::IDENT, "my_var"),
                    value: String::from("my_var"),
                },
                value: Expression::Identifier(Identifier {
                    token: Token::new(TokenType::IDENT, "another_var"),
                    value: String::from("another_var"),
                }),
            })],
            literal: "".into(),
            token: Token {
                token_type: TokenType::None,
                literal: "".into(),
            },
        };
        println!("{}", &program.repr());
        assert!(&program.repr() == "let my_var = another_var;");
    }
}
