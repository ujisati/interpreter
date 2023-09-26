use crate::lexer::Token;

enum Node {
    Statement(StatementType),
    Expression(ExpressionType),
}

enum StatementType {
    LET(),
}
enum ExpressionType {}

struct Let {
    token: Token,
    name: Identifier,
    value: ExpressionType,
}

struct Identifier {
    token: Token,
    value: String,
}

impl Node {
    fn token_literal(&self) {}
}
