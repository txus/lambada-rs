#[derive(Debug, PartialEq)]
pub enum AST {
    Program(Vec<AST>),
    Sexp(Vec<AST>),
    Symbol(String),
    Integer(i32),
    String(String),
}
