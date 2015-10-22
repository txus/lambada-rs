use cons_list::ConsList;

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
    Program(ConsList<AST>),
    Sexp(ConsList<AST>),
    Symbol(String),
    Integer(i32),
    String(String),
}
