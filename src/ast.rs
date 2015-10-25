use cons_list::ConsList;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum AST {
    Program(ConsList<AST>),
    Sexp(ConsList<AST>),
    Symbol(String),
    Integer(i32),
    String(String),
}

#[allow(unused_must_use)]
impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            AST::Program(ref list) => {
                for i in list.iter() {
                    i.fmt(f);
                    write!(f, "\n");
                }
                write!(f, "")
            }
            AST::Sexp(ref list) => {
                write!(f, "(");
                let mut c = 0;
                for i in list.iter() {
                    c = c + 1;
                    i.fmt(f);
                    if c != list.len() {
                        write!(f, " ");
                    }
                }
                write!(f, ")")
            },
            AST::Symbol(ref s) => write!(f, "{}", &s),
            AST::Integer(ref i) => write!(f, "{}", &i),
            AST::String(ref s) => write!(f, "\"{}\"", &s)
        }
    }
}
