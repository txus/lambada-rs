use ast::AST;
use std::boxed::Box;
use hamt::HamtRc;

#[derive(PartialEq, Debug, Clone)]
pub struct Function;

impl Function {
    fn apply(&self, args: &[Box<Value>]) -> Result<Box<Value>, String> {
        Err(format!("can't apply yet, called with args {:?}", args))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Integer(i32),
    String(String),
    Function(Function),
    Nil
}

#[derive(PartialEq, Debug, Clone)]
pub struct Namespace {
    name: String,
    bindings: HamtRc<String, Box<Value>>
}

impl Namespace {
    fn new(name: String) -> Namespace {
        Namespace {
            name: name.clone(),
            bindings: HamtRc::new()
        }
    }
    fn lookup(&self, symbol: &str) -> Option<&Box<Value>> {
        self.bindings.get(symbol)
    }

    fn define(&mut self, symbol: String, value: Box<Value>) {
        self.bindings = self.bindings.insert(&symbol, &value);
    }
}

pub type EvalResult = Result<Box<Value>, String>;

pub fn eval(ns: &mut Namespace, form: &AST) -> EvalResult {
    match *form {
        AST::Integer(ref n) => Ok(Box::new(Value::Integer(n.clone()))),
        AST::String(ref s) => Ok(Box::new(Value::String(s.clone()))),
        AST::Symbol(ref s) => match ns.lookup(s) {
            Some(val) => Ok(val.clone()),
            None => Err(format!("Symbol `{}` is undefined in namespace {}", s.clone(), ns.name))
        },
        AST::Sexp(ref list) => match list.head() {
            Some(&AST::Symbol(ref s)) if s == "def" => {
                match list.tail().head() {
                    Some(&AST::Symbol(ref name)) => {
                        match list.tailn(2).head() {
                            Some(node) => {
                                eval(ns, node).and_then(|v| {
                                    ns.define(name.clone(), v);
                                    Ok(Box::new(Value::Nil))
                                })
                            },
                            _ => Err("`def` takes an expression as its second argument".to_owned())
                        }
                    },
                    _ => Err("`def` takes a symbol as its first argument".to_owned())
                }
            },
            Some(&AST::Symbol(ref s)) if s == "+" => {
                match (list.tail().head(), list.tailn(2).head()) {
                    (Some(op1), Some(op2)) =>
                        eval(ns, op1).and_then(|x| {
                            eval(ns, op2).and_then(|y| {
                                match (x.clone(), y.clone()) {
                                    (box Value::Integer(x), box Value::Integer(y)) => Ok(Box::new(Value::Integer(x + y))),
                                    _ => Err("`+` takes two numbers".to_owned())
                                }
                            })
                        }),
                    _ => Err("`+` takes two arguments".to_owned())
                }
            },
            Some(car) => {
                eval(ns, &car).and_then(|value| {
                    match value {
                        box Value::Function(fun) => {
                            let mut args = vec![];
                            for node in list.tail().iter() {
                                match eval(ns, &node) {
                                    Ok(v) => args.push(v),
                                    e => return e
                                }
                            }
                            fun.apply(&args)
                        },
                        _ => Err(format!("{:?} is not a function", &value))
                    }
                })
            },
            _ => Err("Cannot evaluate empty sexp.".to_owned())
        },
        AST::Program(ref forms) => {
            let mut last = Ok(Box::new(Value::Nil));
            for f in forms {
                match eval(ns, &f) {
                    Ok(v) => last = Ok(v),
                    e => return e
                }
            }
            last
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Namespace, Value};
    use super::{eval};
    use parser::parse;
    use std::boxed::Box;

    macro_rules! assert_evals_ast {
        ( $expected:expr, $ns:expr, $expression:expr) => {
            {
                assert_eq!(Ok(Box::new($expected)), eval($ns, $expression));
            }
        }
    }

    macro_rules! assert_evals {
        ( $expected:expr, $ns:expr, $code:expr) => {
            {
                let parsed = parse($code.to_owned()).unwrap();
                assert_eq!(Ok(Box::new($expected)), eval($ns, &parsed));
            }
        }
    }

    fn empty_ns() -> Namespace {
        Namespace::new("user".to_owned())
    }

    fn lookup(ns: Namespace, symbol: &str) -> Option<Box<Value>> {
        match ns.lookup(symbol) {
            Some(v) => Some(v.clone()),
            None => None
        }
    }

    #[test]
    fn eval_integer_test() {
        assert_evals!(Value::Integer(20), &mut empty_ns(), "20");
    }

    #[test]
    fn eval_string_test() {
        assert_evals!(Value::String("hello".to_owned()), &mut empty_ns(), "\"hello\"");
    }

    #[test]
    fn eval_symbol_test() {
        let value = Value::String("bar".to_owned());
        let mut ns = empty_ns();
        ns.define("foo".to_owned(), Box::new(value));
        assert_evals!(Value::String("bar".to_owned()), &mut ns, "foo")
    }

    #[test]
    fn eval_def_test() {
        let mut ns = empty_ns();
        assert_evals!(Value::Nil, &mut ns, "(def foo \"bar\")");
        assert_eq!(Some(Box::new(Value::String("bar".to_owned()))), lookup(ns, "foo"));
    }
}
