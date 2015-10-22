use ast::AST;
use std::boxed::Box;
use hamt::HamtRc;

use cons_list::ConsList;
use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub struct Closure {
    parent_env: Environment,
    params: ConsList<AST>,
    code: AST
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<function>")
    }
}

impl Closure {
    fn new(parent_env: &Environment, params: ConsList<AST>, code: ConsList<AST>) -> Closure {
        if !params.iter().all(|p| {
            if let &AST::Symbol(ref s) = p {
                let _ = s;
                true
            } else {
                false
            }
        }) {
            panic!("Can't define function with non-symbol parameters. Provided: {:?}", params)
        } else {
            Closure {
                parent_env: parent_env.clone(),
                params: params,
                code: AST::Program(code)
            }
        }
    }

    fn apply(&self, ns: &mut Namespace, args: &[Box<Value>]) -> Result<Box<Value>, String> {
        let mut bindings = HamtRc::<String, Box<Value>>::new();
        if args.len() != self.params.len() {
            Err(format!("wrong number of arguments: supplied {}, but needed {}", args.len(), self.params.len()))
        } else {
            for (arg, param) in args.iter().zip(self.params.iter()) {
                if let &AST::Symbol(ref s) = param {
                    bindings = bindings.insert(&s.clone(), arg)
                } else {
                    return Err("can only use symbols as params. wtf?".to_owned());
                }
            }

            let env = Environment::new(Some(Box::new(self.parent_env.clone())), bindings);
            eval(ns, &env, &self.code)
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Integer(i32),
    String(String),
    Function(Closure),
    Nil
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Integer(ref i) => write!(f, "{}", i),
            Value::String(ref s) => write!(f, "\"{}\"", &s),
            Value::Function(ref closure) => write!(f, "{}", &closure),
            Value::Nil => write!(f, "nil")
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Namespace {
    pub name: String,
    bindings: HamtRc<String, Box<Value>>
}

impl Namespace {
    pub fn new(name: String) -> Namespace {
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

#[derive(PartialEq, Debug, Clone)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    bindings: HamtRc<String, Box<Value>>
}

impl Environment {
    fn new(parent: Option<Box<Environment>>, bindings: HamtRc<String, Box<Value>>) -> Environment {
        Environment {
            parent: parent,
            bindings: bindings
        }
    }
    pub fn empty() -> Environment {
        Environment {
            parent: None,
            bindings: HamtRc::new()
        }
    }

    pub fn define(&self, symbol: String, value: Box<Value>) -> Environment {
        if let Some(ref p) = self.parent {
            Environment::new(Some(p.clone()), self.bindings.insert(&symbol, &value))
        } else {
            Environment::new(None, self.bindings.insert(&symbol, &value))
        }
    }

    fn lookup(&self, symbol: &str) -> Option<&Box<Value>> {
        self.bindings.get(symbol)
            .or_else(|| {
                if let Some(ref p) = self.parent {
                    p.lookup(symbol)
                } else {
                    None
                }
            })
    }
}

pub type EvalResult = Result<Box<Value>, String>;

pub fn eval(ns: &mut Namespace, env: &Environment, form: &AST) -> EvalResult {
    match *form {
        AST::Integer(ref n) => Ok(Box::new(Value::Integer(n.clone()))),
        AST::String(ref s) => Ok(Box::new(Value::String(s.clone()))),
        AST::Symbol(ref s) => match env.lookup(s) {
            Some(val) => Ok(val.clone()),
            None => {
                match ns.lookup(s) {
                    Some(val) => Ok(val.clone()),
                    None => Err(format!("Symbol `{}` is undefined in namespace {}", s.clone(), ns.name))
                }
            }
        },
        AST::Sexp(ref list) => match list.head() {
            Some(&AST::Symbol(ref s)) if s == "def" => {
                match list.tail().head() {
                    Some(&AST::Symbol(ref name)) => {
                        match list.tailn(2).head() {
                            Some(node) => {
                                eval(ns, env, node).and_then(|v| {
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
            Some(&AST::Symbol(ref s)) if s == "fn" => {
                match list.tail().head() {
                    Some(&AST::Sexp(ref args)) => {
                        Ok(Box::new(Value::Function(Closure::new(&env, args.clone(), list.tailn(2)))))
                    },
                    _ => Err("Usage: (fn (a b) (+ a b))".to_owned())
                }
            },
            Some(&AST::Symbol(ref s)) if s == "+" => {
                match (list.tail().head(), list.tailn(2).head()) {
                    (Some(op1), Some(op2)) =>
                        eval(ns, &env, op1).and_then(|x| {
                            eval(ns, &env, op2).and_then(|y| {
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
                eval(ns, env, &car).and_then(|value| {
                    match value {
                        box Value::Function(fun) => {
                            let mut args = vec![];
                            for node in list.tail().iter() {
                                match eval(ns, env, &node) {
                                    Ok(v) => args.push(v),
                                    e => return e
                                }
                            }
                            fun.apply(ns, &args)
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
                match eval(ns, env, &f) {
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
    use super::{Environment, Namespace, Value, Closure};
    use super::{eval};
    use parser::parse;
    use ast::AST;
    use std::boxed::Box;

    macro_rules! list {
        ( $( $x:expr ),* ) => {
            {
                use cons_list::ConsList;
                use std::iter::FromIterator;
                let mut temp_vec = vec![];
                $(
                    temp_vec.push($x);
                    )*
                    ConsList::from_iter(temp_vec.iter().rev().map(|a| a.clone()))
            }
        };
    }

    macro_rules! assert_evals {
        ( $expected:expr, $ns:expr, $env:expr, $code:expr) => {
            {
                let parsed = parse($code.to_owned()).unwrap();
                assert_eq!(Ok(Box::new($expected)), eval($ns, $env, &parsed));
            }
        }
    }

    fn empty_ns() -> Namespace {
        Namespace::new("user".to_owned())
    }

    fn empty_env() -> Environment {
        Environment::empty()
    }

    fn lookup(ns: Namespace, symbol: &str) -> Option<Box<Value>> {
        match ns.lookup(symbol) {
            Some(v) => Some(v.clone()),
            None => None
        }
    }

    #[test]
    fn eval_integer_test() {
        assert_evals!(Value::Integer(20), &mut empty_ns(), &empty_env(), "20");
    }

    #[test]
    fn eval_string_test() {
        assert_evals!(Value::String("hello".to_owned()), &mut empty_ns(), &empty_env(), "\"hello\"");
    }

    #[test]
    fn eval_symbol_test() {
        let value = Value::String("bar".to_owned());
        let mut ns = empty_ns();
        ns.define("foo".to_owned(), Box::new(value));
        assert_evals!(Value::String("bar".to_owned()), &mut ns, &empty_env(), "foo")
    }

    #[test]
    fn eval_def_test() {
        let mut ns = empty_ns();
        assert_evals!(Value::Nil, &mut ns, &empty_env(), "(def foo \"bar\")");
        assert_eq!(Some(Box::new(Value::String("bar".to_owned()))), lookup(ns, "foo"));
    }

    #[test]
    fn eval_fn_test() {
        let expected_closure = Closure {
            parent_env: empty_env(),
            params: list![AST::Symbol("a".to_owned()), AST::Symbol("b".to_owned())],
            code: AST::Program(list![AST::Sexp(list![AST::Symbol("+".to_owned()),
                                                     AST::Symbol("a".to_owned()),
                                                     AST::Symbol("b".to_owned())])])
        };
        assert_evals!(Value::Function(expected_closure), &mut empty_ns(), &empty_env(), "(fn (a b) (+ a b))");
    }

    #[test]
    fn eval_fn_application_test() {
        assert_evals!(Value::Integer(39), &mut empty_ns(), &empty_env(), "((fn (a b) (+ a b)) 10 29)");
    }

    #[test]
    fn eval_fns_close_upon_their_env_test() {
        assert_evals!(Value::Integer(39), &mut empty_ns(), &empty_env(), "(((fn (a) (fn (b) (+ a b))) 10) 29)");
    }
}
