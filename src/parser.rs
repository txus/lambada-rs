use nom::{alphanumeric, multispace, digit};
use nom::{Needed, Err};
use nom::IResult::*;

use std::str;
use std::str::FromStr;
use std::result::Result;

use ast::AST;

named!(program<AST>,
       map!(delimited!(
           opt!(multispace),
           many0!(delimited!(opt!(multispace), form, opt!(multispace))),
           opt!(multispace)),
            AST::Program));

named!(sexp<AST>, map!(delimited!(
    delimited!(opt!(multispace), tag!("("), opt!(multispace)),
    many1!(delimited!(opt!(multispace), form, opt!(multispace))),
    delimited!(opt!(multispace), tag!(")"), opt!(multispace))
        ), AST::Sexp));

named!(number<AST>,
       map!(map_res!(map_res!(digit,
                        str::from_utf8),
               FromStr::from_str),
       AST::Integer));

named!(string<AST>,
       map!(delimited!(tag!("\""), is_not!(b"\""), tag!("\"")),
            |ba| AST::String(String::from_utf8(Vec::from(ba)).unwrap())));

named!(literal<AST>, alt!(number | string));

named!(form<AST>, alt!(literal | symbol | sexp));

named!(symbol<AST>,
       map!(map_res!(map!(alphanumeric, Vec::from),
                     String::from_utf8),
            AST::Symbol));

pub fn parse(str: String) -> Result<AST, String> {
    match program(&str.into_bytes()) {
        Done(_, result) => Ok(result),
        Incomplete(i) => match i {
            Needed::Unknown => Err("incomplete parsing -- don't know how much data we need".to_string()),
            Needed::Size(size) => Err(format!("incomplete parsing -- needed {} more chars", size))
        },
        Error(e) => match e {
            Err::Code(code) => Err(format!("parse error -- code {}", code)),
            Err::Node(code, _) => Err(format!("parse error -- code {}, with more errors", code)),
            Err::Position(code, related_input) => Err(format!("parse error -- code {}, around {:?}", code, str::from_utf8(related_input).unwrap())),
            Err::NodePosition(code, related_input, _) => Err(format!("parse error -- code {}, around {:?}", code, str::from_utf8(related_input).unwrap()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{symbol, sexp, number, string, program, parse};
    use ast::AST;
    use nom::{IResult, Needed, Err};
    use nom::IResult::*;
    use std::str;

    macro_rules! assert_parses {
        ( $expected:expr, $expression:expr ) => {
            {
                let res: IResult<&[u8], AST> = $expression;
                match res {
                    Done(_, result) => assert_eq!($expected, result),
                    Incomplete(i) => match i {
                        Needed::Unknown => panic!("incomplete parsing -- don't know how much data we need"),
                        Needed::Size(size) => panic!("incomplete parsing -- needed {} more chars", size)
                    },
                    Error(e) => match e {
                        Err::Code(code) => panic!("parse error -- code {}", code),
                        Err::Node(code, _) => panic!("parse error -- code {}, with more errors", code),
                        Err::Position(code, related_input) => panic!("parse error -- code {}, around {:?}", code, str::from_utf8(related_input).unwrap()),
                        Err::NodePosition(code, related_input, _) => panic!("parse error -- code {}, around {:?}", code, str::from_utf8(related_input).unwrap())
                    }
                }
            }
        }
    }

    #[test]
    fn number_test() {
        assert_parses!(AST::Integer(23), number(&b"23"[..]));
    }

    #[test]
    fn string_test() {
        assert_parses!(AST::String("hello".to_string()), string(&b"\"hello\""[..]));
        assert_parses!(AST::String("hello world".to_string()), string(&b"\"hello world\""[..]));
    }

    #[test]
    fn symbol_alphanumeric_test() {
        assert_parses!(AST::Symbol("hello".to_string()), symbol(&b"hello"[..]));
    }

    #[test]
    fn sexp_test() {
        assert_parses!(AST::Sexp(vec![AST::Symbol("foo".to_string()),
                                      AST::Symbol("bar".to_string()),
                                      AST::Symbol("baz".to_string())]),
                       sexp(&b" (     foo bar    baz  )  "[..]));

        assert_parses!(AST::Sexp(vec![AST::Symbol("foo".to_string()),
                                      AST::Sexp(vec![AST::Symbol("bar".to_string())]),
                                      AST::Symbol("baz".to_string())]),
                       sexp(&b" (     foo ( bar)    baz  )  "[..]));
    }

    #[test]
    fn program_test() {
        assert_parses!(
            AST::Program(vec![
                AST::Sexp(vec![
                    AST::Symbol("foo".to_string()),
                    AST::Symbol("bar".to_string()),
                    AST::Symbol("baz".to_string())]),
                AST::Symbol("bar".to_string()),
                AST::Integer(2),
                AST::Sexp(vec![AST::Sexp(vec![AST::Symbol("quux".to_string())])])]),
            program(&b" (foo bar baz)\n bar \n2 \n ((quux))"[..]));
    }

    #[test]
    fn parse_test() {
        assert_eq!(Ok(AST::Program(vec![AST::Symbol("foo".to_string())])),
                   parse("foo".to_string()));
    }
}
