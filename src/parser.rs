use nom::{multispace, digit};
use nom::{Needed, Err};
use nom::IResult::*;

use cons_list::ConsList;
use std::iter::FromIterator;

use std::str;
use std::str::FromStr;
use std::result::Result;

use ast::AST;

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

named!(program<AST>,
       map!(
           map!(delimited!(
               opt!(multispace),
               many0!(delimited!(opt!(multispace), form, opt!(multispace))),
               opt!(multispace)),
                |v: Vec<AST>| ConsList::from_iter(v.iter().rev().map(|x| x.clone()))), AST::Program));

named!(sexp<AST>, map!(map!(delimited!(
    delimited!(opt!(multispace), tag!("("), opt!(multispace)),
    many1!(delimited!(opt!(multispace), form, opt!(multispace))),
    delimited!(opt!(multispace), tag!(")"), opt!(multispace))
        ), |v: Vec<AST>| ConsList::from_iter(v.iter().rev().map(|x| x.clone()))), AST::Sexp));

named!(number<AST>,
       map!(map_res!(map_res!(digit,
                        str::from_utf8),
               FromStr::from_str),
       AST::Integer));

named!(string<AST>,
       map!(delimited!(tag!("\""), is_not!(b"\""), tag!("\"")),
            |ba| AST::String(String::from_utf8(Vec::from(ba)).unwrap())));

named!(literal<AST>, alt!(number | string));

named!(quoted<AST>, map!(preceded!(char!('\''), form), |f: AST| AST::Sexp(list![AST::Symbol("quote".to_owned()), f])));
named!(unquoted<AST>, map!(preceded!(char!('~'), form), |f: AST| AST::Sexp(list![AST::Symbol("unquote".to_owned()), f])));
named!(unquoted_splicing<AST>, map!(preceded!(tag!("~@"), form), |f: AST| AST::Sexp(list![AST::Symbol("unquote-splicing".to_owned()), f])));

named!(form<AST>, alt!(literal | quoted | unquoted_splicing | unquoted | symbol | sexp));

named!(symbol<AST>,
       map!(map_res!(map!(is_not!("\" ()\r\n"), Vec::from),
                     String::from_utf8),
            AST::Symbol));

pub fn parse(str: String) -> Result<AST, String> {
    match program(&str.into_bytes()) {
        Done(_, result) => Ok(result),
        Incomplete(i) => match i {
            Needed::Unknown =>
                Err("incomplete parsing -- don't know how much data we need".to_owned()),
            Needed::Size(size) => Err(format!("incomplete parsing -- needed {} more chars", size)),
        },
        Error(e) => match e {
            Err::Code(code) => Err(format!("parse error -- code {:?}", code)),
            Err::Node(code, _) => Err(format!("parse error -- code {:?}, with more errors", code)),
            Err::Position(code, related_input) =>
                Err(format!("parse error -- code {:?}, around {:?}",
                            code,
                            str::from_utf8(related_input).unwrap())),
            Err::NodePosition(code, related_input, _) =>
                Err(format!("parse error -- code {:?}, around {:?}",
                            code,
                            str::from_utf8(related_input).unwrap())),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::{symbol, sexp, number, string, program, parse, quoted, unquoted, unquoted_splicing};
    use ast::AST;
    use nom::{IResult, Needed, Err};
    use nom::IResult::*;
    use std::str;

    use test::Bencher;

    macro_rules! assert_parses {
        ( $expected:expr, $expression:expr ) => {
            {
                let res: IResult<&[u8], AST> = $expression;
                match res {
                    Done(_, result) => assert_eq!($expected, result),
                    Incomplete(i) => match i {
                        Needed::Unknown =>
                            panic!("incomplete parsing -- don't know how much data we need"),
                        Needed::Size(size) =>
                            panic!("incomplete parsing -- needed {} more chars", size)
                    },
                    Error(e) => match e {
                        Err::Code(code) =>
                            panic!("parse error -- code {:?}", code),
                        Err::Node(code, _) =>
                            panic!("parse error -- code {:?}, with more errors", code),
                        Err::Position(code, related_input) =>
                            panic!("parse error -- code {:?}, around {:?}",
                                   code,
                                   str::from_utf8(related_input).unwrap()),
                        Err::NodePosition(code, related_input, _) =>
                            panic!("parse error -- code {:?}, around {:?}",
                                   code,
                                   str::from_utf8(related_input).unwrap())
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
        assert_parses!(AST::String("hello".to_owned()), string(&b"\"hello\""[..]));
        assert_parses!(AST::String("hello world".to_owned()),
                       string(&b"\"hello world\""[..]));
    }

    #[test]
    fn symbol_test() {
        assert_parses!(AST::Symbol("hello".to_owned()), symbol(&b"hello"[..]));
    }

    #[test]
    fn quoted_test() {
        assert_parses!(AST::Sexp(list![AST::Symbol("quote".to_owned()),
                                       AST::Symbol("hello".to_owned())]), quoted(&b"'hello"[..]));
    }

    #[test]
    fn unquoted_test() {
        assert_parses!(AST::Sexp(list![AST::Symbol("unquote".to_owned()),
                                       AST::Symbol("hello".to_owned())]), unquoted(&b"~hello"[..]));
    }

    #[test]
    fn unquoted_splicing_test() {
        assert_parses!(AST::Sexp(list![AST::Symbol("unquote-splicing".to_owned()),
                                       AST::Symbol("hello".to_owned())]), unquoted_splicing(&b"~@hello"[..]));
    }

    #[test]
    fn sexp_test() {
        assert_parses!(AST::Sexp(list![AST::Symbol("foo".to_owned()),
                                       AST::Symbol("bar".to_owned()),
                                       AST::Symbol("baz".to_owned())]),
                       sexp(&b" (     foo bar    baz  )  "[..]));

        assert_parses!(AST::Sexp(list![AST::Symbol("foo".to_owned()),
                                       AST::Sexp(list![AST::Symbol("bar".to_owned())]),
                                       AST::Symbol("baz".to_owned())]),
                       sexp(&b" (     foo ( bar)    baz  )  "[..]));

        assert_parses!(AST::Sexp(list![AST::Symbol("def".to_owned()),
                                       AST::Symbol("foo".to_owned()),
                                       AST::String("bar".to_owned())]),
                       sexp(&b"(def foo \"bar\")"[..]));
    }

    #[test]
    fn program_test() {
        assert_parses!(AST::Program(list![
                AST::Sexp(list![
                    AST::Symbol("foo".to_owned()),
                    AST::Symbol("bar".to_owned()),
                    AST::Symbol("baz".to_owned())]),
                AST::Symbol("bar".to_owned()),
                AST::Integer(2),
                AST::Sexp(list![AST::Sexp(list![AST::Symbol("quux".to_owned())])])]),
                       program(&b" (foo bar baz)\n bar \n2 \n ((quux))"[..]));

        assert_parses!(AST::Program(list![AST::Sexp(list![AST::Symbol("+".to_owned()),
                                                          AST::Symbol("a".to_owned()),
                                                          AST::Symbol("b".to_owned())])]),
                       program(&b"(+ a b)"[..]));
    }

    #[test]
    fn parse_test() {
        assert_eq!(Ok(AST::Program(list![AST::Symbol("foo".to_owned())])),
                   parse("foo".to_owned()));
    }

    #[bench]
    fn bench_parse_symbol(b: &mut Bencher) {
        b.iter(|| symbol(b"foo"))
    }

    #[bench]
    fn bench_parse_number(b: &mut Bencher) {
        b.iter(|| number(b"12384"))
    }

    #[bench]
    fn bench_parse_sexp(b: &mut Bencher) {
        b.iter(|| sexp(b"(foo bar (baz))"))
    }

    #[bench]
    fn bench_parse_program(b: &mut Bencher) {
        b.iter(|| program(b"(bar baz)\n(foo bar (baz))"))
    }
}
