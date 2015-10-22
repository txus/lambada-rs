#![feature(test)]
#![feature(plugin)]
#![feature(box_patterns)]
#![allow(dead_code)]
#![allow(len_zero)]

#![plugin(clippy)]

extern crate test;

#[macro_use]
extern crate nom;

extern crate hamt;
extern crate cons_list;
extern crate rustyline;

mod ast;
mod parser;
mod eval;
mod repl;

use repl::repl;

fn main() {
    repl();
}
