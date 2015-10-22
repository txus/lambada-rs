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

mod ast;
mod parser;
mod eval;

fn main() {
    println!("Hello, world!");
}
