// Copyright (c) 2018 Fabian Schuiki
#![allow(unused_variables)]
#![allow(dead_code)]

extern crate perplex_runtime;

use std::iter::Peekable;

#[derive(Debug, Copy, Clone)]
enum Token {
    A,
    B,
    C,
    D,
    E,
    Eof,
}

#[derive(Debug)]
enum Ast {
    S,
    A,
    B,
}

type Nonterminal = Ast;

trait Parser {
    fn peek(&mut self) -> Token;
    fn shift(&mut self, state_fn: fn(&mut Self), reduced_fn: fn(&mut Self, Nonterminal));
    fn goto(
        &mut self,
        nonterminal: Nonterminal,
        state_fn: fn(&mut Self),
        reduced_fn: fn(&mut Self, Nonterminal),
    );
    fn reduce(&mut self, nonterminal: Nonterminal, length: usize);
    fn accept(&mut self);
}

struct IterParser<I: Iterator<Item = Token>> {
    done: bool,
    input: Peekable<I>,
    stack: Vec<StackEntry<I>>,
}

enum Symbol {
    Terminal(Token),
    Nonterminal(Ast),
}

struct StackEntry<I: Iterator<Item = Token>> {
    symbol: Symbol,
    state_fn: fn(&mut IterParser<I>),
    reduced_fn: fn(&mut IterParser<I>, Nonterminal),
}

impl<I: Iterator<Item = Token>> IterParser<I> {
    fn new(input: I) -> IterParser<I> {
        IterParser {
            done: false,
            input: input.peekable(),
            stack: Vec::new(),
        }
    }

    fn step(&mut self) {
        if self.done {
            return;
        }
        let state_fn = self.stack.last().map(|e| e.state_fn).unwrap_or(state_0);
        state_fn(self);
        print!("stack:");
        for e in &self.stack {
            match e.symbol {
                Symbol::Terminal(ref t) => print!(" {:?}", t),
                Symbol::Nonterminal(ref t) => print!(" {:?}", t),
            }
        }
        println!("");
    }

    fn run(&mut self) {
        println!("running parser");
        while !self.done {
            self.step()
        }
    }
}

impl<I: Iterator<Item = Token>> Parser for IterParser<I> {
    fn peek(&mut self) -> Token {
        let token = self.input.peek().map(|&t| t).unwrap_or(Token::Eof);
        println!("peeking {:?}", token);
        token
    }

    fn shift(&mut self, state_fn: fn(&mut Self), reduced_fn: fn(&mut Self, Nonterminal)) {
        let token = self.input.next().unwrap_or(Token::Eof);
        println!("shifting {:?}", token);
        self.stack.push(StackEntry {
            symbol: Symbol::Terminal(token),
            state_fn: state_fn,
            reduced_fn: reduced_fn,
        });
    }

    fn goto(
        &mut self,
        nonterminal: Nonterminal,
        state_fn: fn(&mut Self),
        reduced_fn: fn(&mut Self, Nonterminal),
    ) {
        println!("goto called");
        self.stack.push(StackEntry {
            symbol: Symbol::Nonterminal(nonterminal),
            state_fn: state_fn,
            reduced_fn: reduced_fn,
        });
    }

    fn reduce(&mut self, nonterminal: Nonterminal, length: usize) {
        println!("reduce {:?} ({} symbols)", nonterminal, length);
        let at = self.stack.len() - length;
        self.stack.split_off(at);
        let reduced_fn = self.stack.last().map(|e| e.reduced_fn).unwrap_or(reduced_0);
        reduced_fn(self, nonterminal);
    }

    fn accept(&mut self) {
        println!("accept");
        self.stack.pop().unwrap().symbol;
        self.done = true;
    }
}

include!("generated/tribble1_parser.rs");

#[test]
fn simple() {
    // Parse the sequence "a c d".
    use Token::*;
    let seq = [A, C, D, Eof];
    println!("seq: {:?}", seq);
    IterParser::new(seq.into_iter().cloned()).run();
}
