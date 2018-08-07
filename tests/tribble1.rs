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
enum NodeS {
    NodeS0(Token, NodeA, Token),
    NodeS1(Token, NodeB, Token),
}

#[derive(Debug)]
struct NodeA(Token);

#[derive(Debug)]
struct NodeB(Token);

trait Parser {
    fn peek(&mut self) -> Token;
    fn shift(&mut self, state_fn: fn(&mut Self), reduced_fn: fn(&mut Self, Nonterminal));
    fn goto(
        &mut self,
        nonterminal: Nonterminal,
        state_fn: fn(&mut Self),
        reduced_fn: fn(&mut Self, Nonterminal),
    );
    fn reduce<F: Fn(Vec<Symbol>) -> Nonterminal>(&mut self, length: usize, f: F);
    fn accept(&mut self);
}

struct IterParser<I: Iterator<Item = Token>> {
    result: Option<NodeS>,
    input: Peekable<I>,
    stack: Vec<StackEntry<I>>,
}

enum Symbol {
    Terminal(Token),
    Nonterminal(Nonterminal),
}

impl Symbol {
    fn unwrap_terminal(self) -> Token {
        match self {
            Symbol::Terminal(t) => t,
            _ => panic!("symbol is not a token"),
        }
    }
    fn unwrap_nonterminal(self) -> Nonterminal {
        match self {
            Symbol::Nonterminal(t) => t,
            _ => panic!("symbol is not a nonterminal"),
        }
    }
}

struct StackEntry<I: Iterator<Item = Token>> {
    symbol: Symbol,
    state_fn: fn(&mut IterParser<I>),
    reduced_fn: fn(&mut IterParser<I>, Nonterminal),
}

impl<I: Iterator<Item = Token>> IterParser<I> {
    fn new(input: I) -> IterParser<I> {
        IterParser {
            result: None,
            input: input.peekable(),
            stack: Vec::new(),
        }
    }

    fn step(&mut self) {
        if self.result.is_some() {
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

    fn run(mut self) -> NodeS {
        println!("running parser");
        while self.result.is_none() {
            self.step()
        }
        self.result.unwrap()
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

    fn reduce<F: Fn(Vec<Symbol>) -> Nonterminal>(&mut self, length: usize, f: F) {
        println!("reduce {} symbols", length);
        let at = self.stack.len() - length;
        let args = self.stack.drain(at..).map(|e| e.symbol).collect();
        let reduced_fn = self.stack.last().map(|e| e.reduced_fn).unwrap_or(reduced_0);
        let nonterminal = f(args);
        reduced_fn(self, nonterminal);
    }

    fn accept(&mut self) {
        println!("accept");
        self.result = Some(
            self.stack
                .pop()
                .unwrap()
                .symbol
                .unwrap_nonterminal()
                .unwrap_nt0(),
        );
    }
}

include!("generated/tribble1_parser.rs");

fn reduce_rule_0(a: Token, nta: NodeA, d: Token) -> NodeS {
    println!("reduced S -> a A d");
    NodeS::NodeS0(a, nta, d)
}

fn reduce_rule_1(a: Token, ntb: NodeB, e: Token) -> NodeS {
    println!("reduced S -> a B e");
    NodeS::NodeS1(a, ntb, e)
}

fn reduce_rule_2(b: Token, nta: NodeA, e: Token) -> NodeS {
    println!("reduced S -> b A e");
    NodeS::NodeS0(b, nta, e)
}

fn reduce_rule_3(b: Token, ntb: NodeB, d: Token) -> NodeS {
    println!("reduced S -> b B d");
    NodeS::NodeS1(b, ntb, d)
}

fn reduce_rule_4(tkn: Token) -> NodeA {
    println!("reduced A");
    NodeA(tkn)
}

fn reduce_rule_5(tkn: Token) -> NodeB {
    println!("reduced B");
    NodeB(tkn)
}

#[test]
fn simple1() {
    // Parse the sequence "a c d".
    use Token::*;
    let seq = [A, C, D];
    let res = IterParser::new(seq.into_iter().cloned()).run();
    assert_eq!(format!("{:?}", res), "NodeS0(A, NodeA(C), D)");
}

#[test]
fn simple2() {
    // Parse the sequence "a c e".
    use Token::*;
    let seq = [A, C, E];
    let res = IterParser::new(seq.into_iter().cloned()).run();
    assert_eq!(format!("{:?}", res), "NodeS1(A, NodeB(C), E)");
}

#[test]
fn simple3() {
    // Parse the sequence "b c e".
    use Token::*;
    let seq = [B, C, E];
    let res = IterParser::new(seq.into_iter().cloned()).run();
    assert_eq!(format!("{:?}", res), "NodeS0(B, NodeA(C), E)");
}

#[test]
fn simple4() {
    // Parse the sequence "b c d".
    use Token::*;
    let seq = [B, C, D];
    let res = IterParser::new(seq.into_iter().cloned()).run();
    assert_eq!(format!("{:?}", res), "NodeS1(B, NodeB(C), D)");
}
