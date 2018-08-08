// Copyright (c) 2018 Fabian Schuiki

//! A parser for grammar descriptions.

#![allow(unused_variables)]

use lexer::{Keyword, Token};
use perplex_runtime::Parser;

type Terminal = Option<Token>;

include!("parser_states.rs");

fn reduce_desc_a(desc: (), item: ()) {}
fn reduce_desc_b(item: ()) {}
fn reduce_desc_c(desc: (), _semicolon: Option<Token>) {}
fn reduce_desc_d(_semicolon: Option<Token>) {}

fn reduce_item_a(token_decl: ()) {}
fn reduce_item_b(rule_decl: ()) {}

fn reduce_token_decl(_keyword: Option<Token>, name: Option<Token>, _semicolon: Option<Token>) {}

fn reduce_rule_decl(
    name: Option<Token>,
    _colon: Option<Token>,
    list: (),
    _semicolon: Option<Token>,
) {
}

fn reduce_rule_list_a(list: (), _pipe: Option<Token>, symbol: Option<Token>) {}
fn reduce_rule_list_b(symbol: Option<Token>) {}

struct StateSpace;

impl ::perplex_runtime::StateSpace for StateSpace {
    type Terminal = Option<Token>;
    type Nonterminal = Nonterminal;
    type Root = ();

    fn root_state_fn<P: Parser<Terminal = Option<Token>, Nonterminal = Nonterminal>>() -> fn(&mut P)
    {
        state_0
    }

    fn root_goto_fn<P: Parser<Terminal = Option<Token>, Nonterminal = Nonterminal>>(
) -> fn(&mut P, Self::Nonterminal) {
        reduced_0
    }
}

type CoreParser<I> = ::perplex_runtime::ParserMachine<I, StateSpace>;

/// Parse a sequence of tokens given by an iterator.
pub fn parse_iter<I: Iterator<Item = Token>>(input: I) -> () {
    CoreParser::from_iter(input).run().unwrap_nt0()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple1() {
        let seq = [
            Token::Keyword(Keyword::Token),
            Token::Ident("hello".into()),
            Token::Semicolon,
        ];
        let res = parse_iter(seq.into_iter().cloned());
        assert_eq!(format!("{:?}", res), "()");
    }
}
