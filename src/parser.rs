// Copyright (c) 2018 Fabian Schuiki

//! A parser for grammar descriptions.

#![allow(unused_variables)]

use lexer::{Keyword, Token};
use perplex_runtime::Parser;

type Terminal = Option<Token>;

include!("parser_states.rs");

#[derive(Debug)]
struct Desc {
    tokens: Vec<TokenDecl>,
    rules: Vec<RuleDecl>,
}

#[derive(Debug)]
enum Item {
    TokenDecl(TokenDecl),
    RuleDecl(RuleDecl),
}

#[derive(Debug)]
struct TokenDecl {
    name: String,
}

#[derive(Debug)]
struct RuleDecl {
    name: String,
    variants: Vec<Vec<String>>,
}

fn reduce_desc_a(mut desc: Desc, item: Item) -> Desc {
    match item {
        Item::TokenDecl(d) => desc.tokens.push(d),
        Item::RuleDecl(d) => desc.rules.push(d),
    }
    desc
}

fn reduce_desc_b(item: Item) -> Desc {
    match item {
        Item::TokenDecl(d) => Desc {
            tokens: vec![d],
            rules: vec![],
        },
        Item::RuleDecl(d) => Desc {
            tokens: vec![],
            rules: vec![d],
        },
    }
}

fn reduce_desc_c(desc: Desc, _semicolon: Option<Token>) -> Desc {
    desc
}

fn reduce_desc_d(_semicolon: Option<Token>) -> Desc {
    Desc {
        tokens: vec![],
        rules: vec![],
    }
}

fn reduce_item_a(token_decl: TokenDecl) -> Item {
    Item::TokenDecl(token_decl)
}

fn reduce_item_b(rule_decl: RuleDecl) -> Item {
    Item::RuleDecl(rule_decl)
}

fn reduce_token_decl(
    _keyword: Option<Token>,
    name: Option<Token>,
    _semicolon: Option<Token>,
) -> TokenDecl {
    TokenDecl {
        name: name.unwrap().unwrap_ident(),
    }
}

fn reduce_rule_decl(
    name: Option<Token>,
    _colon: Option<Token>,
    list: Vec<Vec<String>>,
    _semicolon: Option<Token>,
) -> RuleDecl {
    RuleDecl {
        name: name.unwrap().unwrap_ident(),
        variants: list,
    }
}

fn reduce_rule_list_a(
    mut list: Vec<Vec<String>>,
    _pipe: Option<Token>,
    seq: Vec<String>,
) -> Vec<Vec<String>> {
    list.push(seq);
    list
}

fn reduce_rule_list_b(seq: Vec<String>) -> Vec<Vec<String>> {
    vec![seq]
}

fn reduce_sequence_a(mut seq: Vec<String>, symbol: Option<Token>) -> Vec<String> {
    seq.push(symbol.unwrap().unwrap_ident());
    seq
}

fn reduce_sequence_b(symbol: Option<Token>) -> Vec<String> {
    vec![symbol.unwrap().unwrap_ident()]
}

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
    let desc = CoreParser::from_iter(input).run().unwrap_nt0();
    println!("parsed {:#?}", desc);
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
