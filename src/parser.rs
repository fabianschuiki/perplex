// Copyright (c) 2018 Fabian Schuiki

//! A parser for grammar descriptions.

#![allow(unused_variables)]

use lexer::{Keyword, Token};
use perplex_runtime::Parser;

type Terminal = Option<Token>;

include!("parser_states.rs");

/// The abstract syntax tree of a grammar description.
pub mod ast {
    /// The root node of a grammar description.
    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct Desc {
        /// The token declarations.
        pub tokens: Vec<TokenDecl>,
        /// The rule declarations.
        pub rules: Vec<RuleDecl>,
    }

    /// An item in the grammar description.
    #[allow(missing_docs)]
    #[derive(Debug, PartialEq, Eq, Hash)]
    pub enum Item {
        TokenDecl(TokenDecl),
        RuleDecl(RuleDecl),
    }

    /// A token declaration.
    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct TokenDecl {
        /// The name of the token.
        pub name: String,
    }

    /// A rule declaration.
    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct RuleDecl {
        /// The name of the rule.
        pub name: String,
        /// The different variants of the rule.
        pub variants: Vec<Vec<String>>,
    }
}

fn reduce_desc_a(mut desc: ast::Desc, item: ast::Item) -> ast::Desc {
    match item {
        ast::Item::TokenDecl(d) => desc.tokens.push(d),
        ast::Item::RuleDecl(d) => desc.rules.push(d),
    }
    desc
}

fn reduce_desc_b(item: ast::Item) -> ast::Desc {
    match item {
        ast::Item::TokenDecl(d) => ast::Desc {
            tokens: vec![d],
            rules: vec![],
        },
        ast::Item::RuleDecl(d) => ast::Desc {
            tokens: vec![],
            rules: vec![d],
        },
    }
}

fn reduce_desc_c(desc: ast::Desc, _semicolon: Option<Token>) -> ast::Desc {
    desc
}

fn reduce_desc_d(_semicolon: Option<Token>) -> ast::Desc {
    ast::Desc {
        tokens: vec![],
        rules: vec![],
    }
}

fn reduce_item_a(token_decl: ast::TokenDecl) -> ast::Item {
    ast::Item::TokenDecl(token_decl)
}

fn reduce_item_b(rule_decl: ast::RuleDecl) -> ast::Item {
    ast::Item::RuleDecl(rule_decl)
}

fn reduce_token_decl(
    _keyword: Option<Token>,
    name: Option<Token>,
    _semicolon: Option<Token>,
) -> ast::TokenDecl {
    ast::TokenDecl {
        name: name.unwrap().unwrap_ident(),
    }
}

fn reduce_rule_decl(
    name: Option<Token>,
    _colon: Option<Token>,
    list: Vec<Vec<String>>,
    _semicolon: Option<Token>,
) -> ast::RuleDecl {
    ast::RuleDecl {
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

fn reduce_rule_list_c(
    mut list: Vec<Vec<String>>,
    _pipe: Option<Token>,
    _epsilon: Option<Token>,
) -> Vec<Vec<String>> {
    list.push(vec![]);
    list
}

fn reduce_rule_list_d(_epsilon: Option<Token>) -> Vec<Vec<String>> {
    vec![vec![]]
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
pub fn parse_iter<I: Iterator<Item = Token>>(input: I) -> ast::Desc {
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
        assert_eq!(
            res,
            ast::Desc {
                tokens: vec![
                    ast::TokenDecl {
                        name: "hello".into(),
                    },
                ],
                rules: vec![],
            }
        );
    }
}
