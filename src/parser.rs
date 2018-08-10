// Copyright (c) 2018 Fabian Schuiki

//! A parser for grammar descriptions.

#![allow(unused_variables)]

use std::str;
use std::collections::HashMap;

use lexer::{Keyword, Lexer, Token};
use grammar::{Grammar, NonterminalId, Rule, Symbol, TerminalId, END};
use backend::Backend;
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
        pub name: TokenName,
        /// The match pattern for this token.
        pub pattern: Option<String>,
    }

    /// A token name.
    #[allow(missing_docs)]
    #[derive(Debug, PartialEq, Eq, Hash)]
    pub enum TokenName {
        End,
        Name(String),
    }

    /// A rule declaration.
    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct RuleDecl {
        /// The name of the rule.
        pub name: String,
        /// The type of the nonterminal a reduction of this rule produces.
        pub reduce_type: Option<String>,
        /// The different variants of the rule.
        pub variants: Vec<Variant>,
    }

    /// A rule variant.
    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct Variant {
        /// The sequence of symbols of the rule.
        pub seq: Vec<String>,
        /// The reduction function name of the rule.
        pub reduction_function: Option<String>,
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
    name: ast::TokenName,
    _lparen: Option<Token>,
    pattern: Option<Token>,
    _rparen: Option<Token>,
    _semicolon: Option<Token>,
) -> ast::TokenDecl {
    ast::TokenDecl {
        name: name,
        pattern: Some(pattern.unwrap().unwrap_code()),
    }
}

fn reduce_token_name_a(name: Option<Token>) -> ast::TokenName {
    ast::TokenName::Name(name.unwrap().unwrap_ident())
}

fn reduce_token_name_b(_end: Option<Token>) -> ast::TokenName {
    ast::TokenName::End
}

fn reduce_rule_decl(
    name: Option<Token>,
    _lparen: Option<Token>,
    reduce_type: Option<Token>,
    _rparen: Option<Token>,
    _lbrace: Option<Token>,
    list: Vec<ast::Variant>,
    rbrace: Option<Token>,
) -> ast::RuleDecl {
    ast::RuleDecl {
        name: name.unwrap().unwrap_ident(),
        reduce_type: Some(reduce_type.unwrap().unwrap_code()),
        variants: list,
    }
}

fn reduce_rule_list_a(mut list: Vec<ast::Variant>, variant: ast::Variant) -> Vec<ast::Variant> {
    list.push(variant);
    list
}

fn reduce_rule_list_b(variant: ast::Variant) -> Vec<ast::Variant> {
    vec![variant]
}

fn reduce_variant(
    seq: Vec<String>,
    _lparen: Option<Token>,
    reduction_function: Option<Token>,
    _rparen: Option<Token>,
    _semicolon: Option<Token>,
) -> ast::Variant {
    ast::Variant {
        seq: seq,
        reduction_function: Some(reduction_function.unwrap().unwrap_code()),
    }
}

fn reduce_sequence_or_epsilon_a(seq: Vec<String>) -> Vec<String> {
    seq
}

fn reduce_sequence_or_epsilon_b(_epsilon: Option<Token>) -> Vec<String> {
    vec![]
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

/// Parse a string.
///
/// This first tokenizes the string and then parses it.
pub fn parse_str<S: AsRef<str>>(input: S) -> ast::Desc {
    let lex = Lexer::new(input.as_ref().char_indices());
    parse_iter(lex.map(|(_, _, t)| t))
}

/// Convert the grammar description into an actual grammar.
pub fn make_grammar(desc: &ast::Desc) -> (Grammar, Backend) {
    let mut grammar = Grammar::new();
    let mut backend = Backend::new();

    // Declare the terminals and nonterminals.
    let mut token_map: HashMap<String, TerminalId> = HashMap::new();
    let mut rule_map: HashMap<String, NonterminalId> = HashMap::new();
    for d in &desc.tokens {
        let id = match d.name {
            ast::TokenName::Name(ref name) => {
                let id = grammar.add_terminal(name.clone());
                token_map.insert(name.clone(), id);
                id
            }
            ast::TokenName::End => END,
        };
        if let Some(pat) = d.pattern.clone() {
            backend.add_terminal(id, pat);
        }
    }
    for d in &desc.rules {
        let id = grammar.add_nonterminal(d.name.clone());
        rule_map.insert(d.name.clone(), id);
        if let Some(reduce_type) = d.reduce_type.clone() {
            backend.add_nonterminal(id, reduce_type);
        }
    }

    // Create a unified symbol lookup table.
    let mut symbol_map: HashMap<String, Symbol> = HashMap::new();
    for (n, &i) in &token_map {
        symbol_map.insert(n.clone(), i.into());
    }
    for (n, &i) in &rule_map {
        if symbol_map.insert(n.clone(), i.into()).is_some() {
            panic!("rule name `{}` conflicts with token name `{}`", n, n);
        }
    }

    // Add the rules to the grammar.
    for d in &desc.rules {
        let id = rule_map[&d.name];
        for v in &d.variants {
            let seq = v.seq
                .iter()
                .map(|v| match symbol_map.get(v) {
                    Some(&s) => s,
                    None => panic!("unknown token or rule `{}`", v),
                })
                .collect();
            let rule_id = grammar.add_rule(Rule::new(id, seq));
            if let Some(rf) = v.reduction_function.clone() {
                backend.add_reduction_function(rule_id, rf);
            }
        }
    }

    (grammar, backend)
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
