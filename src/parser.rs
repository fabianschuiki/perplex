// Copyright (c) 2018 Fabian Schuiki

//! A parser for grammar descriptions.

#![allow(unused_variables)]

use std::str;
use std::collections::HashMap;

use lexer::{Keyword, Lexer, Token};
use grammar::{Grammar, NonterminalId, Rule, RuleId, Symbol, TerminalId, END};
use backend::Backend;
use perplex_runtime::Parser;

type Terminal = Option<Token>;

include!("parser_states.rs");

/// The abstract syntax tree of a grammar description.
pub mod ast {
    use std::iter::{once, repeat};
    use std::fmt;

    /// The root node of a grammar description.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Desc {
        /// The token declarations.
        pub tokens: Vec<TokenDecl>,
        /// The rule declarations.
        pub rules: Vec<RuleDecl>,
    }

    /// An item in the grammar description.
    #[allow(missing_docs)]
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Item {
        TokenDecl(TokenDecl),
        RuleDecl(RuleDecl),
    }

    /// A token declaration.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct TokenDecl {
        /// The name of the token.
        pub name: TokenName,
        /// The match pattern for this token.
        pub pattern: Option<String>,
    }

    /// A token name.
    #[allow(missing_docs)]
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum TokenName {
        End,
        Name(String),
    }

    /// A rule declaration.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct RuleDecl {
        /// The name of the rule.
        pub name: String,
        /// The type of the nonterminal a reduction of this rule produces.
        pub reduce_type: Option<String>,
        /// The different variants of the rule.
        pub variants: Vec<Variant>,
    }

    /// A rule variant.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Variant {
        /// The sequence of symbols of the rule.
        pub seq: Vec<Symbol>,
        /// The reduction function name of the rule.
        pub reduction_function: Option<String>,
    }

    /// A symbol in a sequence.
    #[allow(missing_docs)]
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Symbol {
        Token(String),
        Group(Vec<Symbol>),
        Optional(Box<Symbol>),
        Repeat(RepSequence, RepMode),
    }

    impl fmt::Display for Symbol {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                Symbol::Token(ref t) => write!(f, "{}", t),
                Symbol::Group(ref g) => {
                    write!(f, "(")?;
                    fmt_symbol_seq(f, g)?;
                    write!(f, ")")?;
                    Ok(())
                }
                Symbol::Optional(ref s) => write!(f, "{}?", s),
                Symbol::Repeat(ref repseq, mode) => {
                    write!(f, "(")?;
                    if repseq.sep.is_empty() {
                        fmt_symbol_seq(f, &repseq.seq)?;
                    } else {
                        fmt_symbol_seq(f, &repseq.seq)?;
                        write!(f, "; ")?;
                        fmt_symbol_seq(f, &repseq.sep)?;
                    }
                    write!(f, ")")?;
                    match mode {
                        RepMode::ZeroOrMore => write!(f, "*")?,
                        RepMode::OneOrMore => write!(f, "+")?,
                    }
                    Ok(())
                }
            }
        }
    }

    fn fmt_symbol_seq<'a, I: IntoIterator<Item = &'a Symbol>>(
        f: &mut fmt::Formatter,
        symbols: I,
    ) -> fmt::Result {
        for (sep, tkn) in once("").chain(repeat(" ")).zip(symbols.into_iter()) {
            write!(f, "{}{}", sep, tkn)?;
        }
        Ok(())
    }

    /// A sequence to be repeated with interspersed separators.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct RepSequence {
        /// The sequence of tokens to be repeated.
        pub seq: Vec<Symbol>,
        /// The separators in between the repeated sequence.
        pub sep: Vec<Symbol>,
    }

    /// A mode of repetition.
    #[allow(missing_docs)]
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub enum RepMode {
        ZeroOrMore,
        OneOrMore,
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

fn reduce_token_decl_a(
    _keyword: Option<Token>,
    name: ast::TokenName,
    _rarrow: Option<Token>,
    pattern: Option<Token>,
    _semicolon: Option<Token>,
) -> ast::TokenDecl {
    ast::TokenDecl {
        name: name,
        pattern: Some(pattern.unwrap().unwrap_code()),
    }
}

fn reduce_token_decl_b(
    _keyword: Option<Token>,
    name: ast::TokenName,
    _semicolon: Option<Token>,
) -> ast::TokenDecl {
    ast::TokenDecl {
        name: name,
        pattern: None,
    }
}

fn reduce_token_name_a(name: Option<Token>) -> ast::TokenName {
    ast::TokenName::Name(name.unwrap().unwrap_ident())
}

fn reduce_token_name_b(_end: Option<Token>) -> ast::TokenName {
    ast::TokenName::End
}

fn reduce_rule_decl_a(
    name: Option<Token>,
    _rarrow: Option<Token>,
    reduce_type: Option<Token>,
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

fn reduce_rule_decl_b(
    name: Option<Token>,
    _lbrace: Option<Token>,
    list: Vec<ast::Variant>,
    rbrace: Option<Token>,
) -> ast::RuleDecl {
    ast::RuleDecl {
        name: name.unwrap().unwrap_ident(),
        reduce_type: None,
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

fn reduce_variant_a(
    seq: Vec<ast::Symbol>,
    _rarrow: Option<Token>,
    reduction_function: Option<Token>,
    _semicolon: Option<Token>,
) -> ast::Variant {
    ast::Variant {
        seq: seq,
        reduction_function: Some(reduction_function.unwrap().unwrap_code()),
    }
}

fn reduce_variant_b(seq: Vec<ast::Symbol>, _semicolon: Option<Token>) -> ast::Variant {
    ast::Variant {
        seq: seq,
        reduction_function: None,
    }
}

fn reduce_sequence_or_epsilon_a(seq: Vec<ast::Symbol>) -> Vec<ast::Symbol> {
    seq
}

fn reduce_sequence_or_epsilon_b(_epsilon: Option<Token>) -> Vec<ast::Symbol> {
    vec![]
}

fn reduce_sequence_a(mut seq: Vec<ast::Symbol>, symbol: ast::Symbol) -> Vec<ast::Symbol> {
    seq.push(symbol);
    seq
}

fn reduce_sequence_b(symbol: ast::Symbol) -> Vec<ast::Symbol> {
    vec![symbol]
}

fn reduce_symbol_a(primary: ast::Symbol) -> ast::Symbol {
    primary
}

fn reduce_symbol_b(primary: ast::Symbol, _question: Option<Token>) -> ast::Symbol {
    ast::Symbol::Optional(Box::new(primary))
}

fn reduce_symbol_c(repseq: ast::RepSequence, _star: Option<Token>) -> ast::Symbol {
    ast::Symbol::Repeat(repseq, ast::RepMode::ZeroOrMore)
}

fn reduce_symbol_d(repseq: ast::RepSequence, _plus: Option<Token>) -> ast::Symbol {
    ast::Symbol::Repeat(repseq, ast::RepMode::OneOrMore)
}

fn reduce_repetition_sequence_a(primary: ast::Symbol) -> ast::RepSequence {
    ast::RepSequence {
        seq: vec![primary],
        sep: vec![],
    }
}

fn reduce_repetition_sequence_b(
    _lparen: Option<Token>,
    seq: Vec<ast::Symbol>,
    _semicolon: Option<Token>,
    sep: Vec<ast::Symbol>,
    _rparen: Option<Token>,
) -> ast::RepSequence {
    ast::RepSequence { seq: seq, sep: sep }
}

fn reduce_primary_symbol_a(ident: Option<Token>) -> ast::Symbol {
    ast::Symbol::Token(ident.unwrap().unwrap_ident())
}

fn reduce_primary_symbol_b(
    _lparen: Option<Token>,
    seq: Vec<ast::Symbol>,
    _rparen: Option<Token>,
) -> ast::Symbol {
    ast::Symbol::Group(seq)
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
            let rule_id = insert_sequence(id, &v.seq, &symbol_map, &mut grammar);
            if let Some(rf) = v.reduction_function.clone() {
                backend.add_reduction_function(rule_id, rf);
            }
        }
    }

    (grammar, backend)
}

fn insert_sequence<'a, I: IntoIterator<Item = &'a ast::Symbol>>(
    id: NonterminalId,
    seq: I,
    symbol_map: &HashMap<String, Symbol>,
    grammar: &mut Grammar,
) -> RuleId {
    let mut flattened = Vec::new();
    flatten_sequence(id, seq, symbol_map, grammar, &mut flattened);
    let rule_id = grammar.add_rule(Rule::new(id, flattened));
    rule_id
}

fn flatten_sequence<'a, I: IntoIterator<Item = &'a ast::Symbol>>(
    id: NonterminalId,
    seq: I,
    symbol_map: &HashMap<String, Symbol>,
    grammar: &mut Grammar,
    into: &mut Vec<Symbol>,
) {
    for symbol in seq {
        match *symbol {
            ast::Symbol::Token(ref v) => match symbol_map.get(v) {
                Some(&s) => into.push(s),
                None => panic!("unknown token or rule `{}`", v),
            },
            ast::Symbol::Group(ref g) => flatten_sequence(id, g, symbol_map, grammar, into),
            ast::Symbol::Optional(ref s) => {
                let subid = pick_subrule_name(id, grammar);
                into.push(subid.into());
                let mut symbols = Vec::new();
                flatten_sequence(subid, Some(&**s), symbol_map, grammar, &mut symbols);
                grammar.add_rule(Rule::new(subid, vec![]));
                grammar.add_rule(Rule::new(subid, symbols));
            }
            ast::Symbol::Repeat(ref repseq, mode) => {
                let subid = pick_subrule_name(id, grammar);
                into.push(subid.into());
                let subid = match mode {
                    ast::RepMode::ZeroOrMore => {
                        let subid2 = pick_subrule_name(subid, grammar);
                        grammar.add_rule(Rule::new(subid, vec![]));
                        grammar.add_rule(Rule::new(subid, vec![subid2.into()]));
                        subid2
                    }
                    ast::RepMode::OneOrMore => subid,
                };
                let mut symbols_seq = Vec::new();
                let mut symbols_sep = Vec::new();
                flatten_sequence(subid, &repseq.seq, symbol_map, grammar, &mut symbols_seq);
                flatten_sequence(subid, &repseq.sep, symbol_map, grammar, &mut symbols_sep);
                grammar.add_rule(Rule::new(subid, symbols_seq.clone()));
                let mut symbols = vec![Symbol::from(subid)];
                symbols.extend(symbols_sep);
                symbols.extend(symbols_seq);
                grammar.add_rule(Rule::new(subid, symbols));
            }
        }
    }
}

fn pick_subrule_name(id: NonterminalId, grammar: &mut Grammar) -> NonterminalId {
    let mut name = String::from(grammar.nonterminal_name(id));
    while grammar.get_nonterminal(&name).is_some() {
        name.push('\'');
    }
    grammar.add_nonterminal(name)
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
                        name: ast::TokenName::Name("hello".into()),
                        pattern: None,
                    },
                ],
                rules: vec![],
            }
        );
    }
}
