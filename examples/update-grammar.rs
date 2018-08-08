// Copyright (c) 2018 Fabian Schuiki
extern crate perplex;

use std::path::PathBuf;
use std::fs::File;

use perplex::grammar::{self, Grammar, Rule};
use perplex::item_set::ItemSets;
use perplex::machine::StateMachine;
use perplex::backend::{generate_parser, Backend};

fn main() {
    // Build the grammar for grammars (how meta!).
    let mut g = Grammar::new();

    let nt_desc = g.add_nonterminal("desc");
    let nt_item = g.add_nonterminal("item");
    let nt_token_decl = g.add_nonterminal("token_decl");
    let nt_rule_decl = g.add_nonterminal("rule_decl");
    let nt_rule_list = g.add_nonterminal("rule_list");

    let t_ident = g.add_terminal("IDENT");
    let t_kw_token = g.add_terminal("'token'");
    let t_period = g.add_terminal("'.'");
    let t_colon = g.add_terminal("':'");
    let t_comma = g.add_terminal("','");
    let t_semicolon = g.add_terminal("';'");
    let t_pipe = g.add_terminal("'|'");

    // desc : desc item | item | desc ';' | ';' ;
    g.add_rule(Rule::new(nt_desc, vec![nt_desc.into(), nt_item.into()]));
    g.add_rule(Rule::new(nt_desc, vec![nt_item.into()]));
    g.add_rule(Rule::new(nt_desc, vec![nt_desc.into(), t_semicolon.into()]));
    g.add_rule(Rule::new(nt_desc, vec![t_semicolon.into()]));

    // item : token_decl | rule_decl ;
    g.add_rule(Rule::new(nt_item, vec![nt_token_decl.into()]));
    g.add_rule(Rule::new(nt_item, vec![nt_rule_decl.into()]));

    // token_decl : 'token' IDENT ';' ;
    g.add_rule(Rule::new(
        nt_token_decl,
        vec![t_kw_token.into(), t_ident.into(), t_semicolon.into()],
    ));

    // rule_decl : IDENT ':' rule_list ';' ;
    g.add_rule(Rule::new(
        nt_rule_decl,
        vec![
            t_ident.into(),
            t_colon.into(),
            nt_rule_list.into(),
            t_semicolon.into(),
        ],
    ));

    // rule_list : rule_list '|' IDENT | IDENT ;
    g.add_rule(Rule::new(
        nt_rule_list,
        vec![nt_rule_list.into(), t_pipe.into(), t_ident.into()],
    ));
    g.add_rule(Rule::new(nt_rule_list, vec![t_ident.into()]));

    // Compute the item sets for the grammar.
    let is = ItemSets::compute(&g);
    eprintln!("Perplex Grammar Item Sets:");
    eprintln!("{}", is.pretty(&g));

    // Configure the code generation backend.
    let mut backend = Backend::new();

    backend.add_nonterminal(nt_desc, "()");
    backend.add_nonterminal(nt_item, "()");
    backend.add_nonterminal(nt_token_decl, "()");
    backend.add_nonterminal(nt_rule_decl, "()");
    backend.add_nonterminal(nt_rule_list, "()");

    backend.add_terminal(grammar::END, "None");
    backend.add_terminal(t_ident, "Some(Token::Ident(_))");
    backend.add_terminal(t_kw_token, "Some(Token::Keyword(Keyword::Token))");
    backend.add_terminal(t_period, "Some(Token::Period)");
    backend.add_terminal(t_colon, "Some(Token::Colon)");
    backend.add_terminal(t_comma, "Some(Token::Comma)");
    backend.add_terminal(t_semicolon, "Some(Token::Semicolon)");
    backend.add_terminal(t_pipe, "Some(Token::Pipe)");

    // Generate the parser code.
    let mut path = PathBuf::from(file!());
    path.pop();
    path.pop();
    path.push("src");
    path.push("parser_states.rs");
    eprintln!("Generating parser code in {:?}", path);
    let sm = StateMachine::try_from(&is).expect("failed to generate state machine");
    generate_parser(&mut File::create(path).unwrap(), &backend, &sm, &g)
        .expect("failed to generate parser code");
}
