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
    let nt_token_name = g.add_nonterminal("token_name");
    let nt_rule_decl = g.add_nonterminal("rule_decl");
    let nt_rule_list = g.add_nonterminal("rule_list");
    let nt_variant = g.add_nonterminal("variant");
    let nt_sequence_or_epsilon = g.add_nonterminal("sequence_or_epsilon");
    let nt_sequence = g.add_nonterminal("sequence");

    let t_ident = g.add_terminal("IDENT");
    let t_code = g.add_terminal("CODE");
    let t_kw_token = g.add_terminal("'token'");
    let t_kw_epsilon = g.add_terminal("'epsilon'");
    let t_kw_end = g.add_terminal("'end'");
    let t_lparen = g.add_terminal("'('");
    let t_rparen = g.add_terminal("')'");
    let t_lbrace = g.add_terminal("'{'");
    let t_rbrace = g.add_terminal("'}'");
    let t_period = g.add_terminal("'.'");
    let t_colon = g.add_terminal("':'");
    let t_comma = g.add_terminal("','");
    let t_semicolon = g.add_terminal("';'");
    let t_pipe = g.add_terminal("'|'");

    // desc : desc item | item | desc ';' | ';' ;
    let r_desc_a = g.add_rule(Rule::new(nt_desc, vec![nt_desc.into(), nt_item.into()]));
    let r_desc_b = g.add_rule(Rule::new(nt_desc, vec![nt_item.into()]));
    let r_desc_c = g.add_rule(Rule::new(nt_desc, vec![nt_desc.into(), t_semicolon.into()]));
    let r_desc_d = g.add_rule(Rule::new(nt_desc, vec![t_semicolon.into()]));

    // item : token_decl | rule_decl ;
    let r_item_a = g.add_rule(Rule::new(nt_item, vec![nt_token_decl.into()]));
    let r_item_b = g.add_rule(Rule::new(nt_item, vec![nt_rule_decl.into()]));

    // token_decl : 'token' token_name '(' CODE ')' ';' ;
    let r_token_decl = g.add_rule(Rule::new(
        nt_token_decl,
        vec![
            t_kw_token.into(),
            nt_token_name.into(),
            t_lparen.into(),
            t_code.into(),
            t_rparen.into(),
            t_semicolon.into(),
        ],
    ));

    // token_name : IDENT | 'end' ;
    let r_token_name_a = g.add_rule(Rule::new(nt_token_name, vec![t_ident.into()]));
    let r_token_name_b = g.add_rule(Rule::new(nt_token_name, vec![t_kw_end.into()]));

    // rule_decl : IDENT '(' CODE ')' '{' rule_list '}' ;
    let r_rule_decl = g.add_rule(Rule::new(
        nt_rule_decl,
        vec![
            t_ident.into(),
            t_lparen.into(),
            t_code.into(),
            t_rparen.into(),
            t_lbrace.into(),
            nt_rule_list.into(),
            t_rbrace.into(),
        ],
    ));

    // rule_list : rule_list variant | variant;
    let r_rule_list_a = g.add_rule(Rule::new(
        nt_rule_list,
        vec![nt_rule_list.into(), nt_variant.into()],
    ));
    let r_rule_list_b = g.add_rule(Rule::new(nt_rule_list, vec![nt_variant.into()]));

    // variant : sequence_or_epsilon '(' CODE ')' ';'
    let r_variant = g.add_rule(Rule::new(
        nt_variant,
        vec![
            nt_sequence_or_epsilon.into(),
            t_lparen.into(),
            t_code.into(),
            t_rparen.into(),
            t_semicolon.into(),
        ],
    ));

    // sequence_or_epsilon : sequence | 'epsilon' ;
    let r_sequence_or_epsilon_a =
        g.add_rule(Rule::new(nt_sequence_or_epsilon, vec![nt_sequence.into()]));
    let r_sequence_or_epsilon_b =
        g.add_rule(Rule::new(nt_sequence_or_epsilon, vec![t_kw_epsilon.into()]));

    // sequence : sequence IDENT | IDENT ;
    let r_sequence_a = g.add_rule(Rule::new(
        nt_sequence,
        vec![nt_sequence.into(), t_ident.into()],
    ));
    let r_sequence_b = g.add_rule(Rule::new(nt_sequence, vec![t_ident.into()]));

    // Compute the item sets for the grammar.
    let is = ItemSets::compute(&g);
    eprintln!("Perplex Grammar Item Sets:");
    eprintln!("{}", is.pretty(&g));

    // Configure the code generation backend.
    let mut backend = Backend::new();

    backend.add_nonterminal(nt_desc, "ast::Desc");
    backend.add_nonterminal(nt_item, "ast::Item");
    backend.add_nonterminal(nt_token_decl, "ast::TokenDecl");
    backend.add_nonterminal(nt_token_name, "ast::TokenName");
    backend.add_nonterminal(nt_rule_decl, "ast::RuleDecl");
    backend.add_nonterminal(nt_rule_list, "Vec<ast::Variant>");
    backend.add_nonterminal(nt_variant, "ast::Variant");
    backend.add_nonterminal(nt_sequence_or_epsilon, "Vec<String>");
    backend.add_nonterminal(nt_sequence, "Vec<String>");

    backend.add_terminal(grammar::END, "None");
    backend.add_terminal(t_ident, "Some(Token::Ident(_))");
    backend.add_terminal(t_code, "Some(Token::Code(_))");
    backend.add_terminal(t_kw_token, "Some(Token::Keyword(Keyword::Token))");
    backend.add_terminal(t_kw_epsilon, "Some(Token::Keyword(Keyword::Epsilon))");
    backend.add_terminal(t_kw_end, "Some(Token::Keyword(Keyword::End))");
    backend.add_terminal(t_lparen, "Some(Token::LParen)");
    backend.add_terminal(t_rparen, "Some(Token::RParen)");
    backend.add_terminal(t_lbrace, "Some(Token::LBrace)");
    backend.add_terminal(t_rbrace, "Some(Token::RBrace)");
    backend.add_terminal(t_period, "Some(Token::Period)");
    backend.add_terminal(t_colon, "Some(Token::Colon)");
    backend.add_terminal(t_comma, "Some(Token::Comma)");
    backend.add_terminal(t_semicolon, "Some(Token::Semicolon)");
    backend.add_terminal(t_pipe, "Some(Token::Pipe)");

    backend.add_reduction_function(r_desc_a, "reduce_desc_a");
    backend.add_reduction_function(r_desc_b, "reduce_desc_b");
    backend.add_reduction_function(r_desc_c, "reduce_desc_c");
    backend.add_reduction_function(r_desc_d, "reduce_desc_d");
    backend.add_reduction_function(r_item_a, "reduce_item_a");
    backend.add_reduction_function(r_item_b, "reduce_item_b");
    backend.add_reduction_function(r_token_decl, "reduce_token_decl");
    backend.add_reduction_function(r_token_name_a, "reduce_token_name_a");
    backend.add_reduction_function(r_token_name_b, "reduce_token_name_b");
    backend.add_reduction_function(r_rule_decl, "reduce_rule_decl");
    backend.add_reduction_function(r_rule_list_a, "reduce_rule_list_a");
    backend.add_reduction_function(r_rule_list_b, "reduce_rule_list_b");
    backend.add_reduction_function(r_variant, "reduce_variant");
    backend.add_reduction_function(r_sequence_or_epsilon_a, "reduce_sequence_or_epsilon_a");
    backend.add_reduction_function(r_sequence_or_epsilon_b, "reduce_sequence_or_epsilon_b");
    backend.add_reduction_function(r_sequence_a, "reduce_sequence_a");
    backend.add_reduction_function(r_sequence_b, "reduce_sequence_b");

    // // Generate the parser code.
    // let sm = StateMachine::try_from(&is).expect("failed to generate state machine");
    // let stdout = std::io::stdout();
    // generate_parser(&mut stdout.lock(), &backend, &sm, &g).expect("failed to generate parser code");

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
