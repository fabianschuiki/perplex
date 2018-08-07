// Copyright (c) 2018 Fabian Schuiki
extern crate perplex;

use std::fs::File;

use perplex::grammar::{self, Grammar, Rule};
use perplex::item_set::ItemSets;
use perplex::machine::StateMachine;
use perplex::backend::{generate_parser, Backend};

#[allow(non_snake_case)]
fn main() {
    // Build the grammar in David Tribble's example 11.
    let mut g = Grammar::new();
    let (ntS, ntA, ntB) = (
        g.add_nonterminal("S"),
        g.add_nonterminal("A"),
        g.add_nonterminal("B"),
    );
    let (ta, tb, tc, td, te) = (
        g.add_terminal("a"),
        g.add_terminal("b"),
        g.add_terminal("c"),
        g.add_terminal("d"),
        g.add_terminal("e"),
    );
    g.add_rule(Rule::new(ntS, vec![ta.into(), ntA.into(), td.into()]));
    g.add_rule(Rule::new(ntS, vec![ta.into(), ntB.into(), te.into()]));
    g.add_rule(Rule::new(ntS, vec![tb.into(), ntA.into(), te.into()]));
    g.add_rule(Rule::new(ntS, vec![tb.into(), ntB.into(), td.into()]));
    g.add_rule(Rule::new(ntA, vec![tc.into()]));
    g.add_rule(Rule::new(ntB, vec![tc.into()]));

    // Compute the first sets for the grammar.
    let is = ItemSets::compute(&g);
    println!("{}", is.pretty(&g));

    // Generate the parser code.
    let sm = StateMachine::try_from(&is).unwrap();
    let mut backend = Backend::new();
    backend.add_nonterminal(ntS, "NodeS");
    backend.add_nonterminal(ntA, "NodeA");
    backend.add_nonterminal(ntB, "NodeB");
    backend.add_terminal(grammar::END, "Token::Eof");
    backend.add_terminal(ta, "Token::A");
    backend.add_terminal(tb, "Token::B");
    backend.add_terminal(tc, "Token::C");
    backend.add_terminal(td, "Token::D");
    backend.add_terminal(te, "Token::E");
    generate_parser(
        &mut File::create("tests/generated/tribble1_parser.rs").unwrap(),
        &backend,
        &sm,
        &g,
    ).unwrap();
}
