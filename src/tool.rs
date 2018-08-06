// Copyright (c) 2018 Fabian Schuiki
#[macro_use]
extern crate clap;
extern crate perplex;

use clap::App;
use perplex::grammar::{Grammar, Rule};
use perplex::item_set::ItemSets;
use perplex::machine::StateMachine;

#[allow(non_snake_case)]
#[allow(unused_variables)]
fn main() {
    let _matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .get_matches();
    println!("This is perplexing");

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

    // Construct the item sets for the grammar.
    let is = ItemSets::compute(&g);
    println!("item sets:");
    println!("{}", is.pretty(&g));

    // TODO: Create the state machine from the item sets.
    let sm = StateMachine::try_from(&is).expect("unable to create state machine");

    // TODO: Create a backend with proper names for the symbols.
    // TODO: Run code generation on the backend.
}
