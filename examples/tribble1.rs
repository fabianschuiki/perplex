// Copyright (c) 2018 Fabian Schuiki
extern crate perplex;

use perplex::grammar::{Grammar, Rule};
use perplex::item_set::ItemSets;

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
}
