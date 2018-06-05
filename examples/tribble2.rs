// Copyright (c) 2018 Fabian Schuiki
extern crate perplex;

use perplex::grammar::{Grammar, Rule};
use perplex::first::FirstSets;

#[allow(non_snake_case)]
#[allow(unused_variables)]
fn main() {
    // Build the grammar in David Tribble's example 1.
    let mut g = Grammar::new();
    let ntExpr = g.add_nonterminal("Expr");
    let ntFactor = g.add_nonterminal("Factor");
    let tnum = g.add_terminal("num");
    let tlpar = g.add_terminal("'('");
    let trpar = g.add_terminal("')'");
    let tplus = g.add_terminal("'+'");
    g.add_rule(Rule::new(ntExpr, vec![ntFactor.into()]));
    g.add_rule(Rule::new(
        ntExpr,
        vec![tlpar.into(), ntExpr.into(), trpar.into()],
    ));
    g.add_rule(Rule::new(ntFactor, vec![tnum.into()]));
    g.add_rule(Rule::new(ntFactor, vec![tplus.into(), ntFactor.into()]));
    g.add_rule(Rule::new(
        ntFactor,
        vec![ntFactor.into(), tplus.into(), tnum.into()],
    ));

    // Compute the first sets for the grammar.
    let fs = FirstSets::compute(&g);
    println!("{:#?}", fs);
}
