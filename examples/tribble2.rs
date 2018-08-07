// Copyright (c) 2018 Fabian Schuiki
extern crate perplex;

use std::fs::File;

use perplex::grammar::{Grammar, Rule};
use perplex::item_set::ItemSets;
use perplex::machine::StateMachine;
use perplex::backend::{generate_parser, Backend};

#[allow(non_snake_case)]
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

    // Compute the item sets for the grammar.
    let is = ItemSets::compute(&g);
    println!("{}", is.pretty(&g));

    // Generate the parser code.
    // let sm = StateMachine::try_from(&is).unwrap();
    // let backend = Backend::new();
    // generate_parser(
    //     &mut File::create("tests/generated/tribble2_parser.rs").unwrap(),
    //     &backend,
    //     &sm,
    //     &g,
    // ).unwrap();
}
