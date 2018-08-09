// Copyright (c) 2018 Fabian Schuiki
extern crate perplex;

use perplex::grammar::{Grammar, Rule};
use perplex::item_set::ItemSets;
use perplex::glr::GlrAnalysis;

#[allow(non_snake_case)]
fn main() {
    // Build the following grammar which has a shift/reduce conflict in the
    // first item set, which can be resolved with an additional lookahead token.
    //
    //   A : B d e | C d e;
    //   B : d;
    //   C : epsilon;
    //
    let mut g = Grammar::new();

    let nt_a = g.add_nonterminal("A");
    let nt_b = g.add_nonterminal("B");
    let nt_c = g.add_nonterminal("C");
    let t_d = g.add_terminal("d");
    let t_e = g.add_terminal("e");

    g.add_rule(Rule::new(nt_a, vec![nt_b.into(), t_d.into(), t_e.into()]));
    g.add_rule(Rule::new(nt_a, vec![nt_c.into(), t_d.into(), t_e.into()]));
    g.add_rule(Rule::new(nt_b, vec![t_d.into()]));
    g.add_rule(Rule::new(nt_c, vec![]));

    // Compute the item sets for the grammar.
    let is = ItemSets::compute(&g);
    println!("{}", is.pretty(&g));

    // Perform the GLR analysis.
    let ga = GlrAnalysis::compute(&g, &is);
    println!("{:#?}", ga);

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
