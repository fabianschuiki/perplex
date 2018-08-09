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
    //   S : A B z ;
    //
    //   B : B y C | C ;
    //   C : x ;
    //
    //   A : D | E ;
    //   D : x ;
    //   E : epsilon ;
    //
    let mut g = Grammar::new();

    let nt_s = g.add_nonterminal("S");
    let nt_a = g.add_nonterminal("A");
    let nt_b = g.add_nonterminal("B");
    let nt_c = g.add_nonterminal("C");
    let nt_d = g.add_nonterminal("D");
    let nt_e = g.add_nonterminal("E");
    let t_x = g.add_terminal("x");
    let t_y = g.add_terminal("y");
    let t_z = g.add_terminal("z");

    g.add_rule(Rule::new(nt_s, vec![nt_a.into(), nt_b.into(), t_z.into()]));
    g.add_rule(Rule::new(nt_b, vec![nt_b.into(), t_y.into(), nt_c.into()]));
    g.add_rule(Rule::new(nt_b, vec![nt_c.into()]));
    g.add_rule(Rule::new(nt_c, vec![t_x.into()]));
    g.add_rule(Rule::new(nt_a, vec![nt_d.into()]));
    g.add_rule(Rule::new(nt_a, vec![nt_e.into()]));
    g.add_rule(Rule::new(nt_d, vec![t_x.into()]));
    g.add_rule(Rule::new(nt_e, vec![]));

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
