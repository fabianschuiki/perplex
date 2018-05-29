// Copyright (c) 2018 Fabian Schuiki
#[macro_use]
extern crate clap;
extern crate perplex;

use clap::App;
use perplex::grammar::*;

#[allow(non_snake_case)]
fn main() {
    let _matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .get_matches();
    println!("This is perplexing");

    // Build the grammar in David Tribble's example.
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
    println!("{:?}", [ntS, ntA, ntB]);
    println!("{:?}", [ta, tb, tc, td, te]);
    let mut r = Rule::new(g.alloc_name("A".into()), "A".into());
    let p = Production::new(r.alloc_id(), vec![]);
    r.add_production(p);
    g.add_rule(r);
    println!("grammar: {:#?}", g);
}
