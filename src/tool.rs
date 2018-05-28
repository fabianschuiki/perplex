// Copyright (c) 2018 Fabian Schuiki
#[macro_use]
extern crate clap;
extern crate perplex;

use clap::App;
use perplex::grammar::*;

fn main() {
    let _matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .get_matches();
    println!("This is perplexing");

    // Build a dummy debug grammar.
    let mut g = Grammar::new();
    let mut r = Rule::new(g.alloc_name("A".into()), "A".into());
    let p = Production::new(r.alloc_id(), vec![]);
    r.add_production(p);
    g.add_rule(r);
    println!("grammar: {:#?}", g);
}
