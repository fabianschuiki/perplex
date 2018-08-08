// Copyright (c) 2018 Fabian Schuiki
#[macro_use]
extern crate clap;
extern crate memmap;
extern crate perplex;

use std::fs::File;
use std::collections::HashMap;

use clap::{App, Arg};
use memmap::Mmap;
use perplex::grammar::{Grammar, NonterminalId, Rule, Symbol, TerminalId};
use perplex::item_set::ItemSets;
use perplex::machine::StateMachine;
// use perplex::backend::{generate_parser, Backend};
use perplex::lexer::Lexer;
use perplex::parser::parse_iter;

fn main() {
    // Parse the command line arguments.
    let matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .arg(
            Arg::with_name("GRAMMAR")
                .help("The input grammar to process")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("dump_ast")
                .long("dump-ast")
                .help("Dump the input grammar AST and exit"),
        )
        .arg(
            Arg::with_name("dump_sets")
                .long("dump-sets")
                .help("Dump the generated item sets and exit"),
        )
        .get_matches();

    // Parse the input grammar description.
    let desc = {
        let path = matches.value_of("GRAMMAR").unwrap();
        let file = File::open(path).expect("failed to open grammar file");
        let mmap = unsafe { Mmap::map(&file).expect("failed to memory map the grammar file") };
        let text = unsafe { std::str::from_utf8_unchecked(&mmap) };
        let lex = Lexer::new(text.char_indices());
        parse_iter(lex.map(|(_, _, t)| t))
    };

    if matches.is_present("dump_ast") {
        println!("{:#?}", desc);
        return;
    }

    // Create a grammar and backend description from the parsed AST.
    let mut grammar = Grammar::new();
    // let mut backend = Backend::new();
    {
        // Declare the terminals and nonterminals.
        let mut token_map: HashMap<String, TerminalId> = HashMap::new();
        let mut rule_map: HashMap<String, NonterminalId> = HashMap::new();
        for d in &desc.tokens {
            token_map.insert(d.name.clone(), grammar.add_terminal(d.name.clone()));
        }
        for d in &desc.rules {
            rule_map.insert(d.name.clone(), grammar.add_nonterminal(d.name.clone()));
        }

        // Create a unified symbol lookup table.
        let mut symbol_map: HashMap<String, Symbol> = HashMap::new();
        for (n, &i) in &token_map {
            symbol_map.insert(n.clone(), i.into());
        }
        for (n, &i) in &rule_map {
            if symbol_map.insert(n.clone(), i.into()).is_some() {
                panic!("rule name `{}` conflicts with token name `{}`", n, n);
            }
        }

        // Add the rules to the grammar.
        for d in &desc.rules {
            let id = rule_map[&d.name];
            for v in &d.variants {
                let seq = v.iter()
                    .map(|v| match symbol_map.get(v) {
                        Some(&s) => s,
                        None => panic!("unknown token or rule `{}`", v),
                    })
                    .collect();
                grammar.add_rule(Rule::new(id, seq));
            }
        }
    }

    // Determine the item sets.
    let is = ItemSets::compute(&grammar);
    if matches.is_present("dump_sets") {
        println!("{}", is.pretty(&grammar));
        return;
    }

    // Generate the associated state machine.
    let _sm = StateMachine::try_from(&is).expect("failed to generate state machine");
}
