// Copyright (c) 2018 Fabian Schuiki
#[macro_use]
extern crate clap;
#[macro_use]
extern crate log;
extern crate memmap;
extern crate perplex;
extern crate stderrlog;

use std::fs::File;
use std::collections::HashMap;
use std::str::FromStr;

use clap::{App, Arg};
use memmap::Mmap;
use perplex::grammar::{Grammar, NonterminalId, Rule, Symbol, TerminalId};
use perplex::item_set::ItemSets;
use perplex::machine::StateMachine;
// use perplex::backend::{generate_parser, Backend};
use perplex::lexer::Lexer;
use perplex::parser::parse_iter;
use perplex::glr;

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
            Arg::with_name("verbosity")
                .short("v")
                .multiple(true)
                .help("Increase message verbosity"),
        )
        .arg(
            Arg::with_name("quiet")
                .short("q")
                .help("Silence all output"),
        )
        .arg(
            Arg::with_name("timestamp")
                .short("t")
                .help("Prefix messages with a timestamp")
                .takes_value(true)
                .possible_values(&["sec", "ms", "ns"]),
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
        .arg(
            Arg::with_name("conflict_arcs")
                .long("conflict-arcs")
                .help("Compute conflict arcs in the state space"),
        )
        .get_matches();

    // Process the logging options and configure the stderrlog crate.
    let verbose = matches.occurrences_of("verbosity") as usize;
    let quiet = matches.is_present("quiet");
    let ts = matches
        .value_of("timestamp")
        .map(|v| {
            stderrlog::Timestamp::from_str(v).unwrap_or_else(|_| {
                clap::Error {
                    message: "invalid value for 'timestamp'".into(),
                    kind: clap::ErrorKind::InvalidValue,
                    info: None,
                }.exit()
            })
        })
        .unwrap_or(stderrlog::Timestamp::Off);

    stderrlog::new()
        .module(module_path!())
        .quiet(quiet)
        .verbosity(verbose)
        .timestamp(ts)
        .init()
        .unwrap();

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

    // Compute conflict arcs.
    if matches.is_present("conflict_arcs") {
        let conflicts = glr::find_conflicts(&is);
        for (i, conflict) in conflicts.iter().enumerate() {
            println!("#{}: {:#?}", i, conflict);
        }
        for (i, conflict) in conflicts.iter().enumerate() {
            let arc = glr::find_conflict_arc(&conflict, &grammar, &is);
            println!("#{}: {:#?}", i, arc);
        }
        // let ga = GlrAnalysis::compute(&grammar, &is);
        // println!("{:#?}", ga);
        return;
    }

    // Generate the associated state machine.
    let _sm = StateMachine::try_from(&is).expect("failed to generate state machine");
}
