// Copyright (c) 2018 Fabian Schuiki
#[macro_use]
extern crate clap;
extern crate memmap;
extern crate perplex;
extern crate stderrlog;

use std::fs::File;
use std::str::FromStr;

use clap::{App, Arg};
use memmap::Mmap;
use perplex::item_set::{Action, ItemSets};
use perplex::machine::StateMachine;
use perplex::backend::generate_parser;
use perplex::parser;
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
            Arg::with_name("dump_grammar")
                .long("dump-grammar")
                .help("Dump the grammar"),
        )
        .arg(
            Arg::with_name("dump_ext_grammar")
                .long("dump-ext-grammar")
                .help("Dump the extended grammar"),
        )
        .arg(
            Arg::with_name("synth_ast")
                .long("synth-ast")
                .help("Synthesize the AST"),
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
        parser::parse_str(text)
    };

    if matches.is_present("dump_ast") {
        println!("{:#?}", desc);
        return;
    }

    // Create a grammar and backend description from the parsed AST.
    let (grammar, backend) = parser::make_grammar(&desc);
    if matches.is_present("dump_grammar") {
        println!("{:#?}", grammar);
        return;
    }

    let ext_grammar = parser::make_ext_grammar(&desc);
    if matches.is_present("dump_ext_grammar") {
        println!("{:#?}", ext_grammar);
        return;
    }
    if matches.is_present("synth_ast") {
        perplex::ast_synth::AstSynth::with_grammar(&ext_grammar);
        return;
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
            println!("#{}: Conflict in state {:?}", i, conflict.item_set);
            println!("    Cannot decide whether to:");
            for &action in &conflict.actions {
                match action {
                    Action::Reduce(rule_id) => {
                        println!("    Reduce as:");
                        println!("        {}", grammar[rule_id].pretty(&grammar))
                    }
                    Action::Shift(_) => {
                        println!("    Continue as:");
                        for item in is[conflict.item_set]
                            .items()
                            .iter()
                            .filter(|item| item.action() == Some((conflict.symbol, action)))
                        {
                            println!("        {}", item.pretty(&grammar));
                        }
                    }
                }
            }
        }
        for (i, conflict) in conflicts.iter().enumerate() {
            let arc = glr::find_conflict_arc(&conflict, &grammar, &is);
            println!("#{}: {:#?}", i, arc);
            let reconvs = glr::find_reconvergences(&arc);
            for (n, reconv) in reconvs.into_iter().enumerate() {
                let ambig = glr::find_local_ambiguity(&reconv, &arc);
                let unify = glr::resolve_local_ambiguity(&ambig, &grammar, &is);
                println!(
                    "#{}.{}: Ambiguity between {:?} and {:?}",
                    i, n, ambig.first, ambig.last
                );
                for seq in &ambig.seqs {
                    println!("    States {:?} {:?} {:?}", ambig.first, seq, ambig.last);
                }
                println!("    Resolve by replacing the following rules:");
                for slice in &unify {
                    println!(
                        "    {} (the `{}` part)",
                        grammar[slice.rule].pretty(&grammar),
                        slice.pretty(&grammar)
                    );
                }
                let rule = &grammar[unify[0].rule];
                println!("    With:");
                print!("    [{} ->", rule.name().pretty(&grammar));
                for i in 0..unify[0].from {
                    print!(" {}", rule.symbols()[i].pretty(&grammar));
                }
                print!(" X");
                for i in unify[0].to..rule.symbols().len() {
                    print!(" {}", rule.symbols()[i].pretty(&grammar));
                }
                println!("]");
                println!("    Where `X` represents all of the removed parts.");
            }
        }
        // let ga = GlrAnalysis::compute(&grammar, &is);
        // println!("{:#?}", ga);
        return;
    }

    // Generate the associated state machine.
    let sm = StateMachine::try_from(&is).expect("failed to generate state machine");
    let stdout = std::io::stdout();
    generate_parser(&mut stdout.lock(), &backend, &sm, &grammar)
        .expect("failed to generate parser code");
}
