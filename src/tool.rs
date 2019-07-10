// Copyright (c) 2018 Fabian Schuiki
#[macro_use]
extern crate clap;
extern crate memmap;
extern crate perplex;
extern crate stderrlog;

use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::str::FromStr;

use clap::{App, Arg};
use memmap::Mmap;
use perplex::backend::generate_parser;
use perplex::glr;
use perplex::grammar::{Grammar, NonterminalId, RuleId, Symbol, TerminalId};
use perplex::item_set::{Action, ItemSetId, ItemSets};
use perplex::machine::StateMachine;
use perplex::parser;

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
        .arg(
            Arg::with_name("report_conflicts")
                .long("report-conflicts")
                .help("Report unresolved conflicts in the grammar"),
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
                }
                .exit()
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
    let ext_grammar = parser::make_ext_grammar(&desc);
    if matches.is_present("dump_ext_grammar") {
        println!("{:#?}", ext_grammar);
        return;
    }
    let (synth, ext_grammar) = perplex::ast_synth::AstSynth::with_grammar(ext_grammar);
    if matches.is_present("synth_ast") {
        println!("{}", synth.generate_ast());
        println!("");
        println!("{}", synth.generate_reducers());
        return;
    }
    let (grammar, backend) = ext_grammar.lower();
    if matches.is_present("dump_grammar") {
        println!("{}", grammar);
        return;
    }

    // Determine the item sets.
    let is = ItemSets::compute(&grammar);
    if matches.is_present("dump_sets") {
        println!("{}", is.pretty(&grammar));
        return;
    }

    // Report conflicts.
    if matches.is_present("report_conflicts") {
        report_conflicts(&grammar, &is);
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

    // Generate the AST and reduction functions.
    let synth_ast = synth.generate_ast();
    let synth_reducers = synth.generate_reducers();
    if !synth_ast.is_empty() {
        println!("");
        println!("{}", synth_ast);
    }
    if !synth_reducers.is_empty() {
        println!("");
        println!("{}", synth_reducers);
    }
}

fn report_conflicts(grammar: &Grammar, is: &ItemSets) {
    let conflicts = glr::find_conflicts(&is);
    for (i, conflict) in conflicts.iter().enumerate() {
        println!(
            "#{}: Conflict in state {:?} when seeing {}",
            i,
            conflict.item_set,
            conflict.symbol.pretty(grammar)
        );
        find_example_prefix(grammar, is, conflict.item_set);
        println!("    Cannot decide whether to:");
        for &action in &conflict.actions {
            match action {
                Action::Reduce(rule_id) => {
                    println!("    Reduce as:");
                    println!("        {}", grammar[rule_id].pretty(grammar))
                }
                Action::Shift(_) => {
                    println!("    Continue to state:");
                    for item in is[conflict.item_set]
                        .items()
                        .iter()
                        .filter(|item| item.action() == Some((conflict.symbol, action)))
                    {
                        println!("        {}", item.pretty(grammar));
                    }
                }
            }
        }
    }
}

fn find_example_prefix(grammar: &Grammar, is: &ItemSets, start: ItemSetId) {
    // eprintln!("Finding state prefix from {:?}", start);

    // Determine the depth of each item set as a heuristic for how far away from
    // the root rule it is.
    let mut item_set_depths = HashMap::new();
    let mut item_set_todo = VecDeque::new();
    item_set_depths.insert(ItemSetId::from_usize(0), 0);
    item_set_todo.push_back((ItemSetId::from_usize(0), 0));
    while let Some((set_id, depth)) = item_set_todo.pop_front() {
        for &(_, action) in is[set_id].actions() {
            match action {
                Action::Shift(other_set) => {
                    if !item_set_depths.contains_key(&other_set) {
                        item_set_depths.insert(other_set, depth + 1);
                        item_set_todo.push_back((other_set, depth + 1));
                    }
                }
                _ => (),
            }
        }
    }
    // eprintln!("item set depths: {:#?}", item_set_depths);

    // Determine the item set predecessor table.
    let mut predecessors = HashMap::new();
    for set in is.all() {
        for &(_, action) in set.actions() {
            match action {
                Action::Shift(other_set) => {
                    predecessors
                        .entry(other_set)
                        .or_insert_with(HashSet::new)
                        .insert(set.id());
                }
                _ => (),
            }
        }
    }
    // eprintln!("predecessors: {:#?}", predecessors);

    // Find a sequence of item sets that leads to `start`.
    let prefix = match traverse_to_root(start, &predecessors, &item_set_depths, &mut HashSet::new())
    {
        Some(p) => p,
        None => return,
    };
    // eprintln!("{:?}", prefix);

    // Expand the item sets to a sequence of symbols.
    let mut symbols = Vec::new();
    let mut prev_set = prefix[0];
    for &next_set in &prefix[1..] {
        let item_set = &is[prev_set];
        symbols.push(
            item_set
                .actions()
                .filter(|(_, action)| *action == Action::Shift(next_set))
                .map(|(s, _)| s)
                .nth(0)
                .unwrap(),
        );
        prev_set = next_set;
    }
    let exp_tree = MinimalExpansionTree::new(grammar);
    // eprintln!("exp_tree: {:#?}", exp_tree);
    let tokens = symbols
        .into_iter()
        .flat_map(|&sym| exp_tree.expand_symbol(sym).into_iter())
        .map(|sym| format!("{}", sym.pretty(grammar)))
        .collect::<Vec<_>>()
        .join(" ");
    println!("        {}", tokens);
}

fn traverse_to_root(
    from: ItemSetId,
    predecessors: &HashMap<ItemSetId, HashSet<ItemSetId>>,
    depths: &HashMap<ItemSetId, usize>,
    visited: &mut HashSet<ItemSetId>,
) -> Option<Vec<ItemSetId>> {
    if from == ItemSetId::from_usize(0) {
        return Some(vec![from]);
    }
    visited.insert(from);
    // eprintln!("traverse_to_root({:?})", from);
    let mut candidates: Vec<_> = predecessors[&from]
        .iter()
        .filter(|&id| !visited.contains(id))
        .map(|&id| (depths[&id], id))
        .collect();
    candidates.sort();
    // eprintln!("  candidates: {:?}", candidates);
    for (_, id) in candidates {
        let mut prefix = match traverse_to_root(id, predecessors, depths, visited) {
            Some(p) => p,
            None => continue,
        };
        prefix.push(from);
        // eprintln!("found {:?}", id);
        return Some(prefix);
    }
    visited.remove(&from);
    None
}

/// A tree representing the expansions of a grammar's rules with the least
/// number of tokens.
#[derive(Debug)]
pub struct MinimalExpansionTree<'g> {
    grammar: &'g Grammar,
    min: HashMap<NonterminalId, RuleId>,
    lengths: HashMap<RuleId, usize>,
}

impl<'g> MinimalExpansionTree<'g> {
    /// Create a new tree.
    pub fn new(grammar: &'g Grammar) -> Self {
        let mut tree = Self {
            grammar,
            min: Default::default(),
            lengths: Default::default(),
        };
        for nt in grammar.nonterminals() {
            tree.compute(nt, &mut HashSet::new());
        }
        tree
    }

    fn compute(&mut self, nt: NonterminalId, visited: &mut HashSet<NonterminalId>) -> usize {
        if let Some(min) = self.min.get(&nt) {
            return self.lengths[min];
        }
        visited.insert(nt);
        let min = self
            .grammar
            .rules_for_nonterminal(nt)
            .map(|&rid| (self.compute_rule(rid, visited), rid))
            .min()
            .unwrap();
        self.min.insert(nt, min.1);
        visited.remove(&nt);
        min.0
    }

    fn compute_rule(&mut self, rid: RuleId, visited: &mut HashSet<NonterminalId>) -> usize {
        if let Some(&l) = self.lengths.get(&rid) {
            return l;
        }
        let mut l = 0;
        for &sym in self.grammar[rid].symbols() {
            match sym {
                Symbol::Terminal(_) => {
                    l += 1;
                }
                Symbol::Nonterminal(nt) if visited.contains(&nt) => {
                    l = std::usize::MAX;
                    break;
                }
                Symbol::Nonterminal(nt) => {
                    let dl = self.compute(nt, visited);
                    if dl == std::usize::MAX {
                        l = dl;
                        break;
                    } else {
                        l += dl;
                    }
                }
            }
        }
        self.lengths.insert(rid, l);
        l
    }

    /// Expand a nonterminal into its shortest sequence of terminals.
    pub fn expand_nonterminal(&self, nt: NonterminalId) -> Vec<TerminalId> {
        let mut b = vec![];
        self.expand_nonterminal_into(nt, &mut b);
        b
    }

    /// Expand a symbol into its shortest sequence of terminals.
    pub fn expand_symbol(&self, sym: Symbol) -> Vec<TerminalId> {
        let mut b = vec![];
        self.expand_symbol_into(sym, &mut b);
        b
    }

    /// Expand a rule into its shortest sequence of terminals.
    pub fn expand_rule(&self, rid: RuleId) -> Vec<TerminalId> {
        let mut b = vec![];
        self.expand_rule_into(rid, &mut b);
        b
    }

    /// Expand a nonterminal into its shortest sequence of terminals.
    pub fn expand_nonterminal_into(&self, nt: NonterminalId, into: &mut Vec<TerminalId>) {
        if let Some(&rid) = self.min.get(&nt) {
            self.expand_rule_into(rid, into);
        }
    }

    /// Expand a symbol into its shortest sequence of terminals.
    pub fn expand_symbol_into(&self, sym: Symbol, into: &mut Vec<TerminalId>) {
        match sym {
            Symbol::Terminal(t) => into.push(t),
            Symbol::Nonterminal(nt) => self.expand_nonterminal_into(nt, into),
        }
    }

    /// Expand a rule into its shortest sequence of terminals.
    pub fn expand_rule_into(&self, rid: RuleId, into: &mut Vec<TerminalId>) {
        for &sym in self.grammar[rid].symbols() {
            self.expand_symbol_into(sym, into);
        }
    }
}
