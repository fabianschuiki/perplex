// Copyright (c) 2018 Fabian Schuiki

//! First set computation.
//!
//! This module implements computation of the first sets for a grammar. The
//! first set of a rule states all nonterminals that can appear as its first
//! symbol. Since rules may contain other rules and epsilon items, computation
//! is somewhat tricky.

use std::mem::swap;
use std::iter::repeat;
use bit_set::BitSet;
use grammar::{Grammar, Symbol};

/// All first sets of a grammar.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FirstSets(Vec<FirstSet>);

/// The first set of a nonterminal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FirstSet {
    /// The first symbols.
    symbols: BitSet,
    /// Whether one of the productions is empty.
    has_epsilon: bool,
}

impl FirstSets {
    /// Compute the first sets of a grammar.
    pub fn compute(grammar: &Grammar) -> FirstSets {
        compute(grammar)
    }
}

/// The meat of this module. Computes the first set for each rule in a grammar.
fn compute(grammar: &Grammar) -> FirstSets {
    let num_term = grammar.terminal_id_bound();
    let num_nonterm = grammar.nonterminal_id_bound();

    // Determine the sets of nonterminals to be updated.
    let mut update = BitSet::with_capacity(num_nonterm);
    let mut next_update = BitSet::with_capacity(num_nonterm);
    for rule in grammar.rules() {
        update.insert(rule.name().as_usize());
    }

    // Create the initial empty first sets. These will be populated in the main
    // loop.
    let mut fs = FirstSets(
        repeat(FirstSet {
            symbols: BitSet::with_capacity(num_term),
            has_epsilon: false,
        }).take(num_nonterm)
            .collect(),
    );

    // Create a list to keep track of dependencies between the rules.
    let mut deps: Vec<BitSet> = repeat(BitSet::with_capacity(num_nonterm))
        .take(num_nonterm)
        .collect();

    // This is the main update loop which processes nonterminals in sets.
    while !update.is_empty() {
        for current in update.iter() {
            println!("{}:", current);
            if next_update.contains(current) {
                continue;
            }
            let mut new_fs = fs.0[current].clone();

            // Udpate the first set and dependencies.
            for rule in grammar.rules().filter(|r| r.name().as_usize() == current) {
                let tight = collect_symbols(rule.symbols(), &mut |symbol: &Symbol| match *symbol {
                    Symbol::Terminal(id) => {
                        new_fs.symbols.insert(id.as_usize());
                        true
                    }
                    Symbol::Nonterminal(id) => {
                        deps[id.as_usize()].insert(current);
                        new_fs.symbols.union_with(&fs.0[id.as_usize()].symbols);
                        !new_fs.has_epsilon
                    }
                    _ => unreachable!(),
                });
                new_fs.has_epsilon |= !tight;
            }

            // If the first set has changed, trigger an update of everything
            // that depends on us.
            if new_fs != fs.0[current] {
                fs.0[current] = new_fs;
                next_update.union_with(&deps[current]);
            }
        }

        // If we've cleared the update set, swap in the next update set.
        swap(&mut update, &mut next_update);
        next_update.clear();
    }

    fs
}

/// Call a closure on each possible first symbol.
///
/// Given a sequence of symbols, determine which ones should belong into the
/// first set. Returns `true` if the sequence is *tight*, that is, it does not
/// contain epsilon. A sequence with a terminal is tight. Nonterminals may or
/// may not be tight, depending on their first set. The callback function `f`
/// must return `true` if the symbol is a terminal under all circumstances, i.e.
/// if it causes the sequence to be tight.
fn collect_symbols<'a, I, F>(symbols: I, f: &mut F) -> bool
where
    I: IntoIterator<Item = &'a Symbol>,
    F: FnMut(&Symbol) -> bool,
{
    for symbol in symbols {
        let tight = match *symbol {
            Symbol::Terminal(_) => f(symbol),
            Symbol::Nonterminal(_) => f(symbol),
            Symbol::Group(ref symbols) => collect_symbols(symbols, f),
            Symbol::Optional(ref symbol) => {
                collect_symbols(Some(symbol.as_ref()), f);
                false
            }
        };
        if tight {
            return true;
        }
    }
    false
}
